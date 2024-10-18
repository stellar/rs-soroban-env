use super::{
    func_info::HOST_FUNCTIONS,
    parsed_module::{CompilationContext, ParsedModule, VersionedContractCodeCostInputs},
};
use crate::{
    budget::{get_wasmi_config, AsBudget, Budget},
    host::metered_clone::{MeteredClone, MeteredContainer},
    xdr::{Hash, ScErrorCode, ScErrorType},
    Host, HostError, MeteredOrdMap,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    sync::{Arc, Mutex, MutexGuard},
};

/// A [ModuleCache] is a cache of a set of Wasm modules that have been parsed
/// but not yet instantiated, along with a shared and reusable [Engine] storing
/// their code. The cache must be populated eagerly with all the contracts in a
/// single [Host]'s lifecycle (at least) added all at once, since each wasmi
/// [Engine] is locked during execution and no new modules can be added to it.
#[derive(Clone, Default)]
pub struct ModuleCache {
    pub(crate) wasmi_engine: wasmi::Engine,
    pub(crate) wasmi_linker: wasmi::Linker<Host>,
    modules: ModuleCacheMap,
}

// We may use the ModuleCache from multiple C++ theads where
// there's no checking of Send+Sync but we can at least ensure
// Rust thinks its API is thread-safe.
static_assertions::assert_impl_all!(ModuleCache: Send, Sync);

// The module cache was originally designed as an immutable object
// established at host creation time and never updated. In order to support
// longer-lived modules caches, we allow construction of unmetered, "reusable"
// module maps, that imply various changes:
//
// - Modules can be added post-construction.
// - Adding an existing module is a harmless no-op, not an error.
// - The linkers are set to "maximal" mode to cover all possible imports.
// - The cache easily scales to a large number of modules, unlike MeteredOrdMap.
// - There is no metering of cache map operations.
// - The cache can be cloned, but the clone is a shallow copy.
// - The cache is mutable and shared among all copies, using a mutex.

#[derive(Clone)]
enum ModuleCacheMap {
    MeteredSingleUseMap(MeteredOrdMap<Hash, Arc<ParsedModule>, Budget>),
    UnmeteredReusableMap(Arc<Mutex<BTreeMap<Hash, Arc<ParsedModule>>>>),
}

impl Default for ModuleCacheMap {
    fn default() -> Self {
        Self::MeteredSingleUseMap(MeteredOrdMap::new())
    }
}

impl ModuleCacheMap {
    fn lock_map(
        map: &Arc<Mutex<BTreeMap<Hash, Arc<ParsedModule>>>>,
    ) -> Result<MutexGuard<BTreeMap<Hash, Arc<ParsedModule>>>, HostError> {
        map.lock()
            .map_err(|_| HostError::from((ScErrorType::Context, ScErrorCode::InternalError)))
    }

    fn is_reusable(&self) -> bool {
        matches!(self, Self::UnmeteredReusableMap(_))
    }

    fn contains_key(&self, key: &Hash, budget: &Budget) -> Result<bool, HostError> {
        match self {
            Self::MeteredSingleUseMap(map) => map.contains_key(key, budget),
            Self::UnmeteredReusableMap(map) => Ok(Self::lock_map(map)?.contains_key(key)),
        }
    }

    fn get(&self, key: &Hash, budget: &Budget) -> Result<Option<Arc<ParsedModule>>, HostError> {
        match self {
            Self::MeteredSingleUseMap(map) => Ok(map.get(key, budget)?.map(|rc| rc.clone())),
            Self::UnmeteredReusableMap(map) => {
                Ok(Self::lock_map(map)?.get(key).map(|rc| rc.clone()))
            }
        }
    }

    fn insert(
        &mut self,
        key: Hash,
        value: Arc<ParsedModule>,
        budget: &Budget,
    ) -> Result<(), HostError> {
        match self {
            Self::MeteredSingleUseMap(map) => {
                *map = map.insert(key, value, budget)?;
            }
            Self::UnmeteredReusableMap(map) => {
                Self::lock_map(map)?.insert(key, value);
            }
        }
        Ok(())
    }
}

impl ModuleCache {
    pub fn new(host: &Host) -> Result<Self, HostError> {
        let wasmi_config = get_wasmi_config(host.as_budget())?;
        let wasmi_engine = wasmi::Engine::new(&wasmi_config);

        let modules = ModuleCacheMap::MeteredSingleUseMap(MeteredOrdMap::new());
        let wasmi_linker = wasmi::Linker::new(&wasmi_engine);
        let mut cache = Self {
            wasmi_engine,
            modules,
            wasmi_linker,
        };

        // Now add the contracts and rebuild linkers restricted to them.
        cache.add_stored_contracts(host)?;
        cache.wasmi_linker = cache.make_minimal_wasmi_linker_for_cached_modules(host)?;
        Ok(cache)
    }

    pub fn new_reusable<Ctx: CompilationContext>(context: &Ctx) -> Result<Self, HostError> {
        let wasmi_config = get_wasmi_config(context.as_budget())?;
        let wasmi_engine = wasmi::Engine::new(&wasmi_config);

        let modules = ModuleCacheMap::UnmeteredReusableMap(Arc::new(Mutex::new(BTreeMap::new())));

        let wasmi_linker = Host::make_maximal_wasmi_linker(context, &wasmi_engine)?;

        Ok(Self {
            wasmi_engine,
            modules,
            wasmi_linker,
        })
    }

    pub fn is_reusable(&self) -> bool {
        self.modules.is_reusable()
    }

    pub fn add_stored_contracts(&mut self, host: &Host) -> Result<(), HostError> {
        use crate::xdr::{ContractCodeEntry, ContractCodeEntryExt, LedgerEntryData, LedgerKey};
        let storage = host.try_borrow_storage()?;
        for (k, v) in storage.map.iter(host.as_budget())? {
            // In recording mode we build the module cache *after* the contract invocation has
            // finished. This means that if any new Wasm has been uploaded, then we will add it to
            // the cache. However, in the 'real' flow we build the cache first, so any new Wasm
            // upload won't be cached. That's why we should look at the storage in its initial
            // state, which is conveniently provided by the recording mode snapshot.
            #[cfg(any(test, feature = "recording_mode"))]
            let init_value = if host.in_storage_recording_mode()? {
                storage.get_snapshot_value(host, k)?
            } else {
                v.clone()
            };
            #[cfg(any(test, feature = "recording_mode"))]
            let v = &init_value;

            if let LedgerKey::ContractCode(_) = &**k {
                if let Some((e, _)) = v {
                    if let LedgerEntryData::ContractCode(ContractCodeEntry { code, hash, ext }) =
                        &e.data
                    {
                        // We allow empty contracts in testing mode; they exist
                        // to exercise as much of the contract-code-storage
                        // infrastructure as possible, while still redirecting
                        // the actual execution into a `ContractFunctionSet`.
                        // They should never be called, so we do not have to go
                        // as far as making a fake `ParsedModule` for them.
                        if cfg!(any(test, feature = "testutils")) && code.as_slice().is_empty() {
                            continue;
                        }

                        let code_cost_inputs = match ext {
                            ContractCodeEntryExt::V0 => VersionedContractCodeCostInputs::V0 {
                                wasm_bytes: code.len(),
                            },
                            ContractCodeEntryExt::V1(v1) => VersionedContractCodeCostInputs::V1(
                                v1.cost_inputs.metered_clone(host.as_budget())?,
                            ),
                        };
                        self.parse_and_cache_module(
                            host,
                            host.get_ledger_protocol_version()?,
                            hash,
                            code,
                            code_cost_inputs,
                        )?;
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse_and_cache_module_simple<Ctx: CompilationContext>(
        &mut self,
        context: &Ctx,
        curr_ledger_protocol: u32,
        wasm: &[u8],
    ) -> Result<(), HostError> {
        let contract_id = Hash(crate::crypto::sha256_hash_from_bytes_raw(
            wasm,
            context.as_budget(),
        )?);
        self.parse_and_cache_module(
            context,
            curr_ledger_protocol,
            &contract_id,
            wasm,
            VersionedContractCodeCostInputs::V0 {
                wasm_bytes: wasm.len(),
            },
        )
    }

    pub fn parse_and_cache_module<Ctx: CompilationContext>(
        &mut self,
        context: &Ctx,
        curr_ledger_protocol: u32,
        contract_id: &Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<(), HostError> {
        if self
            .modules
            .contains_key(contract_id, context.as_budget())?
        {
            if self.modules.is_reusable() {
                return Ok(());
            } else {
                return Err(context.error(
                    crate::Error::from_type_and_code(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                    ),
                    "module cache already contains contract",
                    &[],
                ));
            }
        }
        let parsed_module = ParsedModule::new(
            context,
            curr_ledger_protocol,
            &self.wasmi_engine,
            &wasm,
            cost_inputs,
        )?;
        self.modules.insert(
            contract_id.metered_clone(context.as_budget())?,
            parsed_module,
            context.as_budget(),
        )?;
        Ok(())
    }

    fn with_minimal_import_symbols<T>(
        &self,
        host: &Host,
        callback: impl FnOnce(&BTreeSet<(&str, &str)>) -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        let mut import_symbols = BTreeSet::new();
        let ModuleCacheMap::MeteredSingleUseMap(modules) = &self.modules else {
            return Err(host.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "with_import_symbols called on non-MeteredSingleUseMap cache",
                &[],
            ));
        };
        for module in modules.values(host.as_budget())? {
            module.with_import_symbols(host, |module_symbols| {
                for hf in HOST_FUNCTIONS {
                    let sym = (hf.mod_str, hf.fn_str);
                    if module_symbols.contains(&sym) {
                        import_symbols.insert(sym);
                    }
                }
                Ok(())
            })?;
        }
        // We approximate the cost of `BTreeSet` with the cost of initializng a
        // `Vec` with the same elements, and we are doing it after the set has
        // been created. The element count has been limited/charged during the
        // parsing phase, so there is no DOS factor. We don't charge for
        // insertion/lookups, since they should be cheap and number of
        // operations on the set is limited (only used during `Linker`
        // creation).
        Vec::<(&str, &str)>::charge_bulk_init_cpy(import_symbols.len() as u64, host)?;
        callback(&import_symbols)
    }

    fn make_minimal_wasmi_linker_for_cached_modules(
        &self,
        host: &Host,
    ) -> Result<wasmi::Linker<Host>, HostError> {
        self.with_minimal_import_symbols(host, |symbols| {
            Host::make_minimal_wasmi_linker_for_symbols(host, &self.wasmi_engine, symbols)
        })
    }

    pub fn contains_module<Ctx: CompilationContext>(
        &self,
        wasm_hash: &Hash,
        context: &Ctx,
    ) -> Result<bool, HostError> {
        self.modules.contains_key(wasm_hash, context.as_budget())
    }

    pub fn get_module<Ctx: CompilationContext>(
        &self,
        context: &Ctx,
        wasm_hash: &Hash,
    ) -> Result<Option<Arc<ParsedModule>>, HostError> {
        if let Some(m) = self.modules.get(wasm_hash, context.as_budget())? {
            Ok(Some(m.clone()))
        } else {
            Ok(None)
        }
    }
}
