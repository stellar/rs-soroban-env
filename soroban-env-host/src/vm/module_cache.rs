use super::parsed_module::{CompilationContext, ParsedModule, VersionedContractCodeCostInputs};
#[cfg(any(test, feature = "testutils"))]
use crate::budget::AsBudget;
use crate::{
    budget::get_wasmi_config,
    host::metered_clone::MeteredClone,
    xdr::{Hash, ScErrorCode, ScErrorType},
    Host, HostError,
};
use std::{
    collections::BTreeMap,
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

// We may use the ModuleCache from multiple C++ threads where
// there's no checking of Send+Sync but we can at least ensure
// Rust thinks its API is thread-safe.
static_assertions::assert_impl_all!(ModuleCache: Send, Sync);

// The module cache was originally designed as an immutable object
// established at host creation time and never updated. In order to support
// longer-lived modules caches it was changed to a new form that is
// a little unlike the rest of soroban, and works differently:
//
// - Modules can be added post-construction, it's not immutable.
// - Adding an existing module is a harmless no-op, not an error.
// - The linkers are set to "maximal" mode to cover all possible imports.
// - The cache easily scales to a large number of modules, unlike MeteredOrdMap.
// - There is no metering of cache map operations.
// - The cache can be cloned, but the clone is a shallow copy.
// - The cache is mutable and shared among all copies, using a mutex.

#[derive(Clone)]
struct ModuleCacheMap(Arc<Mutex<BTreeMap<Hash, Arc<ParsedModule>>>>);

impl Default for ModuleCacheMap {
    fn default() -> Self {
        Self(Arc::new(Mutex::new(BTreeMap::new())))
    }
}

impl ModuleCacheMap {
    fn lock_map(
        map: &Arc<Mutex<BTreeMap<Hash, Arc<ParsedModule>>>>,
    ) -> Result<MutexGuard<'_, BTreeMap<Hash, Arc<ParsedModule>>>, HostError> {
        map.lock()
            .map_err(|_| HostError::from((ScErrorType::Context, ScErrorCode::InternalError)))
    }

    fn contains_key(&self, key: &Hash) -> Result<bool, HostError> {
        Ok(Self::lock_map(&self.0)?.contains_key(key))
    }

    fn get(&self, key: &Hash) -> Result<Option<Arc<ParsedModule>>, HostError> {
        Ok(Self::lock_map(&self.0)?.get(key).map(|rc| rc.clone()))
    }

    fn insert(&self, key: Hash, value: Arc<ParsedModule>) -> Result<(), HostError> {
        Self::lock_map(&self.0)?.insert(key, value);
        Ok(())
    }

    fn clear(&self) -> Result<(), HostError> {
        Self::lock_map(&self.0)?.clear();
        Ok(())
    }

    fn remove(&self, key: &Hash) -> Result<Option<Arc<ParsedModule>>, HostError> {
        Ok(Self::lock_map(&self.0)?.remove(key))
    }
}

impl ModuleCache {
    pub fn new<Ctx: CompilationContext>(context: &Ctx) -> Result<Self, HostError> {
        let wasmi_config = get_wasmi_config(context.as_budget())?;
        let wasmi_engine = wasmi::Engine::new(&wasmi_config);
        let modules = ModuleCacheMap::default();
        let wasmi_linker = Host::make_maximal_wasmi_linker(context, &wasmi_engine)?;
        Ok(Self {
            wasmi_engine,
            modules,
            wasmi_linker,
        })
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn add_stored_contracts(&self, host: &Host) -> Result<(), HostError> {
        use crate::xdr::{ContractCodeEntry, ContractCodeEntryExt, LedgerEntryData, LedgerKey};
        let storage = host.try_borrow_storage()?;
        for (k, v) in storage.map.iter(host.as_budget())? {
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
                        if code.as_slice().is_empty() {
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
        &self,
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
        &self,
        context: &Ctx,
        curr_ledger_protocol: u32,
        contract_id: &Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<(), HostError> {
        if self.modules.contains_key(contract_id)? {
            return Ok(());
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
        )?;
        Ok(())
    }

    pub fn contains_module(&self, wasm_hash: &Hash) -> Result<bool, HostError> {
        self.modules.contains_key(wasm_hash)
    }

    pub fn get_module(&self, wasm_hash: &Hash) -> Result<Option<Arc<ParsedModule>>, HostError> {
        if let Some(m) = self.modules.get(wasm_hash)? {
            Ok(Some(m.clone()))
        } else {
            Ok(None)
        }
    }

    pub fn remove_module(&self, wasm_hash: &Hash) -> Result<Option<Arc<ParsedModule>>, HostError> {
        self.modules.remove(wasm_hash)
    }

    pub fn clear(&self) -> Result<(), HostError> {
        self.modules.clear()
    }
}
