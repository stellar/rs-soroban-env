use super::{
    func_info::HOST_FUNCTIONS,
    parsed_module::{ParsedModule, VersionedContractCodeCostInputs, VersionedParsedModule},
    Wasmi031, Wasmi034, WasmiVersion,
};
use crate::{
    budget::{get_wasmi_config, AsBudget},
    host::metered_clone::{MeteredClone, MeteredContainer},
    xdr::{Hash, ScErrorCode, ScErrorType},
    Host, HostError, MeteredOrdMap, Vm,
};
use std::{collections::BTreeSet, rc::Rc};

#[derive(Clone)]
pub struct ModuleCache(pub(crate) McVer);

#[derive(Clone)]
pub enum McVer {
    Mc031(Rc<VersionedModuleCache<Wasmi031>>),
    Mc034(Rc<VersionedModuleCache<Wasmi034>>),
}

impl From<Rc<VersionedModuleCache<Wasmi031>>> for ModuleCache {
    fn from(mc: Rc<VersionedModuleCache<Wasmi031>>) -> Self {
        ModuleCache(McVer::Mc031(mc))
    }
}

impl From<Rc<VersionedModuleCache<Wasmi034>>> for ModuleCache {
    fn from(mc: Rc<VersionedModuleCache<Wasmi034>>) -> Self {
        ModuleCache(McVer::Mc034(mc))
    }
}

impl ModuleCache {
    // ModuleCache should not be active until protocol version 21.
    pub const MIN_LEDGER_VERSION: u32 = 21;

    pub fn should_use_for_protocol(protocol_version: u32) -> bool {
        protocol_version >= Self::MIN_LEDGER_VERSION
    }

    pub fn new(host: &Host) -> Result<Self, HostError> {
        if Vm::protocol_uses_legacy_stack_vm(host.get_ledger_protocol_version()?) {
            VersionedModuleCache::<Wasmi031>::new(host).map(Into::into)
        } else {
            VersionedModuleCache::<Wasmi034>::new(host).map(Into::into)
        }
    }

    pub fn get_module(
        &self,
        host: &Host,
        wasm_hash: &Hash,
    ) -> Result<Option<ParsedModule>, HostError> {
        match &self.0 {
            McVer::Mc031(cache) => cache.get_module(host, wasm_hash),
            McVer::Mc034(cache) => cache.get_module(host, wasm_hash),
        }
    }
}

/// A [ModuleCache] is a cache of a set of Wasm modules that have been parsed
/// but not yet instantiated, along with a shared and reusable [Engine] storing
/// their code. The cache must be populated eagerly with all the contracts in a
/// single [Host]'s lifecycle (at least) added all at once, since each wasmi
/// [Engine] is locked during execution and no new modules can be added to it.
#[derive(Clone, Default)]
pub(crate) struct VersionedModuleCache<V: WasmiVersion> {
    modules: MeteredOrdMap<Hash, Rc<VersionedParsedModule<V>>, Host>,
    pub(crate) linker: V::Linker,
}

impl<V: WasmiVersion> VersionedModuleCache<V> {
    fn new(host: &Host) -> Result<Rc<Self>, HostError> {
        let config = get_wasmi_config(host.as_budget())?;
        let engine = V::new_engine(&config);
        let mut modules = MeteredOrdMap::new();
        Self::add_stored_contracts(&engine, &mut modules, host)?;
        let linker = Self::make_linker(&engine, &modules, host)?;
        Ok(Rc::new(Self { modules, linker }))
    }

    fn add_stored_contracts(
        engine: &V::Engine,
        modules: &mut MeteredOrdMap<Hash, Rc<VersionedParsedModule<V>>, Host>,
        host: &Host,
    ) -> Result<(), HostError> {
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
                        Self::parse_and_cache_module(
                            engine,
                            modules,
                            host,
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

    fn parse_and_cache_module(
        engine: &V::Engine,
        modules: &mut MeteredOrdMap<Hash, Rc<VersionedParsedModule<V>>, Host>,
        host: &Host,
        contract_id: &Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<(), HostError> {
        if modules.contains_key(contract_id, host)? {
            return Err(host.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "module cache already contains contract",
                &[],
            ));
        }
        let parsed_module = VersionedParsedModule::<V>::new(host, engine, &wasm, cost_inputs)?;
        *modules = modules.insert(contract_id.metered_clone(host)?, parsed_module, host)?;
        Ok(())
    }

    fn with_module_set_import_symbols<T>(
        host: &Host,
        modules: &MeteredOrdMap<Hash, Rc<VersionedParsedModule<V>>, Host>,
        callback: impl FnOnce(&BTreeSet<(&str, &str)>) -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        let mut import_symbols = BTreeSet::new();
        for module in modules.values(host)? {
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

    fn make_linker(
        engine: &V::Engine,
        modules: &MeteredOrdMap<Hash, Rc<VersionedParsedModule<V>>, Host>,
        host: &Host,
    ) -> Result<V::Linker, HostError> {
        Self::with_module_set_import_symbols(host, modules, |symbols| {
            Host::make_linker::<V>(engine, symbols)
        })
    }

    pub fn get_module(
        &self,
        host: &Host,
        wasm_hash: &Hash,
    ) -> Result<Option<ParsedModule>, HostError> {
        if let Some(m) = self.modules.get(wasm_hash, host)? {
            Ok(Some(V::upcast_versioned_parsed_module(m)))
        } else {
            Ok(None)
        }
    }
}
