use super::parsed_module::{ParsedModule, VersionedContractCodeCostInputs};
use crate::{
    budget::{get_wasmi_config, AsBudget},
    host::metered_clone::MeteredClone,
    xdr::{Hash, ScErrorCode, ScErrorType},
    Host, HostError, MeteredOrdMap,
};
use std::rc::Rc;
use wasmi::Engine;

/// A [ModuleCache] is a cache of a set of Wasm modules that have been parsed
/// but not yet instantiated, along with a shared and reusable [Engine] storing
/// their code. The cache must be populated eagerly with all the contracts in a
/// single [Host]'s lifecycle (at least) added all at once, since each wasmi
/// [Engine] is locked during execution and no new modules can be added to it.
#[derive(Clone, Default)]
pub struct ModuleCache {
    pub(crate) engine: Engine,
    modules: MeteredOrdMap<Hash, Rc<ParsedModule>, Host>,
}

impl ModuleCache {
    // ModuleCache should not be active until protocol version 21.
    pub const MIN_LEDGER_VERSION: u32 = 21;

    pub fn new(host: &Host) -> Result<Self, HostError> {
        let config = get_wasmi_config(host.as_budget())?;
        let engine = Engine::new(&config);
        let modules = MeteredOrdMap::new();
        #[allow(unused_mut)]
        let mut cache = Self { engine, modules };
        #[cfg(feature = "next")]
        cache.add_stored_contracts(host)?;
        Ok(cache)
    }

    #[cfg(feature = "next")]
    pub fn add_stored_contracts(&mut self, host: &Host) -> Result<(), HostError> {
        use crate::xdr::{ContractCodeEntry, ContractCodeEntryExt, LedgerEntryData, LedgerKey};
        for (k, v) in host.try_borrow_storage()?.map.iter(host.as_budget())? {
            if let LedgerKey::ContractCode(_) = &**k {
                if let Some((e, _)) = v {
                    if let LedgerEntryData::ContractCode(ContractCodeEntry { code, hash, ext }) =
                        &e.data
                    {
                        let code_cost_inputs = match ext {
                            ContractCodeEntryExt::V0 => VersionedContractCodeCostInputs::V0 {
                                wasm_bytes: code.len(),
                            },
                            ContractCodeEntryExt::V1(v1) => VersionedContractCodeCostInputs::V1(
                                v1.cost_inputs.metered_clone(host.as_budget())?,
                            ),
                        };
                        self.parse_and_cache_module(host, hash, code, code_cost_inputs)?;
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse_and_cache_module(
        &mut self,
        host: &Host,
        contract_id: &Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<(), HostError> {
        if self.modules.contains_key(contract_id, host)? {
            return Err(host.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "module cache already contains contract",
                &[],
            ));
        }
        let parsed_module = Rc::new(ParsedModule::new(host, &self.engine, &wasm, cost_inputs)?);
        self.modules =
            self.modules
                .insert(contract_id.metered_clone(host)?, parsed_module, host)?;
        Ok(())
    }

    pub fn get_module(
        &self,
        host: &Host,
        contract_id: &Hash,
    ) -> Result<Rc<ParsedModule>, HostError> {
        if let Some(m) = self.modules.get(contract_id, host)? {
            return Ok(m.clone());
        } else {
            Err(host.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "module cache missing contract",
                &[],
            ))
        }
    }
}
