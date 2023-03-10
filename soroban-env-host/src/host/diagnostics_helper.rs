use soroban_env_common::{
    xdr::{ContractEventType, Hash, ScBytes},
    BytesObject, EnvBase, Symbol, SymbolSmall, VecObject,
};

use crate::{budget::AsBudget, Host, HostError, RawVal};
use crate::{
    events::{InternalContractEvent, InternalEvent},
    host_object::HostVec,
};

/// None of these functions are metered, which is why they're behind the is_debug check
impl Host {
    fn hash_to_bytesobj(&self, hash: &Hash) -> Result<BytesObject, HostError> {
        self.add_host_object::<ScBytes>(hash.as_slice().to_vec().try_into()?)
    }

    fn record_system_debug_contract_event(
        &self,
        type_: ContractEventType,
        topics: VecObject,
        data: RawVal,
    ) -> Result<(), HostError> {
        let ce = InternalContractEvent {
            type_,
            contract_id: None,
            topics,
            data,
        };
        self.get_events_mut(|events| {
            Ok(events.record(InternalEvent::StructuredDebug(ce), self.as_budget()))
        })?
    }

    // Emits an event with topic = ["fn_call", contract_id, function_name] and
    // data = [arg1, args2, ...]
    pub fn fn_call_diagnostics(
        &self,
        contract_id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<(), HostError> {
        if !self.is_debug() {
            return Ok(());
        }

        self.as_budget().with_free_budget(|| {
            let mut topics: Vec<RawVal> = Vec::new();
            topics.push(SymbolSmall::try_from_str("fn_call")?.into());
            topics.push(self.hash_to_bytesobj(contract_id)?.into());
            topics.push(func.into());

            self.record_system_debug_contract_event(
                ContractEventType::System,
                self.add_host_object(HostVec::from_vec(topics)?)?
                    .try_into()?,
                self.vec_new_from_slice(args)?.into(),
            )
        })
    }

    // Emits an event with topic = ["fn_return", contract_id, function_name] and
    // data = [return_val]
    pub fn fn_return_diagnostics(
        &self,
        contract_id: &Hash,
        func: &Symbol,
        res: &RawVal,
    ) -> Result<(), HostError> {
        if !self.is_debug() {
            return Ok(());
        }

        self.as_budget().with_free_budget(|| {
            let mut topics: Vec<RawVal> = Vec::new();
            topics.push(SymbolSmall::try_from_str("fn_return")?.into());
            topics.push(self.hash_to_bytesobj(contract_id)?.into());
            topics.push(func.into());

            self.record_system_debug_contract_event(
                ContractEventType::System,
                self.add_host_object(HostVec::from_vec(topics)?)?
                    .try_into()?,
                *res,
            )
        })
    }
}
