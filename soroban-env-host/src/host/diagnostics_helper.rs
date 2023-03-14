use soroban_env_common::{
    xdr::{ContractEventType, Hash, ScBytes},
    BytesObject, EnvBase, Symbol, SymbolSmall, VecObject,
};

use crate::{budget::AsBudget, Host, HostError, RawVal};
use crate::{
    events::{InternalContractEvent, InternalEvent},
    host_object::HostVec,
};

use super::Frame;

/// None of these functions are metered, which is why they're behind the is_debug check
impl Host {
    fn hash_to_bytesobj(&self, hash: &Hash) -> Result<BytesObject, HostError> {
        self.add_host_object::<ScBytes>(hash.as_slice().to_vec().try_into()?)
    }

    // Will not return error if frame is missing
    fn get_current_contract_id(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame_opt(|frame| match frame {
            #[cfg(feature = "vm")]
            Some(Frame::ContractVM(vm, _, _)) => Ok(Some(vm.contract_id.clone())),
            Some(Frame::HostFunction(_)) => Ok(None),
            Some(Frame::Token(id, _, _)) => Ok(Some(id.clone())),
            #[cfg(any(test, feature = "testutils"))]
            Some(Frame::TestContract(tc)) => Ok(Some(tc.id.clone())),
            None => Ok(None),
        })
    }

    fn record_system_debug_contract_event(
        &self,
        type_: ContractEventType,
        contract_id: Option<BytesObject>,
        topics: VecObject,
        data: RawVal,
    ) -> Result<(), HostError> {
        let ce = InternalContractEvent {
            type_,
            contract_id,
            topics,
            data,
        };
        self.get_events_mut(|events| {
            Ok(events.record(InternalEvent::StructuredDebug(ce), self.as_budget()))
        })?
    }

    // Emits an event with topic = ["fn_call", called_contract_id, function_name] and
    // data = [arg1, args2, ...]
    // Should called prior to opening a frame for the next call so the calling contract can be inferred correctly
    pub fn fn_call_diagnostics(
        &self,
        called_contract_id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<(), HostError> {
        if !self.is_debug() {
            return Ok(());
        }

        let mut calling_contract: Option<BytesObject> = None;
        if let Some(calling_hash) = self.get_current_contract_id()? {
            calling_contract = Some(self.hash_to_bytesobj(&calling_hash)?);
        }

        self.as_budget().with_free_budget(|| {
            let topics: Vec<RawVal> = vec![
                SymbolSmall::try_from_str("fn_call")?.into(),
                self.hash_to_bytesobj(called_contract_id)?.into(),
                func.into(),
            ];

            self.record_system_debug_contract_event(
                ContractEventType::Diagnostic,
                calling_contract,
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
            let topics: Vec<RawVal> =
                vec![SymbolSmall::try_from_str("fn_return")?.into(), func.into()];

            self.record_system_debug_contract_event(
                ContractEventType::Diagnostic,
                Some(self.hash_to_bytesobj(contract_id)?),
                self.add_host_object(HostVec::from_vec(topics)?)?
                    .try_into()?,
                *res,
            )
        })
    }
}
