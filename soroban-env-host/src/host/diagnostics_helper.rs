use soroban_env_common::{
    xdr::{ContractEventType, Hash, ScVal, ScVec},
    ConversionError, ScValObjRef, Symbol, TryFromVal, TryIntoVal, VecObject,
};

use crate::events::{InternalContractEvent, InternalEvent};
use crate::{budget::AsBudget, Host, HostError, RawVal};

/// None of these functions are metered, which is why they're behind the is_debug check
impl Host {
    fn hash_to_scval(&self, hash: Hash) -> Result<ScVal, HostError> {
        Ok(ScVal::Bytes(self.scbytes_from_hash(&hash)?))
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
            let mut topics: Vec<ScVal> = Vec::new();
            topics.push(ScVal::Symbol(crate::xdr::ScSymbol(
                self.map_err("fn_call".as_bytes().try_into())?,
            )));

            topics.push(self.hash_to_scval(contract_id.clone())?);
            topics.push(func.try_into_val(self)?);

            let scvec_topics: ScVec = self.map_err(topics.try_into())?;
            let scval_topics = ScVal::Vec(Some(scvec_topics));
            let topics_obj_ref = ScValObjRef::classify(&scval_topics)
                .ok_or_else(|| self.err_general("classify failed"))?;

            let data: Result<Vec<ScVal>, ConversionError> = args
                .into_iter()
                .map(|i| ScVal::try_from_val(self, i))
                .collect();
            let data_vec: ScVec = self.map_err(data?.try_into())?;

            self.record_system_debug_contract_event(
                ContractEventType::System,
                self.to_host_obj(&topics_obj_ref)?.try_into()?,
                self.to_host_val(&ScVal::Vec(Some(data_vec)))?,
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
            let mut topics: Vec<ScVal> = Vec::new();
            topics.push(ScVal::Symbol(crate::xdr::ScSymbol(
                self.map_err("fn_return".as_bytes().try_into())?,
            )));

            topics.push(self.hash_to_scval(contract_id.clone())?);
            topics.push(func.try_into_val(self)?);

            let scvec_topics: ScVec = self.map_err(topics.try_into())?;
            let scval_topics = ScVal::Vec(Some(scvec_topics));
            let topics_obj_ref = ScValObjRef::classify(&scval_topics)
                .ok_or_else(|| self.err_general("classify failed"))?;

            self.record_system_debug_contract_event(
                ContractEventType::System,
                self.to_host_obj(&topics_obj_ref)?.try_into()?,
                *res,
            )
        })
    }
}
