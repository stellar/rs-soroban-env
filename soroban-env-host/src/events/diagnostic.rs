use std::rc::Rc;

use soroban_env_common::{
    xdr::{ContractEventType, Hash},
    Error, Symbol, SymbolSmall, VecObject,
};

use crate::{budget::AsBudget, host::Frame, Host, HostError, RawVal};

use super::{internal::InternalDiagnosticEvent, InternalEvent, InternalEventsBuffer};

#[derive(Clone, Default)]
pub enum DiagnosticLevel {
    #[default]
    None,
    Debug,
}

/// None of these functions are metered, which is why they're behind the is_debug check
impl Host {
    pub fn set_diagnostic_level(&self, diagnostic_level: DiagnosticLevel) {
        *self.0.diagnostic_level.borrow_mut() = diagnostic_level;
    }

    // As above, avoids having to import DiagnosticLevel.
    pub fn enable_debug(&self) {
        self.set_diagnostic_level(DiagnosticLevel::Debug)
    }

    pub fn is_debug(&self) -> bool {
        matches!(*self.0.diagnostic_level.borrow(), DiagnosticLevel::Debug)
    }

    /// Records a `System` contract event. `topics` is expected to be a `SCVec`
    /// length <= 4 that cannot contain `Vec`, `Map`, or `Bytes` with length > 32
    pub fn system_event(&self, topics: VecObject, data: RawVal) -> Result<(), HostError> {
        self.record_contract_event(ContractEventType::System, topics, data)?;
        Ok(())
    }

    pub(crate) fn record_system_debug_contract_event(
        &self,
        contract_id: Option<Hash>,
        topics: Vec<RawVal>,
        msg: Option<String>,
        data: Vec<RawVal>,
    ) -> Result<(), HostError> {
        let de = Rc::new(InternalDiagnosticEvent {
            contract_id,
            topics,
            msg,
            data,
        });
        self.with_events_mut(|events| {
            Ok(events.record(InternalEvent::StructuredDebug(de), self.as_budget()))
        })?
    }

    // Will not return error if frame is missing
    pub(crate) fn get_current_contract_id_unmetered(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame_opt(|frame| match frame {
            Some(Frame::ContractVM(vm, _, _)) => Ok(Some(vm.contract_id.clone())),
            Some(Frame::HostFunction(_)) => Ok(None),
            Some(Frame::Token(id, _, _)) => Ok(Some(id.clone())),
            #[cfg(any(test, feature = "testutils"))]
            Some(Frame::TestContract(tc)) => Ok(Some(tc.id.clone())),
            None => Ok(None),
        })
    }

    pub fn debug_diagnostics(&self, msg: &str, args: &[RawVal]) -> Result<(), HostError> {
        if !self.is_debug() {
            return Ok(());
        }
        let calling_contract = self.get_current_contract_id_unmetered()?;
        self.as_budget().with_free_budget(|| {
            let debug_sym: SymbolSmall = SymbolSmall::try_from_str("debug")?;
            let topics: Vec<RawVal> = vec![debug_sym.to_raw()];
            self.record_system_debug_contract_event(
                calling_contract,
                topics,
                Some(msg.to_string()),
                args.to_vec(),
            )
        })
    }

    pub(crate) fn err_diagnostics(
        &self,
        events: &mut InternalEventsBuffer,
        error: Error,
        msg: &str,
        args: &[RawVal],
    ) -> Result<(), HostError> {
        if !self.is_debug() {
            return Ok(());
        }

        self.as_budget().with_free_budget(|| {
            let error_sym: SymbolSmall = SymbolSmall::try_from_str("error")?;
            let contract_id = self.get_current_contract_id_unmetered()?;
            let topics: Vec<RawVal> = vec![error_sym.to_raw(), error.to_raw()];

            // We do the event-recording ourselves here rather than calling
            // self.record_system_debug_contract_event because we can/should
            // only be called with an already-borrowed events buffer (to
            // insulate against double-faulting).
            let ce = Rc::new(InternalDiagnosticEvent {
                contract_id,
                topics,
                msg: Some(msg.to_string()),
                data: args.to_vec(),
            });
            events.record(InternalEvent::StructuredDebug(ce), self.as_budget())
        })
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

        let calling_contract = self.get_current_contract_id_unmetered()?;

        self.as_budget().with_free_budget(|| {
            let topics: Vec<RawVal> =
                vec![SymbolSmall::try_from_str("fn_call")?.into(), func.into()];

            let msg = format!("called contract: {}", called_contract_id);

            self.record_system_debug_contract_event(
                calling_contract,
                topics,
                Some(msg),
                args.to_vec(),
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
                Some(contract_id.clone()),
                topics,
                None,
                vec![*res],
            )
        })
    }
}
