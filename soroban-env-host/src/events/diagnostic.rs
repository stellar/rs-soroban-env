use std::rc::Rc;

use soroban_env_common::{
    xdr::{ContractEventType, Hash, ScBytes, ScString, ScVal, StringM},
    Error, Symbol, SymbolSmall, VecObject,
};

use crate::{budget::AsBudget, host::Frame, Host, HostError, Val};

use super::{
    internal::{InternalDiagnosticArg, InternalDiagnosticEvent},
    InternalEvent, InternalEventsBuffer,
};

#[derive(Clone, Default)]
pub enum DiagnosticLevel {
    #[default]
    None,
    Debug,
}

/// None of these functions are metered, which is why they're behind the is_debug check
impl Host {
    pub fn set_diagnostic_level(&self, diagnostic_level: DiagnosticLevel) -> Result<(), HostError> {
        *self.try_borrow_diagnostic_level_mut()? = diagnostic_level;
        Ok(())
    }

    // As above, avoids having to import DiagnosticLevel.
    pub fn enable_debug(&self) -> Result<(), HostError> {
        self.set_diagnostic_level(DiagnosticLevel::Debug)
    }

    pub fn is_debug(&self) -> Result<bool, HostError> {
        Ok(matches!(
            *self.try_borrow_diagnostic_level()?,
            DiagnosticLevel::Debug
        ))
    }

    /// Records a `System` contract event. `topics` is expected to be a `SCVec`
    /// length <= 4 that cannot contain `Vec`, `Map`, or `Bytes` with length > 32
    pub fn system_event(&self, topics: VecObject, data: Val) -> Result<(), HostError> {
        self.record_contract_event(ContractEventType::System, topics, data)?;
        Ok(())
    }

    pub(crate) fn record_diagnostic_event(
        &self,
        contract_id: Option<Hash>,
        topics: Vec<InternalDiagnosticArg>,
        args: Vec<InternalDiagnosticArg>,
    ) -> Result<(), HostError> {
        let de = Rc::new(InternalDiagnosticEvent {
            contract_id,
            topics,
            args,
        });
        self.with_events_mut(|events| {
            Ok(events.record(InternalEvent::Diagnostic(de), self.as_budget()))
        })?
    }

    // Will not return error if frame is missing
    pub(crate) fn get_current_contract_id_unmetered(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame_opt(|frame| match frame {
            Some(Frame::ContractVM(vm, ..)) => Ok(Some(vm.contract_id.clone())),
            Some(Frame::InitialInvokeHostFunctionOp(..)) => Ok(None),
            Some(Frame::Token(id, ..)) => Ok(Some(id.clone())),
            #[cfg(any(test, feature = "testutils"))]
            Some(Frame::TestContract(tc)) => Ok(Some(tc.id.clone())),
            None => Ok(None),
        })
    }

    pub fn log_diagnostics(&self, msg: &str, args: &[Val]) -> Result<(), HostError> {
        if !self.is_debug()? {
            return Ok(());
        }
        let calling_contract = self.get_current_contract_id_unmetered()?;
        self.as_budget().with_free_budget(|| {
            let log_sym = SymbolSmall::try_from_str("log")?;
            let topics = vec![InternalDiagnosticArg::HostVal(log_sym.to_val())];
            let msg = ScVal::String(ScString::from(StringM::try_from(msg.as_bytes().to_vec())?));
            let args: Vec<_> = std::iter::once(InternalDiagnosticArg::XdrVal(msg))
                .chain(args.iter().map(|rv| InternalDiagnosticArg::HostVal(*rv)))
                .collect();
            self.record_diagnostic_event(calling_contract, topics, args)
        })
    }

    pub(crate) fn err_diagnostics(
        &self,
        events: &mut InternalEventsBuffer,
        error: Error,
        msg: &str,
        args: &[Val],
    ) -> Result<(), HostError> {
        if !self.is_debug()? {
            return Ok(());
        }

        self.as_budget().with_free_budget(|| {
            let error_sym = SymbolSmall::try_from_str("error")?;
            let contract_id = self.get_current_contract_id_unmetered()?;
            let topics = vec![
                InternalDiagnosticArg::HostVal(error_sym.to_val()),
                InternalDiagnosticArg::HostVal(error.to_val()),
            ];
            let msg = ScVal::String(ScString::from(StringM::try_from(msg.as_bytes().to_vec())?));
            let args: Vec<_> = std::iter::once(InternalDiagnosticArg::XdrVal(msg))
                .chain(args.iter().map(|rv| InternalDiagnosticArg::HostVal(*rv)))
                .collect();

            // We do the event-recording ourselves here rather than calling
            // self.record_system_debug_contract_event because we can/should
            // only be called with an already-borrowed events buffer (to
            // insulate against double-faulting).
            let ce = Rc::new(InternalDiagnosticEvent {
                contract_id,
                topics,
                args,
            });
            events.record(InternalEvent::Diagnostic(ce), self.as_budget())
        })
    }

    // Emits an event with topic = ["fn_call", called_contract_id, function_name] and
    // data = [arg1, args2, ...]
    // Should called prior to opening a frame for the next call so the calling contract can be inferred correctly
    pub fn fn_call_diagnostics(
        &self,
        called_contract_id: &Hash,
        func: &Symbol,
        args: &[Val],
    ) -> Result<(), HostError> {
        if !self.is_debug()? {
            return Ok(());
        }

        let calling_contract = self.get_current_contract_id_unmetered()?;

        self.as_budget().with_free_budget(|| {
            let topics = vec![
                InternalDiagnosticArg::HostVal(SymbolSmall::try_from_str("fn_call")?.into()),
                InternalDiagnosticArg::XdrVal(ScVal::Bytes(ScBytes::try_from(
                    called_contract_id.as_slice().to_vec(),
                )?)),
                InternalDiagnosticArg::HostVal(func.into()),
            ];
            self.record_diagnostic_event(
                calling_contract,
                topics,
                args.iter()
                    .map(|rv| InternalDiagnosticArg::HostVal(*rv))
                    .collect(),
            )
        })
    }

    // Emits an event with topic = ["fn_return", function_name] and
    // data = [return_val]
    pub fn fn_return_diagnostics(
        &self,
        contract_id: &Hash,
        func: &Symbol,
        res: &Val,
    ) -> Result<(), HostError> {
        if !self.is_debug()? {
            return Ok(());
        }

        self.as_budget().with_free_budget(|| {
            let topics = vec![
                InternalDiagnosticArg::HostVal(SymbolSmall::try_from_str("fn_return")?.into()),
                InternalDiagnosticArg::HostVal(func.into()),
            ];

            self.record_diagnostic_event(
                Some(contract_id.clone()),
                topics,
                vec![InternalDiagnosticArg::HostVal(*res)],
            )
        })
    }
}
