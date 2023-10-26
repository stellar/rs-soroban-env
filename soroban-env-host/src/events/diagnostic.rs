use std::rc::Rc;

use soroban_env_common::{
    xdr::{Hash, ScBytes, ScString, ScVal, StringM},
    Error, Symbol, SymbolSmall,
};

use crate::{
    host::metered_clone::{MeteredAlloc, MeteredClone, MeteredContainer, MeteredIterator},
    Host, HostError, Val,
};

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

impl Host {
    pub fn set_diagnostic_level(&self, diagnostic_level: DiagnosticLevel) -> Result<(), HostError> {
        *self.try_borrow_diagnostic_level_mut()? = diagnostic_level;
        Ok(())
    }

    // As above, avoids having to import DiagnosticLevel.
    pub fn enable_debug(&self) -> Result<(), HostError> {
        self.set_diagnostic_level(DiagnosticLevel::Debug)
    }

    pub(crate) fn is_debug(&self) -> Result<bool, HostError> {
        Ok(matches!(
            *self.try_borrow_diagnostic_level()?,
            DiagnosticLevel::Debug
        ))
    }

    fn record_diagnostic_event(
        &self,
        contract_id: Option<Hash>,
        topics: Vec<InternalDiagnosticArg>,
        args: Vec<InternalDiagnosticArg>,
    ) -> Result<(), HostError> {
        self.with_debug_mode(
            || {
                let de = Rc::metered_new(
                    InternalDiagnosticEvent {
                        contract_id,
                        topics,
                        args,
                    },
                    self,
                )?;
                self.with_events_mut(|events| events.record(InternalEvent::Diagnostic(de), self))
            },
            || (),
        );
        Ok(())
    }

    pub fn log_diagnostics(&self, msg: &str, args: &[Val]) {
        self.with_debug_mode(
            || {
                let calling_contract = self.get_current_contract_id_opt_internal(false)?;
                let log_sym = SymbolSmall::try_from_str("log")?;
                Vec::<InternalDiagnosticArg>::charge_bulk_init_cpy(1, self)?;
                let topics = vec![InternalDiagnosticArg::HostVal(log_sym.to_val())];
                let msg = ScVal::String(ScString::from(StringM::try_from(
                    self.metered_slice_to_vec(msg.as_bytes())?,
                )?));
                let args: Vec<_> = std::iter::once(InternalDiagnosticArg::XdrVal(msg))
                    .chain(args.iter().map(|rv| InternalDiagnosticArg::HostVal(*rv)))
                    .metered_collect(self)?;
                self.record_diagnostic_event(calling_contract, topics, args)
            },
            || (),
        )
    }

    pub(crate) fn record_err_diagnostics(
        &self,
        events: &mut InternalEventsBuffer,
        error: Error,
        msg: &str,
        args: &[Val],
    ) {
        self.with_debug_mode(
            || {
                let error_sym = SymbolSmall::try_from_str("error")?;
                let contract_id = self.get_current_contract_id_opt_internal(false)?;
                Vec::<InternalDiagnosticArg>::charge_bulk_init_cpy(2, self)?;
                let topics = vec![
                    InternalDiagnosticArg::HostVal(error_sym.to_val()),
                    InternalDiagnosticArg::HostVal(error.to_val()),
                ];
                let msg = ScVal::String(ScString::from(StringM::try_from(
                    self.metered_slice_to_vec(msg.as_bytes())?,
                )?));
                let args: Vec<_> = std::iter::once(InternalDiagnosticArg::XdrVal(msg))
                    .chain(args.iter().map(|rv| InternalDiagnosticArg::HostVal(*rv)))
                    .metered_collect(self)?;

                // We do the event-recording ourselves here rather than calling
                // self.record_system_debug_contract_event because we can/should
                // only be called with an already-borrowed events buffer (to
                // insulate against double-faulting).
                let ce = Rc::metered_new(
                    InternalDiagnosticEvent {
                        contract_id,
                        topics,
                        args,
                    },
                    self,
                )?;
                events.record(InternalEvent::Diagnostic(ce), self)
            },
            || (),
        )
    }

    // Emits an event with topic = ["fn_call", called_contract_id, function_name] and
    // data = [arg1, args2, ...]
    // Should called prior to opening a frame for the next call so the calling contract can be inferred correctly
    pub fn fn_call_diagnostics(&self, called_contract_id: &Hash, func: &Symbol, args: &[Val]) {
        self.with_debug_mode(
            || {
                let calling_contract = self.get_current_contract_id_opt_internal(false)?;
                Vec::<InternalDiagnosticArg>::charge_bulk_init_cpy(3, self)?;
                let topics = vec![
                    InternalDiagnosticArg::HostVal(SymbolSmall::try_from_str("fn_call")?.into()),
                    InternalDiagnosticArg::XdrVal(ScVal::Bytes(ScBytes::try_from(
                        self.metered_slice_to_vec(called_contract_id.as_slice())?,
                    )?)),
                    InternalDiagnosticArg::HostVal(func.into()),
                ];
                let args = args
                    .iter()
                    .map(|rv| InternalDiagnosticArg::HostVal(*rv))
                    .metered_collect(self)?;
                self.record_diagnostic_event(calling_contract, topics, args)
            },
            || (),
        )
    }

    // Emits an event with topic = ["fn_return", function_name] and
    // data = [return_val]
    pub fn fn_return_diagnostics(&self, contract_id: &Hash, func: &Symbol, res: &Val) {
        self.with_debug_mode(
            || {
                Vec::<InternalDiagnosticArg>::charge_bulk_init_cpy(2, self)?;
                let topics = vec![
                    InternalDiagnosticArg::HostVal(SymbolSmall::try_from_str("fn_return")?.into()),
                    InternalDiagnosticArg::HostVal(func.into()),
                ];
                Vec::<InternalDiagnosticArg>::charge_bulk_init_cpy(1, self)?;
                let args = vec![InternalDiagnosticArg::HostVal(*res)];
                self.record_diagnostic_event(Some(contract_id.metered_clone(self)?), topics, args)
            },
            || (),
        )
    }
}
