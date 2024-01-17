use std::rc::Rc;

use crate::{
    budget::AsBudget,
    events::{Events, HostEvent},
    host::metered_clone::{MeteredClone, MeteredContainer, MeteredIterator},
    xdr::{self, ScVal},
    BytesObject, Host, HostError, Val, VecObject,
};

/// The internal representation of a `ContractEvent` that is stored in the
/// events buffer and designed to be cheap to clone.
// This is exposed as a pub type for benches.
#[derive(Clone, Debug)]
pub(crate) struct InternalContractEvent {
    pub type_: xdr::ContractEventType,
    pub contract_id: Option<BytesObject>,
    pub topics: VecObject,
    pub data: Val,
}

impl InternalContractEvent {
    // Metering: covered by components
    fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics = host.vecobject_to_scval_vec(self.topics)?;
        let data = host.from_host_val(self.data)?;
        let contract_id = match self.contract_id {
            Some(id) => Some(host.hash_from_bytesobj_input("contract_id", id)?),
            None => None,
        };
        Ok(xdr::ContractEvent {
            ext: xdr::ExtensionPoint::V0,
            contract_id,
            type_: self.type_,
            body: xdr::ContractEventBody::V0(xdr::ContractEventV0 { topics, data }),
        })
    }
}

/// Internal representation of a structured debug message which is logically
/// similar to an InternalContractEvent (and will project to a ContractEvent the
/// same way when we externalize it to XDR) but internally is different: it
/// stores more "plain" rust data values -- Vec as well as XDR types like ScVal
/// -- capturing the input to diagnostics, rather than interacting with the host
/// object system. It does this for two reasons:
///
///   1. to avoid perturbing object numbering when debug-mode is
///      enabled -- this could potentially cause hosts running on watcher nodes
///      with debug enabled to diverge from validators with debug disabled.
///
///   2. to avoid re-entering the host object management code by adding (say)
///      host objects for the contract ID, topics vector or such in the middle
///      of a diagnostic call occurs during a read from the host object table
///      (eg. validating a host function call's input arguments).

#[derive(Clone, Debug)]
pub(crate) struct InternalDiagnosticEvent {
    pub contract_id: Option<crate::xdr::Hash>,
    pub topics: Vec<InternalDiagnosticArg>,
    pub args: Vec<InternalDiagnosticArg>,
}

impl std::hash::Hash for InternalEvent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            InternalEvent::Contract(c) => {
                c.type_.hash(state);
                match c.contract_id {
                    Some(cid) => cid.to_val().get_payload().hash(state),
                    None => 0.hash(state),
                }
                c.data.get_payload().hash(state);
                c.topics.to_val().get_payload().hash(state);
            }
            // These are not included in the hash because they not supposed to
            // be observable, and we only have hashing to support
            // test-observation.
            InternalEvent::Diagnostic(_) => (),
        }
    }
}

impl std::hash::Hash for EventError {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            EventError::FromFailedCall => 0.hash(state),
            EventError::FromSuccessfulCall => 1.hash(state),
        }
    }
}

// As mentioned above, we want to support storing "plain" rust datatypes as
// arguments to diagnostic events (in the form of ScVals), but in certain cases
// the user will be providing an _existing_ host object reference in which case
// we allow storing that as an argument too. We avoid eagerly converting it to
// an ScVal to avoid wasting CPU in the standard case where nobody is going to
// observe the event anyway.
#[derive(Clone, Debug)]
pub(crate) enum InternalDiagnosticArg {
    HostVal(Val),
    XdrVal(ScVal),
}

fn externalize_args(host: &Host, args: &[InternalDiagnosticArg]) -> Result<Vec<ScVal>, HostError> {
    if !host.as_budget().is_in_shadow_mode()? {
        return Err(host.err(
            xdr::ScErrorType::Events,
            xdr::ScErrorCode::InternalError,
            "`externalize_args` can only be called in debug mode",
            &[],
        ));
    }

    args.iter()
        .map(|arg| match arg {
            InternalDiagnosticArg::HostVal(h) => host.from_host_val(*h),
            InternalDiagnosticArg::XdrVal(v) => v.metered_clone(host),
        })
        .metered_collect::<Result<Vec<ScVal>, HostError>>(host)?
}

impl InternalDiagnosticEvent {
    fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        if !host.as_budget().is_in_shadow_mode()? {
            return Err(host.err(
                xdr::ScErrorType::Events,
                xdr::ScErrorCode::InternalError,
                "`InternalDiagnosticEvent::to_xdr` can only be called in debug mode",
                &[],
            ));
        }

        let topics: xdr::VecM<ScVal> = externalize_args(host, &self.topics)?.try_into()?;
        let args = externalize_args(host, &self.args)?;
        let data = if args.len() > 1 {
            ScVal::Vec(Some(xdr::ScVec::from(xdr::VecM::try_from(args)?)))
        } else if let Some(arg) = args.into_iter().next() {
            arg
        } else {
            ScVal::Void
        };
        Ok(xdr::ContractEvent {
            ext: xdr::ExtensionPoint::V0,
            contract_id: self.contract_id.metered_clone(host)?,
            type_: xdr::ContractEventType::Diagnostic,
            body: xdr::ContractEventBody::V0(xdr::ContractEventV0 { topics, data }),
        })
    }
}

/// The internal representation of an `Event` that is stored in the events buffer
/// and designed to be cheap to clone.
#[derive(Clone, Debug)]
pub(crate) enum InternalEvent {
    Contract(InternalContractEvent),
    Diagnostic(Rc<InternalDiagnosticEvent>),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum EventError {
    FromFailedCall,
    FromSuccessfulCall,
}

/// The events buffer. Stores `InternalEvent`s in the chronological order.
#[derive(Clone, Default)]
pub(crate) struct InternalEventsBuffer {
    pub(crate) vec: Vec<(InternalEvent, EventError)>,
}

impl InternalEventsBuffer {
    // Records an InternalEvent
    pub(crate) fn record(&mut self, e: InternalEvent, host: &Host) -> Result<(), HostError> {
        let mut metered_internal_event_push = |e: InternalEvent| -> Result<(), HostError> {
            // Metering: we use the cost of instantiating a size=1 `Vec` as an
            // estimate for the cost `Vec.push(event)`.  Because the buffer length
            // may be different on different instances due to diagnostic events
            // and we need a deterministic cost across all instances, the cost
            // needs to be amortized and buffer size-independent.
            Vec::<(InternalEvent, EventError)>::charge_bulk_init_cpy(1, host)?;
            self.vec.push((e, EventError::FromSuccessfulCall));
            Ok(())
        };

        match &e {
            InternalEvent::Contract(_) => metered_internal_event_push(e)?,
            InternalEvent::Diagnostic(_) => host.with_debug_mode(|| metered_internal_event_push(e)),
        }

        Ok(())
    }

    /// "Rolls back" the event buffer starting at `events` by marking all
    /// subsequent events as failed calls.
    pub(crate) fn rollback(&mut self, events: usize) -> Result<(), HostError> {
        // note that we first skip the events that are not being rolled back
        // Metering: free (or conceptually: paid for when pushing the event)
        for e in self.vec.iter_mut().skip(events) {
            e.1 = EventError::FromFailedCall;
        }

        Ok(())
    }

    /// Converts the internal events into their external representation. This
    /// should only be called either when the host is finished (via
    /// `try_finish`), or when an error occurs.
    pub(crate) fn externalize(&self, host: &Host) -> Result<Events, HostError> {
        // This line is intentionally unmetered. We want to separate out
        // charging the main budget for `Contract` events (with "observable"
        // costs) from charging the debug budget for `Diagnostic` events (with
        // "non-observable" costs). Both event types are stored in the same
        // input vector so we must not do a bulk charge based on its length, but
        // rather walk through it charging to one budget or the other on an
        // event-by-event basis.
        let mut vec = Vec::with_capacity(self.vec.len());

        let mut metered_external_event_push =
            |event: xdr::ContractEvent, status: &EventError| -> Result<(), HostError> {
                // Metering: we use the cost of instantiating a size=1 `Vec` as
                // an estimate for the cost collecting 1 `HostEvent` into the
                // events buffer. Because the resulting buffer length may be
                // different on different instances (due to diagnostic events)
                // and we need a deterministic cost across all instances, the
                // cost needs to be amortized and buffer size-independent.
                Vec::<HostEvent>::charge_bulk_init_cpy(1, host)?;
                vec.push(HostEvent {
                    event,
                    failed_call: *status == EventError::FromFailedCall,
                });
                Ok(())
            };

        for (event, status) in self.vec.iter() {
            match event {
                InternalEvent::Contract(c) => {
                    metered_external_event_push(c.to_xdr(host)?, status)?;
                }
                InternalEvent::Diagnostic(d) => {
                    host.with_debug_mode(|| metered_external_event_push(d.to_xdr(host)?, status));
                }
            }
        }

        Ok(Events(vec))
    }
}
