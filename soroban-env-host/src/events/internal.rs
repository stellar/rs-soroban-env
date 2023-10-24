use std::rc::Rc;

use soroban_env_common::{BytesObject, VecObject};

use super::{Events, HostEvent};
use crate::{
    budget::Budget,
    host::metered_clone::{MeteredClone, MeteredContainer, MeteredIterator},
    xdr,
    xdr::ScVal,
    Host, HostError, Val,
};

/// The internal representation of a `ContractEvent` that is stored in the events buffer
/// and designed to be cheap to clone.
// This is exposed as a pub type for benches.
#[derive(Clone, Debug)]
pub struct InternalContractEvent {
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
pub struct InternalDiagnosticEvent {
    pub contract_id: Option<crate::xdr::Hash>,
    pub topics: Vec<InternalDiagnosticArg>,
    pub args: Vec<InternalDiagnosticArg>,
}

// As mentioned above, we want to support storing "plain" rust datatypes as
// arguments to diagnostic events (in the form of ScVals), but in certain cases
// the user will be providing an _existing_ host object reference in which case
// we allow storing that as an argument too. We avoid eagerly converting it to
// an ScVal to avoid wasting CPU in the standard case where nobody is going to
// observe the event anyway.
#[derive(Clone, Debug)]
pub enum InternalDiagnosticArg {
    HostVal(Val),
    XdrVal(ScVal),
}

impl InternalDiagnosticEvent {
    fn to_xdr(&self, host: &Host) -> Result<Option<xdr::ContractEvent>, HostError> {
        // this exist as an internal closure to prevent it from being accidentally
        // called from the outside without `with_debug_budget`
        let externalize_args =
            |host: &Host, args: &[InternalDiagnosticArg]| -> Result<Vec<ScVal>, HostError> {
                args.iter()
                    .map(|arg| match arg {
                        InternalDiagnosticArg::HostVal(h) => host.from_host_val(*h),
                        InternalDiagnosticArg::XdrVal(v) => v.metered_clone(host),
                    })
                    .metered_collect::<Result<Vec<ScVal>, HostError>>(host)?
            };

        let opt_ce = host.with_debug_budget(
            || {
                let topics: xdr::VecM<ScVal> = externalize_args(host, &self.topics)?.try_into()?;
                let args = externalize_args(host, &self.args)?;
                let data = if args.len() > 1 {
                    ScVal::Vec(Some(xdr::ScVec::from(xdr::VecM::try_from(args)?)))
                } else if let Some(arg) = args.into_iter().next() {
                    arg
                } else {
                    ScVal::Void
                };
                Ok(Some(xdr::ContractEvent {
                    ext: xdr::ExtensionPoint::V0,
                    contract_id: self.contract_id.metered_clone(host)?,
                    type_: xdr::ContractEventType::Diagnostic,
                    body: xdr::ContractEventBody::V0(xdr::ContractEventV0 { topics, data }),
                }))
            },
            || None,
        );
        Ok(opt_ce)
    }
}

/// The internal representation of an `Event` that is stored in the events buffer
/// and designed to be cheap to clone.
#[derive(Clone, Debug)]
pub enum InternalEvent {
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
    //the bool keeps track of if the call this event was emitted in failed
    pub(crate) vec: Vec<(InternalEvent, EventError)>,
}

impl InternalEventsBuffer {
    // Records an InternalEvent
    pub fn record(&mut self, e: InternalEvent, budget: &Budget) -> Result<(), HostError> {
        // Metering: we use the cost of instantiating a size=1 `Vec` as an estimate
        // for the cost `Vec.push(event)`. Because the buffer length may be different
        // on different instances due to diagnostic events and we need a deterministic
        // cost across all instances, the cost needs to be amortized and buffer
        // size-independent.
        let charge_internal_event_push =
            || Vec::<(InternalEvent, EventError)>::charge_bulk_init_cpy(1, budget);

        match e {
            InternalEvent::Contract(_) => charge_internal_event_push()?,
            InternalEvent::Diagnostic(_) => {
                budget.with_internal_mode(charge_internal_event_push, || ())
            }
        }

        self.vec.push((e, EventError::FromSuccessfulCall));
        Ok(())
    }

    /// Rolls back the event buffer starting at `events`.
    pub fn rollback(&mut self, events: usize) -> Result<(), HostError> {
        // note that we first skip the events that are not being rolled back
        // Metering: free
        for e in self.vec.iter_mut().skip(events) {
            e.1 = EventError::FromFailedCall;
        }

        Ok(())
    }

    /// Converts the internal events into their external representation. This should only be called
    /// either when the host is finished (via `try_finish`), or when an error occurs.
    pub fn externalize(&self, host: &Host) -> Result<Events, HostError> {
        let mut vec: Vec<HostEvent> = vec![];
        vec.reserve(self.vec.len());

        for (event, status) in self.vec.iter() {
            match event {
                InternalEvent::Contract(c) => {
                    // Metering: we use the cost of instantiating a size=1 `Vec` as an estimate
                    // for the cost collecting 1 `HostEvent` into the events buffer. Because
                    // the resulting buffer length may be different on different instances
                    // (due to diagnostic events) and we need a deterministic cost across all
                    // instances, the cost needs to be amortized and buffer size-independent.
                    Vec::<HostEvent>::charge_bulk_init_cpy(1, host)?;
                    vec.push(HostEvent {
                        event: c.to_xdr(host)?,
                        failed_call: *status == EventError::FromFailedCall,
                    });
                }
                InternalEvent::Diagnostic(d) => {
                    // If the host is not in debug mode, the diagnostic event
                    // (which should not be generated in the first place), will be incored.
                    let de: Option<HostEvent> = host.with_debug_budget(
                        || match d.to_xdr(host)? {
                            Some(event) => Ok(Some(HostEvent {
                                event,
                                failed_call: *status == EventError::FromFailedCall,
                            })),
                            None => Ok(None),
                        },
                        || None,
                    );
                    if let Some(de) = de {
                        vec.push(de)
                    }
                }
            }
        }

        Ok(Events(vec))
    }
}
