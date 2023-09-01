use std::rc::Rc;

use soroban_env_common::{BytesObject, VecObject};

use super::{Events, HostEvent};
use crate::{
    budget::{AsBudget, Budget},
    host::metered_clone::MeteredContainer,
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
    pub fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics = host.call_args_to_sc_val_vec(self.topics)?;
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
/// stores "plain" rust data values -- String and Vec as well as XDR types like
/// ScVal -- capturing the input to diagnostics, rather than interacting with
/// the host object system. It does this for two reasons:
///
///   1. to avoid to avoid perturbing object numbering when debug-mode is
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

fn externalize_args(host: &Host, args: &[InternalDiagnosticArg]) -> Result<Vec<ScVal>, HostError> {
    let mut scargs: Vec<ScVal> = Vec::new();
    for arg in args.iter() {
        match arg {
            InternalDiagnosticArg::HostVal(h) => scargs.push(host.from_host_val(*h)?),
            InternalDiagnosticArg::XdrVal(v) => scargs.push(v.clone()),
        }
    }
    Ok(scargs)
}

impl InternalDiagnosticEvent {
    pub fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
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
            contract_id: self.contract_id.clone(),
            type_: xdr::ContractEventType::Diagnostic,
            body: xdr::ContractEventBody::V0(xdr::ContractEventV0 { topics, data }),
        })
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
        // Metering: we use the cost of instantiating a size=1 `Vec` as an estimate for the cost
        // `Vec.push(event)`. Because the buffer length may be different on different instances
        // due to diagnostic events and we need a deterministic cost across all instances,
        // the cost needs to be amortized and buffer size-independent.

        if let InternalEvent::Contract(_) = e {
            Vec::<(InternalEvent, EventError)>::charge_bulk_init_cpy(1, budget)?;
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
        let vec: Result<Vec<HostEvent>, HostError> = self
            .vec
            .iter()
            .map(|e| match &e.0 {
                InternalEvent::Contract(c) => {
                    // Metering: we use the cost of instantiating a size=1 `Vec` as an estimate
                    // for the cost collecting 1 `HostEvent` into the events buffer. Because
                    // the resulting buffer length may be different on different instances
                    // (due to diagnostic events) and we need a deterministic cost across all
                    // instances, the cost needs to be amortized and buffer size-independent.
                    Vec::<HostEvent>::charge_bulk_init_cpy(1, host)?;
                    Ok(HostEvent {
                        event: c.to_xdr(host)?,
                        failed_call: e.1 == EventError::FromFailedCall,
                    })
                }
                InternalEvent::Diagnostic(c) => host.as_budget().with_free_budget(|| {
                    Ok(HostEvent {
                        event: c.to_xdr(host)?,
                        failed_call: e.1 == EventError::FromFailedCall,
                    })
                }),
            })
            .collect();
        Ok(Events(vec?))
    }
}
