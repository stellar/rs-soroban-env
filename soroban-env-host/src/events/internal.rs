use std::rc::Rc;

use soroban_env_common::{
    xdr::{ScErrorCode, ScErrorType},
    BytesObject, VecObject,
};

use super::{Event, Events, HostEvent};
use crate::{
    budget::{AsBudget, Budget},
    xdr,
    xdr::ScVal,
    Host, HostError, RawVal,
};

/// The internal representation of a `ContractEvent` that is stored in the events buffer
/// and designed to be cheap to clone.
// This is exposed as a pub type for benches.
#[derive(Clone, Debug)]
pub struct InternalContractEvent {
    pub type_: xdr::ContractEventType,
    pub contract_id: Option<BytesObject>,
    pub topics: VecObject,
    pub data: RawVal,
}

impl InternalContractEvent {
    // Metering: covered by components
    pub fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics = if let ScVal::Vec(Some(v)) = host.from_host_obj(self.topics)?.into() {
            Ok(v)
        } else {
            Err(host.err(
                ScErrorType::Events,
                ScErrorCode::InvalidInput,
                "converting event topics to vector",
                &[self.topics.to_raw()],
            ))
        }?;
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
/// stores "plain" rust data values -- String and Vec -- capturing the input to
/// diagnostics, rather than interacting with the host object system. It does
/// this for two reasons:
///
///   1. to avoid to avoid perturbing object numbering when debug-mode is
///      enabled
///
///   2. to avoid re-entering the host object management code by adding (say)
///      host objects for the contract ID, topics vector or such in the middle
///      of a diagnostic call occurs during a read from the host object table
///      (eg. validating a host function call's input arguments).

#[derive(Clone, Debug)]
pub struct InternalDiagnosticEvent {
    pub contract_id: Option<crate::xdr::Hash>,
    pub topics: Vec<RawVal>,
    pub msg: Option<String>,
    pub data: Vec<RawVal>,
}

impl InternalDiagnosticEvent {
    pub fn to_xdr(&self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics: Result<Vec<ScVal>, HostError> =
            self.topics.iter().map(|t| host.from_host_val(*t)).collect();
        let topics = xdr::ScVec::from(xdr::VecM::try_from(topics?)?);
        let data: Result<Vec<ScVal>, HostError> =
            self.data.iter().map(|d| host.from_host_val(*d)).collect();
        let mut data = data?;
        if let Some(msg) = &self.msg {
            data.insert(
                0,
                ScVal::String(xdr::ScString::from(xdr::StringM::try_from(msg)?)),
            );
        }
        let data = ScVal::Vec(Some(xdr::ScVec::from(xdr::VecM::try_from(data)?)));
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
    StructuredDebug(Rc<InternalDiagnosticEvent>),
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
    pub fn record(&mut self, e: InternalEvent, _budget: &Budget) -> Result<(), HostError> {
        //TODO:Add metering for non-diagnostic events
        self.vec.push((e, EventError::FromSuccessfulCall));
        Ok(())
    }

    /// Rolls back the event buffer starting at `events`.
    pub fn rollback(&mut self, events: usize) -> Result<(), HostError> {
        // note that we first skip the events that are not being rolled back
        for e in self.vec.iter_mut().skip(events) {
            e.1 = EventError::FromFailedCall;
        }

        Ok(())
    }

    #[allow(unused)]
    pub fn dump_to_debug_log(&self) {
        use log::debug;
        debug!("=======Start of events=======");
        for e in self.vec.iter() {
            match &e.0 {
                InternalEvent::Contract(c) => debug!("Contract event: {:?}", c),
                InternalEvent::StructuredDebug(c) => debug!("StructuredDebug event: {:?}", c),
            }
        }
        debug!("========End of events========")
    }

    /// Converts the internal events into their external representation. This should only be called
    /// either when the host is finished (via `try_finish`), or when an error occurs.
    // TODO: Metering for non-diagnostic events?
    pub fn externalize(&self, host: &Host) -> Result<Events, HostError> {
        let vec: Result<Vec<HostEvent>, HostError> = self
            .vec
            .iter()
            .map(|e| match &e.0 {
                InternalEvent::Contract(c) => Ok(HostEvent {
                    event: Event::Contract(c.to_xdr(host)?),
                    failed_call: e.1 == EventError::FromFailedCall,
                }),
                InternalEvent::StructuredDebug(c) => host.as_budget().with_free_budget(|| {
                    Ok(HostEvent {
                        event: Event::StructuredDebug(c.to_xdr(host)?),
                        failed_call: e.1 == EventError::FromFailedCall,
                    })
                }),
            })
            .collect();
        Ok(Events(vec?))
    }
}
