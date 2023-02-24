use soroban_env_common::{BytesObject, VecObject};

use super::{DebugEvent, Event, Events, HostEvent};
use crate::{
    budget::{AsBudget, Budget},
    xdr,
    xdr::ScVal,
    Host, HostError, RawVal,
};

/// The internal representation of a `ContractEvent` that is stored in the events buffer
/// and designed to be cheap to clone.
#[derive(Clone, Debug)]
pub(crate) struct InternalContractEvent {
    pub(crate) type_: xdr::ContractEventType,
    pub(crate) contract_id: Option<BytesObject>,
    pub(crate) topics: VecObject,
    pub(crate) data: RawVal,
}

impl InternalContractEvent {
    // Metering: covered by components
    pub fn to_xdr(self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics = if let ScVal::Vec(Some(v)) = host.from_host_obj(self.topics)?.into() {
            Ok(v)
        } else {
            Err(host.err_status(xdr::ScHostObjErrorCode::UnexpectedType))
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

/// The internal representation of an `Event` that is stored in the events buffer
/// and designed to be cheap to cloned.
#[derive(Clone, Debug, Default)]
pub(crate) enum InternalEvent {
    Contract(InternalContractEvent),
    Debug(DebugEvent),
    StructuredDebug(InternalContractEvent),
    #[default]
    None,
}

/// The events buffer. Stores `InternalEvent`s in the chronological order.
#[derive(Clone, Default)]
pub(crate) struct InternalEventsBuffer {
    //the bool keeps track of if the call this event was emitted in failed
    pub(crate) vec: Vec<(InternalEvent, bool)>,
}

impl InternalEventsBuffer {
    // Records an InternalEvent
    pub fn record(&mut self, e: InternalEvent, _budget: &Budget) -> Result<(), HostError> {
        //TODO:Add metering for non-diagnostic events
        self.vec.push((e, false));
        Ok(())
    }

    /// Rolls back the event buffer starting at `events`. Any `ContractEvent` will be converted
    /// to a `DebugEvent` indicating the event has been rolled back. An additional `DebugEvent`
    /// will be pushed at the end indicating the rollback happened.
    pub fn rollback(&mut self, events: usize) -> Result<(), HostError> {
        // note that we first skip the events that are not being rolled back
        for e in self.vec.iter_mut().skip(events) {
            e.1 = true;
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
                InternalEvent::Debug(d) => debug!("Debug event: {}", d),
                InternalEvent::StructuredDebug(c) => debug!("StructuredDebug event: {:?}", c),
                InternalEvent::None => (),
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
            .filter(|e| (host.is_debug() || !matches!(e.0, InternalEvent::Contract(_)) || !e.1)) //filter out rolledback Contract events if diagnostics are disabled
            .map(|e| match &e.0 {
                InternalEvent::Contract(c) => Ok(HostEvent {
                    event: Event::Contract(c.clone().to_xdr(host)?),
                    failed_call: e.1,
                }),
                InternalEvent::Debug(d) => Ok(HostEvent {
                    event: Event::Debug(d.clone()),
                    failed_call: e.1,
                }),
                InternalEvent::StructuredDebug(c) => host.as_budget().with_free_budget(|| {
                    Ok(HostEvent {
                        event: Event::StructuredDebug(c.clone().to_xdr(host)?),
                        failed_call: e.1,
                    })
                }),
                InternalEvent::None => Err(host.err_general("Unexpected event type")),
            })
            .collect();
        Ok(Events(vec?))
    }
}
