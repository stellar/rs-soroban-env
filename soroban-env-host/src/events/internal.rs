use soroban_env_common::{BytesObject, VecObject};

use super::{DebugEvent, Events, HostEvent};
use crate::{
    budget::{AsBudget, Budget},
    xdr,
    xdr::ScVal,
    Host, HostError, MeteredVector, RawVal,
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
            Some(id) => Some(host.hash_from_obj_input("contract_id", id)?),
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
    #[default]
    None,
}

/// The events buffer. Stores `InternalEvent`s in the chronological order.
#[derive(Clone, Default)]
pub(crate) struct InternalEventsBuffer {
    pub(crate) vec: MeteredVector<InternalEvent>,
}

impl InternalEventsBuffer {
    // Records an InternalEvent
    // Metering covered by the `MeteredVec`.
    pub fn record(&mut self, e: InternalEvent, budget: &Budget) -> Result<(), HostError> {
        self.vec = self.vec.push_back(e, budget)?;
        Ok(())
    }

    fn defunct_contract_event(c: &mut InternalContractEvent) -> InternalEvent {
        let ty: RawVal = <i32>::from(c.type_).into();
        let id: RawVal = c.contract_id.map_or(RawVal::VOID.into(), |obj| obj.into());
        let dbg = DebugEvent::new()
            .msg("rolled-back contract event: type {}, id {}, topics {}, data {}")
            .arg(ty)
            .arg(id)
            .arg(RawVal::from(c.topics))
            .arg(c.data);
        InternalEvent::Debug(dbg)
    }

    /// Rolls back the event buffer starting at `events`. Any `ContractEvent` will be converted
    /// to a `DebugEvent` indicating the event has been rolled back. An additional `DebugEvent`
    /// will be pushed at the end indicating the rollback happened.
    // Metering covered by the `MeteredVec`
    pub fn rollback(&mut self, events: usize, host: &Host) -> Result<(), HostError> {
        let mut rollback_count = 0u32;
        self.vec = self.vec.retain_mut(
            |i, e| {
                if i < events {
                    Ok(true)
                } else {
                    if let InternalEvent::Contract(c) = e {
                        *e = Self::defunct_contract_event(c);
                        rollback_count += 1;
                        Ok(true)
                    } else {
                        Ok(true)
                    }
                }
            },
            host.as_budget(),
        )?;
        // If any events were rolled back, we push one more debug event at the end to
        // let the user know.
        if rollback_count > 0 {
            self.vec = self.vec.push_back(
                InternalEvent::Debug(
                    DebugEvent::new()
                        .msg("{} contract events rolled back. Rollback start pos = {}")
                        .arg(RawVal::from(rollback_count))
                        .arg(host.usize_to_rawval_u32(events)?.to_raw()),
                ),
                host.as_budget(),
            )?;
        }
        Ok(())
    }

    #[allow(unused)]
    pub fn dump_to_debug_log(&self) {
        use log::debug;
        debug!("=======Start of events=======");
        for e in self.vec.iter() {
            match e {
                InternalEvent::Contract(c) => debug!("Contract event: {:?}", c),
                InternalEvent::Debug(d) => debug!("Debug event: {}", d),
                InternalEvent::None => (),
            }
        }
        debug!("========End of events========")
    }

    /// Converts the internal events into their external representation. This should only be called
    /// either when the host is finished (via `try_finish`), or when an error occurs.
    // Metering: the new vec allocation is not charged, but that should be fine.
    pub fn externalize(&self, host: &Host) -> Result<Events, HostError> {
        let vec: Result<Vec<HostEvent>, HostError> = self
            .vec
            .iter()
            .map(|e| match e {
                InternalEvent::Contract(c) => Ok(HostEvent::Contract(c.clone().to_xdr(host)?)),
                InternalEvent::Debug(d) => Ok(HostEvent::Debug(d.clone())),
                InternalEvent::None => Err(host.err_general("Unexpected event type")),
            })
            .collect();
        Ok(Events(vec?))
    }
}
