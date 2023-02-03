use super::{DebugEvent, Events, HostEvent};
use crate::{
    budget::Budget,
    host::metered_clone::MeteredClone,
    xdr,
    xdr::{Hash, ScObject},
    Host, HostError, MeteredVector, Object, RawVal,
};
use log::debug;

#[derive(Clone, Debug)]
pub(crate) struct InternalContractEvent {
    pub(crate) type_: xdr::ContractEventType,
    pub(crate) contract_id: Option<Hash>,
    pub(crate) topics: Object,
    pub(crate) data: RawVal,
}

impl InternalContractEvent {
    // Metering: covered by components
    pub fn to_xdr(self, host: &Host) -> Result<xdr::ContractEvent, HostError> {
        let topics = if let ScObject::Vec(v) = host.from_host_obj(self.topics)? {
            Ok(v)
        } else {
            Err(host.err_status(xdr::ScHostObjErrorCode::UnexpectedType))
        }?;
        let data = host.from_host_val(self.data)?;
        Ok(xdr::ContractEvent {
            ext: xdr::ExtensionPoint::V0,
            contract_id: self.contract_id,
            type_: self.type_,
            body: xdr::ContractEventBody::V0(xdr::ContractEventV0 { topics, data }),
        })
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) enum InternalEvent {
    Contract(InternalContractEvent),
    Debug(DebugEvent),
    #[default]
    None,
}

impl MeteredClone for InternalEvent {}

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

    // Metering covered by the `MeteredVec`
    pub fn rollback(&mut self, events: usize, budget: &Budget) -> Result<(), HostError> {
        let mut i = events;
        let mut v = self.vec.clone();
        loop {
            match v.get(i, budget)? {
                InternalEvent::Contract(_) => {
                    v = v.remove(i, budget)?;
                }
                InternalEvent::Debug(_) => {
                    i += 1;
                }
                InternalEvent::None => {
                    i += 1;
                }
            }
            if i == v.len() {
                break;
            }
        }
        self.vec = v;
        Ok(())
    }

    #[allow(unused)]
    pub fn dump_to_debug_log(&self) {
        for e in self.vec.iter() {
            match e {
                InternalEvent::Contract(c) => debug!("Contract event: {:?}", c),
                InternalEvent::Debug(d) => debug!("Debug event: {}", d),
                InternalEvent::None => (),
            }
        }
    }

    // Metering: the new vec allocation is not charged. But that should be fine, since externalize
    // should only be called once, either when finishing the host or erroring.
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
