use super::{DebugEvent, Events, HostEvent};
use crate::{
    budget::{AsBudget, Budget},
    host::metered_clone::MeteredClone,
    xdr,
    xdr::ScObject,
    Host, HostError, MeteredVector, Object, RawVal,
};
use log::debug;

#[derive(Clone, Debug)]
pub(crate) struct InternalContractEvent {
    pub(crate) type_: xdr::ContractEventType,
    pub(crate) contract_id: Option<Object>,
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
        let contract_id = match self.contract_id {
            Some(id) => Some(host.hash_from_obj_input("contract_id", id)?),
            None => None,
        };
        Ok(xdr::ContractEvent {
            ext: xdr::ExtensionPoint::V0,
            contract_id: contract_id,
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

    fn defunct_contract_event(c: &mut InternalContractEvent) -> InternalEvent {
        let ty: RawVal = <i32>::from(c.type_).into();
        let id: RawVal = c.contract_id.map_or(RawVal::from_void(), |obj| obj.into());
        let dbg = DebugEvent::new()
            .msg("rolled-back contract event: type {}, id {}, topics {}, data {}")
            .arg(ty)
            .arg(id)
            .arg(RawVal::from(c.topics))
            .arg(c.data);
        InternalEvent::Debug(dbg)
    }

    // Metering covered by the `MeteredVec`
    pub fn rollback(&mut self, events: usize, host: &Host) -> Result<(), HostError> {
        self.vec = self.vec.retain_mut(
            |i, e| {
                if i < events {
                    Ok(true)
                } else {
                    if let InternalEvent::Contract(c) = e {
                        *e = Self::defunct_contract_event(c);
                        Ok(true)
                    } else {
                        Ok(true)
                    }
                }
            },
            host.as_budget(),
        )?;
        self.vec = self.vec.push_back(
            InternalEvent::Debug(
                DebugEvent::new()
                    .msg("Events rolled back, starting from buffer position {}")
                    .arg(host.usize_to_rawval_u32(events)?),
            ),
            host.as_budget(),
        )?;
        self.dump_to_debug_log();
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
