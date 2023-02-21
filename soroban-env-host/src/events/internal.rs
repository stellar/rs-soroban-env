use super::{DebugEvent, Events, HostEvent, RolledbackDiagnosticEvents};
use crate::{
    budget::{AsBudget, Budget},
    host::metered_clone::MeteredClone,
    xdr,
    xdr::ScObject,
    Host, HostError, Object, RawVal,
};

/// The internal representation of a `ContractEvent` that is stored in the events buffer
/// and designed to be cheap to clone.
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

impl MeteredClone for InternalEvent {}

/// The events buffer. Stores `InternalEvent`s in the chronological order.
#[derive(Clone, Default)]
pub(crate) struct InternalEventsBuffer {
    pub(crate) vec: Vec<InternalEvent>,
    // Store the xdr diagnostic events before rolling back. This is required because
    // the objects that the events point to will be truncated on rollback.
    pub(crate) rolled_back_diagnostics: RolledbackDiagnosticEvents,
}

impl InternalEventsBuffer {
    // Records an InternalEvent
    pub fn record(&mut self, e: InternalEvent, _budget: &Budget) -> Result<(), HostError> {
        //TODO:Add metering for non-diagnostic events
        self.vec.push(e);
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

    /// Rolls back the event buffer starting at `events`. Any `ContractEvent` will be converted
    /// to a `DebugEvent` indicating the event has been rolled back. An additional `DebugEvent`
    /// will be pushed at the end indicating the rollback happened.
    pub fn rollback(&mut self, events: usize, host: &Host) -> Result<(), HostError> {
        //TODO: Metering for non-diagnostic events?

        let mut rollback_count = 0u32;
        // note that we first skip the events that are not being rolled back
        for e in self.vec.iter_mut().skip(events) {
            match e {
                InternalEvent::Contract(c) => {
                    *e = Self::defunct_contract_event(c);
                    rollback_count += 1;
                }
                InternalEvent::StructuredDebug(c) => {
                    let xdr_event = host
                        .as_budget()
                        .with_free_budget(|| Ok(c.clone().to_xdr(host)?))?;

                    self.rolled_back_diagnostics.0.push(xdr_event);

                    *e = Self::defunct_contract_event(c);
                    rollback_count += 1;
                }
                InternalEvent::Debug(_) | InternalEvent::None => {}
            }
        }

        // If any events were rolled back, we push one more debug event at the end to
        // let the user know.
        if rollback_count > 0 {
            self.vec.push(InternalEvent::Debug(
                DebugEvent::new()
                    .msg("{} contract events rolled back. Rollback start pos = {}")
                    .arg(RawVal::from(rollback_count))
                    .arg(host.usize_to_rawval_u32(events)?),
            ));
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
                InternalEvent::StructuredDebug(c) => debug!("StructuredDebug event: {:?}", c),
                InternalEvent::None => (),
            }
        }
        debug!("========End of events========")
    }

    /// Converts the internal events into their external representation. This should only be called
    /// either when the host is finished (via `try_finish`), or when an error occurs.
    // TODO: Metering for non-diagnostic events?
    pub fn externalize(
        &self,
        host: &Host,
    ) -> Result<(Events, RolledbackDiagnosticEvents), HostError> {
        let vec: Result<Vec<HostEvent>, HostError> = self
            .vec
            .iter()
            .map(|e| match e {
                InternalEvent::Contract(c) => Ok(HostEvent::Contract(c.clone().to_xdr(host)?)),
                InternalEvent::Debug(d) => Ok(HostEvent::Debug(d.clone())),
                InternalEvent::StructuredDebug(c) => host
                    .as_budget()
                    .with_free_budget(|| Ok(HostEvent::StructuredDebug(c.clone().to_xdr(host)?))),
                InternalEvent::None => Err(host.err_general("Unexpected event type")),
            })
            .collect();
        Ok((Events(vec?), self.rolled_back_diagnostics.clone()))
    }
}
