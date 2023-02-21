mod debug;
mod internal;

pub use debug::{DebugArg, DebugError, DebugEvent};
pub(crate) use internal::{InternalContractEvent, InternalEvent, InternalEventsBuffer};

/// The external representation of a host event.
// TODO: optimize storage on this to use pools / bumpalo / etc.
#[derive(Clone, Debug)]
pub enum HostEvent {
    Contract(crate::xdr::ContractEvent),
    Debug(DebugEvent),
    StructuredDebug(crate::xdr::ContractEvent),
}

/// The external representation of events in the chronological order.
#[derive(Clone, Debug, Default)]
pub struct Events(pub Vec<HostEvent>);

#[derive(Clone, Default)]
pub struct RolledbackDiagnosticEvents(pub Vec<crate::xdr::ContractEvent>);

// Maximum number of topics in a `ContractEvent`. This applies to both
// `Contract` and `System` types of contract events.
pub(crate) const CONTRACT_EVENT_TOPICS_LIMIT: usize = 4;
// Maximum number of bytes in a topic `Bytes`.
pub(crate) const TOPIC_BYTES_LENGTH_LIMIT: usize = 32;
