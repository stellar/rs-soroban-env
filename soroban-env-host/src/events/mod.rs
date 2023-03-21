mod debug;
mod internal;

pub use debug::{DebugArg, DebugError, DebugEvent};
pub(crate) use internal::InternalEventsBuffer;
// expose them as pub use for benches
pub use internal::{InternalContractEvent, InternalEvent};

/// The external representation of a host event.
// TODO: optimize storage on this to use pools / bumpalo / etc.
#[derive(Clone, Debug)]
pub struct HostEvent {
    pub event: Event,
    // failed_call keeps track of if the call this event was emitted in failed
    pub failed_call: bool,
}

#[derive(Clone, Debug)]
pub enum Event {
    Contract(crate::xdr::ContractEvent),
    // Debug events are metered and will not be reported in tx meta
    Debug(DebugEvent),
    // StructuredDebug should not affect metering
    StructuredDebug(crate::xdr::ContractEvent),
}

/// The external representation of events in the chronological order.
#[derive(Clone, Debug, Default)]
pub struct Events(pub Vec<HostEvent>);

// Maximum number of topics in a `ContractEvent`. This applies to both
// `Contract` and `System` types of contract events.
pub(crate) const CONTRACT_EVENT_TOPICS_LIMIT: usize = 4;
// Maximum number of bytes in a topic `Bytes`.
pub(crate) const TOPIC_BYTES_LENGTH_LIMIT: usize = 64;
