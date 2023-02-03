mod debug;
mod internal;

use crate::xdr::ContractEvent;
pub use debug::{DebugArg, DebugError, DebugEvent};
pub(crate) use internal::{InternalContractEvent, InternalEvent, InternalEventsBuffer};

// TODO: optimize storage on this to use pools / bumpalo / etc.
#[derive(Clone, Debug)]
pub enum HostEvent {
    Contract(ContractEvent),
    Debug(DebugEvent),
}

#[derive(Clone, Debug, Default)]
pub struct Events(pub Vec<HostEvent>);

// Maximum number of topics in a `ContractEvent`. This applies to both
// `Contract` and `System` types of contract events.
pub(crate) const CONTRACT_EVENT_TOPICS_LIMIT: usize = 4;
// Maximum number of bytes in a topic `Bytes`.
pub(crate) const TOPIC_BYTES_LENGTH_LIMIT: usize = 32;
