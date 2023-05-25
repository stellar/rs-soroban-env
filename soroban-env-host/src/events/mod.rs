pub(crate) mod diagnostic;
mod internal;
pub(crate) mod system_events;

pub(crate) use internal::InternalEventsBuffer;
// expose them as pub use for benches
pub use internal::{InternalContractEvent, InternalEvent};
use soroban_env_common::{
    num::{i256_from_pieces, u256_from_pieces},
    xdr::{
        ContractEventBody, ContractEventType,
        PublicKey::PublicKeyTypeEd25519,
        ScAddress,
        ScContractExecutable::{Token, WasmRef},
        ScVal,
    },
    Error, RawVal, VecObject,
};

use crate::{budget::AsBudget, Host, HostError};

/// The external representation of a host event.
#[derive(Clone, Debug)]
pub struct HostEvent {
    pub event: crate::xdr::ContractEvent,
    // failed_call keeps track of if the call this event was emitted in failed
    pub failed_call: bool,
}

fn display_address(addr: &ScAddress, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match addr {
        ScAddress::Account(acct) => match &acct.0 {
            PublicKeyTypeEd25519(e) => write!(f, "Address(Account({}))", e),
        },
        ScAddress::Contract(hash) => write!(f, "Address(Contract({}))", hash),
    }
}

fn display_scval(scv: &ScVal, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match scv {
        ScVal::Bool(v) => write!(f, "{}", v),
        ScVal::Void => write!(f, "Void"),
        ScVal::Error(e) => write!(f, "{:?}", Error::from_scerror(e.clone())),
        ScVal::U32(v) => write!(f, "{}", v),
        ScVal::I32(v) => write!(f, "{}", v),
        ScVal::U64(v) => write!(f, "{}", v),
        ScVal::I64(v) => write!(f, "{}", v),
        ScVal::Timepoint(v) => write!(f, "TimePoint({})", v.0),
        ScVal::Duration(v) => write!(f, "Duration({})", v.0),
        ScVal::U128(v) => write!(f, "{}", u128::from(v)),
        ScVal::I128(v) => write!(f, "{}", i128::from(v)),
        ScVal::U256(v) => write!(
            f,
            "{}",
            u256_from_pieces(v.hi_hi, v.hi_lo, v.lo_hi, v.lo_lo)
        ),
        ScVal::I256(v) => write!(
            f,
            "{}",
            i256_from_pieces(v.hi_hi, v.hi_lo, v.lo_hi, v.lo_lo)
        ),
        ScVal::Bytes(v) => write!(f, "Bytes({})", v.0),
        ScVal::String(v) => write!(f, "\"{}\"", v.0),
        ScVal::Symbol(v) => write!(f, "{}", v.0),
        ScVal::Vec(None) => write!(f, "[]"),
        ScVal::Vec(Some(vec)) => {
            write!(f, "[")?;
            for (i, e) in vec.0.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                display_scval(e, f)?;
            }
            write!(f, "]")
        }
        ScVal::Map(None) => write!(f, "{{}}"),
        ScVal::Map(Some(pairs)) => {
            write!(f, "{{")?;
            for (i, e) in pairs.0.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                display_scval(&e.key, f)?;
                write!(f, ": ")?;
                display_scval(&e.val, f)?;
            }
            write!(f, "}}")
        }
        ScVal::ContractExecutable(WasmRef(hash)) => {
            write!(f, "ContractExecutable(WasmRef({}))", hash)
        }
        ScVal::ContractExecutable(Token) => write!(f, "ContractExecutable(Token)"),
        ScVal::Address(addr) => display_address(addr, f),
        ScVal::LedgerKeyContractExecutable => write!(f, "LedgerKeyContractExecutable"),
        ScVal::LedgerKeyNonce(n) => {
            write!(f, "LedgerKeyNonce(")?;
            display_address(&n.nonce_address, f)?;
            write!(f, ")")
        }
        ScVal::StorageType(t) => {
            write!(f, "StorageType{}", t)
        }
    }
}

impl core::fmt::Display for HostEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.failed_call {
            write!(f, "[Failed {} Event (not emitted)] ", self.event.type_)?;
        } else {
            write!(f, "[{} Event] ", self.event.type_)?;
        }
        match &self.event.contract_id {
            None => (),
            Some(hash) => write!(f, "contract:{}, ", *hash)?,
        }
        match &self.event.body {
            ContractEventBody::V0(ceb) => {
                write!(f, "topics:[")?;
                for (i, topic) in ceb.topics.0.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    display_scval(topic, f)?;
                }
                write!(f, "], data:")?;
                display_scval(&ceb.data, f)
            }
        }
    }
}

/// The external representation of events in the chronological order.
#[derive(Clone, Debug, Default)]
pub struct Events(pub Vec<HostEvent>);

// Maximum number of topics in a `ContractEvent`. This applies to both
// `Contract` and `System` types of contract events.
pub(crate) const CONTRACT_EVENT_TOPICS_LIMIT: usize = 4;
// Maximum number of bytes in a topic `Bytes`.
pub(crate) const TOPIC_BYTES_LENGTH_LIMIT: usize = 64;

impl Host {
    pub(crate) fn with_events_mut<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut InternalEventsBuffer) -> Result<U, HostError>,
    {
        f(&mut self.0.events.borrow_mut())
    }

    pub fn get_events(&self) -> Result<Events, HostError> {
        self.0.events.borrow().externalize(self)
    }

    // Records a contract event.
    pub(crate) fn record_contract_event(
        &self,
        type_: ContractEventType,
        topics: VecObject,
        data: RawVal,
    ) -> Result<(), HostError> {
        self.validate_contract_event_topics(topics)?;
        let ce = InternalContractEvent {
            type_,
            contract_id: self.bytesobj_from_internal_contract_id()?,
            topics,
            data,
        };
        self.with_events_mut(|events| {
            Ok(events.record(InternalEvent::Contract(ce), self.as_budget()))
        })?
    }
}
