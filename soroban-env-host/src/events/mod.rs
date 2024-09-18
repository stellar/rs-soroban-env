pub(crate) mod diagnostic;
mod internal;
pub(crate) mod system_events;
pub(crate) use internal::{
    EventError, InternalDiagnosticArg, InternalDiagnosticEvent, InternalEventsBuffer,
};
// expose them as pub use for benches
use crate::{
    num::{i256_from_pieces, u256_from_pieces},
    xdr::{
        ContractEventBody, ContractEventType, ContractExecutable, PublicKey::PublicKeyTypeEd25519,
        ScAddress, ScContractInstance, ScVal,
    },
    Error, Host, HostError, Val, VecObject,
};
pub(crate) use internal::{InternalContractEvent, InternalEvent};

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
            PublicKeyTypeEd25519(e) => {
                let strkey = stellar_strkey::ed25519::PublicKey(e.0);
                write!(f, "{}", strkey)
            }
        },
        ScAddress::Contract(hash) => {
            let strkey = stellar_strkey::Contract(hash.0);
            write!(f, "{}", strkey)
        }
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
        ScVal::Address(addr) => display_address(addr, f),
        ScVal::LedgerKeyContractInstance => write!(f, "LedgerKeyContractInstance"),
        ScVal::LedgerKeyNonce(n) => {
            write!(f, "LedgerKeyNonce({})", n.nonce)
        }
        ScVal::ContractInstance(ScContractInstance {
            executable: ContractExecutable::Wasm(hash),
            ..
        }) => {
            write!(f, "ContractInstance(Wasm({}))", hash)
        }
        ScVal::ContractInstance(ScContractInstance {
            executable: ContractExecutable::StellarAsset,
            ..
        }) => write!(f, "ContractInstance(StellarAsset)"),
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
            Some(hash) => {
                let strkey = stellar_strkey::Contract(hash.0);
                write!(f, "contract:{}, ", strkey)?
            }
        }
        match &self.event.body {
            ContractEventBody::V0(ceb) => {
                write!(f, "topics:[")?;

                let mut is_fn_call = false;
                for (i, topic) in ceb.topics.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    // The second topic of the fn_call event is the contract id as ScBytes,
                    // but we want to display it as a C key instead, so this block
                    // tries to deduce if the event is the fn_call event.
                    if i == 1 && is_fn_call {
                        if let ScVal::Bytes(bytes) = topic {
                            let try_convert_to_hash =
                                TryInto::<[u8; 32]>::try_into(bytes.0.clone());
                            if let Ok(contract_id) = try_convert_to_hash {
                                let strkey = stellar_strkey::Contract(contract_id);
                                write!(f, "{}", strkey)?;
                                continue;
                            }
                        }
                    }

                    display_scval(topic, f)?;

                    if i == 0 {
                        if let ScVal::Symbol(first_topic_str) = topic {
                            if first_topic_str.0.as_slice() == "fn_call".as_bytes() {
                                is_fn_call = true;
                            }
                        }
                    }
                }
                write!(f, "], data:")?;
                display_scval(&ceb.data, f)
            }
        }
    }
}

#[test]
fn host_event_contract_id_is_strkey() {
    use crate::xdr::{
        AccountId, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0,
        ExtensionPoint, Hash, PublicKey,
    };
    let he = HostEvent {
        event: ContractEvent {
            ext: ExtensionPoint::V0,
            contract_id: Some(Hash([0; 32])),
            type_: ContractEventType::Diagnostic,
            body: ContractEventBody::V0(ContractEventV0 {
                topics: vec![ScVal::Address(ScAddress::Account(AccountId(
                    PublicKey::PublicKeyTypeEd25519([0; 32].into()),
                )))]
                .try_into()
                .unwrap(),
                data: ScVal::Void,
            }),
        },
        failed_call: false,
    };
    assert_eq!(
        format!("{}", he),
        "[Diagnostic Event] contract:CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABSC4, topics:[GAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAWHF], data:Void"
    );
}

/// The external representation of events in the chronological order.
#[derive(Clone, Debug, Default)]
pub struct Events(pub Vec<HostEvent>);

impl Host {
    pub(crate) fn with_events_mut<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut InternalEventsBuffer) -> Result<U, HostError>,
    {
        f(&mut *self.try_borrow_events_mut()?)
    }

    pub fn get_events(&self) -> Result<Events, HostError> {
        self.try_borrow_events()?.externalize(self)
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn get_diagnostic_events(&self) -> Result<Events, HostError> {
        self.try_borrow_events()?.externalize_diagnostics(self)
    }

    // Records a contract event.
    pub(crate) fn record_contract_event(
        &self,
        type_: ContractEventType,
        topics: VecObject,
        data: Val,
    ) -> Result<(), HostError> {
        let ce = InternalContractEvent {
            type_,
            contract_id: self.bytesobj_from_internal_contract_id()?,
            topics,
            data,
        };
        self.with_events_mut(|events| Ok(events.record(InternalEvent::Contract(ce), self)))?
    }
}
