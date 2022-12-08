use crate::Host;

use super::{
    host::metered_map::MeteredOrdMap,
    host::metered_vector::MeteredVector,
    xdr::{self, ScObjectType},
    RawVal,
};

pub(crate) type HostMap = MeteredOrdMap<RawVal, RawVal, Host>;
pub(crate) type HostVec = MeteredVector<RawVal>;

#[derive(Clone)]
pub(crate) enum HostObject {
    Vec(HostVec),
    Map(HostMap),
    U64(u64),
    I64(i64),
    U128(u128),
    I128(i128),
    Bytes(Vec<u8>),
    ContractCode(xdr::ScContractCode),
    AccountId(xdr::AccountId),
}

pub(crate) trait HostObjectType: Sized {
    fn get_type() -> ScObjectType;
    fn inject(self) -> HostObject;
    fn try_extract(obj: &HostObject) -> Option<&Self>;
}

macro_rules! declare_host_object_type {
    ($TY:ty, $CODE:ident, $CASE:ident) => {
        impl HostObjectType for $TY {
            fn get_type() -> ScObjectType {
                ScObjectType::$CODE
            }

            fn inject(self) -> HostObject {
                HostObject::$CASE(self)
            }

            fn try_extract(obj: &HostObject) -> Option<&Self> {
                match obj {
                    HostObject::$CASE(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

// ${type of contained data}, ${identifier for ScObject}, ${case in HostObject}
declare_host_object_type!(HostMap, Map, Map);
declare_host_object_type!(HostVec, Vec, Vec);
declare_host_object_type!(u64, U64, U64);
declare_host_object_type!(i64, I64, I64);
declare_host_object_type!(u128, U128, U128);
declare_host_object_type!(i128, I128, I128);
declare_host_object_type!(Vec<u8>, Bytes, Bytes);
declare_host_object_type!(xdr::ScContractCode, ContractCode, ContractCode);
declare_host_object_type!(xdr::AccountId, AccountId, AccountId);
