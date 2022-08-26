use super::{
    host::metered_bigint::MeteredBigInt,
    host::metered_map::MeteredOrdMap,
    host::metered_vector::MeteredVector,
    weak_host::WeakHost,
    xdr::{self, ScObjectType},
    EnvVal, Object, RawVal,
};

pub(crate) type HostObj = EnvVal<WeakHost, Object>;
pub(crate) type HostVal = EnvVal<WeakHost, RawVal>;
pub(crate) type HostMap = MeteredOrdMap<HostVal, HostVal>;
pub(crate) type HostVec = MeteredVector<HostVal>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum HostObject {
    Vec(HostVec),
    Map(HostMap),
    U64(u64),
    I64(i64),
    Bin(Vec<u8>),
    BigInt(MeteredBigInt),
    Hash(xdr::ScHash),
    PublicKey(xdr::PublicKey),
    ContractCode(xdr::ScContractCode),
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
declare_host_object_type!(Vec<u8>, Bytes, Bin);
declare_host_object_type!(MeteredBigInt, BigInt, BigInt);
declare_host_object_type!(xdr::ScHash, Hash, Hash);
declare_host_object_type!(xdr::PublicKey, PublicKey, PublicKey);
declare_host_object_type!(xdr::ScContractCode, ContractCode, ContractCode);
