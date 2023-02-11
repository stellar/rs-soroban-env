use soroban_env_common::{
    DurationSmall, I128Small, I256Small, I64Small, SymbolSmall, SymbolStr, TimepointSmall,
    U128Small, U256Small, U64Small,
};

use crate::host::metered_clone::MeteredClone;

use super::{
    host::metered_map::MeteredOrdMap,
    host::metered_vector::MeteredVector,
    num::{I256, U256},
    xdr, AddressObject, BytesObject, ContractExecutableObject, DurationObject, Host, I128Object,
    I256Object, I64Object, LedgerKeyNonceObject, MapObject, Object, RawVal, StringObject,
    SymbolObject, TimepointObject, U128Object, U256Object, U64Object, VecObject,
};

pub(crate) type HostMap = MeteredOrdMap<RawVal, RawVal, Host>;
pub(crate) type HostVec = MeteredVector<RawVal>;

#[derive(Clone)]
pub(crate) enum HostObject {
    Vec(HostVec),
    Map(HostMap),
    U64(u64),
    I64(i64),
    TimePoint(xdr::TimePoint),
    Duration(xdr::Duration),
    U128(u128),
    I128(i128),
    U256(U256),
    I256(I256),
    Bytes(xdr::ScBytes),
    String(xdr::ScString),
    Symbol(xdr::ScSymbol),
    ContractExecutable(xdr::ScContractExecutable),
    Address(xdr::ScAddress),
    NonceKey(xdr::ScNonceKey),
}

impl HostObject {
    // Temporarily performs a shallow comparison against a RawVal of the
    // associated small value type, returning None if the RawVal is of
    // the wrong type.
    //
    // FIXME: these are fixed size but to be precise they need to connect to metering.
    pub(crate) fn try_compare_to_small(&self, rv: RawVal) -> Option<core::cmp::Ordering> {
        match self {
            HostObject::U64(u) => {
                let Ok(small) = U64Small::try_from(rv) else { return None };
                let small: u64 = small.into();
                Some(u.cmp(&small))
            }
            HostObject::I64(i) => {
                let Ok(small) = I64Small::try_from(rv) else { return None };
                let small: i64 = small.into();
                Some(i.cmp(&small))
            }
            HostObject::TimePoint(tp) => {
                let Ok(small) = TimepointSmall::try_from(rv) else { return None };
                let small: u64 = small.into();
                Some(tp.0.cmp(&small))
            }
            HostObject::Duration(d) => {
                let Ok(small) = DurationSmall::try_from(rv) else { return None };
                let small: u64 = small.into();
                Some(d.0.cmp(&small))
            }
            HostObject::U128(u) => {
                let Ok(small) = U128Small::try_from(rv) else { return None };
                let small: u128 = small.into();
                Some(u.cmp(&small))
            }
            HostObject::I128(i) => {
                let Ok(small) = I128Small::try_from(rv) else { return None };
                let small: i128 = small.into();
                Some(i.cmp(&small))
            }
            HostObject::U256(u) => {
                let Ok(small) = U256Small::try_from(rv) else { return None };
                let small: U256 = small.into();
                Some(u.cmp(&small))
            }
            HostObject::I256(i) => {
                let Ok(small) = I256Small::try_from(rv) else { return None };
                let small: I256 = small.into();
                Some(i.cmp(&small))
            }
            HostObject::Symbol(s) => {
                let Ok(small) = SymbolSmall::try_from(rv) else { return None };
                let small: SymbolStr = small.into();
                let rhs: &[u8] = small.as_ref();
                Some(s.as_vec().as_slice().cmp(rhs))
            }

            HostObject::Vec(_)
            | HostObject::Map(_)
            | HostObject::Bytes(_)
            | HostObject::String(_)
            | HostObject::ContractExecutable(_)
            | HostObject::Address(_)
            | HostObject::NonceKey(_) => None,
        }
    }
}

pub(crate) trait HostObjectType: MeteredClone {
    type Wrapper: Into<Object>;
    fn new_from_handle(handle: u32) -> Self::Wrapper;
    fn inject(self) -> HostObject;
    fn try_extract(obj: &HostObject) -> Option<&Self>;
}

// Some host objects are "a slab of memory" which we want
// to treat fairly uniformly in memory-related host functions.
pub(crate) trait MemHostObjectType:
    HostObjectType + TryFrom<Vec<u8>, Error = xdr::Error> + Into<Vec<u8>>
{
    fn as_byte_slice(&self) -> &[u8];
}

macro_rules! declare_host_object_type {
    ($TY:ty, $TAG:ident, $CASE:ident) => {
        impl HostObjectType for $TY {
            type Wrapper = $TAG;
            fn new_from_handle(handle: u32) -> Self::Wrapper {
                unsafe { $TAG::from_handle(handle) }
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

macro_rules! declare_mem_host_object_type {
    ($TY:ty, $TAG:ident, $CASE:ident) => {
        declare_host_object_type!($TY, $TAG, $CASE);
        impl MemHostObjectType for $TY {
            fn as_byte_slice(&self) -> &[u8] {
                self.as_slice()
            }
        }
    };
}

// ${type of contained data}, ${object-wrapper common type}, ${case in HostObject}
declare_host_object_type!(HostMap, MapObject, Map);
declare_host_object_type!(HostVec, VecObject, Vec);
declare_host_object_type!(u64, U64Object, U64);
declare_host_object_type!(i64, I64Object, I64);
declare_host_object_type!(xdr::TimePoint, TimepointObject, TimePoint);
declare_host_object_type!(xdr::Duration, DurationObject, Duration);
declare_host_object_type!(u128, U128Object, U128);
declare_host_object_type!(i128, I128Object, I128);
declare_host_object_type!(U256, U256Object, U256);
declare_host_object_type!(I256, I256Object, I256);
declare_mem_host_object_type!(xdr::ScBytes, BytesObject, Bytes);
declare_mem_host_object_type!(xdr::ScString, StringObject, String);
declare_mem_host_object_type!(xdr::ScSymbol, SymbolObject, Symbol);
declare_host_object_type!(
    xdr::ScContractExecutable,
    ContractExecutableObject,
    ContractExecutable
);
declare_host_object_type!(xdr::ScAddress, AddressObject, Address);
declare_host_object_type!(xdr::ScNonceKey, LedgerKeyNonceObject, NonceKey);
