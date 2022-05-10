use super::{
    weak_host::WeakHost,
    xdr::{
        AccountId, Asset, LedgerKey, Operation, OperationResult, Price, ScObjectType, Transaction,
    },
    EnvObj, EnvVal,
};

use im_rc::{OrdMap, Vector};
use num_bigint::BigInt;
use num_rational::BigRational;

pub(crate) type HostObj = EnvObj<WeakHost>;
pub(crate) type HostVal = EnvVal<WeakHost>;
pub(crate) type HostMap = OrdMap<HostVal, HostVal>;
pub(crate) type HostVec = Vector<HostVal>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum HostObject {
    Box(HostVal),
    Vec(HostVec),
    Map(HostMap),
    U64(u64),
    I64(i64),
    Str(String),
    Bin(Vec<u8>),
    BigInt(BigInt),
    BigRat(BigRational),
    LedgerKey(LedgerKey),
    Operation(Operation),
    OperationResult(OperationResult),
    Transaction(Transaction),
    Asset(Asset),
    Price(Price),
    AccountID(AccountId),
}

pub(crate) trait HostObjectType: Sized {
    fn get_type() -> ScObjectType;
    fn inject(self) -> HostObject;
    fn try_extract(obj: &HostObject) -> Option<&Self>;
}

macro_rules! declare_host_object_type {
    ($TY:ty, $CODE:ident, $CTOR:ident) => {
        impl HostObjectType for $TY {
            fn get_type() -> ScObjectType {
                ScObjectType::$CODE
            }

            fn inject(self) -> HostObject {
                HostObject::$CTOR(self)
            }

            fn try_extract(obj: &HostObject) -> Option<&Self> {
                match obj {
                    HostObject::$CTOR(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

declare_host_object_type!(HostVal, ScoBox, Box);
declare_host_object_type!(HostMap, ScoMap, Map);
declare_host_object_type!(HostVec, ScoVec, Vec);
declare_host_object_type!(u64, ScoU64, U64);
declare_host_object_type!(i64, ScoI64, I64);
declare_host_object_type!(String, ScoString, Str);
declare_host_object_type!(Vec<u8>, ScoBinary, Bin);
declare_host_object_type!(BigInt, ScoBigint, BigInt);
declare_host_object_type!(BigRational, ScoBigrat, BigRat);

declare_host_object_type!(LedgerKey, ScoLedgerkey, LedgerKey);
declare_host_object_type!(Operation, ScoOperation, Operation);
declare_host_object_type!(OperationResult, ScoOperationResult, OperationResult);
declare_host_object_type!(Transaction, ScoTransaction, Transaction);
declare_host_object_type!(Asset, ScoAsset, Asset);
declare_host_object_type!(Price, ScoPrice, Price);
declare_host_object_type!(AccountId, ScoAccountid, AccountID);
