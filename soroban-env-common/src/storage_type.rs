use num_derive::FromPrimitive;
use stellar_xdr::ContractDataType;

use crate::declare_wasmi_marshal_for_enum;

/// This is just a distinct enum local to the env interface that is used as
/// an argument to storage functions. It doesn't correspond to any [`Val`] types,
/// and is passed by direct marshalling as a u64.
#[derive(Debug, FromPrimitive, PartialEq, Eq)]
pub enum StorageType {
    Temporary = 0,
    Persistent = 1,
    Instance = 2,
}

impl From<StorageType> for ContractDataType {
    fn from(value: StorageType) -> Self {
        match value {
            StorageType::Temporary => ContractDataType::Temporary,
            StorageType::Persistent | StorageType::Instance => ContractDataType::Persistent,
        }
    }
}

declare_wasmi_marshal_for_enum!(StorageType);
