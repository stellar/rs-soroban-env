use crate::{
    declare_wasmi_marshal_for_enum,
    xdr::{ContractDataDurability, ScErrorCode, ScErrorType},
};
use num_derive::FromPrimitive;

/// This is just a distinct enum local to the env interface that is used as
/// an argument to storage functions. It doesn't correspond to any [`Val`] types,
/// and is passed by direct marshalling as a u64.
#[repr(u64)]
#[derive(Debug, FromPrimitive, PartialEq, Eq, Clone, Copy)]
pub enum StorageType {
    Temporary = 0,
    Persistent = 1,
    Instance = 2,
}

impl TryFrom<StorageType> for ContractDataDurability {
    type Error = crate::Error;

    fn try_from(value: StorageType) -> Result<Self, Self::Error> {
        match value {
            StorageType::Temporary => Ok(ContractDataDurability::Temporary),
            StorageType::Persistent => Ok(ContractDataDurability::Persistent),
            StorageType::Instance => Err(crate::Error::from_type_and_code(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
            )),
        }
    }
}

declare_wasmi_marshal_for_enum!(StorageType);
