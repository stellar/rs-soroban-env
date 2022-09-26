use crate::ConversionError;

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u64)]
pub enum InvokerType {
    Account = 0,
    Contract = 1,
}

impl From<InvokerType> for u64 {
    fn from(v: InvokerType) -> Self {
        v as u64
    }
}

impl TryFrom<u64> for InvokerType {
    type Error = ConversionError;

    fn try_from(v: u64) -> Result<Self, Self::Error> {
        const ACCOUNT: u64 = InvokerType::Account as u64;
        const CONTRACT: u64 = InvokerType::Contract as u64;
        match v {
            ACCOUNT => Ok(Self::Account),
            CONTRACT => Ok(Self::Contract),
            _ => Err(ConversionError),
        }
    }
}
