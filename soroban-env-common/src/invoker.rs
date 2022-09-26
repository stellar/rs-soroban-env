use crate::ConversionError;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u32)]
pub enum InvokerType {
    Account = 0,
    Contract = 1,
}

impl From<InvokerType> for u32 {
    fn from(v: InvokerType) -> Self {
        v as u32
    }
}

impl TryFrom<u32> for InvokerType {
    type Error = ConversionError;

    fn try_from(v: u32) -> Result<Self, Self::Error> {
        const ACCOUNT: u32 = InvokerType::Account as u32;
        const CONTRACT: u32 = InvokerType::Contract as u32;
        match v {
            ACCOUNT => Ok(Self::Account),
            CONTRACT => Ok(Self::Contract),
            _ => Err(ConversionError),
        }
    }
}
