use crate::host::HostError;
use soroban_env_common::ConversionError;

#[derive(Debug)]
pub enum Error {
    HostError(HostError),
    ContractError,
}

impl From<ConversionError> for Error {
    fn from(e: ConversionError) -> Self {
        Error::HostError(e.into())
    }
}

impl From<HostError> for Error {
    fn from(e: HostError) -> Self {
        Error::HostError(e)
    }
}

impl From<Error> for HostError {
    fn from(e: Error) -> Self {
        match e {
            Error::HostError(he) => he,
            Error::ContractError => crate::xdr::ScUnknownErrorCode::General.into(),
        }
    }
}
