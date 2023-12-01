use num_derive::FromPrimitive;
use soroban_env_common::Error;

// Use the same error type for all the built-in contract errors.
// In theory we could have a separate enum for each built-in contract, but it's
// not clear how to distinguish them if multiple built-in contracts are involved.
#[derive(Debug, FromPrimitive, PartialEq, Eq)]
pub(crate) enum ContractError {
    InternalError = 1,
    OperationNotSupportedError = 2,
    AlreadyInitializedError = 3,

    UnauthorizedError = 4,
    AuthenticationError = 5,
    AccountMissingError = 6,
    AccountIsNotClassic = 7,

    NegativeAmountError = 8,
    AllowanceError = 9,
    BalanceError = 10,
    BalanceDeauthorizedError = 11,
    OverflowError = 12,
    TrustlineMissingError = 13,
}

impl From<ContractError> for Error {
    fn from(err: ContractError) -> Self {
        Error::from_contract_error(err as u32)
    }
}
