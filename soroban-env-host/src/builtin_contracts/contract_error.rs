#![allow(non_local_definitions)]
use num_derive::FromPrimitive;
use soroban_env_common::Error;

// Use the same error type for all the built-in contract errors.
// In theory we could have a separate enum for each built-in contract, but it's
// not clear how to distinguish them if multiple built-in contracts are involved.
#[derive(Debug, FromPrimitive, PartialEq, Eq)]
pub(crate) enum ContractError {
    // Value 1 is reserved (was InternalError, now use host internal error
    // instead).
    #[allow(dead_code)]
    _Reserved1 = 1,
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
    InsufficientAccountReserve = 14,
    TooManyAccountSubentries = 15,
}

impl From<ContractError> for Error {
    fn from(err: ContractError) -> Self {
        Error::from_contract_error(err as u32)
    }
}
