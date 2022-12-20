use num_derive::FromPrimitive;
use soroban_env_common::Status;

#[derive(Debug, FromPrimitive, PartialEq, Eq)]
pub enum ContractError {
    InternalError = 1,
    OperationNotSupportedError = 2,
    AlreadyInitializedError = 3,

    UnauthorizedError = 4,
    AuthenticationError = 5,
    NonceError = 6,
    SignatureError = 7,
    AccountMissingError = 8,

    NegativeAmountError = 9,
    AllowanceError = 10,
    BalanceError = 11,
    BalanceDeauthorizedError = 12,
    OverflowError = 13,
    TrustlineMissingError = 14,
}

impl From<ContractError> for Status {
    fn from(err: ContractError) -> Self {
        Status::from_contract_error(err as u32)
    }
}
