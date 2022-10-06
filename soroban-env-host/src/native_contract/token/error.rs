use soroban_env_common::Status;

#[derive(Debug)]
pub enum ContractError {
    InternalError = 1,
    OperationNotSupportedError = 2,
    AlreadyInitializedError = 3,

    UnauthorizedError = 4,
    AuthenticationError = 5,
    NonceError = 6,
    SignatureError = 7,

    NegativeAmountError = 8,
    AllowanceError = 9,
    BalanceError = 10,
    BalanceFrozenError = 11,
}

impl From<ContractError> for Status {
    fn from(err: ContractError) -> Self {
        Status::from_contract_error(err as u32)
    }
}
