use crate::{events::DebugError, host::HostError, Host};
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

pub fn contract_err(host: &Host, err: ContractError, msg: &'static str) -> HostError {
    host.err(DebugError::new(Status::from_contract_error(err as u32)).msg(msg))
}
