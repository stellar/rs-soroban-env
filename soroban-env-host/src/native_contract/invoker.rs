use soroban_env_common::{xdr::AccountId, CheckedEnv, Convert, InvokerType};
use soroban_native_sdk_macros::contracttype;

use crate::{Host, HostError};

use super::base_types::BytesN;

#[derive(Clone)]
#[contracttype]
pub enum Invoker {
    Account(AccountId),
    Contract(BytesN<32>),
}

pub fn invoker(env: &Host) -> Result<Invoker, HostError> {
    let invoker_type: InvokerType = env.get_invoker_type()?.try_into()?;
    Ok(match invoker_type {
        InvokerType::Account => Invoker::Account(env.convert(env.get_invoking_account()?)?),
        InvokerType::Contract => Invoker::Contract(env.convert(env.get_invoking_contract()?)?),
    })
}
