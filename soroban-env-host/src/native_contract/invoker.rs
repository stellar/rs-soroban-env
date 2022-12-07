use soroban_env_common::{xdr::AccountId, CheckedEnv, InvokerType, TryIntoVal};
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
    let invoker_type: InvokerType = Host::get_invoker_type(&env)?.try_into()?;
    Ok(match invoker_type {
        InvokerType::Account => {
            Invoker::Account(Host::get_invoking_account(&env)?.try_into_val(env)?)
        }
        InvokerType::Contract => {
            Invoker::Contract(Host::get_invoking_contract(&env)?.try_into_val(env)?)
        }
    })
}
