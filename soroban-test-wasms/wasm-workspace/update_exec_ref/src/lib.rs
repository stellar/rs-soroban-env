#![no_std]

use soroban_sdk::{
    contract, contractimpl, symbol_short, Address, Env, Error, ExecutableTag, IntoVal, String,
};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn update(env: Env, owner: Address, tag: String, fail: bool) -> i32 {
        let tag = ExecutableTag::from_string(&tag);
        // Modify instance storage to make sure that both instance storage and
        // executable are updated.
        env.storage().instance().set(&symbol_short!("foo"), &111);
        env.deployer()
            .update_current_contract_executable_ref(&owner, &tag);
        if fail {
            panic!();
        }
        123
    }

    pub fn try_upd(
        env: Env,
        contract: Address,
        owner: Address,
        tag: String,
        fail: bool,
    ) -> Option<i32> {
        let res = env.try_invoke_contract::<i32, Error>(
            &contract,
            &symbol_short!("update"),
            (owner, tag, fail).into_val(&env),
        );
        if let Ok(v) = res {
            Some(v.unwrap())
        } else {
            None
        }
    }
}
