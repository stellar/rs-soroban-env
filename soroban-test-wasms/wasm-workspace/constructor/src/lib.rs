#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, Address, Env, IntoVal, Symbol};

#[contract]
pub struct TestConstructorContract;

#[contractimpl]
impl TestConstructorContract {
    // This relies on `auth_contract` to be an instance of `auth` test contract.
    pub fn __constructor(
        env: Env,
        authorizer: Address,
        key: Symbol,
        val: u32,
        auth_contract: Address,
    ) {
        authorizer.require_auth();
        env.invoke_contract::<()>(
            &auth_contract,
            &symbol_short!("do_auth"),
            (&authorizer, val).into_val(&env),
        );
        env.storage().persistent().set(&key, &val);
        env.storage().instance().set(&key, &(val + 1));
        env.storage().temporary().set(&key, &(val + 2));
    }

    pub fn get_data(env: Env, key: Symbol) -> u32 {
        env.storage().persistent().get::<_, u32>(&key).unwrap()
            + env.storage().instance().get::<_, u32>(&key).unwrap()
            + env.storage().temporary().get::<_, u32>(&key).unwrap()
    }
}
