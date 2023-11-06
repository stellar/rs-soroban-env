#![no_std]

use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, BytesN, Env, Val, Vec, symbol_short, IntoVal, Address,
};

#[contract]
struct ReentryAccount;

#[contracttype]
enum DataKey {
    SacAddress
}

// Account that tries to do a token transfer in `__check_auth`.
// This would cause reentry if called while performing authorization in
// the same token, but should succeed otherwise.
#[contractimpl]
impl ReentryAccount {
    pub fn set_addr(env: Env, sac_address: Address) {
        env.storage().persistent().set(&DataKey::SacAddress, &sac_address);
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        _signature: Val,
        _auth_context: Vec<Context>,
    ) {
        let sac_address = env.storage().persistent().get(&DataKey::SacAddress).unwrap();
        env.invoke_contract::<()>(&sac_address, &symbol_short!("transfer"),
                                  (env.current_contract_address(),
                                   env.current_contract_address(),
                                   1_i128
                                  ).into_val(&env));
    }
}
