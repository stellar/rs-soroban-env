#![no_std]
#[allow(unused_imports)]
use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, BytesN, Env, IntoVal, Val, Vec,
};

#[contract]
struct ContextBasedCustomAccount;

#[contractimpl]
impl ContextBasedCustomAccount {
    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        signature: Val,
        auth_context: Vec<Context>,
    ) {
        let signature_context: Vec<Context> = signature.into_val(&env);
        if signature_context != auth_context {
            panic!("context mismatch");
        }
    }
}
