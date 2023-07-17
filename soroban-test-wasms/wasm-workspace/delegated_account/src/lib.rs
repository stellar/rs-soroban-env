#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, Address, BytesN, Env, IntoVal, Val, Vec,
};

#[contract]
struct DelegatedAccount;

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Owner,
}

#[contractimpl]
impl DelegatedAccount {
    pub fn init(env: Env, owner: Address) {
        env.storage().persistent().set(&DataKey::Owner, &owner);
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        signature_payload: BytesN<32>,
        signature: Val,
        _auth_context: Vec<Context>,
    ) {
        if !signature.is_void() {
            panic!("Unexpected signature provided");
        }
        env.storage()
            .persistent()
            .get::<_, Address>(&DataKey::Owner)
            .unwrap()
            .require_auth_for_args((signature_payload,).into_val(&env));
    }
}
