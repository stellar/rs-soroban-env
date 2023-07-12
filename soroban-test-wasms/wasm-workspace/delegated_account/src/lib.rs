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
        signature_args: Vec<Val>,
        _auth_context: Vec<Context>,
    ) {
        if signature_args.len() != 0 {
            panic!("incorrect number of signature args");
        }
        env.storage()
            .persistent()
            .get::<_, Address>(&DataKey::Owner)
            .unwrap()
            .require_auth_for_args((signature_payload,).into_val(&env));
    }
}
