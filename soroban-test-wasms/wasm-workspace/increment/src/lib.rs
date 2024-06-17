#![no_std]
use soroban_sdk::{
    auth::CustomAccountInterface, contract, contracterror, contractimpl, contracttype,
    crypto::Hash, BytesN, Env,
};

#[contract]
pub struct IncrementContract;

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Owner,
    Counter,
}

#[contracterror]
#[derive(Clone, Copy)]
pub enum AccError {
    InvalidSignature = 1,
}

#[contractimpl]
impl IncrementContract {
    pub fn init(env: Env, verifying_key: BytesN<65>) {
        env.storage()
            .persistent()
            .set(&DataKey::Owner, &verifying_key);
        env.storage().instance().set(&DataKey::Counter, &0_u32)
    }

    pub fn increment(env: Env) -> u32 {
        env.current_contract_address().require_auth();
        let mut count: u32 = env.storage().instance().get(&DataKey::Counter).unwrap_or(0);
        count += 1;
        env.storage().instance().set(&DataKey::Counter, &count);
        count
    }

    // #[allow(non_snake_case)]
    // pub fn __check_auth(
    //     env: Env,
    //     signature_payload: BytesN<32>,
    //     signature: BytesN<64>,
    //     _auth_context: Vec<Context>,
    // ) {
    //     let verifying_key = env.storage().persistent().get(&DataKey::Owner).unwrap();
    //     let digest: Hash<32> = signature_payload.into();
    //     env.crypto()
    //         .secp256r1_verify(&verifying_key, &digest, &signature);
    // }
}

#[contractimpl]
impl CustomAccountInterface for IncrementContract {
    type Signature = BytesN<64>;

    type Error = AccError;

    #[allow(non_snake_case)]
    fn __check_auth(
        env: Env,
        signature_payload: Hash<32>,
        signatures: BytesN<64>,
        _auth_contexts: soroban_sdk::Vec<soroban_sdk::auth::Context>,
    ) -> Result<(), AccError> {
        let verifying_key = env.storage().persistent().get(&DataKey::Owner).unwrap();
        env.crypto()
            .secp256r1_verify(&verifying_key, &signature_payload, &signatures);
        Ok(())
    }
}
