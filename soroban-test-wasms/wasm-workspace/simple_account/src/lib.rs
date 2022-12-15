#![no_std]

struct SimpleAccount;

use soroban_account::AuthorizationContext;
use soroban_sdk::{contractimpl, contracttype, Account, BytesN, Env, IntoVal, Vec};

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Owner,
}

#[contractimpl]
impl SimpleAccount {
    pub fn init(env: Env, public_key: BytesN<32>) {
        env.data().set(DataKey::Owner, public_key);
    }

    pub fn set_owner(env: Env, curr_account: Account, new_owner: BytesN<32>) {
        if curr_account.address() != env.current_contract_account().address() {
            panic!("unauthorized owner");
        }
        curr_account.authorize((new_owner.clone(),).into_val(&env));
        env.data().set(DataKey::Owner, new_owner);
    }

    pub fn check_auth(
        env: Env,
        signature_payload: BytesN<32>,
        signature_args: Vec<BytesN<64>>,
        _auth_context: Vec<AuthorizationContext>,
    ) {
        if signature_args.len() != 1 {
            panic!("incorrect number of signature args");
        }
        let public_key: BytesN<32> = env.data().get(DataKey::Owner).unwrap().unwrap();
        env.verify_sig_ed25519(
            &public_key,
            &signature_payload.into(),
            &signature_args.get(0).unwrap().unwrap(),
        );
    }
}
