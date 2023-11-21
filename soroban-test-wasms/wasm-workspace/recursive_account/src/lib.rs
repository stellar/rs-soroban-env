#![no_std]

use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, symbol_short, xdr::FromXdr, xdr::ToXdr,
    Address, Bytes, BytesN, Env, IntoVal, Val, Vec,
};

#[contract]
struct RecursiveAccount;

#[contracttype]
pub enum Signature {
    RedirectAddress(Address),
    SerializeValue,
    DeserializeValue(Bytes),
}

fn create_recursive_val(env: &Env, depth: u32) -> Val {
    let mut curr = Vec::<Val>::new(env);
    for _ in 0..depth {
        curr = (curr.to_val(),).into_val(env);
    }
    curr.into()
}

// Account contract for testing the deep host recursion.
// This redirects `__check_auth` calls to the provided address (which
// should be an instance of `RecursiveAccount` as well), and then runs
// nested value serialization with customizable depth in the leaf call.
#[contractimpl]
impl RecursiveAccount {
    pub fn set_depth(env: Env, depth: u32) {
        env.storage().instance().set(&symbol_short!("d"), &depth);
    }

    pub fn call(env: Env, addr: Address) {
        addr.require_auth_for_args(().into_val(&env));
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        _signature_payload: BytesN<32>,
        signature_redirect: Signature,
        _auth_context: Vec<Context>,
    ) {
        match signature_redirect {
            Signature::RedirectAddress(address) => address.require_auth_for_args(().into_val(&env)),
            Signature::SerializeValue => {
                let val = create_recursive_val(
                    &env,
                    env.storage().instance().get(&symbol_short!("d")).unwrap(),
                );
                let _ = val.to_xdr(&env);
            }
            Signature::DeserializeValue(b) => {
                let _: Val = FromXdr::from_xdr(&env, &b).unwrap();
            }
        }
    }
}
