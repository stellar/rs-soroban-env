#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, Address, BytesN, Env, Error, IntoVal};

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn update(env: Env, wasm_hash: BytesN<32>, fail: bool) -> i32 {
        // Modify instance storage to make sure that both instance storage and
        // executable are updated.
        env.storage().instance().set(&symbol_short!("foo"), &111);
        env.deployer().update_current_contract_wasm(wasm_hash);
        if fail {
            panic!();
        }
        123
    }

    pub fn try_upd(env: Env, contract: Address, wasm_hash: BytesN<32>, fail: bool) -> Option<i32> {
        let res = env.try_invoke_contract::<i32, Error>(
            &contract,
            &symbol_short!("update"),
            (wasm_hash, fail).into_val(&env),
        );
        if let Ok(v) = res {
            Some(v.unwrap())
        } else {
            None
        }
    }
}
