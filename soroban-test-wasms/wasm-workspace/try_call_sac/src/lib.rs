#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, Address, Env, Error, IntoVal};

#[contract]
struct TryCallSac;

#[contractimpl]
impl TryCallSac {
    pub fn mint(env: Env, sac_address: Address, to: Address) -> bool {
        let res = env.try_invoke_contract::<(), Error>(
            &sac_address,
            &symbol_short!("mint"),
            (to, 1_i128).into_val(&env),
        );
        res.is_ok()
    }
}
