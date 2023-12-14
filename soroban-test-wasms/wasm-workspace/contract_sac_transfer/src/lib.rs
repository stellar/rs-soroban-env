#![no_std]

use soroban_sdk::{contract, contractimpl, Env, symbol_short, IntoVal, Address,
};

#[contract]
struct SacTransfer;

#[contractimpl]
impl SacTransfer {
    pub fn transfer_1(env: Env, sac_address: Address, to: Address) {
        env.invoke_contract::<()>(&sac_address, &symbol_short!("transfer"),
                                  (env.current_contract_address(),
                                   to,
                                   1_i128
                                  ).into_val(&env));
    }
}