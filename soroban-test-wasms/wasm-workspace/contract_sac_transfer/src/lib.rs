#![no_std]

use soroban_sdk::{contract, contractimpl, Env, symbol_short, IntoVal, Address, Vec,
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

    pub fn transfer_amount(env: Env, sac_address: Address, to: Address, amount:i128) {
        env.invoke_contract::<()>(&sac_address, &symbol_short!("transfer"),
                                  (env.current_contract_address(),
                                   to,
                                   amount
                                  ).into_val(&env));
    }

    pub fn batch_transfer(env: Env, sac_address: Address, destinations: Vec<Address>) {
        let amount = 1_i128;
        let from = env.current_contract_address();

        for dest in destinations.iter() {
            env.invoke_contract::<()>(&sac_address, &symbol_short!("transfer"),
                                      (from.clone(), dest, amount).into_val(&env));
        }
    }
}