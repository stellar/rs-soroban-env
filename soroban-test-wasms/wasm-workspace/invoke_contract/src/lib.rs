#![no_std]
use soroban_env_common::{AddressObject, Env as _, TryFromVal};
use soroban_sdk::{contractimpl, vec, Address, BytesN, Env, IntoVal, Symbol};

pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn add(env: Env, a: i32, b: i32) -> i32 {
        env.events().publish((Symbol::short("add"),), (a, b));
        a + b
    }

    pub fn add_with(env: Env, x: i32, y: i32, contract_id: BytesN<32>) -> i32 {
        env.events()
            .publish((Symbol::short("add_with"),), (x, y, contract_id.clone()));
        let ao: AddressObject = env.contract_id_to_address(contract_id.to_object()).unwrap();
        env.invoke_contract(
            &Address::try_from_val(&env, &ao).unwrap(),
            &Symbol::short("add"),
            vec![&env, x.into_val(&env), y.into_val(&env)],
        )
    }
}
