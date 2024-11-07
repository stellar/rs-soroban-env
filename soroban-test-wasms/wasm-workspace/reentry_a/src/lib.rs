#![no_std]
use soroban_sdk::{contract, contractimpl, Env, Address, Symbol, TryIntoVal, Vec, Val};

#[link(wasm_import_module = "d")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "_"]
    pub fn call_contract(contract: i64, func: i64, args: i64, ) -> i64;

    #[allow(improper_ctypes)]
    #[link_name = "3"]
    pub fn set_reentrant(enabled: i64, ) -> i64;
}

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn test_reentry(env: Env, called: Address) {
        let args: Vec<Val> = (env.current_contract_address(), ).try_into_val(&env).unwrap();
        let func = Symbol::new(&env, "do_reentry");
        let called_val = called.as_val().get_payload() as i64;
        let func_val = func.as_val().get_payload() as i64;
        let args_val = args.as_val().get_payload() as i64;

        let set_reentrant_flag = Val::from_bool(true).as_val().get_payload() as i64;
        
        unsafe {
            set_reentrant(set_reentrant_flag);
            call_contract(called_val, func_val, args_val);
        };
    }

    pub fn do_nothing(env: Env) {
        env.events().publish((Symbol::new(&env, "first_soroban_reentry"),), ());
    }
}
