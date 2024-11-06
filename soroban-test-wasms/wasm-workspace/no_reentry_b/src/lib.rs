#![no_std]
use soroban_sdk::{contract, contractimpl, Env, Val, Address, Vec, Symbol, vec};

#[link(wasm_import_module = "d")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "_"]
    pub fn call_reentrant(contract: i64, func: i64, args: i64, ) -> i64;
}

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn do_reentry(env: Env, caller: Address) {
        let args: Vec<Val> = vec![&env];
        let func = Symbol::new(&env, "do_nothing");
        let called_val = caller.as_val().get_payload() as i64;
        let func_val = func.as_val().get_payload() as i64;
        let args_val = args.as_val().get_payload() as i64;
        
        unsafe {
            call_reentrant(called_val, func_val, args_val);
        };
    }
}
