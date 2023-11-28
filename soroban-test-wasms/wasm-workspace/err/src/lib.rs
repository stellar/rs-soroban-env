#![no_std]
use soroban_sdk::{contract, contractimpl, contracterror, Val, Error, Env, Symbol, Address, symbol_short, token, bytes};
use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
#[contract]
pub struct Contract;

#[contracterror]
#[derive(Copy,Clone)]
pub enum Eek
{
    BADNESS = 12345
}

// This test exists to provide a variety of different contract return values
// (both Val and Result<...>) that are all encoded by the SDK as a
// Val-that-is-Error, are therefore indistinguishable from the host's
// perspective, and that should all therefore be mapped to Err(Error) by the
// host's contract-frame-management code when returning from a contract.

#[contractimpl]
impl Contract {

    pub fn err_eek() -> Result<(), Eek> {
        Err(Eek::BADNESS)
    }

    pub fn err_err() -> Result<(), Error> {
        let e: Error = Error::from_contract_error(12345).into();
        Err(e)
    }

    pub fn ok_err() -> Result<Error, Eek> {
        let e: Error = Error::from_contract_error(12345).into();
        Ok(e)
    }

    pub fn ok_val_err() -> Result<Val, Eek> {
        let v: Val = Error::from_contract_error(12345).into();
        Ok(v)
    }

    pub fn err() -> Error {
        Error::from_contract_error(12345)
    }

    pub fn val() -> Val {
        Error::from_contract_error(12345).into()
    }

    pub fn missing_wasm(env: Env) {
        let bin = bytes![&env, [0, 5, 6, 37, 7]];

        let hash = env.crypto().sha256(&bin);
        env.deployer().update_current_contract_wasm(hash);
    }

    pub fn divide(denominator: u32) -> u32 {
        // Pass in zero to error
        10 / denominator
    }

    // This function is used in a try_call invocation test to make sure
    // state is rolled back on failure.
    pub fn fail_after_updates(env: Env, token: Address) -> Result<(), Error> {
        env.events()
            .publish((Symbol::new(&env, &"fail_after_updates"),), ());

        env.storage().instance().set(&symbol_short!("key"), &symbol_short!("val"));
        env.storage().persistent().set(&symbol_short!("key"), &symbol_short!("val"));
        env.storage().temporary().set(&symbol_short!("key"), &symbol_short!("val"));

        let client = token::Client::new(&env, &token);
        let contract_address = env.current_contract_address();
        // Transfer to the token address to make the test easier.
        client.transfer(&contract_address, &token, &10);

        let e: Error = Error::from_contract_error(12345).into();
        Err(e)
    }

    pub fn storage_updated(e: Env) -> bool {
        e.storage().instance().has(&symbol_short!("key")) ||
        e.storage().persistent().has(&symbol_short!("key")) ||
        e.storage().temporary().has(&symbol_short!("key"))
    }

    pub fn spoof(_e: Env) -> Result<(), Error> {
        Err(Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError))
    }
}
