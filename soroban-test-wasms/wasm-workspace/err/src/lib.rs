#![no_std]
use soroban_sdk::{contract, contractimpl, contracterror, Val, Error};

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

}
