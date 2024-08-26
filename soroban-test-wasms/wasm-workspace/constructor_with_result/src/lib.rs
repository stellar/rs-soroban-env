#![no_std]

use soroban_sdk::{contract, contracterror, contractimpl, panic_with_error, Env};

#[contract]
pub struct TestConstructorWithResult;

#[contracterror]
#[derive(Copy, Clone)]
#[repr(u32)]
pub enum Error {
    Returned = 0,
    Panic = 1,
}

#[contractimpl]
impl TestConstructorWithResult {
    pub fn __constructor(env: Env, input: u32) -> Result<(), Error> {
        if input == 0 {
            return Err(Error::Returned);
        }
        if input == 1 {
            panic_with_error!(&env, Error::Panic);
        }
        if input == 2 {
            panic!("just panic");
        }
        Ok(())
    }
}
