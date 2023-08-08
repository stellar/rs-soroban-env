#![no_std]
use soroban_sdk::{contract, contractimpl, Env, Bytes, BytesN};

pub(crate) const BUMP_AMOUNT: u32 = 518400; // 30 days

#[contract]
pub struct WriteBytesContract;

/*
This contract is used by a stellar core test case to do a Soroban ConfigSetting upgrade.
A copy of this contract also exists in the scripts directory of stellar-core, which is used
by a python script to execute the upgrade. If there's an easy way for the python script to use
this contract in env, the one in stellar-core should be deleted.
*/
#[contractimpl]
impl WriteBytesContract {
    pub fn write(env: Env, xdr_bytes: Bytes) -> BytesN<32> {
        let hash = env.crypto().sha256(&xdr_bytes);
        env.storage().temporary().set(&hash, &xdr_bytes);
        env.storage().temporary().bump(&hash, BUMP_AMOUNT);

        env.storage().instance().bump(BUMP_AMOUNT);

        hash
    }
}
