use crate::{xdr, Host, HostError};
use soroban_builtin_sdk_macros::contracttype;
use soroban_env_common::TryIntoVal;

use super::base_types::BytesN;

#[derive(Clone)]
#[contracttype]
pub enum ContractExecutable {
    Wasm(BytesN<32>),
    StellarAsset,
}

impl ContractExecutable {
    pub fn from_xdr(host: &Host, xdr: &xdr::ContractExecutable) -> Result<Self, HostError> {
        match xdr {
            xdr::ContractExecutable::Wasm(wasm_hash) => Ok(ContractExecutable::Wasm(
                BytesN::<32>::from_slice(host, &wasm_hash.0)?,
            )),
            xdr::ContractExecutable::StellarAsset => Ok(ContractExecutable::StellarAsset),
        }
    }
}
