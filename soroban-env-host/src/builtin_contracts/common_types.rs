use super::base_types::BytesN;
use crate::{xdr, Host, HostError, TryIntoVal};
use soroban_builtin_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub(crate) enum ContractExecutable {
    Wasm(BytesN<32>),
    StellarAsset,
}

impl ContractExecutable {
    pub(crate) fn from_xdr(host: &Host, xdr: &xdr::ContractExecutable) -> Result<Self, HostError> {
        match xdr {
            xdr::ContractExecutable::Wasm(wasm_hash) => Ok(ContractExecutable::Wasm(
                BytesN::<32>::from_slice(host, &wasm_hash.0)?,
            )),
            xdr::ContractExecutable::StellarAsset => Ok(ContractExecutable::StellarAsset),
        }
    }
}
