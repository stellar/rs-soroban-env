use crate::host::Host;
use crate::native_contract::base_types::{Bytes, BytesN, Map, Vec};
use crate::native_contract::token::error::Error;
use soroban_env_common::{CheckedEnv, Symbol, TryIntoVal};
use soroban_native_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub struct Ed25519Signature {
    pub public_key: BytesN<32>,
    pub signature: BytesN<64>,
}

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub struct AccountSignatures {
    pub account_id: BytesN<32>,
    pub signatures: Vec,
}

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub enum Signature {
    Contract,
    Ed25519(Ed25519Signature),
    Account(AccountSignatures),
}

impl Signature {
    pub fn get_identifier(&self, env: &Host) -> Result<Identifier, Error> {
        Ok(match self {
            Signature::Contract => {
                Identifier::Contract(env.get_invoking_contract()?.to_raw().try_into_val(env)?)
            }
            Signature::Ed25519(kea) => Identifier::Ed25519(kea.public_key.clone()),
            Signature::Account(kaa) => Identifier::Account(kaa.account_id.clone()),
        })
    }
}

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub enum Identifier {
    Contract(BytesN<32>),
    Ed25519(BytesN<32>),
    Account(BytesN<32>),
}

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub struct SignaturePayloadV0 {
    pub function: Symbol,
    pub contract: BytesN<32>,
    pub network: Bytes,
    pub args: Vec,
}

#[derive(Clone)]
#[contracttype(lib = "soroban_sdk_auth")]
pub enum SignaturePayload {
    V0(SignaturePayloadV0),
}
