use crate::host::Host;
use crate::native_contract::base_types::{BigInt, BytesN, Map, Vec};
use soroban_env_common::{CheckedEnv, TryIntoVal};
use soroban_env_macros::contracttype;

pub type U256 = BytesN<32>;
pub type U512 = BytesN<64>;

#[derive(Clone)]
#[contracttype]
pub struct KeyedEd25519Signature {
    pub public_key: U256,
    pub signature: U512,
}

#[derive(Clone)]
#[contracttype]
pub struct KeyedAccountAuthorization {
    pub public_key: U256,
    pub signatures: Vec,
}

#[derive(Clone)]
#[contracttype]
pub enum Authorization {
    Contract,
    Ed25519(U512),
    Account(Vec),
}

#[derive(Clone)]
#[contracttype]
pub enum KeyedAuthorization {
    Contract,
    Ed25519(KeyedEd25519Signature),
    Account(KeyedAccountAuthorization),
}

impl KeyedAuthorization {
    pub fn get_identifier(&self, env: &Host) -> Result<Identifier, ()> {
        Ok(match self {
            KeyedAuthorization::Contract => Identifier::Contract(
                env.get_invoking_contract()
                    .map_err(|_| ())?
                    .in_env(env)
                    .try_into()?,
            ),
            KeyedAuthorization::Ed25519(kea) => Identifier::Ed25519(kea.public_key.clone()),
            KeyedAuthorization::Account(kaa) => Identifier::Account(kaa.public_key.clone()),
        })
    }
}

#[derive(Clone)]
#[contracttype]
pub enum Identifier {
    Contract(U256),
    Ed25519(U256),
    Account(U256),
}

#[derive(Clone)]
#[contracttype]
pub struct MessageV0 {
    pub nonce: BigInt,
    pub domain: u32,
    pub parameters: Vec,
}

#[derive(Clone)]
#[contracttype]
pub enum Message {
    V0(MessageV0),
}
