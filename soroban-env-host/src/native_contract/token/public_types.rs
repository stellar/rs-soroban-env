use crate::host::Host;
use crate::native_contract::base_types::{BigInt, BytesN, Map, Vec};
use soroban_env_common::{CheckedEnv, EnvVal, Object, RawVal, Symbol, TryIntoVal};

pub type U256 = BytesN<32>;
pub type U512 = BytesN<64>;

#[derive(Clone)]
pub struct KeyedEd25519Signature {
    pub public_key: U256,
    pub signature: U512,
}

impl TryFrom<EnvVal<Host, Object>> for KeyedEd25519Signature {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        let map = Map::try_from(value)?;
        Ok(KeyedEd25519Signature {
            public_key: map.get(Symbol::from_str("public_key"))?,
            signature: map.get(Symbol::from_str("signature"))?,
        })
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for KeyedEd25519Signature {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for KeyedEd25519Signature {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut map = Map::new(env)?;
        map.set(Symbol::from_str("public_key"), self.public_key)?;
        map.set(Symbol::from_str("signature"), self.signature)?;
        map.try_into_val(env)
    }
}

#[derive(Clone)]
pub struct KeyedAccountAuthorization {
    pub public_key: U256,
    pub signatures: Vec,
}

impl TryFrom<EnvVal<Host, Object>> for KeyedAccountAuthorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        let map = Map::try_from(value)?;
        Ok(KeyedAccountAuthorization {
            public_key: map.get(Symbol::from_str("public_key"))?,
            signatures: map.get(Symbol::from_str("signatures"))?,
        })
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for KeyedAccountAuthorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for KeyedAccountAuthorization {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut map = Map::new(env)?;
        map.set(Symbol::from_str("public_key"), self.public_key)?;
        map.set(Symbol::from_str("signatures"), self.signatures)?;
        map.try_into_val(env)
    }
}

#[derive(Clone)]
pub enum Authorization {
    Contract,
    Ed25519(U512),
    Account(Vec),
}

impl TryFrom<EnvVal<Host, Object>> for Authorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        const CONTRACT_DISCRIMINANT: u64 = Symbol::from_str("Contract").to_raw().get_payload();
        const ED25519_DISCRIMINANT: u64 = Symbol::from_str("Ed25519").to_raw().get_payload();
        const ACCOUNT_DISCRIMINANT: u64 = Symbol::from_str("Account").to_raw().get_payload();

        let vec = Vec::try_from(value)?;
        let discriminant: Symbol = vec.get(0)?;
        match discriminant.to_raw().get_payload() {
            CONTRACT_DISCRIMINANT => Ok(Self::Contract),
            ED25519_DISCRIMINANT => Ok(Self::Ed25519(vec.get(1)?)),
            ACCOUNT_DISCRIMINANT => Ok(Self::Account(vec.get(1)?)),
            _ => Err(()),
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Authorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Authorization {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut vec = Vec::new(env)?;
        match self {
            Self::Contract => {
                vec.push(Symbol::from_str("Contract"))?;
            }
            Self::Ed25519(sig) => {
                vec.push(Symbol::from_str("Ed25519"))?;
                vec.push(sig)?;
            }
            Self::Account(sigs) => {
                vec.push(Symbol::from_str("Account"))?;
                vec.push(sigs)?;
            }
        };
        vec.try_into_val(env)
    }
}

#[derive(Clone)]
pub enum KeyedAuthorization {
    Contract,
    Ed25519(KeyedEd25519Signature),
    Account(KeyedAccountAuthorization),
}

impl TryFrom<EnvVal<Host, Object>> for KeyedAuthorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        const CONTRACT_DISCRIMINANT: u64 = Symbol::from_str("Contract").to_raw().get_payload();
        const ED25519_DISCRIMINANT: u64 = Symbol::from_str("Ed25519").to_raw().get_payload();
        const ACCOUNT_DISCRIMINANT: u64 = Symbol::from_str("Account").to_raw().get_payload();

        let vec = Vec::try_from(value)?;
        let discriminant: Symbol = vec.get(0)?;
        match discriminant.to_raw().get_payload() {
            CONTRACT_DISCRIMINANT => Ok(Self::Contract),
            ED25519_DISCRIMINANT => Ok(Self::Ed25519(vec.get(1)?)),
            ACCOUNT_DISCRIMINANT => Ok(Self::Account(vec.get(1)?)),
            _ => Err(()),
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for KeyedAuthorization {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for KeyedAuthorization {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut vec = Vec::new(env)?;
        match self {
            Self::Contract => {
                vec.push(Symbol::from_str("Contract"))?;
            }
            Self::Ed25519(keyed_sig) => {
                vec.push(Symbol::from_str("Ed25519"))?;
                vec.push(keyed_sig)?;
            }
            Self::Account(keyed_auth) => {
                vec.push(Symbol::from_str("Account"))?;
                vec.push(keyed_auth)?;
            }
        };
        vec.try_into_val(env)
    }
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
pub enum Identifier {
    Contract(U256),
    Ed25519(U256),
    Account(U256),
}

impl TryFrom<EnvVal<Host, Object>> for Identifier {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        const CONTRACT_DISCRIMINANT: u64 = Symbol::from_str("Contract").to_raw().get_payload();
        const ED25519_DISCRIMINANT: u64 = Symbol::from_str("Ed25519").to_raw().get_payload();
        const ACCOUNT_DISCRIMINANT: u64 = Symbol::from_str("Account").to_raw().get_payload();

        let vec = Vec::try_from(value)?;
        let discriminant: Symbol = vec.get(0)?;
        match discriminant.to_raw().get_payload() {
            CONTRACT_DISCRIMINANT => Ok(Self::Contract(vec.get(1)?)),
            ED25519_DISCRIMINANT => Ok(Self::Ed25519(vec.get(1)?)),
            ACCOUNT_DISCRIMINANT => Ok(Self::Account(vec.get(1)?)),
            _ => Err(()),
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Identifier {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Identifier {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut vec = Vec::new(env)?;
        match self {
            Self::Contract(u) => {
                vec.push(Symbol::from_str("Contract"))?;
                vec.push(u)?;
            }
            Self::Ed25519(u) => {
                vec.push(Symbol::from_str("Ed25519"))?;
                vec.push(u)?;
            }
            Self::Account(u) => {
                vec.push(Symbol::from_str("Account"))?;
                vec.push(u)?;
            }
        };
        vec.try_into_val(env)
    }
}

#[derive(Clone)]
pub struct MessageV0 {
    pub nonce: BigInt,
    pub domain: u32,
    pub parameters: Vec,
}

impl TryFrom<EnvVal<Host, Object>> for MessageV0 {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        let map = Map::try_from(value)?;
        Ok(MessageV0 {
            nonce: map.get(Symbol::from_str("nonce"))?,
            domain: map.get(Symbol::from_str("domain"))?,
            parameters: map.get(Symbol::from_str("parameters"))?,
        })
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for MessageV0 {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for MessageV0 {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut map = Map::new(env)?;
        map.set(Symbol::from_str("nonce"), self.nonce)?;
        map.set(Symbol::from_str("domain"), self.domain)?;
        map.set(Symbol::from_str("parameters"), self.parameters)?;
        map.try_into_val(env)
    }
}

#[derive(Clone)]
pub enum Message {
    V0(MessageV0),
}

impl TryFrom<EnvVal<Host, Object>> for Message {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        const V0_DISCRIMINANT: u64 = Symbol::from_str("V0").to_raw().get_payload();

        let vec = Vec::try_from(value)?;
        let discriminant: Symbol = vec.get(0)?;
        match discriminant.to_raw().get_payload() {
            V0_DISCRIMINANT => Ok(Self::V0(vec.get(1)?)),
            _ => Err(()),
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Message {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Message {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut vec = Vec::new(env)?;
        match self {
            Self::V0(v0) => {
                vec.push(Symbol::from_str("V0"))?;
                vec.push(v0)?;
            }
        };
        vec.try_into_val(env)
    }
}
