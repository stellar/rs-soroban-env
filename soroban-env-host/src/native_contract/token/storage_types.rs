use crate::host::Host;
use crate::native_contract::base_types::{Map, Vec};
use crate::native_contract::token::public_types::Identifier;
use soroban_env_common::{RawVal, Symbol, TryIntoVal};

pub struct AllowanceDataKey {
    pub from: Identifier,
    pub spender: Identifier,
}

impl TryIntoVal<Host, RawVal> for AllowanceDataKey {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut map = Map::new(env)?;
        map.set(Symbol::from_str("from"), self.from)?;
        map.set(Symbol::from_str("spender"), self.spender)?;
        map.try_into_val(env)
    }
}

pub enum DataKey {
    Allowance(AllowanceDataKey),
    Balance(Identifier),
    Nonce(Identifier),
    State(Identifier),
    Admin,
    Decimals,
    Name,
    Symbol,
}

impl TryIntoVal<Host, RawVal> for DataKey {
    type Error = ();

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        let mut vec = Vec::new(env)?;
        match self {
            Self::Allowance(adk) => {
                vec.push(Symbol::from_str("Allowance"))?;
                vec.push(adk)?;
            }
            Self::Balance(id) => {
                vec.push(Symbol::from_str("Balance"))?;
                vec.push(id)?;
            }
            Self::Nonce(id) => {
                vec.push(Symbol::from_str("Nonce"))?;
                vec.push(id)?;
            }
            Self::State(id) => {
                vec.push(Symbol::from_str("State"))?;
                vec.push(id)?;
            }
            Self::Admin => vec.push(Symbol::from_str("Admin"))?,
            Self::Decimals => vec.push(Symbol::from_str("Decimals"))?,
            Self::Name => vec.push(Symbol::from_str("Name"))?,
            Self::Symbol => vec.push(Symbol::from_str("Symbol"))?,
        };
        vec.try_into_val(env)
    }
}
