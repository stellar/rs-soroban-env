use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Vec};
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Identifier;
use soroban_env_common::{CheckedEnv, Symbol, TryIntoVal};

pub(crate) fn approve(
    e: &Host,
    from: Identifier,
    to: Identifier,
    amount: BigInt,
) -> Result<(), Error> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("approve"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Identifier,
    to: Identifier,
    amount: BigInt,
) -> Result<(), Error> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("xfer"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}
