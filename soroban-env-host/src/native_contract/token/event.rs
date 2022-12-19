use crate::host::Host;
use crate::native_contract::base_types::Vec;
use crate::native_contract::token::public_types::Identifier;
use crate::HostError;
use soroban_env_common::{CheckedEnv, Symbol, TryIntoVal};

pub(crate) fn incr_allow(
    e: &Host,
    from: Identifier,
    to: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("incr_allow"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn decr_allow(
    e: &Host,
    from: Identifier,
    to: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("decr_allow"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Identifier,
    to: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("transfer"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn mint(
    e: &Host,
    admin: Identifier,
    to: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("mint"))?;
    topics.push(admin)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn clawback(
    e: &Host,
    admin: Identifier,
    from: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("clawback"))?;
    topics.push(admin)?;
    topics.push(from)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn freeze(e: &Host, admin: Identifier, id: Identifier) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("freeze"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), id.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn unfreeze(e: &Host, admin: Identifier, id: Identifier) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("unfreeze"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), id.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_admin(
    e: &Host,
    admin: Identifier,
    new_admin: Identifier,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("set_admin"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), new_admin.try_into_val(e)?)?;
    Ok(())
}
