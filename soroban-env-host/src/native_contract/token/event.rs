use crate::native_contract::base_types::{Bytes, Vec};
use crate::HostError;
use crate::{host::Host, native_contract::base_types::Address};
use soroban_env_common::{Env, Symbol, TryIntoVal};

pub(crate) fn incr_allow(
    e: &Host,
    from: Address,
    to: Address,
    name: Bytes,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("incr_allow"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn decr_allow(
    e: &Host,
    from: Address,
    to: Address,
    name: Bytes,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("decr_allow"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Address,
    to: Address,
    name: Bytes,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("transfer"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn mint(
    e: &Host,
    admin: Address,
    to: Address,
    name: Bytes,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("mint"))?;
    topics.push(&admin)?;
    topics.push(&to)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn clawback(
    e: &Host,
    admin: Address,
    from: Address,
    name: Bytes,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("clawback"))?;
    topics.push(&admin)?;
    topics.push(&from)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_auth(
    e: &Host,
    admin: Address,
    id: Address,
    name: Bytes,
    authorize: bool,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("set_auth"))?;
    topics.push(&admin)?;
    topics.push(&id)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), authorize.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_admin(
    e: &Host,
    admin: Address,
    name: Bytes,
    new_admin: Address,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("set_admin"))?;
    topics.push(&admin)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), new_admin.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn burn(e: &Host, from: Address, name: Bytes, amount: i128) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("burn"))?;
    topics.push(&from)?;
    topics.push(&name)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}
