use crate::native_contract::base_types::{Bytes, Vec};
use crate::HostError;
use crate::{host::Host, native_contract::base_types::Address};
use soroban_env_common::{Env, Symbol};

pub(crate) fn incr_allow(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("incr_allow"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn decr_allow(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("decr_allow"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("transfer"))?;
    topics.push(&from)?;
    topics.push(&to)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn mint(
    e: &Host,
    admin: Address,
    to: Address,
    amount: i128,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("mint"))?;
    topics.push(&admin)?;
    topics.push(&to)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn clawback(
    e: &Host,
    admin: Address,
    from: Address,
    amount: i128,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("clawback"))?;
    topics.push(&admin)?;
    topics.push(&from)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn set_auth(
    e: &Host,
    admin: Address,
    id: Address,
    authorize: bool,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("set_auth"))?;
    topics.push(&admin)?;
    topics.push(&id)?;
    let mut data = Vec::new(e)?;
    data.push(&authorize)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn set_admin(
    e: &Host,
    admin: Address,
    new_admin: Address,
    name: Bytes,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("set_admin"))?;
    topics.push(&admin)?;
    let mut data = Vec::new(e)?;
    data.push(&new_admin)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn burn(e: &Host, from: Address, amount: i128, name: Bytes) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::from_str("burn"))?;
    topics.push(&from)?;
    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&name)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}
