use crate::host::Host;
use crate::native_contract::base_types::Vec;
use crate::HostError;
use soroban_env_common::xdr::ScAddress;
use soroban_env_common::{CheckedEnv, Symbol, TryIntoVal};

pub(crate) fn approve(
    e: &Host,
    from: ScAddress,
    to: ScAddress,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("approve"))?;
    topics.push(from)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: ScAddress,
    to: ScAddress,
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
    admin: ScAddress,
    to: ScAddress,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("mint"))?;
    topics.push(admin)?;
    topics.push(to)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn burn(
    e: &Host,
    admin: ScAddress,
    from: ScAddress,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("burn"))?;
    topics.push(admin)?;
    topics.push(from)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn freeze(e: &Host, admin: ScAddress, id: ScAddress) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("freeze"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), id.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn unfreeze(e: &Host, admin: ScAddress, id: ScAddress) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("unfreeze"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), id.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_admin(
    e: &Host,
    admin: ScAddress,
    new_admin: ScAddress,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("set_admin"))?;
    topics.push(admin)?;
    e.contract_event(topics.into(), new_admin.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn import(e: &Host, id: ScAddress, amount: i64) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("import"))?;
    topics.push(id)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn export(e: &Host, id: ScAddress, amount: i64) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(Symbol::from_str("export"))?;
    topics.push(id)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}
