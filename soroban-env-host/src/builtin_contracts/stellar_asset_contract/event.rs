use crate::builtin_contracts::base_types::Vec;
use crate::HostError;
use crate::{builtin_contracts::base_types::Address, host::Host};
use soroban_env_common::{Env, Symbol, TryFromVal, TryIntoVal};

use super::metadata::read_name;

pub(crate) fn approve(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
    live_until_ledger: u32,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"approve")?)?;
    topics.push(&from)?;
    topics.push(&to)?;
    topics.push(&read_name(e)?)?;

    let mut data = Vec::new(e)?;
    data.push(&amount)?;
    data.push(&live_until_ledger)?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"transfer")?)?;
    topics.push(&from)?;
    topics.push(&to)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn mint(e: &Host, admin: Address, to: Address, amount: i128) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"mint")?)?;
    topics.push(&admin)?;
    topics.push(&to)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn clawback(
    e: &Host,
    admin: Address,
    from: Address,
    amount: i128,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"clawback")?)?;
    topics.push(&admin)?;
    topics.push(&from)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_authorized(
    e: &Host,
    admin: Address,
    id: Address,
    authorize: bool,
) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"set_authorized")?)?;
    topics.push(&admin)?;
    topics.push(&id)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), authorize.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_admin(e: &Host, admin: Address, new_admin: Address) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"set_admin")?)?;
    topics.push(&admin)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), new_admin.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
    let mut topics = Vec::new(e)?;
    topics.push(&Symbol::try_from_val(e, &"burn")?)?;
    topics.push(&from)?;
    topics.push(&read_name(e)?)?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}
