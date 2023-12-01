use crate::{
    builtin_contracts::base_types::Address, host::Host, Env, HostError, Symbol, TryFromVal,
    TryIntoVal,
};

use super::metadata::read_name;

pub(crate) fn approve(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
    live_until_ledger: u32,
) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"approve")?,
        from,
        to,
        read_name(e)?
    ]?;
    let data = host_vec![e, amount, live_until_ledger]?;
    e.contract_event(topics.into(), data.into())?;
    Ok(())
}

pub(crate) fn transfer(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"transfer")?,
        from,
        to,
        read_name(e)?
    ]?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn mint(e: &Host, admin: Address, to: Address, amount: i128) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"mint")?,
        admin,
        to,
        read_name(e)?
    ]?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn clawback(
    e: &Host,
    admin: Address,
    from: Address,
    amount: i128,
) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"clawback")?,
        admin,
        from,
        read_name(e)?
    ]?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_authorized(
    e: &Host,
    admin: Address,
    id: Address,
    authorize: bool,
) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"set_authorized")?,
        admin,
        id,
        read_name(e)?
    ]?;
    e.contract_event(topics.into(), authorize.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn set_admin(e: &Host, admin: Address, new_admin: Address) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"set_admin")?,
        admin,
        read_name(e)?
    ]?;
    e.contract_event(topics.into(), new_admin.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
    let topics = host_vec![e, Symbol::try_from_val(e, &"burn")?, from, read_name(e)?]?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}
