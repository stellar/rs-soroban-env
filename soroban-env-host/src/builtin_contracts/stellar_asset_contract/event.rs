use std::cmp::Ordering;

use soroban_env_common::Compare;

use crate::{
    builtin_contracts::base_types::{Address, BytesN},
    host::Host,
    Env, HostError, Symbol, TryFromVal, TryIntoVal,
};

use super::{asset_info::read_asset_info, metadata::read_name, public_types::AssetInfo};

fn is_issuer(e: &Host, addr: &Address) -> Result<bool, HostError> {
    let issuer_check = |issuer: BytesN<32>, address: &Address| -> Result<bool, HostError> {
        let issuer_account_id = e.account_id_from_bytesobj(issuer.into())?;
        let issuer_address = Address::from_account(e, &issuer_account_id)?;

        Ok(e.compare(&issuer_address, &address)? == Ordering::Equal)
    };

    match read_asset_info(e)? {
        AssetInfo::Native => Ok(false),
        AssetInfo::AlphaNum4(asset) => issuer_check(asset.issuer, addr),
        AssetInfo::AlphaNum12(asset) => issuer_check(asset.issuer, addr),
    }
}

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

pub(crate) fn transfer_maybe_with_issuer(
    e: &Host,
    from: Address,
    to: Address,
    amount: i128,
) -> Result<(), HostError> {
    if e.compare(&from, &to)? == Ordering::Equal {
        transfer(e, from, to, amount)?;
    } else if is_issuer(e, &from)? {
        mint(e, to, amount)?;
    } else if is_issuer(e, &to)? {
        burn(e, from, amount)?;
    } else {
        transfer(e, from, to, amount)?;
    }

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

pub(crate) fn mint(e: &Host, to: Address, amount: i128) -> Result<(), HostError> {
    let topics = host_vec![e, Symbol::try_from_val(e, &"mint")?, to, read_name(e)?]?;
    e.contract_event(topics.into(), amount.try_into_val(e)?)?;
    Ok(())
}

pub(crate) fn clawback(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
    let topics = host_vec![
        e,
        Symbol::try_from_val(e, &"clawback")?,
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
