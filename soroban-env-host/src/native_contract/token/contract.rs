use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Bytes, Vec};
use crate::native_contract::token::admin::{
    has_administrator, to_administrator_authorization, write_administrator,
};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    read_balance, read_state, receive_balance, spend_balance, write_state,
};
use crate::native_contract::token::cryptography::{check_auth, Domain};
use crate::native_contract::token::metadata::{
    read_decimal, read_name, read_symbol, write_decimal, write_name, write_symbol,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{Authorization, Identifier, KeyedAuthorization};
use soroban_env_common::{RawVal, TryIntoVal};

pub fn initialize(
    e: &Host,
    admin: Identifier,
    decimal: u32,
    name: Bytes,
    symbol: Bytes,
) -> Result<RawVal, ()> {
    if has_administrator(&e)? {
        panic!("already initialized")
    }
    write_administrator(&e, admin)?;

    write_decimal(&e, u8::try_from(decimal).expect("Decimal must fit in a u8"))?;
    write_name(&e, name)?;
    write_symbol(&e, symbol)?;
    Ok(().into())
}

pub fn nonce(e: &Host, id: Identifier) -> Result<RawVal, ()> {
    read_nonce(e, id)?.try_into_val(e)
}

pub fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<RawVal, ()> {
    read_allowance(&e, from, spender)?.try_into_val(e)
}

pub fn approve(
    e: &Host,
    from: KeyedAuthorization,
    spender: Identifier,
    amount: BigInt,
) -> Result<RawVal, ()> {
    let from_id = from.get_identifier(&e)?;
    let mut args = Vec::new(e)?;
    args.push(spender.clone())?;
    args.push(amount.clone())?;
    check_auth(&e, from, Domain::Approve, args)?;
    write_allowance(&e, from_id, spender, amount)?;
    Ok(().into())
}

pub fn balance(e: &Host, id: Identifier) -> Result<RawVal, ()> {
    read_balance(e, id)?.try_into_val(e)
}

pub fn is_frozen(e: &Host, id: Identifier) -> Result<RawVal, ()> {
    Ok(read_state(&e, id)?.into())
}

pub fn xfer(
    e: &Host,
    from: KeyedAuthorization,
    to: Identifier,
    amount: BigInt,
) -> Result<RawVal, ()> {
    let from_id = from.get_identifier(&e)?;
    let mut args = Vec::new(e)?;
    args.push(to.clone())?;
    args.push(amount.clone())?;
    check_auth(&e, from, Domain::Transfer, args)?;
    spend_balance(&e, from_id, amount.clone())?;
    receive_balance(&e, to, amount)?;
    Ok(().into())
}

pub fn xfer_from(
    e: &Host,
    spender: KeyedAuthorization,
    from: Identifier,
    to: Identifier,
    amount: BigInt,
) -> Result<RawVal, ()> {
    let spender_id = spender.get_identifier(&e)?;
    let mut args = Vec::new(e)?;
    args.push(from.clone())?;
    args.push(to.clone())?;
    args.push(amount.clone())?;
    check_auth(&e, spender, Domain::TransferFrom, args)?;
    spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
    spend_balance(&e, from, amount.clone())?;
    receive_balance(&e, to, amount)?;
    Ok(().into())
}

pub fn burn(
    e: &Host,
    admin: Authorization,
    from: Identifier,
    amount: BigInt,
) -> Result<RawVal, ()> {
    let auth = to_administrator_authorization(&e, admin)?;
    let mut args = Vec::new(e)?;
    args.push(from.clone())?;
    args.push(amount.clone())?;
    check_auth(&e, auth, Domain::Burn, args)?;
    spend_balance(&e, from, amount)?;
    Ok(().into())
}

pub fn freeze(e: &Host, admin: Authorization, id: Identifier) -> Result<RawVal, ()> {
    let auth = to_administrator_authorization(&e, admin)?;
    let mut args = Vec::new(e)?;
    args.push(id.clone())?;
    check_auth(&e, auth, Domain::Freeze, args)?;
    write_state(&e, id, true)?;
    Ok(().into())
}

pub fn mint(e: &Host, admin: Authorization, to: Identifier, amount: BigInt) -> Result<RawVal, ()> {
    let auth = to_administrator_authorization(&e, admin)?;
    let mut args = Vec::new(e)?;
    args.push(to.clone())?;
    args.push(amount.clone())?;
    check_auth(&e, auth, Domain::Mint, args)?;
    receive_balance(&e, to, amount)?;
    Ok(().into())
}

pub fn set_admin(e: &Host, admin: Authorization, new_admin: Identifier) -> Result<RawVal, ()> {
    let auth = to_administrator_authorization(&e, admin)?;
    let mut args = Vec::new(e)?;
    args.push(new_admin.clone())?;
    check_auth(&e, auth, Domain::SetAdministrator, args)?;
    write_administrator(&e, new_admin)?;
    Ok(().into())
}

pub fn unfreeze(e: &Host, admin: Authorization, id: Identifier) -> Result<RawVal, ()> {
    let auth = to_administrator_authorization(&e, admin)?;
    let mut args = Vec::new(e)?;
    args.push(id.clone())?;
    check_auth(&e, auth, Domain::Unfreeze, args)?;
    write_state(&e, id, false)?;
    Ok(().into())
}

pub fn decimals(e: &Host) -> Result<RawVal, ()> {
    read_decimal(&e)?.try_into().map_err(|_| ())
}

pub fn name(e: &Host) -> Result<RawVal, ()> {
    read_name(&e)?.try_into_val(e).map_err(|_| ())
}

pub fn symbol(e: &Host) -> Result<RawVal, ()> {
    read_symbol(&e)?.try_into_val(e).map_err(|_| ())
}
