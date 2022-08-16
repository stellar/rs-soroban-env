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
use crate::native_contract::token::error::Error;
use crate::native_contract::token::metadata::{
    read_decimal, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{
    Authorization, Identifier, KeyedAuthorization, Metadata,
};
use soroban_env_common::TryIntoVal;
use soroban_native_sdk_macros::contractimpl;

pub trait TokenTrait {
    fn initialize(e: &Host, admin: Identifier, metadata: Metadata) -> Result<(), Error>;

    fn nonce(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error>;

    fn approve(
        e: &Host,
        from: KeyedAuthorization,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn balance(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, Error>;

    fn xfer(
        e: &Host,
        from: KeyedAuthorization,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn xfer_from(
        e: &Host,
        spender: KeyedAuthorization,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn burn(e: &Host, admin: Authorization, from: Identifier, amount: BigInt) -> Result<(), Error>;

    fn freeze(e: &Host, admin: Authorization, id: Identifier) -> Result<(), Error>;

    fn mint(e: &Host, admin: Authorization, to: Identifier, amount: BigInt) -> Result<(), Error>;

    fn set_admin(e: &Host, admin: Authorization, new_admin: Identifier) -> Result<(), Error>;

    fn unfreeze(e: &Host, admin: Authorization, id: Identifier) -> Result<(), Error>;

    fn decimals(e: &Host) -> Result<u32, Error>;

    fn name(e: &Host) -> Result<Bytes, Error>;

    fn symbol(e: &Host) -> Result<Bytes, Error>;
}

pub struct Token;

#[contractimpl]
impl TokenTrait for Token {
    fn initialize(e: &Host, admin: Identifier, metadata: Metadata) -> Result<(), Error> {
        if has_administrator(&e)? {
            return Err(Error::ContractError);
        }
        write_administrator(&e, admin)?;
        write_metadata(&e, metadata)?;
        Ok(())
    }

    fn nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
        read_nonce(e, id)
    }

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error> {
        read_allowance(&e, from, spender)
    }

    fn approve(
        e: &Host,
        from: KeyedAuthorization,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(spender.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, Domain::Approve, args)?;
        write_allowance(&e, from_id, spender, amount)?;
        Ok(())
    }

    fn balance(e: &Host, id: Identifier) -> Result<BigInt, Error> {
        read_balance(e, id)
    }

    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, Error> {
        read_state(&e, id)
    }

    fn xfer(
        e: &Host,
        from: KeyedAuthorization,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, Domain::Transfer, args)?;
        spend_balance(&e, from_id, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn xfer_from(
        e: &Host,
        spender: KeyedAuthorization,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let spender_id = spender.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, spender, Domain::TransferFrom, args)?;
        spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
        spend_balance(&e, from, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn burn(e: &Host, admin: Authorization, from: Identifier, amount: BigInt) -> Result<(), Error> {
        let auth = to_administrator_authorization(&e, admin)?;
        let mut args = Vec::new(e)?;
        args.push(from.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, auth, Domain::Burn, args)?;
        spend_balance(&e, from, amount)?;
        Ok(())
    }

    fn freeze(e: &Host, admin: Authorization, id: Identifier) -> Result<(), Error> {
        let auth = to_administrator_authorization(&e, admin)?;
        let mut args = Vec::new(e)?;
        args.push(id.clone())?;
        check_auth(&e, auth, Domain::Freeze, args)?;
        write_state(&e, id, true)?;
        Ok(())
    }

    fn mint(e: &Host, admin: Authorization, to: Identifier, amount: BigInt) -> Result<(), Error> {
        let auth = to_administrator_authorization(&e, admin)?;
        let mut args = Vec::new(e)?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, auth, Domain::Mint, args)?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn set_admin(e: &Host, admin: Authorization, new_admin: Identifier) -> Result<(), Error> {
        let auth = to_administrator_authorization(&e, admin)?;
        let mut args = Vec::new(e)?;
        args.push(new_admin.clone())?;
        check_auth(&e, auth, Domain::SetAdministrator, args)?;
        write_administrator(&e, new_admin)?;
        Ok(())
    }

    fn unfreeze(e: &Host, admin: Authorization, id: Identifier) -> Result<(), Error> {
        let auth = to_administrator_authorization(&e, admin)?;
        let mut args = Vec::new(e)?;
        args.push(id.clone())?;
        check_auth(&e, auth, Domain::Unfreeze, args)?;
        write_state(&e, id, false)?;
        Ok(())
    }

    fn decimals(e: &Host) -> Result<u32, Error> {
        read_decimal(&e)
    }

    fn name(e: &Host) -> Result<Bytes, Error> {
        read_name(&e)
    }

    fn symbol(e: &Host) -> Result<Bytes, Error> {
        read_symbol(&e)
    }
}
