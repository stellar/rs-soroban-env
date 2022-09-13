use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Bytes, Vec};
use crate::native_contract::token::admin::{check_admin, has_administrator, write_administrator};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    read_balance, read_state, receive_balance, spend_balance, transfer_classic_balance, write_state,
};
use crate::native_contract::token::cryptography::check_auth;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::metadata::{
    read_decimal, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{Identifier, Signature};
use soroban_env_common::{Symbol, TryIntoVal};
use soroban_native_sdk_macros::contractimpl;

pub trait TokenTrait {
    fn initialize(e: &Host, admin: Identifier, metadata: Metadata) -> Result<(), Error>;

    fn nonce(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error>;

    fn approve(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn balance(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, Error>;

    fn xfer(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: BigInt,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn burn(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        from: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn freeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error>;

    fn mint(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        new_admin: Identifier,
    ) -> Result<(), Error>;

    fn unfreeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error>;

    fn decimals(e: &Host) -> Result<u32, Error>;

    fn name(e: &Host) -> Result<Bytes, Error>;

    fn symbol(e: &Host) -> Result<Bytes, Error>;

    fn to_smart(e: &Host, id: KeyedAuthorization, amount: i64) -> Result<(), Error>;

    fn to_classic(e: &Host, id: KeyedAuthorization, amount: i64) -> Result<(), Error>;
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
        from: Signature,
        nonce: BigInt,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(spender.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("approve"), args)?;
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
        from: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("xfer"), args)?;
        spend_balance(&e, from_id, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: BigInt,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        let spender_id = spender.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(spender.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, spender, nonce, Symbol::from_str("xfer_from"), args)?;
        spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
        spend_balance(&e, from, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn burn(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        from: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("burn"), args)?;
        spend_balance(&e, from, amount)?;
        Ok(())
    }

    fn freeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("freeze"), args)?;
        write_state(&e, id, true)?;
        Ok(())
    }

    fn mint(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("mint"), args)?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        new_admin: Identifier,
    ) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(new_admin.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("set_admin"), args)?;
        write_administrator(&e, new_admin)?;
        Ok(())
    }

    fn unfreeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("unfreeze"), args)?;
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

    fn to_smart(e: &Host, id: KeyedAuthorization, amount: i64) -> Result<(), Error> {
        if amount < 0 {
            return Err(Error::ContractError);
        }

        let id_key = match &id {
            KeyedAuthorization::Account(acc) => Ok(acc.public_key.clone()),
            _ => Err(Error::ContractError),
        }?;
        let mut args = Vec::new(e)?;
        args.push(amount.clone())?;
        check_auth(&e, id, Domain::ToSmart, args)?;

        transfer_classic_balance(e, id_key.clone(), amount)?;
        receive_balance(
            &e,
            Identifier::Account(id_key),
            BigInt::from_u64(&e, amount.try_into().map_err(|_| Error::ContractError)?)?,
        )?;
        Ok(())
    }

    fn to_classic(e: &Host, id: KeyedAuthorization, amount: i64) -> Result<(), Error> {
        if amount < 0 {
            return Err(Error::ContractError);
        }

        let id_key = match &id {
            KeyedAuthorization::Account(acc) => Ok(acc.public_key.clone()),
            _ => Err(Error::ContractError),
        }?;
        let mut args = Vec::new(e)?;
        args.push(amount.clone())?;
        check_auth(&e, id, Domain::ToClassic, args)?;

        transfer_classic_balance(e, id_key.clone(), -amount)?;
        spend_balance(
            &e,
            Identifier::Account(id_key),
            BigInt::from_u64(&e, amount.try_into().map_err(|_| Error::ContractError)?)?,
        )?;
        Ok(())
    }
}
