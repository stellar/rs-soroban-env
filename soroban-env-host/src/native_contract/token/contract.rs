use crate::host::Host;
use crate::native_contract::base_types::{Address, Bytes, BytesN, Vec};
use crate::native_contract::contract_error::ContractError;
use crate::native_contract::token::admin::{check_admin, write_administrator};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    is_authorized, read_balance, receive_balance, spend_balance, write_authorization,
};
use crate::native_contract::token::event;
use crate::native_contract::token::metadata::{
    has_metadata, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::public_types::Metadata;
use crate::{err, HostError};

use soroban_env_common::xdr::Asset;
use soroban_env_common::{EnvBase, TryFromVal, TryIntoVal};
use soroban_native_sdk_macros::contractimpl;

use super::balance::{
    check_clawbackable, get_spendable_balance, spend_balance_no_authorization_check,
};
use super::metadata::read_metadata;
use super::public_types::{AlphaNum12Metadata, AlphaNum4Metadata};

pub trait TokenTrait {
    /// init_asset can create a contract for a wrapped classic asset
    /// (Native, AlphaNum4, or AlphaNum12). It will fail if the contractID
    /// of this contract does not match the expected contractID for this asset
    /// returned by Host::get_contract_id_from_asset. This function should only be
    /// called internally by the host.
    ///
    /// No admin will be set for the Native token, so any function that checks the admin
    /// (clawback, set_auth, mint, set_admin) will always fail
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError>;

    fn allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError>;

    fn incr_allow(e: &Host, from: Address, spender: Address, amount: i128)
        -> Result<(), HostError>;

    fn decr_allow(e: &Host, from: Address, spender: Address, amount: i128)
        -> Result<(), HostError>;

    fn balance(e: &Host, addr: Address) -> Result<i128, HostError>;

    fn spendable(e: &Host, addr: Address) -> Result<i128, HostError>;

    fn authorized(e: &Host, addr: Address) -> Result<bool, HostError>;

    fn xfer(e: &Host, from: Address, to: Address, amount: i128) -> Result<(), HostError>;

    fn xfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError>;

    fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError>;

    fn burn_from(e: &Host, spender: Address, from: Address, amount: i128) -> Result<(), HostError>;

    fn set_auth(e: &Host, admin: Address, addr: Address, authorize: bool) -> Result<(), HostError>;

    fn mint(e: &Host, admin: Address, to: Address, amount: i128) -> Result<(), HostError>;

    fn clawback(e: &Host, admin: Address, from: Address, amount: i128) -> Result<(), HostError>;

    fn set_admin(e: &Host, admin: Address, new_admin: Address) -> Result<(), HostError>;

    fn decimals(e: &Host) -> Result<u32, HostError>;

    fn name(e: &Host) -> Result<Bytes, HostError>;

    fn symbol(e: &Host) -> Result<Bytes, HostError>;
}

pub struct Token;

fn check_nonnegative_amount(e: &Host, amount: i128) -> Result<(), HostError> {
    if amount < 0 {
        Err(err!(
            e,
            ContractError::NegativeAmountError,
            "negative amount is not allowed: {}",
            amount
        ))
    } else {
        Ok(())
    }
}

fn check_non_native(e: &Host) -> Result<(), HostError> {
    match read_metadata(e)? {
        Metadata::Native => Err(e.err_status_msg(
            ContractError::OperationNotSupportedError,
            "operation invalid on native asset",
        )),
        Metadata::AlphaNum4(_) | Metadata::AlphaNum12(_) => Ok(()),
    }
}

#[contractimpl]
// Metering: *mostly* covered by components.
impl TokenTrait for Token {
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError> {
        if has_metadata(&e)? {
            return Err(e.err_status_msg(
                ContractError::AlreadyInitializedError,
                "token has been already initialized",
            ));
        }

        let asset: Asset = e.metered_from_xdr_obj(asset_bytes.into())?;

        let curr_contract_id = e.get_current_contract_id_internal()?;
        let expected_contract_id = e.get_contract_id_from_asset(asset.clone())?;
        if curr_contract_id != expected_contract_id {
            return Err(err!(
                e,
                ContractError::InternalError,
                "bad id for asset contract: '{}' expected, got '{}'",
                expected_contract_id.0,
                curr_contract_id.0
            ));
        }
        match asset {
            Asset::Native => {
                write_metadata(&e, Metadata::Native)?;
                //No admin for the Native token
            }
            Asset::CreditAlphanum4(asset4) => {
                write_administrator(&e, Address::from_account(e, &asset4.issuer)?)?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum4(AlphaNum4Metadata {
                        asset_code: BytesN::<4>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&asset4.asset_code.0)?,
                        )?,
                        issuer: BytesN::<32>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&e.to_u256_from_account(&asset4.issuer)?.0)?,
                        )?,
                    }),
                )?;
            }
            Asset::CreditAlphanum12(asset12) => {
                write_administrator(&e, Address::from_account(e, &asset12.issuer)?)?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum12(AlphaNum12Metadata {
                        asset_code: BytesN::<12>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&asset12.asset_code.0)?,
                        )?,
                        issuer: BytesN::<32>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&e.to_u256_from_account(&asset12.issuer)?.0)?,
                        )?,
                    }),
                )?;
            }
        }
        Ok(())
    }

    fn allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
        read_allowance(e, from, spender)
    }

    // Metering: covered by components
    fn incr_allow(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let mut args = Vec::new(e)?;
        args.push(&from)?;
        args.push(&spender)?;
        args.push(&amount)?;
        from.authorize(args)?;
        let allowance = read_allowance(&e, from.clone(), spender.clone())?;
        let new_allowance = allowance
            .checked_add(amount)
            .ok_or_else(|| e.err_status(ContractError::OverflowError))?;
        write_allowance(&e, from.clone(), spender.clone(), new_allowance)?;
        event::incr_allow(e, from, spender, amount)?;
        Ok(())
    }

    fn decr_allow(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let mut args = Vec::new(e)?;
        args.push(&from)?;
        args.push(&spender)?;
        args.push(&amount)?;
        from.authorize(args)?;
        let allowance = read_allowance(&e, from.clone(), spender.clone())?;
        if amount >= allowance {
            write_allowance(&e, from.clone(), spender.clone(), 0)?;
        } else {
            write_allowance(&e, from.clone(), spender.clone(), allowance - amount)?;
        }
        event::decr_allow(e, from, spender, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn balance(e: &Host, addr: Address) -> Result<i128, HostError> {
        read_balance(e, addr)
    }

    fn spendable(e: &Host, addr: Address) -> Result<i128, HostError> {
        get_spendable_balance(e, addr)
    }

    // Metering: covered by components
    fn authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
        is_authorized(&e, addr)
    }

    // Metering: covered by components
    fn xfer(e: &Host, from: Address, to: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let mut args = Vec::new(e)?;
        args.push(&from)?;
        args.push(&to)?;
        args.push(&amount)?;
        from.authorize(args)?;
        spend_balance(e, from.clone(), amount)?;
        receive_balance(e, to.clone(), amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn xfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let mut args = Vec::new(e)?;
        args.push(&spender)?;
        args.push(&from)?;
        args.push(&to)?;
        args.push(&amount)?;
        spender.authorize(args)?;
        spend_allowance(e, from.clone(), spender, amount)?;
        spend_balance(e, from.clone(), amount)?;
        receive_balance(e, to.clone(), amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        let mut args = Vec::new(e)?;
        args.push(&from)?;
        args.push(&amount)?;
        from.authorize(args)?;
        spend_balance(&e, from.clone(), amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn burn_from(e: &Host, spender: Address, from: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        let mut args = Vec::new(e)?;
        args.push(&spender)?;
        args.push(&from)?;
        args.push(&amount)?;
        spender.authorize(args)?;
        spend_allowance(&e, from.clone(), spender, amount)?;
        spend_balance(&e, from.clone(), amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn clawback(e: &Host, admin: Address, from: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_admin(e, &admin)?;
        check_clawbackable(&e, from.clone())?;
        let mut args = Vec::new(e)?;
        args.push(&admin)?;
        args.push(&from)?;
        args.push(&amount)?;
        admin.authorize(args)?;
        spend_balance_no_authorization_check(e, from.clone(), amount.clone())?;
        event::clawback(e, admin, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_auth(e: &Host, admin: Address, addr: Address, authorize: bool) -> Result<(), HostError> {
        check_admin(e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(&admin)?;
        args.push(&addr)?;
        args.push(&authorize)?;
        admin.authorize(args)?;
        write_authorization(e, addr.clone(), authorize)?;
        event::set_auth(e, admin, addr, authorize)?;
        Ok(())
    }

    // Metering: covered by components
    fn mint(e: &Host, admin: Address, to: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_admin(e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(&admin)?;
        args.push(&to)?;
        args.push(&amount)?;
        admin.authorize(args)?;
        receive_balance(e, to.clone(), amount)?;
        event::mint(e, admin, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_admin(e: &Host, admin: Address, new_admin: Address) -> Result<(), HostError> {
        check_admin(e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(&admin)?;
        args.push(&new_admin)?;
        admin.authorize(args)?;
        write_administrator(e, new_admin.clone())?;
        event::set_admin(e, admin, new_admin)?;
        Ok(())
    }

    fn decimals(_e: &Host) -> Result<u32, HostError> {
        Ok(7)
    }

    fn name(e: &Host) -> Result<Bytes, HostError> {
        read_name(&e)
    }

    fn symbol(e: &Host) -> Result<Bytes, HostError> {
        read_symbol(&e)
    }
}
