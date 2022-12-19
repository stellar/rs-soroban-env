use core::cmp::Ordering;

use crate::host::metered_clone::MeteredClone;
use crate::host::Host;
use crate::native_contract::base_types::{Bytes, BytesN, Vec};
use crate::native_contract::token::admin::{check_admin, write_administrator};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    is_deauthorized, read_balance, receive_balance, spend_balance, write_authorization,
};
use crate::native_contract::token::cryptography::check_auth;
use crate::native_contract::token::event;
use crate::native_contract::token::metadata::{
    has_metadata, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{Identifier, Metadata, Signature};
use crate::{err, HostError};

use soroban_env_common::xdr::Asset;
use soroban_env_common::{CheckedEnv, Compare, EnvBase, Symbol, TryFromVal, TryIntoVal};
use soroban_native_sdk_macros::contractimpl;

use super::balance::{
    check_clawbackable, get_spendable_balance, spend_balance_no_authorization_check,
};
use super::error::ContractError;
use super::public_types::{AlphaNum12Metadata, AlphaNum4Metadata};

pub trait TokenTrait {
    /// init_asset can create a contract for a wrapped classic asset
    /// (Native, AlphaNum4, or AlphaNum12). It will fail if the contractID
    /// of this contract does not match the expected contractID for this asset
    /// returned by Host::get_contract_id_from_asset. This function should only be
    /// called by the create_token_from_asset host function for this reason.
    ///
    /// No admin will be set for the Native token, so any function that checks the admin
    /// (clawback, freeze, unfreeze, mint, set_admin) will always fail
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError>;

    fn nonce(e: &Host, id: Identifier) -> Result<i128, HostError>;

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<i128, HostError>;

    fn incr_allow(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn decr_allow(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn balance(e: &Host, id: Identifier) -> Result<i128, HostError>;

    fn spendable(e: &Host, id: Identifier) -> Result<i128, HostError>;

    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, HostError>;

    fn xfer(
        e: &Host,
        from: Signature,
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: i128,
        from: Identifier,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn freeze(e: &Host, admin: Signature, nonce: i128, id: Identifier) -> Result<(), HostError>;

    fn unfreeze(e: &Host, admin: Signature, nonce: i128, id: Identifier) -> Result<(), HostError>;

    fn mint(
        e: &Host,
        admin: Signature,
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn clawback(
        e: &Host,
        admin: Signature,
        nonce: i128,
        from: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: i128,
        new_admin: Identifier,
    ) -> Result<(), HostError>;

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

        let curr_contract_id = BytesN::<32>::try_from_val(e, e.get_current_contract()?)?;
        let expected_contract_id =
            BytesN::<32>::try_from_val(e, e.get_contract_id_from_asset(asset.clone())?)?;
        if e.compare(&curr_contract_id, &expected_contract_id)? != Ordering::Equal {
            return Err(err!(
                e,
                ContractError::InternalError,
                "bad id for asset contract: '{}' expected, got '{}'",
                expected_contract_id,
                curr_contract_id
            ));
        }
        match asset {
            Asset::Native => {
                write_metadata(&e, Metadata::Native)?;
                //No admin for the Native token
            }
            Asset::CreditAlphanum4(asset4) => {
                write_administrator(
                    &e,
                    Identifier::Account(asset4.issuer.metered_clone(e.budget_ref())?),
                )?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum4(AlphaNum4Metadata {
                        asset_code: BytesN::<4>::try_from_val(
                            e,
                            e.bytes_new_from_slice(&asset4.asset_code.0)?,
                        )?,
                        issuer: asset4.issuer,
                    }),
                )?;
            }
            Asset::CreditAlphanum12(asset12) => {
                write_administrator(
                    &e,
                    Identifier::Account(asset12.issuer.metered_clone(e.budget_ref())?),
                )?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum12(AlphaNum12Metadata {
                        asset_code: BytesN::<12>::try_from_val(
                            e,
                            e.bytes_new_from_slice(&asset12.asset_code.0)?,
                        )?,
                        issuer: asset12.issuer,
                    }),
                )?;
            }
        }
        Ok(())
    }

    fn nonce(e: &Host, id: Identifier) -> Result<i128, HostError> {
        read_nonce(e, id)
    }

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<i128, HostError> {
        read_allowance(&e, from, spender)
    }

    // Metering: covered by components
    fn incr_allow(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(spender.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("incr_allow"), args)?;

        let allowance = read_allowance(&e, from_id.clone(), spender.clone())?;
        let new_allowance = allowance
            .checked_add(amount)
            .ok_or_else(|| e.err_status(ContractError::OverflowError))?;

        write_allowance(&e, from_id.clone(), spender.clone(), new_allowance)?;
        event::incr_allow(e, from_id, spender, amount)?;
        Ok(())
    }

    fn decr_allow(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(spender.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("decr_allow"), args)?;

        let allowance = read_allowance(&e, from_id.clone(), spender.clone())?;

        if amount >= allowance {
            write_allowance(&e, from_id.clone(), spender.clone(), 0)?;
        } else {
            write_allowance(
                &e,
                from_id.clone(),
                spender.clone(),
                allowance - amount.clone(),
            )?;
        }
        event::decr_allow(e, from_id, spender, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn balance(e: &Host, id: Identifier) -> Result<i128, HostError> {
        read_balance(e, id)
    }

    fn spendable(e: &Host, id: Identifier) -> Result<i128, HostError> {
        get_spendable_balance(e, id)
    }

    // Metering: covered by components
    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, HostError> {
        is_deauthorized(&e, id)
    }

    // Metering: covered by components
    fn xfer(
        e: &Host,
        from: Signature,
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("xfer"), args)?;
        spend_balance(&e, from_id.clone(), amount.clone())?;
        receive_balance(&e, to.clone(), amount.clone())?;
        event::transfer(e, from_id, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: i128,
        from: Identifier,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let spender_id = spender.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(spender.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, spender, nonce, Symbol::from_str("xfer_from"), args)?;
        spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
        spend_balance(&e, from.clone(), amount.clone())?;
        receive_balance(&e, to.clone(), amount.clone())?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn clawback(
        e: &Host,
        admin: Signature,
        nonce: i128,
        from: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_admin(&e, &admin)?;
        check_clawbackable(&e, from.clone())?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(admin_id.clone())?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("clawback"), args)?;
        // admin can clawback a frozen balance
        spend_balance_no_authorization_check(&e, from.clone(), amount.clone())?;
        event::clawback(e, admin_id, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn freeze(e: &Host, admin: Signature, nonce: i128, id: Identifier) -> Result<(), HostError> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(admin_id.clone())?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("freeze"), args)?;
        write_authorization(&e, id.clone(), false)?;
        event::freeze(e, admin_id, id)?;
        Ok(())
    }

    // Metering: covered by components
    fn mint(
        e: &Host,
        admin: Signature,
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(admin_id.clone())?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("mint"), args)?;
        receive_balance(&e, to.clone(), amount.clone())?;
        event::mint(e, admin_id, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: i128,
        new_admin: Identifier,
    ) -> Result<(), HostError> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(admin_id.clone())?;
        args.push(nonce.clone())?;
        args.push(new_admin.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("set_admin"), args)?;
        write_administrator(&e, new_admin.clone())?;
        event::set_admin(e, admin_id, new_admin)?;
        Ok(())
    }

    // Metering: covered by components
    fn unfreeze(e: &Host, admin: Signature, nonce: i128, id: Identifier) -> Result<(), HostError> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(admin_id.clone())?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("unfreeze"), args)?;
        write_authorization(&e, id.clone(), true)?;
        event::unfreeze(e, admin_id, id)?;
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
