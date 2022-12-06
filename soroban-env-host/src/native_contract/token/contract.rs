use std::cmp::Ordering;

use crate::host::metered_clone::MeteredClone;
use crate::host::Host;
use crate::native_contract::base_types::{Bytes, BytesN, Vec};
use crate::native_contract::token::admin::{check_admin, write_administrator};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    read_balance, read_state, receive_balance, spend_balance, transfer_classic_balance, write_state,
};
use crate::native_contract::token::cryptography::check_auth;
use crate::native_contract::token::event;
use crate::native_contract::token::metadata::{
    has_metadata, read_decimal, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{Identifier, Metadata, Signature, TokenMetadata};
use crate::{err, HostError};

use soroban_env_common::xdr::Asset;
use soroban_env_common::{CheckedEnv, Compare, Convert, EnvBase, Symbol};
use soroban_native_sdk_macros::contractimpl;

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
    /// (burn, freeze, unfreeze, mint, set_admin) will always fail
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError>;

    /// init creates a token contract that does not wrap an asset on the classic side.
    fn init(e: &Host, admin: Identifier, metadata: TokenMetadata) -> Result<(), HostError>;

    fn nonce(e: &Host, id: Identifier) -> Result<i128, HostError>;

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<i128, HostError>;

    fn approve(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError>;

    fn balance(e: &Host, id: Identifier) -> Result<i128, HostError>;

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

    fn burn(
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

    fn import(e: &Host, id: Signature, nonce: i128, amount: i64) -> Result<(), HostError>;

    fn export(e: &Host, id: Signature, nonce: i128, amount: i64) -> Result<(), HostError>;
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

        let curr_contract_id: BytesN<32> = e.convert(e.get_current_contract()?)?;
        let expected_contract_id: BytesN<32> =
            e.convert(e.get_contract_id_from_asset(asset.clone())?)?;
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
                        asset_code: e.convert(e.bytes_new_from_slice(&asset4.asset_code.0)?)?,
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
                        asset_code: e.convert(e.bytes_new_from_slice(&asset12.asset_code.0)?)?,
                        issuer: asset12.issuer,
                    }),
                )?;
            }
        }
        Ok(())
    }

    fn init(e: &Host, admin: Identifier, metadata: TokenMetadata) -> Result<(), HostError> {
        if has_metadata(&e)? {
            return Err(e.err_status_msg(
                ContractError::AlreadyInitializedError,
                "token has been already initialized",
            ));
        }

        write_administrator(&e, admin)?;
        write_metadata(&e, Metadata::Token(metadata))?;
        Ok(())
    }

    fn nonce(e: &Host, id: Identifier) -> Result<i128, HostError> {
        read_nonce(e, id)
    }

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<i128, HostError> {
        read_allowance(&e, from, spender)
    }

    // Metering: covered by components
    fn approve(
        e: &Host,
        from: Signature,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(&from.get_identifier(&e)?)?;
        args.push(&nonce)?;
        args.push(&spender)?;
        args.push(&amount)?;
        check_auth(&e, from, nonce, Symbol::from_str("approve"), args)?;
        write_allowance(&e, from_id.clone(), spender.clone(), amount.clone())?;
        event::approve(e, from_id, spender, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn balance(e: &Host, id: Identifier) -> Result<i128, HostError> {
        read_balance(e, id)
    }

    // Metering: covered by components
    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, HostError> {
        read_state(&e, id)
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
        args.push(&from.get_identifier(&e)?)?;
        args.push(&nonce)?;
        args.push(&to)?;
        args.push(&amount)?;
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
        args.push(&spender.get_identifier(&e)?)?;
        args.push(&nonce)?;
        args.push(&from)?;
        args.push(&to)?;
        args.push(&amount)?;
        check_auth(&e, spender, nonce, Symbol::from_str("xfer_from"), args)?;
        spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
        spend_balance(&e, from.clone(), amount.clone())?;
        receive_balance(&e, to.clone(), amount.clone())?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn burn(
        e: &Host,
        admin: Signature,
        nonce: i128,
        from: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(&admin_id)?;
        args.push(&nonce)?;
        args.push(&from)?;
        args.push(&amount)?;
        check_auth(&e, admin, nonce, Symbol::from_str("burn"), args)?;
        spend_balance(&e, from.clone(), amount.clone())?;
        event::burn(e, admin_id, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn freeze(e: &Host, admin: Signature, nonce: i128, id: Identifier) -> Result<(), HostError> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        let admin_id = admin.get_identifier(&e)?;
        args.push(&admin_id)?;
        args.push(&nonce)?;
        args.push(&id)?;
        check_auth(&e, admin, nonce, Symbol::from_str("freeze"), args)?;
        write_state(&e, id.clone(), true)?;
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
        args.push(&admin_id)?;
        args.push(&nonce)?;
        args.push(&to)?;
        args.push(&amount)?;
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
        args.push(&admin_id)?;
        args.push(&nonce)?;
        args.push(&new_admin)?;
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
        args.push(&admin_id)?;
        args.push(&nonce)?;
        args.push(&id)?;
        check_auth(&e, admin, nonce, Symbol::from_str("unfreeze"), args)?;
        write_state(&e, id.clone(), false)?;
        event::unfreeze(e, admin_id, id)?;
        Ok(())
    }

    fn decimals(e: &Host) -> Result<u32, HostError> {
        read_decimal(&e)
    }

    fn name(e: &Host) -> Result<Bytes, HostError> {
        read_name(&e)
    }

    fn symbol(e: &Host) -> Result<Bytes, HostError> {
        read_symbol(&e)
    }

    // Metering: covered by components
    fn import(e: &Host, id: Signature, nonce: i128, amount: i64) -> Result<(), HostError> {
        let amount_i128: i128 = amount as i128;
        check_nonnegative_amount(e, amount_i128)?;

        let account_id = id.get_account_id(e)?;

        let mut args = Vec::new(e)?;
        let ident = id.get_identifier(&e)?;
        args.push(&ident)?;
        args.push(&nonce)?;
        args.push(&amount)?;
        check_auth(&e, id, nonce, Symbol::from_str("import"), args)?;

        transfer_classic_balance(e, account_id.metered_clone(e.budget_ref())?, -amount)?;
        receive_balance(&e, Identifier::Account(account_id), amount_i128)?;
        event::import(e, ident, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn export(e: &Host, id: Signature, nonce: i128, amount: i64) -> Result<(), HostError> {
        let amount_i128: i128 = amount as i128;
        check_nonnegative_amount(e, amount_i128)?;

        let account_id = id.get_account_id(e)?;

        let mut args = Vec::new(e)?;
        let ident = id.get_identifier(&e)?;
        args.push(&ident)?;
        args.push(&nonce)?;
        args.push(&amount)?;
        check_auth(&e, id, nonce, Symbol::from_str("export"), args)?;

        transfer_classic_balance(e, account_id.metered_clone(e.budget_ref())?, amount)?;
        spend_balance(&e, Identifier::Account(account_id), amount_i128)?;
        event::export(e, ident, amount)?;
        Ok(())
    }
}
