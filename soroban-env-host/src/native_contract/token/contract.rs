use std::cmp::Ordering;

use crate::host::Host;
use crate::native_contract::base_types::{Address, Bytes, BytesN, String};
use crate::native_contract::contract_error::ContractError;
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::asset_info::{has_asset_info, write_asset_info};
use crate::native_contract::token::balance::{
    is_authorized, read_balance, receive_balance, spend_balance, write_authorization,
};
use crate::native_contract::token::event;
use crate::native_contract::token::public_types::AssetInfo;
use crate::{err, HostError};

use soroban_env_common::xdr::Asset;
use soroban_env_common::{Compare, ConversionError, EnvBase, TryFromVal, TryIntoVal};
use soroban_native_sdk_macros::contractimpl;

use super::admin::{read_administrator, write_administrator};
use super::asset_info::read_asset_info;
use super::balance::{
    check_clawbackable, get_spendable_balance, spend_balance_no_authorization_check,
};
use super::metadata::{read_name, read_symbol, set_metadata, DECIMAL};
use super::public_types::{AlphaNum12AssetInfo, AlphaNum4AssetInfo};

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

    fn approve(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
        expiration_ledger: u32,
    ) -> Result<(), HostError>;

    fn balance(e: &Host, addr: Address) -> Result<i128, HostError>;

    fn spendable_balance(e: &Host, addr: Address) -> Result<i128, HostError>;

    fn authorized(e: &Host, addr: Address) -> Result<bool, HostError>;

    fn transfer(e: &Host, from: Address, to: Address, amount: i128) -> Result<(), HostError>;

    fn transfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError>;

    fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError>;

    fn burn_from(e: &Host, spender: Address, from: Address, amount: i128) -> Result<(), HostError>;

    fn set_authorized(e: &Host, addr: Address, authorize: bool) -> Result<(), HostError>;

    fn mint(e: &Host, to: Address, amount: i128) -> Result<(), HostError>;

    fn clawback(e: &Host, from: Address, amount: i128) -> Result<(), HostError>;

    fn set_admin(e: &Host, new_admin: Address) -> Result<(), HostError>;

    fn is_admin(e: &Host, addres: Address) -> Result<bool, HostError>;

    fn decimals(e: &Host) -> Result<u32, HostError>;

    fn name(e: &Host) -> Result<String, HostError>;

    fn symbol(e: &Host) -> Result<String, HostError>;
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
    match read_asset_info(e)? {
        AssetInfo::Native => Err(e.error(
            ContractError::OperationNotSupportedError.into(),
            "operation invalid on native asset",
            &[],
        )),
        AssetInfo::AlphaNum4(_) | AssetInfo::AlphaNum12(_) => Ok(()),
    }
}

#[contractimpl]
// Metering: *mostly* covered by components.
impl TokenTrait for Token {
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError> {
        if has_asset_info(e)? {
            return Err(e.error(
                ContractError::AlreadyInitializedError.into(),
                "token has been already initialized",
                &[],
            ));
        }

        let asset: Asset = e.metered_from_xdr_obj(asset_bytes.into())?;

        let curr_contract_id = e.get_current_contract_id_internal()?;
        let expected_contract_id = e.get_asset_contract_id_hash(asset.clone())?;
        if curr_contract_id != expected_contract_id {
            return Err(e.error(
                ContractError::InternalError.into(),
                "bad id for asset contract",
                &[],
            ));
        }
        match asset {
            Asset::Native => {
                write_asset_info(e, AssetInfo::Native)?;
                //No admin for the Native token
            }
            Asset::CreditAlphanum4(asset4) => {
                write_administrator(e, Address::from_account(e, &asset4.issuer)?)?;
                write_asset_info(
                    e,
                    AssetInfo::AlphaNum4(AlphaNum4AssetInfo {
                        asset_code: String::try_from_val(
                            e,
                            &e.string_new_from_slice(
                                core::str::from_utf8(&asset4.asset_code.0)
                                    .map_err(|_| ConversionError)?,
                            )?,
                        )?,
                        issuer: BytesN::<32>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&e.u256_from_account(&asset4.issuer)?.0)?,
                        )?,
                    }),
                )?;
            }
            Asset::CreditAlphanum12(asset12) => {
                write_administrator(e, Address::from_account(e, &asset12.issuer)?)?;
                write_asset_info(
                    e,
                    AssetInfo::AlphaNum12(AlphaNum12AssetInfo {
                        asset_code: String::try_from_val(
                            e,
                            &e.string_new_from_slice(
                                core::str::from_utf8(&asset12.asset_code.0)
                                    .map_err(|_| ConversionError)?,
                            )?,
                        )?,
                        issuer: BytesN::<32>::try_from_val(
                            e,
                            &e.bytes_new_from_slice(&e.u256_from_account(&asset12.issuer)?.0)?,
                        )?,
                    }),
                )?;
            }
        }

        //Write metadata only after asset_info is set
        set_metadata(e)?;
        Ok(())
    }

    fn allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
        read_allowance(e, from, spender)
    }

    // Metering: covered by components
    fn approve(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
        expiration_ledger: u32,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;
        write_allowance(e, from.clone(), spender.clone(), amount, expiration_ledger)?;
        event::approve(e, from, spender, amount, expiration_ledger)?;
        Ok(())
    }

    // Metering: covered by components
    fn balance(e: &Host, addr: Address) -> Result<i128, HostError> {
        read_balance(e, addr)
    }

    fn spendable_balance(e: &Host, addr: Address) -> Result<i128, HostError> {
        get_spendable_balance(e, addr)
    }

    // Metering: covered by components
    fn authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
        is_authorized(e, addr)
    }

    // Metering: covered by components
    fn transfer(e: &Host, from: Address, to: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;
        spend_balance(e, from.clone(), amount)?;
        receive_balance(e, to.clone(), amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn transfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        spender.require_auth()?;
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
        from.require_auth()?;
        spend_balance(e, from.clone(), amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn burn_from(e: &Host, spender: Address, from: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        spender.require_auth()?;
        spend_allowance(e, from.clone(), spender, amount)?;
        spend_balance(e, from.clone(), amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn clawback(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        check_clawbackable(e, from.clone())?;
        let admin = read_administrator(e)?;
        admin.require_auth()?;
        spend_balance_no_authorization_check(e, from.clone(), amount)?;
        event::clawback(e, admin, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_authorized(e: &Host, addr: Address, authorize: bool) -> Result<(), HostError> {
        let admin = read_administrator(e)?;
        admin.require_auth()?;
        write_authorization(e, addr.clone(), authorize)?;
        event::set_authorized(e, admin, addr, authorize)?;
        Ok(())
    }

    // Metering: covered by components
    fn mint(e: &Host, to: Address, amount: i128) -> Result<(), HostError> {
        check_nonnegative_amount(e, amount)?;
        let admin = read_administrator(e)?;
        admin.require_auth()?;
        receive_balance(e, to.clone(), amount)?;
        event::mint(e, admin, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_admin(e: &Host, new_admin: Address) -> Result<(), HostError> {
        let admin = read_administrator(e)?;
        admin.require_auth()?;
        write_administrator(e, new_admin.clone())?;
        event::set_admin(e, admin, new_admin)?;
        Ok(())
    }

    fn is_admin(e: &Host, address: Address) -> Result<bool, HostError> {
        let current_admin = read_administrator(e);
        match current_admin {
            Ok(admin) => return Ok(e.compare(&admin, &address)? == Ordering::Equal),
            Err(_) => Ok(false),
        }
    }

    fn decimals(_e: &Host) -> Result<u32, HostError> {
        // no need to load metadata since this is fixed for all SAC tokens
        Ok(DECIMAL)
    }

    fn name(e: &Host) -> Result<String, HostError> {
        read_name(e)
    }

    fn symbol(e: &Host) -> Result<String, HostError> {
        read_symbol(e)
    }
}
