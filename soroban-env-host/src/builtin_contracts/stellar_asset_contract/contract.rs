use crate::builtin_contracts::base_types::{Address, Bytes, BytesN, String};
use crate::builtin_contracts::contract_error::ContractError;
use crate::builtin_contracts::stellar_asset_contract::allowance::{
    read_allowance, spend_allowance, write_allowance,
};
use crate::builtin_contracts::stellar_asset_contract::asset_info::{
    has_asset_info, write_asset_info,
};
use crate::builtin_contracts::stellar_asset_contract::balance::{
    is_authorized, read_balance, receive_balance, spend_balance, write_authorization,
};
use crate::builtin_contracts::stellar_asset_contract::event;
use crate::builtin_contracts::stellar_asset_contract::public_types::AssetInfo;
use crate::host::{metered_clone::MeteredClone, Host};
use crate::{err, HostError};

use soroban_builtin_sdk_macros::contractimpl;
use soroban_env_common::xdr::Asset;
use soroban_env_common::{ConversionError, Env, EnvBase, TryFromVal, TryIntoVal};

use super::admin::{read_administrator, write_administrator};
use super::asset_info::read_asset_info;
use super::balance::{check_clawbackable, spend_balance_no_authorization_check};
use super::metadata::{read_name, read_symbol, set_metadata, DECIMAL};
use super::public_types::{AlphaNum12AssetInfo, AlphaNum4AssetInfo};
use super::storage_types::{INSTANCE_EXTEND_AMOUNT, INSTANCE_TTL_THRESHOLD};

pub struct StellarAssetContract;

fn check_nonnegative_amount(e: &Host, amount: i128) -> Result<(), HostError> {
    if amount < 0 {
        Err(err!(
            e,
            ContractError::NegativeAmountError,
            "negative amount is not allowed",
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
impl StellarAssetContract {
    pub fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract init_asset");
        if has_asset_info(e)? {
            return Err(e.error(
                ContractError::AlreadyInitializedError.into(),
                "StellarAssetContract has been already initialized",
                &[],
            ));
        }

        let asset: Asset = e.metered_from_xdr_obj(asset_bytes.into())?;

        let curr_contract_id = e.get_current_contract_id_internal()?;
        let expected_contract_id = e.get_asset_contract_id_hash(asset.metered_clone(e)?)?;
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
                //No admin for the stellar asset contract
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

    pub fn allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
        let _span = tracy_span!("stellar asset contract allowance");
        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        read_allowance(e, from, spender)
    }

    // Metering: covered by components
    pub fn approve(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
        live_until_ledger: u32,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract approve");
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        write_allowance(
            e,
            from.metered_clone(e)?,
            spender.metered_clone(e)?,
            amount,
            live_until_ledger,
        )?;
        event::approve(e, from, spender, amount, live_until_ledger)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn balance(e: &Host, addr: Address) -> Result<i128, HostError> {
        let _span = tracy_span!("stellar asset contract balance");
        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        read_balance(e, addr)
    }

    // Metering: covered by components
    pub fn authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
        let _span = tracy_span!("stellar asset contract authorized");
        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        is_authorized(e, addr)
    }

    // Metering: covered by components
    pub fn transfer(e: &Host, from: Address, to: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract transfer");
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance(e, from.metered_clone(e)?, amount)?;
        receive_balance(e, to.metered_clone(e)?, amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn transfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract transfer_from");
        check_nonnegative_amount(e, amount)?;
        spender.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_allowance(e, from.metered_clone(e)?, spender, amount)?;
        spend_balance(e, from.metered_clone(e)?, amount)?;
        receive_balance(e, to.metered_clone(e)?, amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract burn");
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        from.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance(e, from.metered_clone(e)?, amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn burn_from(
        e: &Host,
        spender: Address,
        from: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract burn_from");
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        spender.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_allowance(e, from.metered_clone(e)?, spender, amount)?;
        spend_balance(e, from.metered_clone(e)?, amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn clawback(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract clawback");
        check_nonnegative_amount(e, amount)?;
        check_clawbackable(e, from.metered_clone(e)?)?;
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance_no_authorization_check(e, from.metered_clone(e)?, amount)?;
        event::clawback(e, admin, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn set_authorized(e: &Host, addr: Address, authorize: bool) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract set_authorized");
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        write_authorization(e, addr.metered_clone(e)?, authorize)?;
        event::set_authorized(e, admin, addr, authorize)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn mint(e: &Host, to: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract mint");
        check_nonnegative_amount(e, amount)?;
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        receive_balance(e, to.metered_clone(e)?, amount)?;
        event::mint(e, admin, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub fn set_admin(e: &Host, new_admin: Address) -> Result<(), HostError> {
        let _span = tracy_span!("stellar asset contract set_admin");
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        write_administrator(e, new_admin.metered_clone(e)?)?;
        event::set_admin(e, admin, new_admin)?;
        Ok(())
    }

    pub fn admin(e: &Host) -> Result<Address, HostError> {
        let _span = tracy_span!("stellar asset contract admin");
        read_administrator(e)
    }

    pub fn decimals(_e: &Host) -> Result<u32, HostError> {
        let _span = tracy_span!("stellar asset contract decimals");
        // no need to load metadata since this is fixed for all SAC tokens
        Ok(DECIMAL)
    }

    pub fn name(e: &Host) -> Result<String, HostError> {
        let _span = tracy_span!("stellar asset contract name");
        read_name(e)
    }

    pub fn symbol(e: &Host) -> Result<String, HostError> {
        let _span = tracy_span!("stellar asset contract symbol");
        read_symbol(e)
    }
}
