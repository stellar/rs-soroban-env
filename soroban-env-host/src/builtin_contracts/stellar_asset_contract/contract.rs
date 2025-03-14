use core::cmp::Ordering;

use crate::{
    builtin_contracts::{
        base_types::{Address, BytesN, String},
        contract_error::ContractError,
        stellar_asset_contract::{
            admin::{read_administrator, write_administrator},
            allowance::{read_allowance, spend_allowance, write_allowance},
            asset_info::{has_asset_info, read_asset_info, validate_asset, write_asset_info},
            balance::{
                check_clawbackable, is_authorized, read_balance, receive_balance, spend_balance,
                spend_balance_no_authorization_check, write_authorization,
            },
            event,
            metadata::{read_name, read_symbol, set_metadata, DECIMAL},
            public_types::{AlphaNum12AssetInfo, AlphaNum4AssetInfo, AssetInfo},
            storage_types::{INSTANCE_EXTEND_AMOUNT, INSTANCE_TTL_THRESHOLD},
        },
    },
    err,
    host::{metered_clone::MeteredClone, Host},
    xdr::Asset,
    BytesObject, Compare, Env, EnvBase, HostError, TryFromVal, TryIntoVal,
};

use soroban_builtin_sdk_macros::contractimpl;

pub(crate) struct StellarAssetContract;

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

fn check_not_issuer(e: &Host, addr: &Address) -> Result<(), HostError> {
    let issuer_check = |issuer: BytesN<32>, address: &Address| -> Result<(), HostError> {
        let issuer_account_id = e.account_id_from_bytesobj(issuer.into())?;
        let issuer_address = Address::from_account(e, &issuer_account_id)?;
        if e.compare(&issuer_address, &address)? == Ordering::Equal {
            Err(e.error(
                ContractError::OperationNotSupportedError.into(),
                "operation invalid on issuer",
                &[],
            ))
        } else {
            Ok(())
        }
    };

    match read_asset_info(e)? {
        AssetInfo::Native => Ok(()),
        AssetInfo::AlphaNum4(asset) => issuer_check(asset.issuer, addr),
        AssetInfo::AlphaNum12(asset) => issuer_check(asset.issuer, addr),
    }
}

#[contractimpl]
// Metering: covered by components.
impl StellarAssetContract {
    pub(crate) fn init_asset(e: &Host, asset_bytes: BytesObject) -> Result<(), HostError> {
        let _span = tracy_span!("SAC init_asset");
        if has_asset_info(e)? {
            return Err(e.error(
                ContractError::AlreadyInitializedError.into(),
                "StellarAssetContract has been already initialized",
                &[],
            ));
        }

        let asset: Asset = e.metered_from_xdr_obj(asset_bytes)?;
        validate_asset(e, &asset)?;

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
                            &e.string_new_from_slice(&asset4.asset_code.0)?,
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
                            &e.string_new_from_slice(&asset12.asset_code.0)?,
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

    pub(crate) fn allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
        let _span = tracy_span!("SAC allowance");
        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        read_allowance(e, from, spender)
    }

    // Metering: covered by components
    pub(crate) fn approve(
        e: &Host,
        from: Address,
        spender: Address,
        amount: i128,
        live_until_ledger: u32,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("SAC approve");
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
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
    pub(crate) fn balance(e: &Host, addr: Address) -> Result<i128, HostError> {
        let _span = tracy_span!("SAC balance");
        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        read_balance(e, addr)
    }

    // Metering: covered by components
    pub(crate) fn authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
        let _span = tracy_span!("SAC authorized");
        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;
        is_authorized(e, addr)
    }

    // Metering: covered by components
    pub(crate) fn transfer(
        e: &Host,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("SAC transfer");
        check_nonnegative_amount(e, amount)?;
        from.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance(e, from.metered_clone(e)?, amount)?;
        receive_balance(e, to.metered_clone(e)?, amount)?;
        event::transfer(e, from, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn transfer_from(
        e: &Host,
        spender: Address,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("SAC transfer_from");
        check_nonnegative_amount(e, amount)?;
        spender.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
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
    pub(crate) fn burn(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("SAC burn");
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        check_not_issuer(e, &from)?;

        from.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance(e, from.metered_clone(e)?, amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn burn_from(
        e: &Host,
        spender: Address,
        from: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("SAC burn_from");
        check_nonnegative_amount(e, amount)?;
        check_non_native(e)?;
        check_not_issuer(e, &from)?;

        spender.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_allowance(e, from.metered_clone(e)?, spender, amount)?;
        spend_balance(e, from.metered_clone(e)?, amount)?;
        event::burn(e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn clawback(e: &Host, from: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("SAC clawback");
        check_nonnegative_amount(e, amount)?;
        check_clawbackable(e, from.metered_clone(e)?)?;
        check_not_issuer(e, &from)?;

        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        spend_balance_no_authorization_check(e, from.metered_clone(e)?, amount)?;
        event::clawback(e, admin, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn set_authorized(
        e: &Host,
        addr: Address,
        authorize: bool,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("SAC set_authorized");
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        write_authorization(e, addr.metered_clone(e)?, authorize)?;
        event::set_authorized(e, admin, addr, authorize)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn mint(e: &Host, to: Address, amount: i128) -> Result<(), HostError> {
        let _span = tracy_span!("SAC mint");
        check_nonnegative_amount(e, amount)?;
        check_not_issuer(e, &to)?;

        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        receive_balance(e, to.metered_clone(e)?, amount)?;
        event::mint(e, admin, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    pub(crate) fn set_admin(e: &Host, new_admin: Address) -> Result<(), HostError> {
        let _span = tracy_span!("SAC set_admin");
        let admin = read_administrator(e)?;
        admin.require_auth()?;

        e.extend_current_contract_instance_and_code_ttl(
            INSTANCE_TTL_THRESHOLD.into(),
            INSTANCE_EXTEND_AMOUNT.into(),
        )?;

        write_administrator(e, new_admin.metered_clone(e)?)?;
        event::set_admin(e, admin, new_admin)?;
        Ok(())
    }

    pub(crate) fn admin(e: &Host) -> Result<Address, HostError> {
        let _span = tracy_span!("SAC admin");
        read_administrator(e)
    }

    pub(crate) fn decimals(_e: &Host) -> Result<u32, HostError> {
        let _span = tracy_span!("SAC decimals");
        // no need to load metadata since this is fixed for all SAC tokens
        Ok(DECIMAL)
    }

    pub(crate) fn name(e: &Host) -> Result<String, HostError> {
        let _span = tracy_span!("SAC name");
        read_name(e)
    }

    pub(crate) fn symbol(e: &Host) -> Result<String, HostError> {
        let _span = tracy_span!("SAC symbol");
        read_symbol(e)
    }
}
