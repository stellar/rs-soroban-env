use std::rc::Rc;

use crate::budget::AsBudget;
use crate::builtin_contracts::base_types::{Address, BytesN};
use crate::builtin_contracts::contract_error::ContractError;
use crate::builtin_contracts::stellar_asset_contract::asset_info::read_asset_info;
use crate::builtin_contracts::stellar_asset_contract::public_types::AssetInfo;
use crate::builtin_contracts::stellar_asset_contract::storage_types::DataKey;
use crate::builtin_contracts::storage_utils::StorageUtils;
use crate::host::metered_clone::MeteredClone;
use crate::host::Host;
use crate::storage::Storage;
use crate::{err, HostError};
use soroban_env_common::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1Ext, AccountFlags, AccountId,
    LedgerEntry, LedgerEntryData, LedgerKey, ScAddress, TrustLineAsset, TrustLineEntry,
    TrustLineEntryExt, TrustLineFlags,
};
use soroban_env_common::{Env, StorageType, TryIntoVal};

use super::storage_types::{BalanceValue, BALANCE_EXTEND_AMOUNT, BALANCE_TTL_THRESHOLD};

/// This module handles all balance and authorization related logic for both
/// Accounts and non-Accounts. For Accounts, a trustline is expected (unless this
/// contract is for the native asset) and trustline semantics will be followed,
/// while non-Accounts will use ContractData.
///
/// Even though non-account balances don't use trustlines, some issuer/trustline
/// semantics have been implemented for these balances. If the asset issuer has
/// the AUTH_REQUIRED flag set, then the non-account identifier must first be authorized
/// by the issuer/admin before it's allowed to hold a balance.

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub(crate) fn read_balance(e: &Host, addr: Address) -> Result<i128, HostError> {
    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => Ok(get_classic_balance(e, acc_id)?.into()),
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                e.extend_contract_data_ttl(
                    key.try_into_val(e)?,
                    StorageType::Persistent,
                    BALANCE_TTL_THRESHOLD.into(),
                    BALANCE_EXTEND_AMOUNT.into(),
                )?;
                let balance: BalanceValue = raw_balance.try_into_val(e)?;
                Ok(balance.amount)
            } else {
                Ok(0)
            }
        }
    }
}

// Metering: *mostly* covered by components.
fn write_balance(e: &Host, addr: Address, balance: BalanceValue) -> Result<(), HostError> {
    let key = DataKey::Balance(addr);
    e.put_contract_data(
        key.try_into_val(e)?,
        balance.try_into_val(e)?,
        StorageType::Persistent,
    )?;

    e.extend_contract_data_ttl(
        key.try_into_val(e)?,
        StorageType::Persistent,
        BALANCE_TTL_THRESHOLD.into(),
        BALANCE_EXTEND_AMOUNT.into(),
    )?;
    Ok(())
}

// Metering: covered by components.
pub(crate) fn receive_balance(e: &Host, addr: Address, amount: i128) -> Result<(), HostError> {
    if !is_authorized(e, addr.metered_clone(e)?)? {
        return Err(e.error(
            ContractError::BalanceDeauthorizedError.into(),
            "balance is deauthorized",
            &[],
        ));
    }

    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => {
            let i64_amount = i64::try_from(amount).map_err(|_| {
                e.error(
                    ContractError::OverflowError.into(),
                    "received amount is too large for an i64",
                    &[],
                )
            })?;
            Ok(transfer_classic_balance(e, acc_id, i64_amount)?)
        }
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr.metered_clone(e)?);
            let mut balance = if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                raw_balance.try_into_val(e)?
            } else {
                // balance passed the authorization check at the top of this function, so write true.
                BalanceValue {
                    amount: 0,
                    authorized: true,
                    clawback: is_asset_clawback_enabled(e)?,
                }
            };

            let new_balance = balance.amount.checked_add(amount).ok_or_else(|| {
                e.error(
                    ContractError::OverflowError.into(),
                    "balance overflow in receive_balance",
                    &[],
                )
            })?;

            balance.amount = new_balance;
            write_balance(e, addr, balance)
        }
    }
}

// TODO: Metering analysis
pub(crate) fn spend_balance_no_authorization_check(
    e: &Host,
    addr: Address,
    amount: i128,
) -> Result<(), HostError> {
    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => {
            let i64_amount = i64::try_from(amount).map_err(|_| {
                e.error(
                    ContractError::OverflowError.into(),
                    "spent amount is too large for an i64",
                    &[],
                )
            })?;
            transfer_classic_balance(e, acc_id, -i64_amount)
        }
        ScAddress::Contract(_) => {
            // If a balance exists, calculate new amount and write the existing authorized state as is because
            // this can be used to clawback when deauthorized.
            let key = DataKey::Balance(addr.metered_clone(e)?);
            if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                let mut balance: BalanceValue = raw_balance.try_into_val(e)?;
                if balance.amount < amount {
                    return Err(err!(
                        e,
                        ContractError::BalanceError,
                        "balance is not sufficient to spend",
                        balance,
                        amount
                    ));
                } else {
                    let new_balance = balance.amount.checked_sub(amount).ok_or_else(|| {
                        e.error(
                            ContractError::OverflowError.into(),
                            "balance overflow in spend_balance_no_authorization_check",
                            &[],
                        )
                    })?;
                    balance.amount = new_balance;

                    write_balance(e, addr, balance)?
                }
            } else if amount > 0 {
                return Err(err!(
                    e,
                    ContractError::BalanceError,
                    "zero balance is not sufficient to spend",
                    amount
                ));
            }
            Ok(())
        }
    }
}

// Metering: covered by components.
pub(crate) fn spend_balance(e: &Host, addr: Address, amount: i128) -> Result<(), HostError> {
    if !is_authorized(e, addr.metered_clone(e)?)? {
        return Err(e.error(
            ContractError::BalanceDeauthorizedError.into(),
            "balance is deauthorized",
            &[],
        ));
    }

    spend_balance_no_authorization_check(e, addr, amount)
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub(crate) fn is_authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => is_account_authorized(e, acc_id),
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                let balance: BalanceValue = raw_balance.try_into_val(e)?;
                Ok(balance.authorized)
            } else {
                Ok(!is_asset_auth_required(e)?)
            }
        }
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub(crate) fn write_authorization(
    e: &Host,
    addr: Address,
    authorize: bool,
) -> Result<(), HostError> {
    if !authorize && !is_asset_auth_revocable(e)? {
        return Err(e.error(
            ContractError::OperationNotSupportedError.into(),
            "issuer does not have AUTH_REVOCABLE set",
            &[],
        ));
    }

    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => set_authorization(e, acc_id, authorize),
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr.metered_clone(e)?);
            if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                let mut balance: BalanceValue = raw_balance.try_into_val(e)?;
                balance.authorized = authorize;
                write_balance(e, addr, balance)
            } else {
                // Balance does not exist, so write a 0 amount along with the authorization flag.
                // No need to check auth_required because this function can only be called by the admin.
                let balance = BalanceValue {
                    amount: 0,
                    authorized: authorize,
                    clawback: is_asset_clawback_enabled(e)?,
                };
                write_balance(e, addr, balance)
            }
        }
    }
}

// TODO: Metering analysis
pub(crate) fn check_clawbackable(e: &Host, addr: Address) -> Result<(), HostError> {
    let validate_trustline =
        |asset: TrustLineAsset, issuer: AccountId, account: AccountId| -> Result<(), HostError> {
            if issuer == account {
                return Err(e.error(
                    ContractError::OperationNotSupportedError.into(),
                    "cannot clawback from issuer",
                    &[],
                ));
            }
            let tl_flags = get_trustline_flags(e, account, asset)?;
            if tl_flags & (TrustLineFlags::TrustlineClawbackEnabledFlag as u32) == 0 {
                return Err(e.error(
                    ContractError::BalanceError.into(),
                    "trustline isn't clawbackable",
                    &[],
                ));
            }
            Ok(())
        };

    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => match read_asset_info(e)? {
            AssetInfo::Native => Err(e.error(
                ContractError::OperationNotSupportedError.into(),
                "cannot clawback native asset",
                &[],
            )),
            AssetInfo::AlphaNum4(asset) => {
                let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
                validate_trustline(
                    e.create_asset_4(
                        asset.asset_code.to_array()?,
                        issuer_account_id.metered_clone(e)?,
                    ),
                    issuer_account_id,
                    acc_id,
                )
            }
            AssetInfo::AlphaNum12(asset) => {
                let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
                validate_trustline(
                    e.create_asset_12(
                        asset.asset_code.to_array()?,
                        issuer_account_id.metered_clone(e)?,
                    ),
                    issuer_account_id,
                    acc_id,
                )
            }
        },
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                StorageUtils::try_get(e, key.try_into_val(e)?, StorageType::Persistent)?
            {
                let balance: BalanceValue = raw_balance.try_into_val(e)?;
                if !balance.clawback {
                    return Err(e.error(
                        ContractError::BalanceError.into(),
                        "balance isn't clawbackable",
                        &[],
                    ));
                }
            } else {
                // We fail even if the clawback amount is 0. This is a better alternative than
                // checking the issuer to make sure clawback is enabled and then succeeding
                // in the 0 clawback case.
                return Err(e.error(
                    ContractError::BalanceError.into(),
                    "no balance to clawback",
                    &[],
                ));
            }

            Ok(())
        }
    }
}

// Metering: covered by components
pub(crate) fn transfer_classic_balance(
    e: &Host,
    to_key: AccountId,
    amount: i64,
) -> Result<(), HostError> {
    let transfer_trustline_balance_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<(), HostError> {
            if issuer == to {
                return Ok(());
            }

            transfer_trustline_balance(e, to, asset, amount)
        };

    match read_asset_info(e)? {
        AssetInfo::Native => transfer_account_balance(e, to_key, amount)?,
        AssetInfo::AlphaNum4(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            transfer_trustline_balance_safe(
                e.create_asset_4(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )?
        }
        AssetInfo::AlphaNum12(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            transfer_trustline_balance_safe(
                e.create_asset_12(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )?
        }
    };
    Ok(())
}

// TODO: Metering analysis
//returns (total balance, spendable balance)
fn get_classic_balance(e: &Host, to_key: AccountId) -> Result<i64, HostError> {
    let get_trustline_balance_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<i64, HostError> {
            if issuer == to {
                return Ok(i64::MAX);
            }

            get_trustline_balance(e, to, asset)
        };

    match read_asset_info(e)? {
        AssetInfo::Native => get_account_balance(e, to_key),
        AssetInfo::AlphaNum4(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            get_trustline_balance_safe(
                e.create_asset_4(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )
        }

        AssetInfo::AlphaNum12(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            get_trustline_balance_safe(
                e.create_asset_12(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )
        }
    }
}

// Metering: *mostly* covered by components. The arithmetics are free.
fn transfer_account_balance(
    host: &Host,
    account_id: AccountId,
    amount: i64,
) -> Result<(), HostError> {
    let lk = host.to_account_key(account_id)?;

    host.with_mut_storage(|storage| {
        let mut le = read_account_entry(host, storage, &lk)?;

        let mut ae = match &le.data {
            LedgerEntryData::Account(ae) => Ok(ae.metered_clone(host)?),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        let (min_balance, max_balance) = get_min_max_account_balance(host, &ae)?;

        let Some(new_balance) = ae.balance.checked_add(amount) else {
            return Err(host.error(
                ContractError::BalanceError.into(),
                "resulting balance overflow",
                &[],
            ));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            ae.balance = new_balance;
            le = Host::modify_ledger_entry_data(host, &le, LedgerEntryData::Account(ae))?;
            storage.put(&lk, &le, None, host.as_budget())
        } else {
            Err(err!(
                host,
                ContractError::BalanceError,
                "resulting balance is not within the allowed range",
                min_balance,
                new_balance,
                max_balance
            ))
        }
    })
}

fn read_account_entry(
    host: &Host,
    storage: &mut Storage,
    lk: &Rc<LedgerKey>,
) -> Result<Rc<LedgerEntry>, HostError> {
    storage
        .try_get(&lk, host.as_budget())
        .map_err(|e| {
            host.decorate_account_footprint_error(
                e,
                lk,
                "trying to access account entry outside of the footprint",
            )
        })?
        .ok_or_else(|| {
            let account_address = host.account_address_from_key(lk);
            match account_address {
                Ok(account_address) => host.error(
                    ContractError::AccountMissingError.into(),
                    "account entry is missing",
                    &[account_address],
                ),
                Err(e) => e,
            }
        })
}

fn read_trustline_entry(
    host: &Host,
    storage: &mut Storage,
    lk: &Rc<LedgerKey>,
) -> Result<Rc<LedgerEntry>, HostError> {
    storage
        .try_get(&lk, host.as_budget())
        .map_err(|e| {
            host.decorate_account_footprint_error(
                e,
                lk,
                "trying to access account trustline entry outside of the footprint",
            )
        })?
        .ok_or_else(|| {
            let account_address = host.account_address_from_key(lk);
            match account_address {
                Ok(account_address) => host.error(
                    ContractError::TrustlineMissingError.into(),
                    "trustline entry is missing for account",
                    &[account_address],
                ),
                Err(e) => e,
            }
        })
}

// Metering: *mostly* covered by components. The arithmatics are free.
fn transfer_trustline_balance(
    host: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
    amount: i64,
) -> Result<(), HostError> {
    let lk = host.to_trustline_key(account_id, asset)?;
    host.with_mut_storage(|storage| {
        let mut le = read_trustline_entry(host, storage, &lk)?;

        let mut tl = match &le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl.metered_clone(host)?),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        let (min_balance, max_balance) = get_min_max_trustline_balance(host, &tl)?;

        let Some(new_balance) = tl.balance.checked_add(amount) else {
            return Err(host.error(
                ContractError::BalanceError.into(),
                "resulting balance overflow",
                &[],
            ));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            tl.balance = new_balance;
            le = Host::modify_ledger_entry_data(host, &le, LedgerEntryData::Trustline(tl))?;
            storage.put(&lk, &le, None, host.as_budget())
        } else {
            Err(err!(
                host,
                ContractError::BalanceError,
                "resulting balance is not within the allowed range",
                min_balance,
                new_balance,
                max_balance
            ))
        }
    })
}

// Metering: covered by components
//returns (total balance, spendable balance)
fn get_account_balance(host: &Host, account_id: AccountId) -> Result<i64, HostError> {
    let lk = host.to_account_key(account_id)?;

    host.with_mut_storage(|storage| {
        let le = read_account_entry(host, storage, &lk)?;

        let ae = match &le.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        let min = get_min_max_account_balance(host, ae)?.0;
        if ae.balance < min {
            return Err(host.error(
                ContractError::InternalError.into(),
                "account has balance < spendable_balance",
                &[],
            ));
        }
        Ok(ae.balance)
    })
}

// TODO: Metering analysis
fn get_min_max_account_balance(e: &Host, ae: &AccountEntry) -> Result<(i64, i64), HostError> {
    if ae.balance < 0 {
        return Err(e.error(
            ContractError::InternalError.into(),
            "initial balance is negative",
            &[],
        ));
    }

    let base_reserve = e.with_ledger_info(|li| Ok(li.base_reserve))? as i64;
    if let AccountEntryExt::V1(ext1) = &ae.ext {
        let net_entries = if let AccountEntryExtensionV1Ext::V2(ext2) = &ext1.ext {
            2i64 + (ae.num_sub_entries as i64) + (ext2.num_sponsoring as i64)
                - (ext2.num_sponsored as i64)
        } else {
            2i64 + ae.num_sub_entries as i64
        };
        let min_balance = net_entries * base_reserve + ext1.liabilities.selling;
        let max_balance = i64::MAX - ext1.liabilities.buying;
        Ok((min_balance, max_balance))
    } else {
        let net_entries = 2i64 + (ae.num_sub_entries as i64);
        let min_balance = net_entries * base_reserve;
        let max_balance = i64::MAX;
        Ok((min_balance, max_balance))
    }
}

// Metering: *mostly* covered by components. The arithmatics are free.
// returns (total balance, spendable balance)
fn get_trustline_balance(
    host: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<i64, HostError> {
    let lk = host.to_trustline_key(account_id, asset)?;
    host.with_mut_storage(|storage| {
        let le = read_trustline_entry(host, storage, &lk)?;

        let tl = match &le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl.metered_clone(host)?),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        let min = get_min_max_trustline_balance(host, &tl)?.0;
        if tl.balance < min {
            return Err(host.error(
                ContractError::InternalError.into(),
                "trustline has balance < spendable_balance",
                &[],
            ));
        }
        Ok(tl.balance)
    })
}

// TODO: Metering analysis
fn get_min_max_trustline_balance(e: &Host, tl: &TrustLineEntry) -> Result<(i64, i64), HostError> {
    if tl.balance < 0 {
        return Err(e.error(
            ContractError::InternalError.into(),
            "initial balance is negative",
            &[],
        ));
    }

    if let TrustLineEntryExt::V1(ext1) = &tl.ext {
        let min_balance = ext1.liabilities.selling;
        if tl.limit < ext1.liabilities.buying {
            return Err(e.error(
                ContractError::InternalError.into(),
                "limit is lower than liabilities",
                &[],
            ));
        }
        let max_balance = tl.limit - ext1.liabilities.buying;
        Ok((min_balance, max_balance))
    } else {
        let min_balance = 0;
        let max_balance = tl.limit;
        Ok((min_balance, max_balance))
    }
}

// TODO: Metering analysis
fn is_account_authorized(e: &Host, account_id: AccountId) -> Result<bool, HostError> {
    let is_trustline_authorized_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<bool, HostError> {
            if issuer == to {
                return Ok(true);
            }
            is_trustline_authorized(e, to, asset)
        };

    match read_asset_info(e)? {
        AssetInfo::Native => Ok(true),
        AssetInfo::AlphaNum4(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            is_trustline_authorized_safe(
                e.create_asset_4(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                account_id,
            )
        }
        AssetInfo::AlphaNum12(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            is_trustline_authorized_safe(
                e.create_asset_12(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                account_id,
            )
        }
    }
}

// TODO: Metering analysis
fn get_trustline_flags(
    host: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<u32, HostError> {
    let lk = host.to_trustline_key(account_id, asset)?;
    host.with_mut_storage(|storage| {
        let le = read_trustline_entry(host, storage, &lk)?;

        let tl = match &le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        Ok(tl.flags)
    })
}

// Metering: *mostly* covered by components. The arithmatics are free.
fn is_trustline_authorized(
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<bool, HostError> {
    let tl_flags = get_trustline_flags(e, account_id, asset)?;
    Ok(tl_flags & (TrustLineFlags::AuthorizedFlag as u32) != 0)
}

fn set_authorization(e: &Host, to_key: AccountId, authorize: bool) -> Result<(), HostError> {
    let set_trustline_authorization_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<(), HostError> {
            if issuer == to {
                return Err(e.error(
                    ContractError::OperationNotSupportedError.into(),
                    "issuer doesn't have a trustline",
                    &[],
                ));
            }

            set_trustline_authorization(e, to, asset, authorize)
        };

    match read_asset_info(e)? {
        AssetInfo::Native => Err(e.error(
            ContractError::OperationNotSupportedError.into(),
            "expected trustline asset",
            &[],
        )),
        AssetInfo::AlphaNum4(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            set_trustline_authorization_safe(
                e.create_asset_4(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )
        }
        AssetInfo::AlphaNum12(asset) => {
            let issuer_account_id = e.account_id_from_bytesobj(asset.issuer.into())?;
            set_trustline_authorization_safe(
                e.create_asset_12(
                    asset.asset_code.to_array()?,
                    issuer_account_id.metered_clone(e)?,
                ),
                issuer_account_id,
                to_key,
            )
        }
    }
}

// Metering: covered by components
fn set_trustline_authorization(
    host: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
    authorize: bool,
) -> Result<(), HostError> {
    let lk = host.to_trustline_key(account_id, asset)?;
    host.with_mut_storage(|storage| {
        let mut le = read_trustline_entry(host, storage, &lk)?;

        let mut tl = match &le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl.metered_clone(host)?),
            _ => Err(host.error(
                ContractError::InternalError.into(),
                "unexpected entry found",
                &[],
            )),
        }?;

        if authorize {
            tl.flags &= !(TrustLineFlags::AuthorizedToMaintainLiabilitiesFlag as u32);
            tl.flags |= TrustLineFlags::AuthorizedFlag as u32;
        } else {
            // Set AuthorizedToMaintainLiabilitiesFlag to indicate deauthorization so
            // offers don't need to get pulled and pool shares don't get redeemed.
            tl.flags &= !(TrustLineFlags::AuthorizedFlag as u32);
            tl.flags |= TrustLineFlags::AuthorizedToMaintainLiabilitiesFlag as u32;
        }
        le = Host::modify_ledger_entry_data(host, &le, LedgerEntryData::Trustline(tl))?;
        storage.put(&lk, &le, None, host.as_budget())
    })
}

fn is_asset_auth_required(e: &Host) -> Result<bool, HostError> {
    is_asset_issuer_flag_set(e, AccountFlags::RequiredFlag)
}

fn is_asset_clawback_enabled(e: &Host) -> Result<bool, HostError> {
    is_asset_issuer_flag_set(e, AccountFlags::ClawbackEnabledFlag)
}

fn is_asset_auth_revocable(e: &Host) -> Result<bool, HostError> {
    is_asset_issuer_flag_set(e, AccountFlags::RevocableFlag)
}

fn is_asset_issuer_flag_set(e: &Host, flag: AccountFlags) -> Result<bool, HostError> {
    match read_asset_info(e)? {
        AssetInfo::Native => Ok(false),
        AssetInfo::AlphaNum4(asset) => is_issuer_flag_set(e, asset.issuer, flag),
        AssetInfo::AlphaNum12(asset) => is_issuer_flag_set(e, asset.issuer, flag),
    }
}

fn is_issuer_flag_set(
    e: &Host,
    issuer_id: BytesN<32>,
    flag: AccountFlags,
) -> Result<bool, HostError> {
    let issuer_acc = e.load_account(e.account_id_from_bytesobj(issuer_id.into())?)?;
    Ok(issuer_acc.flags & (flag as u32) != 0)
}
