use crate::budget::AsBudget;
use crate::host::Host;
use crate::native_contract::token::metadata::read_metadata;
use crate::native_contract::token::public_types::{Identifier, Metadata};
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use soroban_env_common::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1Ext, AccountFlags, AccountId,
    LedgerEntryData, TrustLineAsset, TrustLineEntry, TrustLineEntryExt, TrustLineFlags,
};
use soroban_env_common::{CheckedEnv, TryIntoVal};

use super::error::ContractError;

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn read_balance(e: &Host, id: Identifier) -> Result<i128, HostError> {
    match id {
        Identifier::Account(acc_id) => Ok(get_classic_balance(e, acc_id)?.0.into()),
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            let key = DataKey::Balance(id);
            if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
                Ok(balance.try_into_val(e)?)
            } else {
                Ok(0)
            }
        }
    }
}

// Metering: *mostly* covered by components.
pub fn get_spendable_balance(e: &Host, id: Identifier) -> Result<i128, HostError> {
    match id {
        Identifier::Account(acc_id) => Ok(get_classic_balance(e, acc_id)?.1.into()),
        Identifier::Contract(_) | Identifier::Ed25519(_) => read_balance(e, id),
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
fn write_balance(e: &Host, id: Identifier, amount: i128) -> Result<(), HostError> {
    let key = DataKey::Balance(id);
    e.put_contract_data(key.try_into_val(e)?, amount.try_into_val(e)?)?;
    Ok(())
}

// Metering: covered by components.
pub fn receive_balance(e: &Host, id: Identifier, amount: i128) -> Result<(), HostError> {
    if !is_authorized(e, id.clone())? {
        return Err(e.err_status_msg(
            ContractError::BalanceDeauthorizedError,
            "balance is deauthorized",
        ));
    }

    match id {
        Identifier::Account(acc_id) => {
            let i64_amount = i64::try_from(amount).map_err(|_| {
                e.err_status_msg(
                    ContractError::OverflowError,
                    "received amount is too large for an i64",
                )
            })?;
            Ok(transfer_classic_balance(e, acc_id, i64_amount)?)
        }
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            let balance = read_balance(e, id.clone())?;
            let new_balance = balance
                .checked_add(amount)
                .ok_or_else(|| e.err_status(ContractError::OverflowError))?;
            write_balance(e, id, new_balance)
        }
    }
}

// TODO: Metering analysis
pub fn spend_balance_no_authorization_check(
    e: &Host,
    id: Identifier,
    amount: i128,
) -> Result<(), HostError> {
    match id {
        Identifier::Account(acc_id) => {
            let i64_amount = i64::try_from(amount).map_err(|_| {
                e.err_status_msg(
                    ContractError::OverflowError,
                    "spent amount is too large for an i64",
                )
            })?;
            transfer_classic_balance(e, acc_id, -(i64_amount as i64))
        }
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            let balance = read_balance(e, id.clone())?;
            if balance < amount {
                Err(err!(
                    e,
                    ContractError::BalanceError,
                    "balance is not sufficient to spend: {} < {}",
                    balance,
                    amount
                ))
            } else {
                let new_balance = balance
                    .checked_sub(amount)
                    .ok_or_else(|| e.err_status(ContractError::OverflowError))?;
                write_balance(e, id, new_balance)
            }
        }
    }
}

// Metering: covered by components.
pub fn spend_balance(e: &Host, id: Identifier, amount: i128) -> Result<(), HostError> {
    if !is_authorized(e, id.clone())? {
        return Err(e.err_status_msg(
            ContractError::BalanceDeauthorizedError,
            "balance is deauthorized",
        ));
    }

    spend_balance_no_authorization_check(e, id, amount)
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn is_authorized(e: &Host, id: Identifier) -> Result<bool, HostError> {
    match id {
        Identifier::Account(acc_id) => is_account_authorized(e, acc_id),
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            let key = DataKey::State(id);
            if let Ok(state) = e.get_contract_data(key.try_into_val(e)?) {
                Ok(state.try_into()?)
            } else {
                Ok(true)
            }
        }
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn write_authorization(e: &Host, id: Identifier, authorize: bool) -> Result<(), HostError> {
    match id {
        Identifier::Account(acc_id) => set_authorization(e, acc_id, authorize),
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            let key = DataKey::State(id);
            e.put_contract_data(key.try_into_val(e)?, authorize.into())?;
            Ok(())
        }
    }
}

// TODO: Metering analysis
pub fn check_clawbackable(e: &Host, id: Identifier) -> Result<(), HostError> {
    let validate_trustline =
        |asset: TrustLineAsset, issuer: AccountId, account: AccountId| -> Result<(), HostError> {
            if issuer == account {
                return Err(e.err_status_msg(
                    ContractError::OperationNotSupportedError,
                    "cannot clawback from issuer",
                ));
            }
            let tl_flags = get_trustline_flags(e, account, asset)?;
            if tl_flags & (TrustLineFlags::TrustlineClawbackEnabledFlag as u32) == 0 {
                return Err(
                    e.err_status_msg(ContractError::BalanceError, "trustline isn't clawbackable")
                );
            }
            Ok(())
        };

    match id {
        Identifier::Account(acc_id) => match read_metadata(e)? {
            Metadata::Native => {
                return Err(e.err_status_msg(
                    ContractError::OperationNotSupportedError,
                    "cannot clawback native asset",
                ))
            }
            Metadata::AlphaNum4(asset) => validate_trustline(
                e.create_asset_4(asset.asset_code.to_array()?, asset.issuer.clone()),
                asset.issuer,
                acc_id,
            ),
            Metadata::AlphaNum12(asset) => validate_trustline(
                e.create_asset_12(asset.asset_code.to_array()?, asset.issuer.clone()),
                asset.issuer,
                acc_id,
            ),
        },
        Identifier::Contract(_) | Identifier::Ed25519(_) => {
            // TODO: Non-account balances are always clawbackable for now if admin is set. Revisit this.
            Ok(())
        }
    }
}

// Metering: covered by components
pub fn transfer_classic_balance(e: &Host, to_key: AccountId, amount: i64) -> Result<(), HostError> {
    let transfer_trustline_balance_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<(), HostError> {
            if issuer == to {
                return Ok(());
            }

            transfer_trustline_balance(e, to, asset, amount)
        };

    match read_metadata(e)? {
        Metadata::Native => transfer_account_balance(e, to_key, amount)?,
        Metadata::AlphaNum4(asset) => transfer_trustline_balance_safe(
            e.create_asset_4(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        )?,
        Metadata::AlphaNum12(asset) => transfer_trustline_balance_safe(
            e.create_asset_12(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        )?,
    };
    Ok(())
}

// TODO: Metering analysis
//returns (total balance, spendable balance)
fn get_classic_balance(e: &Host, to_key: AccountId) -> Result<(i64, i64), HostError> {
    let get_trustline_balance_safe = |asset: TrustLineAsset,
                                      issuer: AccountId,
                                      to: AccountId|
     -> Result<(i64, i64), HostError> {
        if issuer == to {
            return Ok((i64::MAX, i64::MAX));
        }

        get_trustline_balance(e, to, asset)
    };

    match read_metadata(e)? {
        Metadata::Native => get_account_balance(e, to_key),
        Metadata::AlphaNum4(asset) => get_trustline_balance_safe(
            e.create_asset_4(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
        Metadata::AlphaNum12(asset) => get_trustline_balance_safe(
            e.create_asset_12(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
    }
}

// Metering: *mostly* covered by components. The arithmetics are free.
fn transfer_account_balance(e: &Host, account_id: AccountId, amount: i64) -> Result<(), HostError> {
    let lk = e.to_account_key(account_id.clone());

    e.with_mut_storage(|storage| {
        let mut le = storage
            .get(&lk, e.as_budget())
            .map_err(|_| e.err_status_msg(ContractError::AccountMissingError, "account missing"))?;

        let ae = match &mut le.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
        }?;

        let (min_balance, max_balance) = get_min_max_account_balance(e, ae)?;

        let new_balance = if amount <= 0 {
            ae.balance + amount
        } else if ae.balance <= i64::MAX - amount {
            ae.balance + amount
        } else {
            return Err(e.err_status_msg(ContractError::BalanceError, "resulting balance overflow"));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            ae.balance = new_balance;
            storage.put(&lk, &le, e.as_budget())
        } else {
            Err(err!(
                e,
                ContractError::BalanceError,
                "resulting balance is not within the allowed range: {} < {} < {} does not hold",
                min_balance,
                new_balance,
                max_balance
            ))
        }
    })
}

// Metering: *mostly* covered by components. The arithmatics are free.
fn transfer_trustline_balance(
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
    amount: i64,
) -> Result<(), HostError> {
    let lk = e.to_trustline_key(account_id, asset);
    e.with_mut_storage(|storage| {
        let mut le = storage.get(&lk, e.as_budget()).map_err(|_| {
            e.err_status_msg(ContractError::TrustlineMissingError, "trustline missing")
        })?;

        let tl = match &mut le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
        }?;

        let (min_balance, max_balance) = get_min_max_trustline_balance(e, &tl)?;

        let new_balance = if amount <= 0 {
            tl.balance + amount
        } else if tl.balance <= i64::MAX - amount {
            tl.balance + amount
        } else {
            return Err(e.err_status_msg(ContractError::BalanceError, "resulting balance overflow"));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            tl.balance = new_balance;
            storage.put(&lk, &le, e.as_budget())
        } else {
            Err(err!(
                e,
                ContractError::BalanceError,
                "resulting balance is not within the allowed range: {} < {} < {} does not hold",
                min_balance,
                new_balance,
                max_balance
            ))
        }
    })
}

// TODO: Metering analysis
//returns (total balance, spendable balance)
fn get_account_balance(e: &Host, account_id: AccountId) -> Result<(i64, i64), HostError> {
    let lk = e.to_account_key(account_id.clone());

    e.with_mut_storage(|storage| {
        let le = storage
            .get(&lk, e.as_budget())
            .map_err(|_| e.err_status_msg(ContractError::AccountMissingError, "account missing"))?;

        let ae = match le.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
        }?;

        let min = get_min_max_account_balance(e, &ae)?.0;
        if ae.balance < min {
            return Err(e.err_status_msg(
                ContractError::InternalError,
                "account has balance < spendable_balance",
            ));
        }
        Ok((ae.balance, ae.balance - min))
    })
}

// TODO: Metering analysis
fn get_min_max_account_balance(e: &Host, ae: &AccountEntry) -> Result<(i64, i64), HostError> {
    if ae.balance < 0 {
        return Err(e.err_status_msg(ContractError::InternalError, "initial balance is negative"));
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
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<(i64, i64), HostError> {
    let lk = e.to_trustline_key(account_id, asset);
    e.with_mut_storage(|storage| {
        let mut le = storage.get(&lk, e.as_budget()).map_err(|_| {
            e.err_status_msg(ContractError::TrustlineMissingError, "trustline missing")
        })?;

        let tl = match &mut le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
        }?;

        let min = get_min_max_trustline_balance(e, tl)?.0;
        if tl.balance < min {
            return Err(e.err_status_msg(
                ContractError::InternalError,
                "trustline has balance < spendable_balance",
            ));
        }
        Ok((tl.balance, tl.balance - min))
    })
}

// TODO: Metering analysis
fn get_min_max_trustline_balance(e: &Host, tl: &TrustLineEntry) -> Result<(i64, i64), HostError> {
    if tl.balance < 0 {
        return Err(e.err_status_msg(ContractError::InternalError, "initial balance is negative"));
    }

    if let TrustLineEntryExt::V1(ext1) = &tl.ext {
        let min_balance = ext1.liabilities.selling;
        if tl.limit < ext1.liabilities.buying {
            return Err(e.err_status_msg(
                ContractError::InternalError,
                "limit is lower than liabilities",
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
fn is_account_authorized(e: &Host, to_key: AccountId) -> Result<bool, HostError> {
    let is_trustline_authorized_safe =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<bool, HostError> {
            if issuer == to {
                return Ok(true);
            }
            is_trustline_authorized(e, to, asset)
        };

    match read_metadata(e)? {
        Metadata::Native => Ok(true),
        Metadata::AlphaNum4(asset) => is_trustline_authorized_safe(
            e.create_asset_4(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
        Metadata::AlphaNum12(asset) => is_trustline_authorized_safe(
            e.create_asset_12(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
    }
}

// TODO: Metering analysis
fn get_trustline_flags(
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<u32, HostError> {
    let lk = e.to_trustline_key(account_id, asset);
    e.with_mut_storage(|storage| {
        let le = storage.get(&lk, e.as_budget()).map_err(|_| {
            e.err_status_msg(ContractError::TrustlineMissingError, "trustline missing")
        })?;

        let tl = match le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
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
                return Err(e.err_status_msg(
                    ContractError::OperationNotSupportedError,
                    "issuer doesn't have a trustline",
                ));
            }

            let issuer_acc = e.load_account(issuer.clone())?;

            if !authorize && (issuer_acc.flags & (AccountFlags::RevocableFlag as u32) == 0) {
                return Err(e.err_status_msg(
                    ContractError::OperationNotSupportedError,
                    "issuer does not have AUTH_REVOCABLE set",
                ));
            }

            set_trustline_authorization(e, to, asset, authorize)
        };

    match read_metadata(e)? {
        Metadata::Native => {
            return Err(e.err_status_msg(
                ContractError::OperationNotSupportedError,
                "expected trustline asset",
            ))
        }
        Metadata::AlphaNum4(asset) => set_trustline_authorization_safe(
            e.create_asset_4(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
        Metadata::AlphaNum12(asset) => set_trustline_authorization_safe(
            e.create_asset_12(asset.asset_code.to_array()?, asset.issuer.clone()),
            asset.issuer,
            to_key,
        ),
    }
}

// TODO: Metering analysis
fn set_trustline_authorization(
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
    authorize: bool,
) -> Result<(), HostError> {
    let lk = e.to_trustline_key(account_id, asset);
    e.with_mut_storage(|storage| {
        let mut le = storage.get(&lk, e.as_budget()).map_err(|_| {
            e.err_status_msg(ContractError::TrustlineMissingError, "trustline missing")
        })?;

        let tl = match &mut le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(e.err_status_msg(ContractError::InternalError, "unexpected entry found")),
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
        storage.put(&lk, &le, e.as_budget())
    })
}
