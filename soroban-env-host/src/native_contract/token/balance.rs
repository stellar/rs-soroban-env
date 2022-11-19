use crate::host::Host;
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::metadata::read_metadata;
use crate::native_contract::token::public_types::{Identifier, Metadata};
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{
    AccountEntryExt, AccountEntryExtensionV1Ext, AccountFlags, AccountId, LedgerEntryData,
    TrustLineAsset, TrustLineEntryExt, TrustLineFlags,
};
use soroban_env_common::{CheckedEnv, TryIntoVal};

use super::error::ContractError;
use super::storage_types::BalanceValue;

fn is_issuer_auth_required(e: &Host, issuer_id: AccountId) -> Result<bool, HostError> {
    let lk = e.to_account_key(issuer_id);
    e.with_mut_storage(|storage| {
        //TODO: Should we add a try_get method to storage?
        if storage.has(&lk)? {
            let le = storage.get(&lk)?;
            let auth_required = match le.data {
                LedgerEntryData::Account(ae) => {
                    (ae.flags & (AccountFlags::RequiredFlag as u32)) != 0
                }
                _ => {
                    return Err(e.err_status_msg(
                        ContractError::InternalError,
                        "non-account entry found for account key",
                    ))
                }
            };

            Ok(auth_required)
        } else {
            //issuer is missing, so authorization is not required because anyone
            //can re-create the issuer without issuer flags
            Ok(false)
        }
    })
}

fn is_asset_auth_required(e: &Host) -> Result<bool, HostError> {
    match read_metadata(e)? {
        Metadata::Token(_) => Ok(false),
        Metadata::Native => Ok(false),
        Metadata::AlphaNum4(asset) => is_issuer_auth_required(e, asset.issuer),
        Metadata::AlphaNum12(asset) => is_issuer_auth_required(e, asset.issuer),
    }
}

fn write_balance_and_auth(
    e: &Host,
    id: Identifier,
    amount: BigInt,
    authorized: bool,
) -> Result<(), HostError> {
    let key = DataKey::Balance(id);
    e.put_contract_data(
        key.try_into_val(e)?,
        BalanceValue { amount, authorized }.try_into_val(e)?,
    )?;
    Ok(())
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn read_balance(e: &Host, id: Identifier) -> Result<BigInt, HostError> {
    let key = DataKey::Balance(id);
    if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
        let balance: BalanceValue = balance.try_into_val(e)?;
        Ok(balance.amount)
    } else {
        Ok(BigInt::from_u64(e, 0)?)
    }
}

// Metering: covered by components.
pub fn receive_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), HostError> {
    let key = DataKey::Balance(id.clone());
    if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
        let balance: BalanceValue = balance.try_into_val(e)?;
        if balance.authorized {
            write_balance_and_auth(e, id, (balance.amount + amount)?, true)
        } else {
            Err(e.err_status_msg(ContractError::BalanceFrozenError, "balance is frozen"))
        }
    } else if is_asset_auth_required(e)? {
        // Balance does not exist, so now we need to enforce auth_required IF this is a wrapped token
        // with an auth_required issuer.
        Err(e.err_status_msg(
            ContractError::IssuerAuthRequiredError,
            "issuer is auth required",
        ))
    } else {
        // Write a new authorized balance
        write_balance_and_auth(e, id, amount, true)
    }
}

// Metering: covered by components.
pub fn spend_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), HostError> {
    let key = DataKey::Balance(id.clone());
    if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
        let balance: BalanceValue = balance.try_into_val(e)?;
        if balance.authorized {
            if balance.amount.compare(&amount)? == Ordering::Less {
                Err(err!(
                    e,
                    ContractError::BalanceError,
                    "balance is not sufficient to spend: {} < {}",
                    balance,
                    amount
                ))
            } else {
                // Write if balance exists, is authorized, and has sufficient funds
                write_balance_and_auth(e, id, (balance.amount - amount)?, true)
            }
        } else {
            Err(e.err_status_msg(ContractError::BalanceFrozenError, "balance is frozen"))
        }
    } else {
        Err(e.err_status_msg(ContractError::BalanceError, "balance does not exist"))
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn read_authorization(e: &Host, id: Identifier) -> Result<bool, HostError> {
    let key = DataKey::Balance(id);
    if let Ok(raw_balance) = e.get_contract_data(key.try_into_val(e)?) {
        let balance: BalanceValue = raw_balance.try_into_val(e)?;
        Ok(balance.authorized)
    } else {
        Ok(!is_asset_auth_required(e)?)
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn write_authorization(e: &Host, id: Identifier, authorized: bool) -> Result<(), HostError> {
    let key = DataKey::Balance(id.clone());
    if let Ok(raw_balance) = e.get_contract_data(key.try_into_val(e)?) {
        let balance: BalanceValue = raw_balance.try_into_val(e)?;
        write_balance_and_auth(e, id, balance.amount, authorized)
    } else {
        // Balance does not exist, so write a 0 amount along with the authorization flag.
        // No need to check auth_required because this function can only be called by the admin.
        write_balance_and_auth(e, id, BigInt::from_u64(&e, 0)?, authorized)
    }
}

// Metering: covered by components
pub fn transfer_classic_balance(e: &Host, to_key: AccountId, amount: i64) -> Result<(), HostError> {
    match read_metadata(e)? {
        Metadata::Token(_) => {
            return Err(e.err_status_msg(
                ContractError::OperationNotSupportedError,
                "smart tokens don't support conversions to/from classic",
            ))
        }
        Metadata::Native => transfer_account_balance(e, to_key, amount)?,
        Metadata::AlphaNum4(asset) => transfer_trustline_balance(
            e,
            to_key,
            e.create_asset_4(asset.asset_code.to_array()?, asset.issuer),
            amount,
        )?,
        Metadata::AlphaNum12(asset) => transfer_trustline_balance(
            e,
            to_key,
            e.create_asset_12(asset.asset_code.to_array()?, asset.issuer),
            amount,
        )?,
    };
    Ok(())
}

// Metering: *mostly* covered by components. The arithmetics are free.
fn transfer_account_balance(e: &Host, account_id: AccountId, amount: i64) -> Result<(), HostError> {
    let lk = e.to_account_key(account_id.clone());

    e.with_mut_storage(|storage| {
        let mut le = storage.get(&lk)?;
        let ae = match &mut le.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(err!(
                e,
                ContractError::AccountMissingError,
                "account '{}' doesn't exist",
                account_id
            )),
        }?;
        if ae.balance < 0 {
            return Err(
                e.err_status_msg(ContractError::InternalError, "initial balance is negative")
            );
        }

        let base_reserve = e.with_ledger_info(|li| Ok(li.base_reserve))? as i64;
        let (min_balance, max_balance) = if let AccountEntryExt::V1(ext1) = &ae.ext {
            let net_entries = if let AccountEntryExtensionV1Ext::V2(ext2) = &ext1.ext {
                2i64 + (ae.num_sub_entries as i64) + (ext2.num_sponsoring as i64)
                    - (ext2.num_sponsored as i64)
            } else {
                2i64 + ae.num_sub_entries as i64
            };
            let min_balance = net_entries * base_reserve + ext1.liabilities.selling;
            let max_balance = i64::MAX - ext1.liabilities.buying;
            (min_balance, max_balance)
        } else {
            let net_entries = 2i64 + (ae.num_sub_entries as i64);
            let min_balance = net_entries * base_reserve;
            let max_balance = i64::MAX;
            (min_balance, max_balance)
        };

        let new_balance = if amount <= 0 {
            ae.balance + amount
        } else if ae.balance <= i64::MAX - amount {
            ae.balance + amount
        } else {
            return Err(e.err_status_msg(ContractError::BalanceError, "resulting balance overflow"));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            ae.balance = new_balance;
            storage.put(&lk, &le)
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
        let mut le = storage.get(&lk)?;
        let tl = match &mut le.data {
            LedgerEntryData::Trustline(tl) => Ok(tl),
            _ => Err(e.err_status_msg(
                ContractError::AccountMissingError,
                "trustline doesn't exist",
            )),
        }?;
        if tl.balance < 0 {
            return Err(
                e.err_status_msg(ContractError::InternalError, "initial balance is negative")
            );
        }
        if tl.flags & (TrustLineFlags::AuthorizedFlag as u32) == 0 {
            return Err(e.err_status_msg(ContractError::BalanceError, "trustline isn't authorized"));
        }

        let (min_balance, max_balance) = if let TrustLineEntryExt::V1(ext1) = &tl.ext {
            let min_balance = ext1.liabilities.selling;
            if tl.limit < ext1.liabilities.buying {
                return Err(e.err_status_msg(
                    ContractError::InternalError,
                    "limit is lower than liabilities",
                ));
            }
            let max_balance = tl.limit - ext1.liabilities.buying;
            (min_balance, max_balance)
        } else {
            let min_balance = 0;
            let max_balance = tl.limit;
            (min_balance, max_balance)
        };

        let new_balance = if amount <= 0 {
            tl.balance + amount
        } else if tl.balance <= i64::MAX - amount {
            tl.balance + amount
        } else {
            return Err(e.err_status_msg(ContractError::BalanceError, "resulting balance overflow"));
        };
        if new_balance >= min_balance && new_balance <= max_balance {
            tl.balance = new_balance;
            storage.put(&lk, &le)
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
