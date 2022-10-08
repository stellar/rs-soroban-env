use crate::host::Host;
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::metadata::read_metadata;
use crate::native_contract::token::public_types::{Identifier, Metadata};
use crate::native_contract::token::storage_types::DataKey;
use crate::{err, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{
    AccountEntryExt, AccountEntryExtensionV1Ext, AccountId, LedgerEntryData, TrustLineAsset,
    TrustLineEntryExt, TrustLineFlags,
};
use soroban_env_common::{CheckedEnv, TryIntoVal};

use super::error::ContractError;

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn read_balance(e: &Host, id: Identifier) -> Result<BigInt, HostError> {
    let key = DataKey::Balance(id);
    if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(balance.try_into_val(e)?)
    } else {
        Ok(BigInt::from_u64(e, 0)?)
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
fn write_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), HostError> {
    let key = DataKey::Balance(id);
    e.put_contract_data(key.try_into_val(e)?, amount.try_into_val(e)?)?;
    Ok(())
}

// Metering: covered by components.
pub fn receive_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), HostError> {
    let balance = read_balance(e, id.clone())?;
    let is_frozen = read_state(e, id.clone())?;
    if is_frozen {
        Err(e.err_status_msg(ContractError::BalanceFrozenError, "balance is frozen"))
    } else {
        write_balance(e, id, (balance + amount)?)
    }
}

// Metering: covered by components.
pub fn spend_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), HostError> {
    let balance = read_balance(e, id.clone())?;
    let is_frozen = read_state(e, id.clone())?;
    if is_frozen {
        Err(e.err_status_msg(ContractError::BalanceFrozenError, "balance is frozen"))
    } else if balance.compare(&amount)? == Ordering::Less {
        Err(err!(
            e,
            ContractError::BalanceError,
            "balance is not sufficient to spend: {} < {}",
            balance,
            amount
        ))
    } else {
        write_balance(e, id, (balance - amount)?)
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn read_state(e: &Host, id: Identifier) -> Result<bool, HostError> {
    let key = DataKey::State(id);
    if let Ok(state) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(state.try_into()?)
    } else {
        Ok(false)
    }
}

// Metering: *mostly* covered by components. Not sure about `try_into_val`.
pub fn write_state(e: &Host, id: Identifier, is_frozen: bool) -> Result<(), HostError> {
    let key = DataKey::State(id);
    e.put_contract_data(key.try_into_val(e)?, is_frozen.into())?;
    Ok(())
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
