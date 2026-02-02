use std::rc::Rc;

use crate::{
    builtin_contracts::{
        base_types::{Address, BytesN},
        contract_error::ContractError,
        stellar_asset_contract::{
            asset_info::{read_asset, read_asset_info},
            public_types::AssetInfo,
            storage_types::DataKey,
        },
    },
    err,
    host::metered_clone::{MeteredAlloc, MeteredClone},
    storage::Storage,
    xdr::{
        AccountEntry, AccountEntryExt, AccountEntryExtensionV1Ext, AccountFlags, AccountId, Asset,
        LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey, ScAddress, ScErrorCode,
        ScErrorType, SequenceNumber, Thresholds, TrustLineAsset, TrustLineEntry, TrustLineEntryExt,
        TrustLineFlags,
    },
    Env, ErrorHandler, Host, HostError, StorageType, TryIntoVal, Val,
};

use super::storage_types::{BalanceValue, BALANCE_EXTEND_AMOUNT, BALANCE_TTL_THRESHOLD};

/// Maximum number of sub-entries an account can have.
/// This is a constant defined in Stellar Core. As there are no plans for
/// changing it, it should be safe to just re-define it here instead of doing
/// plumbing to get it from Core.
const MAX_ACCOUNT_SUBENTRIES: u32 = 1000;

/// This module handles all balance and authorization related logic for both
/// Accounts and non-Accounts. For Accounts, a trustline is expected (unless this
/// contract is for the native asset) and trustline semantics will be followed,
/// while non-Accounts will use ContractData.
///
/// Even though non-account balances don't use trustlines, some issuer/trustline
/// semantics have been implemented for these balances. If the asset issuer has
/// the AUTH_REQUIRED flag set, then the non-account identifier must first be authorized
/// by the issuer/admin before it's allowed to hold a balance.
//
// Metering: covered by components.
pub(crate) fn read_balance(e: &Host, addr: Address) -> Result<i128, HostError> {
    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => {
            Ok(get_classic_balance(e, acc_id, addr.as_object().to_val())?.into())
        }
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
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
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
    }
}

// Metering: covered by components.
fn write_contract_balance(
    e: &Host,
    addr: Address,
    balance: BalanceValue,
    // We take an unused reference to a "witness" contract-id Hash here, to help
    // ensure this function is only called from a context where `addr` has been
    // matched as an ScAddress::Contract(hash) rather than ScAddress::Account(_)
    _witness_addr_contract_id: &crate::xdr::ContractId,
) -> Result<(), HostError> {
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
            Ok(transfer_classic_balance(
                e,
                acc_id,
                i64_amount,
                addr.as_object().to_val(),
            )?)
        }
        ScAddress::Contract(id) => {
            let key = DataKey::Balance(addr.metered_clone(e)?);
            let mut balance = if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
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
            write_contract_balance(e, addr, balance, &id)
        }
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
    }
}

// Metering: covered by components.
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
            transfer_classic_balance(e, acc_id, -i64_amount, addr.as_object().to_val())
        }
        ScAddress::Contract(id) => {
            // If a balance exists, calculate new amount and write the existing authorized state as is because
            // this can be used to clawback when deauthorized.
            let key = DataKey::Balance(addr.metered_clone(e)?);
            if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
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

                    write_contract_balance(e, addr, balance, &id)?
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
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
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

// Metering: covered by components.
pub(crate) fn is_authorized(e: &Host, addr: Address) -> Result<bool, HostError> {
    match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => is_account_authorized(e, acc_id),
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
            {
                let balance: BalanceValue = raw_balance.try_into_val(e)?;
                Ok(balance.authorized)
            } else {
                Ok(!is_asset_auth_required(e)?)
            }
        }
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
    }
}

// Metering: covered by components.
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
        ScAddress::Contract(id) => {
            let key = DataKey::Balance(addr.metered_clone(e)?);
            if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
            {
                let mut balance: BalanceValue = raw_balance.try_into_val(e)?;
                balance.authorized = authorize;
                write_contract_balance(e, addr, balance, &id)
            } else {
                // Balance does not exist, so write a 0 amount along with the authorization flag.
                // No need to check auth_required because this function can only be called by the admin.
                let balance = BalanceValue {
                    amount: 0,
                    authorized: authorize,
                    clawback: is_asset_clawback_enabled(e)?,
                };
                write_contract_balance(e, addr, balance, &id)
            }
        }
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
    }
}

// Metering: covered by components.
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
        ScAddress::Account(acc_id) => match read_asset(e)? {
            Asset::Native => Err(e.error(
                ContractError::OperationNotSupportedError.into(),
                "cannot clawback native asset",
                &[],
            )),
            Asset::CreditAlphanum4(asset) => {
                let issuer = asset.issuer.metered_clone(e)?;
                let tlasset = TrustLineAsset::CreditAlphanum4(asset);
                validate_trustline(tlasset, issuer, acc_id)
            }
            Asset::CreditAlphanum12(asset) => {
                let issuer = asset.issuer.metered_clone(e)?;
                let tlasset = TrustLineAsset::CreditAlphanum12(asset);
                validate_trustline(tlasset, issuer, acc_id)
            }
        },
        ScAddress::Contract(_) => {
            let key = DataKey::Balance(addr);
            if let Some(raw_balance) =
                e.try_get_contract_data(key.try_into_val(e)?, StorageType::Persistent)?
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
        _ => Err(e.err(
            ScErrorType::Object,
            ScErrorCode::InternalError,
            "Unexpected ScAddress type",
            &[addr.as_object().into()],
        )),
    }
}

// Metering: covered by components
pub(crate) fn transfer_classic_balance(
    e: &Host,
    to_key: AccountId,
    amount: i64,
    addr_val: Val,
) -> Result<(), HostError> {
    let transfer_trustline_balance_unless_issuer =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<(), HostError> {
            if issuer == to {
                return Ok(());
            }

            transfer_trustline_balance(e, to, asset, amount)
        };

    match read_asset(e)? {
        Asset::Native => transfer_account_balance(e, to_key, amount, addr_val),
        Asset::CreditAlphanum4(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum4(asset);
            transfer_trustline_balance_unless_issuer(tlasset, issuer, to_key)
        }
        Asset::CreditAlphanum12(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum12(asset);
            transfer_trustline_balance_unless_issuer(tlasset, issuer, to_key)
        }
    }
}

// Metering: covered by components.
fn get_classic_balance(e: &Host, acct: AccountId, addr_val: Val) -> Result<i64, HostError> {
    let get_trustline_balance_or_max_if_issuer =
        |asset: TrustLineAsset, issuer: AccountId, acct: AccountId| -> Result<i64, HostError> {
            if issuer == acct {
                return Ok(i64::MAX);
            }

            get_trustline_balance(e, acct, asset)
        };

    match read_asset(e)? {
        Asset::Native => get_account_balance(e, acct, addr_val),
        Asset::CreditAlphanum4(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum4(asset);
            get_trustline_balance_or_max_if_issuer(tlasset, issuer, acct)
        }
        Asset::CreditAlphanum12(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum12(asset);
            get_trustline_balance_or_max_if_issuer(tlasset, issuer, acct)
        }
    }
}

// Metering: covered by components.
fn transfer_account_balance(
    host: &Host,
    account_id: AccountId,
    amount: i64,
    addr_val: Val,
) -> Result<(), HostError> {
    let lk = host.to_account_key(account_id.metered_clone(host)?)?;

    host.with_mut_storage(|storage| {
        // Try to get the account entry - it may not exist for receives
        let existing_entry = storage.try_get(&lk, host, None)?;

        match existing_entry {
            Some(le) => {
                // Account exists - update the balance
        let mut ae = match &le.data {
            LedgerEntryData::Account(ae) => Ok(ae.metered_clone(host)?),
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
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
                    let updated_le =
                        Host::modify_ledger_entry_data(host, &le, LedgerEntryData::Account(ae))?;
                    storage.put(&lk, &updated_le, None, host, None)
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
            }
            None => {
                // Account doesn't exist - create it if this is a positive transfer
                if amount <= 0 {
                    // Cannot create account with non-positive amount (or spend from non-existent)
                    return Err(host.error(
                        ContractError::AccountMissingError.into(),
                        "account entry is missing",
                        &[addr_val],
                    ));
                }

                // Check minimum balance for new account: 2 * base_reserve
                let (base_reserve, ledger_seq) =
                    host.with_ledger_info(|li| Ok((li.base_reserve, li.sequence_number)))?;
                let min_new_account_balance = 2i64 * (base_reserve as i64);

                if amount < min_new_account_balance {
                    return Err(err!(
                        host,
                        ContractError::InsufficientAccountReserve,
                        "transfer amount is below minimum balance for new account",
                        amount,
                        min_new_account_balance
                    ));
                }

                // Calculate starting sequence number: (ledger_seq << 32)
                // This matches the Stellar Core behavior in getStartingSequenceNumber
                if ledger_seq > i32::MAX as u32 {
                    return Err(host.err(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                        "ledger sequence number overflow in starting sequence calculation",
                        &[],
                    ));
                }
                let starting_seq_num = SequenceNumber((ledger_seq as i64) << 32);

                // Create a new account entry with the transfer amount as balance
                let new_account = AccountEntry {
                    account_id,
                    balance: amount,
                    seq_num: starting_seq_num,
                    num_sub_entries: 0,
                    inflation_dest: None,
                    flags: 0,
                    home_domain: Default::default(),
                    thresholds: Thresholds([1, 0, 0, 0]), // Master weight = 1, thresholds = 0
                    signers: Default::default(),
                    ext: AccountEntryExt::V0,
                };

                let new_le = Rc::metered_new(
                    LedgerEntry {
                        last_modified_ledger_seq: 0,
                        data: LedgerEntryData::Account(new_account),
                        ext: LedgerEntryExt::V0,
                    },
                    host,
                )?;

                storage.put(&lk, &new_le, None, host, None)
            }
        }
    })
}

fn read_account_entry(
    host: &Host,
    storage: &mut Storage,
    lk: &Rc<LedgerKey>,
    addr_val: Val,
) -> Result<Rc<LedgerEntry>, HostError> {
    storage.try_get(&lk, &host, None)?.ok_or_else(|| {
        host.error(
                ContractError::AccountMissingError.into(),
                "account entry is missing",
            &[addr_val],
        )
    })
}

fn read_trustline_entry(
    host: &Host,
    storage: &mut Storage,
    lk: &Rc<LedgerKey>,
) -> Result<Rc<LedgerEntry>, HostError> {
    storage.try_get(&lk, &host, None)?.ok_or_else(|| {
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

// Metering: covered by components.
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
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
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
            storage.put(&lk, &le, None, &host, None)
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
fn get_account_balance(
    host: &Host,
    account_id: AccountId,
    addr_val: Val,
) -> Result<i64, HostError> {
    let lk = host.to_account_key(account_id)?;

    host.with_mut_storage(|storage| {
        let le = read_account_entry(host, storage, &lk, addr_val)?;

        let ae = match &le.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected entry found",
                &[],
            )),
        }?;

        let (min, max) = get_min_max_account_balance(host, ae)?;
        if ae.balance < min {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "account has balance < minimum",
                &[],
            ));
        }
        if ae.balance > max {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "account has balance > maximum",
                &[],
            ));
        }
        Ok(ae.balance)
    })
}

// Metering: covered by components.
fn get_min_max_account_balance(e: &Host, ae: &AccountEntry) -> Result<(i64, i64), HostError> {
    if ae.balance < 0 {
        return Err(e.err(
            ScErrorType::Storage,
            ScErrorCode::InternalError,
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

// Metering: covered by components.
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
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected entry found",
                &[],
            )),
        }?;

        let (min, max) = get_min_max_trustline_balance(host, &tl)?;
        if tl.balance < min {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trustline has balance < minimum",
                &[],
            ));
        }
        if tl.balance > max {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trustline has balance > maximum",
                &[],
            ));
        }
        Ok(tl.balance)
    })
}

// Metering: covered by components.
fn get_min_max_trustline_balance(e: &Host, tl: &TrustLineEntry) -> Result<(i64, i64), HostError> {
    if tl.balance < 0 {
        return Err(e.err(
            ScErrorType::Storage,
            ScErrorCode::InternalError,
            "initial balance is negative",
            &[],
        ));
    }

    if let TrustLineEntryExt::V1(ext1) = &tl.ext {
        let min_balance = ext1.liabilities.selling;
        if tl.limit < ext1.liabilities.buying {
            return Err(e.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
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

// Metering: covered by components.
fn is_account_authorized(e: &Host, account_id: AccountId) -> Result<bool, HostError> {
    let is_trustline_authorized_or_issuer =
        |asset: TrustLineAsset, issuer: AccountId, to: AccountId| -> Result<bool, HostError> {
            if issuer == to {
                return Ok(true);
            }
            is_trustline_authorized(e, to, asset)
        };

    match read_asset(e)? {
        Asset::Native => Ok(true),
        Asset::CreditAlphanum4(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum4(asset);
            is_trustline_authorized_or_issuer(tlasset, issuer, account_id)
        }
        Asset::CreditAlphanum12(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum12(asset);
            is_trustline_authorized_or_issuer(tlasset, issuer, account_id)
        }
    }
}

// Metering: covered by components.
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
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected entry found",
                &[],
            )),
        }?;

        Ok(tl.flags)
    })
}

// Metering: covered by components.
fn is_trustline_authorized(
    e: &Host,
    account_id: AccountId,
    asset: TrustLineAsset,
) -> Result<bool, HostError> {
    let tl_flags = get_trustline_flags(e, account_id, asset)?;
    Ok(tl_flags & (TrustLineFlags::AuthorizedFlag as u32) != 0)
}

fn set_authorization(e: &Host, acct: AccountId, authorize: bool) -> Result<(), HostError> {
    let set_trustline_authorization_unless_issuer =
        |asset: TrustLineAsset, issuer: AccountId, acct: AccountId| -> Result<(), HostError> {
            if issuer == acct {
                return Err(e.error(
                    ContractError::OperationNotSupportedError.into(),
                    "issuer doesn't have a trustline",
                    &[],
                ));
            }

            set_trustline_authorization(e, acct, asset, authorize)
        };

    match read_asset(e)? {
        Asset::Native => Err(e.error(
            ContractError::OperationNotSupportedError.into(),
            "expected trustline asset",
            &[],
        )),
        Asset::CreditAlphanum4(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum4(asset);
            set_trustline_authorization_unless_issuer(tlasset, issuer, acct)
        }
        Asset::CreditAlphanum12(asset) => {
            let issuer = asset.issuer.metered_clone(e)?;
            let tlasset = TrustLineAsset::CreditAlphanum12(asset);
            set_trustline_authorization_unless_issuer(tlasset, issuer, acct)
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
            _ => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected entry found",
                &[],
            )),
        }?;

        let is_authorized = tl.flags & (TrustLineFlags::AuthorizedFlag as u32) != 0;
        if is_authorized == authorize {
            return Ok(());
        }

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
        storage.put(&lk, &le, None, &host, None)
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

/// Creates an unlimited trustline for the given address if it doesn't
/// already exist.
/// This is a no-op for contract addresses or if a trustline already exists.
/// If a trustline is created, authorization from the address is required.
// Metering: covered by components.
pub(crate) fn create_trustline_if_needed(e: &Host, addr: Address) -> Result<(), HostError> {
    let account_id = match addr.to_sc_address()? {
        ScAddress::Account(acc_id) => acc_id,
        ScAddress::Contract(_) => return Ok(()),
        _ => {
            return Err(e.err(
                ScErrorType::Object,
                ScErrorCode::InternalError,
                "Unexpected ScAddress type",
                &[addr.as_object().into()],
            ))
        }
    };

    // Get the asset info - must be a credit asset (non-native)
    let (asset, issuer_account_id) = match read_asset(e)? {
        Asset::Native => {
            return Err(e.error(
                ContractError::OperationNotSupportedError.into(),
                "trust operation is not supported for native asset",
                &[],
            ))
        }
        Asset::CreditAlphanum4(asset4) => {
            let issuer = asset4.issuer.metered_clone(e)?;
            (TrustLineAsset::CreditAlphanum4(asset4), issuer)
        }
        Asset::CreditAlphanum12(asset12) => {
            let issuer = asset12.issuer.metered_clone(e)?;
            (TrustLineAsset::CreditAlphanum12(asset12), issuer)
        }
    };

    // Check that the address is not the issuer
    if account_id == issuer_account_id {
        return Err(e.error(
            ContractError::OperationNotSupportedError.into(),
            "cannot create trustline for issuer",
            &[],
        ));
    }

    let tl_key = e.to_trustline_key(account_id.metered_clone(e)?, asset.metered_clone(e)?)?;

    // Check if trustline already exists - if so, no-op
    let trustline_exists = e.with_mut_storage(|storage| storage.try_get(&tl_key, e, None))?;
    if trustline_exists.is_some() {
        return Ok(());
    }

    // Trustline doesn't exist - require authorization before creating it
    addr.require_auth()?;

    // Load issuer account once to determine trustline flags
    let issuer_acc = e.load_account(issuer_account_id)?;
    let auth_required = issuer_acc.flags & (AccountFlags::RequiredFlag as u32) != 0;
    let clawback_enabled = issuer_acc.flags & (AccountFlags::ClawbackEnabledFlag as u32) != 0;

    let mut tl_flags = 0u32;
    if !auth_required {
        tl_flags |= TrustLineFlags::AuthorizedFlag as u32;
    }
    if clawback_enabled {
        tl_flags |= TrustLineFlags::TrustlineClawbackEnabledFlag as u32;
    }
    let acc_key = e.to_account_key(account_id.metered_clone(e)?)?;
    // Load the account to check sub-entry limit and reserve, then create trustline
    e.with_mut_storage(|storage| {
        let acc_entry = storage.try_get(&acc_key, e, None)?.ok_or_else(|| {
            e.error(
                ContractError::AccountMissingError.into(),
                "account entry is missing",
                &[addr.as_object().to_val()],
            )
        })?;

        let mut ae = match &acc_entry.data {
            LedgerEntryData::Account(ae) => ae.metered_clone(e)?,
            _ => {
                return Err(e.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "unexpected entry found for account",
                    &[],
                ))
            }
        };

        // Check sub-entry limit (1000 max)
        if ae.num_sub_entries >= MAX_ACCOUNT_SUBENTRIES {
            return Err(e.error(
                ContractError::TooManyAccountSubentries.into(),
                "account has reached maximum number of sub-entries",
                &[],
            ));
        }

        // Check reserve requirement: adding a trustline requires one additional base_reserve
        let base_reserve = e.with_ledger_info(|li| Ok(li.base_reserve))? as i64;
        let (current_min_balance, _) = get_min_max_account_balance(e, &ae)?;
        if ae.balance < current_min_balance + base_reserve {
            return Err(e.error(
                ContractError::InsufficientAccountReserve.into(),
                "account has insufficient reserve for new trustline",
                &[],
            ));
        }

        // Create the trustline entry (move account_id and asset to avoid extra clone)
        let trustline_entry = TrustLineEntry {
            account_id,
            asset,
            balance: 0,
            limit: i64::MAX,
            flags: tl_flags,
            ext: TrustLineEntryExt::V0,
        };

        let tl_ledger_entry = Rc::metered_new(
            LedgerEntry {
                last_modified_ledger_seq: 0,
                data: LedgerEntryData::Trustline(trustline_entry),
                ext: LedgerEntryExt::V0,
            },
            e,
        )?;

        // Increment account's num_sub_entries
        ae.num_sub_entries += 1;
        let updated_acc_entry =
            Host::modify_ledger_entry_data(e, &acc_entry, LedgerEntryData::Account(ae))?;

        // Write both entries
        storage.put(&tl_key, &tl_ledger_entry, None, e, None)?;
        storage.put(&acc_key, &updated_acc_entry, None, e, None)?;

        Ok(())
    })
}
