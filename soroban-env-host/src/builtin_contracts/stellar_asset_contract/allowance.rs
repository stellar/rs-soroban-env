use crate::{
    builtin_contracts::{
        base_types::Address,
        contract_error::ContractError,
        stellar_asset_contract::storage_types::{AllowanceDataKey, DataKey},
    },
    err,
    host::{metered_clone::MeteredClone, Host},
    Env, HostError, StorageType, TryIntoVal,
};

use super::storage_types::AllowanceValue;

// Metering: covered by components
pub(crate) fn read_allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });
    if let Some(allowance) =
        e.try_get_contract_data(key.try_into_val(e)?, StorageType::Temporary)?
    {
        let val: AllowanceValue = allowance.try_into_val(e)?;
        if val.live_until_ledger < e.get_ledger_sequence()?.into() {
            Ok(0)
        } else {
            Ok(val.amount)
        }
    } else {
        Ok(0)
    }
}

// Metering: covered by components
pub(crate) fn write_allowance(
    e: &Host,
    from: Address,
    spender: Address,
    amount: i128,
    live_until: u32,
) -> Result<(), HostError> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });

    // Validates live_until and then returns the ledger seq
    // The live_until can be less than ledger seq if clearing an allowance
    let ledger_seq = e.with_ledger_info(|li| {
        if live_until > e.max_live_until_ledger()? {
            Err(err!(
                e,
                ContractError::AllowanceError,
                "live_until is greater than max",
                live_until,
                li.max_entry_ttl
            ))
        } else if amount > 0 && live_until < li.sequence_number {
            Err(err!(
                e,
                ContractError::AllowanceError,
                "live_until must be >= ledger sequence",
                live_until,
                li.sequence_number
            ))
        } else {
            Ok(li.sequence_number)
        }
    })?;

    // Returns the allowance to write and the previous live_until ledger of the existing allowance.
    // If an allowance didn't exist, then the previous live_until ledger will be None.
    let allowance_with_live_until_option: Option<(AllowanceValue, Option<u32>)> =
        if let Some(allowance) =
            e.try_get_contract_data(key.try_into_val(e)?, StorageType::Temporary)?
        {
            let mut updated_allowance: AllowanceValue = allowance.try_into_val(e)?;
            updated_allowance.amount = amount;

            let old_live_until = updated_allowance.live_until_ledger;
            updated_allowance.live_until_ledger = live_until;
            Some((updated_allowance, Some(old_live_until)))
        } else if amount > 0 {
            Some((
                AllowanceValue {
                    amount,
                    live_until_ledger: live_until,
                },
                None,
            ))
        } else {
            None
        };

    match allowance_with_live_until_option {
        Some(allowance_with_live_until) => {
            e.put_contract_data(
                key.try_into_val(e)?,
                allowance_with_live_until.0.try_into_val(e)?,
                StorageType::Temporary,
            )?;

            if allowance_with_live_until.0.amount > 0
                && allowance_with_live_until.1.unwrap_or(0) < live_until
            {
                let live_for = live_until.saturating_sub(ledger_seq).saturating_add(1);
                e.extend_contract_data_ttl(
                    key.try_into_val(e)?,
                    StorageType::Temporary,
                    live_for.into(),
                    live_for.into(),
                )?;
            }
        }
        None => {}
    }

    Ok(())
}

// allowance is expected to exist
fn write_allowance_amount(
    e: &Host,
    from: Address,
    spender: Address,
    amount: i128,
) -> Result<(), HostError> {
    let key = DataKey::Allowance(AllowanceDataKey {
        from: from.metered_clone(e)?,
        spender: spender.metered_clone(e)?,
    });

    let allowance: AllowanceValue = e
        .get_contract_data(key.try_into_val(e)?, StorageType::Temporary)?
        .try_into_val(e)?;
    write_allowance(e, from, spender, amount, allowance.live_until_ledger)
}

// Metering: covered by components
pub(crate) fn spend_allowance(
    e: &Host,
    from: Address,
    spender: Address,
    amount: i128,
) -> Result<(), HostError> {
    let allowance = read_allowance(e, from.metered_clone(e)?, spender.metered_clone(e)?)?;
    if allowance < amount {
        return Err(err!(
            e,
            ContractError::AllowanceError,
            "not enough allowance to spend",
            allowance,
            amount
        ));
    } else if amount > 0 {
        let new_allowance = allowance.checked_sub(amount).ok_or_else(|| {
            e.error(
                ContractError::OverflowError.into(),
                "allowance overflowed",
                &[],
            )
        })?;
        write_allowance_amount(e, from, spender, new_allowance)?;
    }
    Ok(())
}
