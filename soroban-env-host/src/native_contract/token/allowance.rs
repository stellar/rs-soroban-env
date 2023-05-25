use crate::host::Host;
use crate::native_contract::base_types::Address;
use crate::native_contract::contract_error::ContractError;
use crate::native_contract::token::storage_types::{AllowanceDataKey, DataKey};
use crate::{err, HostError};
use soroban_env_common::{Env, StorageType, TryIntoVal};

// Metering: covered by components
pub fn read_allowance(e: &Host, from: Address, spender: Address) -> Result<i128, HostError> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });
    if let Ok(allowance) = e.get_contract_data(key.try_into_val(e)?, StorageType::RECREATABLE) {
        Ok(allowance.try_into_val(e)?)
    } else {
        Ok(0)
    }
}

// Metering: covered by components
pub fn write_allowance(
    e: &Host,
    from: Address,
    spender: Address,
    amount: i128,
) -> Result<(), HostError> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });
    e.put_contract_data(
        key.try_into_val(e)?,
        amount.try_into_val(e)?,
        StorageType::RECREATABLE,
        ().into(),
    )?;
    Ok(())
}

// Metering: covered by components
pub fn spend_allowance(
    e: &Host,
    from: Address,
    spender: Address,
    amount: i128,
) -> Result<(), HostError> {
    let allowance = read_allowance(e, from.clone(), spender.clone())?;
    if allowance < amount {
        Err(err!(
            e,
            ContractError::AllowanceError,
            "not enough allowance to spend: {} < {}",
            allowance,
            amount
        ))
    } else {
        let new_allowance = allowance.checked_sub(amount).ok_or_else(|| {
            e.error(
                ContractError::OverflowError.into(),
                "allowance overflowed",
                &[],
            )
        })?;
        write_allowance(e, from, spender, new_allowance)
    }
}
