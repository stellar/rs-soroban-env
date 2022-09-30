use crate::host::metered_clone::MeteredClone;
use crate::host::Host;
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::Identifier;
use crate::native_contract::token::storage_types::{AllowanceDataKey, DataKey};
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, TryIntoVal};

// Metering: covered by components
pub fn read_allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });
    if let Ok(allowance) = e.get_contract_data(key.try_into_val(e)?) {
        Ok(allowance.try_into_val(e)?)
    } else {
        Ok(BigInt::from_u64(e, 0)?)
    }
}

// Metering: covered by components
pub fn write_allowance(
    e: &Host,
    from: Identifier,
    spender: Identifier,
    amount: BigInt,
) -> Result<(), Error> {
    let key = DataKey::Allowance(AllowanceDataKey { from, spender });
    e.put_contract_data(key.try_into_val(e)?, amount.try_into_val(e)?)?;
    Ok(())
}

// Metering: covered by components
pub fn spend_allowance(
    e: &Host,
    from: Identifier,
    spender: Identifier,
    amount: BigInt,
) -> Result<(), Error> {
    let allowance = read_allowance(
        e,
        from.metered_clone(&e.0.budget)?,
        spender.metered_clone(&e.0.budget)?,
    )?;
    if allowance.compare(&amount)? == Ordering::Less {
        Err(Error::ContractError)
    } else {
        write_allowance(e, from, spender, (allowance - amount)?)
    }
}
