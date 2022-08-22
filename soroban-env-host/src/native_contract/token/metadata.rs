use crate::host::Host;
use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn read_decimal(e: &Host) -> Result<u32, Error> {
    let key = DataKey::Decimals;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into()?)
}

pub fn write_decimal(e: &Host, d: u8) -> Result<(), Error> {
    let key = DataKey::Decimals;
    e.put_contract_data(key.try_into_val(e)?, u32::from(d).into())?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<Bytes, Error> {
    let key = DataKey::Name;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into_val(e)?)
}

pub fn write_name(e: &Host, d: Bytes) -> Result<(), Error> {
    let key = DataKey::Name;
    e.put_contract_data(key.try_into_val(e)?, d.try_into_val(e)?)?;
    Ok(())
}

pub fn read_symbol(e: &Host) -> Result<Bytes, Error> {
    let key = DataKey::Symbol;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into_val(e)?)
}

pub fn write_symbol(e: &Host, d: Bytes) -> Result<(), Error> {
    let key = DataKey::Symbol;
    e.put_contract_data(key.try_into_val(e)?, d.try_into_val(e)?)?;
    Ok(())
}
