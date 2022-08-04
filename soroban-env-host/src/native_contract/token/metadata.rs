use crate::host::Host;
use crate::native_contract::base_types::Bytes;
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn read_decimal(e: &Host) -> Result<u32, ()> {
    let key = DataKey::Decimals;
    let rv = e.get_contract_data(key.try_into_val(e)?).map_err(|_| ())?;
    rv.try_into().map_err(|_| ())
}

pub fn write_decimal(e: &Host, d: u8) -> Result<(), ()> {
    let key = DataKey::Decimals;
    e.put_contract_data(key.try_into_val(e)?, u32::from(d).into())
        .map_err(|_| ())?;
    Ok(())
}

pub fn read_name(e: &Host) -> Result<Bytes, ()> {
    let key = DataKey::Name;
    let rv = e.get_contract_data(key.try_into_val(e)?).map_err(|_| ())?;
    rv.in_env(e).try_into().map_err(|_| ())
}

pub fn write_name(e: &Host, d: Bytes) -> Result<(), ()> {
    let key = DataKey::Name;
    e.put_contract_data(key.try_into_val(e)?, d.try_into_val(e)?)
        .map_err(|_| ())?;
    Ok(())
}

pub fn read_symbol(e: &Host) -> Result<Bytes, ()> {
    let key = DataKey::Symbol;
    let rv = e.get_contract_data(key.try_into_val(e)?).map_err(|_| ())?;
    rv.in_env(e).try_into().map_err(|_| ())
}

pub fn write_symbol(e: &Host, d: Bytes) -> Result<(), ()> {
    let key = DataKey::Symbol;
    e.put_contract_data(key.try_into_val(e)?, d.try_into_val(e)?)
        .map_err(|_| ())?;
    Ok(())
}
