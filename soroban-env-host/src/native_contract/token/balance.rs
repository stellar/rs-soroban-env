use crate::host::Host;
use crate::native_contract::base_types::BigInt;
use crate::native_contract::token::public_types::Identifier;
use crate::native_contract::token::storage_types::DataKey;
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn read_balance(e: &Host, id: Identifier) -> Result<BigInt, ()> {
    let key = DataKey::Balance(id);
    if let Ok(balance) = e.get_contract_data(key.try_into_val(e)?) {
        balance.in_env(e).try_into()
    } else {
        BigInt::from_u64(e, 0)
    }
}

fn write_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), ()> {
    let key = DataKey::Balance(id);
    e.put_contract_data(key.try_into_val(e)?, amount.try_into_val(e)?)
        .map_err(|_| ())?;
    Ok(())
}

pub fn receive_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), ()> {
    let balance = read_balance(e, id.clone())?;
    let is_frozen = read_state(e, id.clone())?;
    if is_frozen {
        Err(())
    } else {
        write_balance(e, id, (balance + amount)?)
    }
}

pub fn spend_balance(e: &Host, id: Identifier, amount: BigInt) -> Result<(), ()> {
    let balance = read_balance(e, id.clone())?;
    let is_frozen = read_state(e, id.clone())?;
    if is_frozen {
        Err(())
    } else if balance.compare(&amount)? == Ordering::Less {
        Err(())
    } else {
        write_balance(e, id, (balance - amount)?)
    }
}

pub fn read_state(e: &Host, id: Identifier) -> Result<bool, ()> {
    let key = DataKey::State(id);
    if let Ok(state) = e.get_contract_data(key.try_into_val(e)?) {
        state.try_into().map_err(|_| ())
    } else {
        Ok(false)
    }
}

pub fn write_state(e: &Host, id: Identifier, is_frozen: bool) -> Result<(), ()> {
    let key = DataKey::State(id);
    e.put_contract_data(key.try_into_val(e)?, is_frozen.into())
        .map_err(|_| ())?;
    Ok(())
}
