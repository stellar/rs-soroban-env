use crate::host::Host;
use crate::native_contract::base_types::BytesN;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::{Identifier, Signature};
use crate::native_contract::token::storage_types::DataKey;
use soroban_env_common::{CheckedEnv, TryFromVal, TryIntoVal};
use std::cmp::Ordering;

fn read_administrator(e: &Host) -> Result<Identifier, Error> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(Identifier::try_from_val(e, rv)?)
}

pub fn write_administrator(e: &Host, id: Identifier) -> Result<(), Error> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}

pub fn check_admin(e: &Host, auth: &Signature) -> Result<(), Error> {
    let admin = read_administrator(e)?;
    match (admin, auth) {
        (Identifier::Contract(admin_id), Signature::Contract) => {
            let invoker_id: BytesN<32> = e.get_invoking_contract()?.to_raw().try_into_val(e)?;
            if admin_id.compare(&invoker_id)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                Ok(())
            }
        }
        (Identifier::Ed25519(admin_id), Signature::Ed25519(signature)) => {
            if admin_id.compare(&signature.public_key)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                Ok(())
            }
        }
        (Identifier::Account(admin_id), Signature::Account(signatures)) => {
            if admin_id.compare(&signatures.account_id)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                Ok(())
            }
        }
        _ => Err(Error::ContractError),
    }
}
