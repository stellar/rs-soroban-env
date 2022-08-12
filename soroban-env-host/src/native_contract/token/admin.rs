use crate::host::Host;
use crate::native_contract::base_types::BytesN;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::public_types::{
    Authorization, Identifier, KeyedAccountAuthorization, KeyedAuthorization, KeyedEd25519Signature,
};
use crate::native_contract::token::storage_types::DataKey;
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn has_administrator(e: &Host) -> Result<bool, Error> {
    let key = DataKey::Admin;
    let rv = e.has_contract_data(key.try_into_val(e)?)?;
    Ok(rv.try_into()?)
}

fn read_administrator(e: &Host) -> Result<Identifier, Error> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?)?;
    Ok(rv.in_env(e).try_into()?)
}

pub fn to_administrator_authorization(
    e: &Host,
    auth: Authorization,
) -> Result<KeyedAuthorization, Error> {
    let admin = read_administrator(e)?;
    match (admin, auth) {
        (Identifier::Contract(admin_id), Authorization::Contract) => {
            let invoker_id: BytesN<32> = e.get_invoking_contract()?.in_env(e).try_into()?;
            if admin_id.compare(&invoker_id)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                Ok(KeyedAuthorization::Contract)
            }
        }
        (Identifier::Ed25519(admin_id), Authorization::Ed25519(signature)) => {
            Ok(KeyedAuthorization::Ed25519(KeyedEd25519Signature {
                public_key: admin_id,
                signature,
            }))
        }
        (Identifier::Account(admin_id), Authorization::Account(signatures)) => {
            Ok(KeyedAuthorization::Account(KeyedAccountAuthorization {
                public_key: admin_id,
                signatures,
            }))
        }
        _ => Err(Error::ContractError),
    }
}

pub fn write_administrator(e: &Host, id: Identifier) -> Result<(), Error> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)?;
    Ok(())
}
