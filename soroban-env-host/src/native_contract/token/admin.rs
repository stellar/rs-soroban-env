use crate::host::Host;
use crate::native_contract::base_types::BytesN;
use crate::native_contract::token::public_types::{
    Authorization, Identifier, KeyedAccountAuthorization, KeyedAuthorization, KeyedEd25519Signature,
};
use crate::native_contract::token::storage_types::DataKey;
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, TryIntoVal};

pub fn has_administrator(e: &Host) -> Result<bool, ()> {
    let key = DataKey::Admin;
    let rv = e.has_contract_data(key.try_into_val(e)?).map_err(|_| ())?;
    rv.try_into().map_err(|_| ())
}

fn read_administrator(e: &Host) -> Result<Identifier, ()> {
    let key = DataKey::Admin;
    let rv = e.get_contract_data(key.try_into_val(e)?).map_err(|_| ())?;
    rv.in_env(e).try_into()
}

pub fn to_administrator_authorization(
    e: &Host,
    auth: Authorization,
) -> Result<KeyedAuthorization, ()> {
    let admin = read_administrator(e)?;
    match (admin, auth) {
        (Identifier::Contract(admin_id), Authorization::Contract) => {
            let invoker_id: BytesN<32> = e
                .get_invoking_contract()
                .map_err(|_| ())?
                .in_env(e)
                .try_into()?;
            if admin_id.compare(&invoker_id)? != Ordering::Equal {
                Err(())
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
        _ => Err(()),
    }
}

pub fn write_administrator(e: &Host, id: Identifier) -> Result<(), ()> {
    let key = DataKey::Admin;
    e.put_contract_data(key.try_into_val(e)?, id.try_into_val(e)?)
        .map_err(|_| ())?;
    Ok(())
}
