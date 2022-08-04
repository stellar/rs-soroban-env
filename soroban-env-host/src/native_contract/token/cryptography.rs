use crate::host::Host;
use crate::native_contract::base_types::Vec;
use crate::native_contract::token::nonce::read_and_increment_nonce;
use crate::native_contract::token::public_types::{
    Identifier, KeyedAccountAuthorization, KeyedAuthorization, KeyedEd25519Signature, Message,
    MessageV0, U256,
};
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, TryIntoVal};

#[repr(u32)]
pub enum Domain {
    Approve = 0,
    Transfer = 1,
    TransferFrom = 2,
    Burn = 3,
    Freeze = 4,
    Mint = 5,
    SetAdministrator = 6,
    Unfreeze = 7,
}

fn check_ed25519_auth(
    e: &Host,
    auth: KeyedEd25519Signature,
    domain: Domain,
    parameters: Vec,
) -> Result<(), ()> {
    let msg = MessageV0 {
        nonce: read_and_increment_nonce(&e, Identifier::Ed25519(auth.public_key.clone()))?,
        domain: domain as u32,
        parameters: parameters.try_into().unwrap(),
    };
    let msg_bin = e
        .serialize_to_binary(Message::V0(msg).try_into_val(e)?)
        .map_err(|_| ())?;

    e.verify_sig_ed25519(auth.public_key.into(), msg_bin, auth.signature.into())
        .map_err(|_| ())?;
    Ok(())
}

fn check_account_auth(
    e: &Host,
    auth: KeyedAccountAuthorization,
    domain: Domain,
    parameters: Vec,
) -> Result<(), ()> {
    let msg = MessageV0 {
        nonce: read_and_increment_nonce(&e, Identifier::Account(auth.public_key.clone()))?,
        domain: domain as u32,
        parameters: parameters.try_into().unwrap(),
    };
    let msg_bin = e
        .serialize_to_binary(Message::V0(msg).try_into_val(e)?)
        .map_err(|_| ())?;

    let mut weight = 0u32;
    let sigs = &auth.signatures;
    let mut prev_pk: Option<U256> = None;
    for i in 0..sigs.len()? {
        let sig: KeyedEd25519Signature = sigs.get(i)?;

        // Cannot take multiple signatures from the same key
        if let Some(prev) = prev_pk {
            if prev.compare(&sig.public_key)? != Ordering::Less {
                panic!("signature out of order")
            }
        }

        e.verify_sig_ed25519(
            sig.public_key.clone().into(),
            msg_bin.clone(),
            sig.signature.into(),
        )
        .map_err(|_| ())?;
        let signer_weight_rv = e
            .account_get_signer_weight(
                auth.public_key.clone().into(),
                sig.public_key.clone().into(),
            )
            .map_err(|_| ())?;
        let signer_weight: u32 = signer_weight_rv.try_into().map_err(|_| ())?;
        // TODO: Check for overflow
        weight += signer_weight;

        prev_pk = Some(sig.public_key);
    }

    let threshold_rv = e
        .account_get_medium_threshold(auth.public_key.into())
        .map_err(|_| ())?;
    if weight < threshold_rv.try_into().map_err(|_| ())? {
        Err(())
    } else {
        Ok(())
    }
}

pub fn check_auth(
    e: &Host,
    auth: KeyedAuthorization,
    domain: Domain,
    parameters: Vec,
) -> Result<(), ()> {
    match auth {
        KeyedAuthorization::Contract => {
            e.get_invoking_contract().map_err(|_| ())?;
            Ok(())
        }
        KeyedAuthorization::Ed25519(kea) => check_ed25519_auth(e, kea, domain, parameters),
        KeyedAuthorization::Account(kaa) => check_account_auth(e, kaa, domain, parameters),
    }
}
