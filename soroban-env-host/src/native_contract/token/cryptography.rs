use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Bytes, BytesN, Vec};
use crate::native_contract::token::error::Error;
use crate::native_contract::token::nonce::read_and_increment_nonce;
use crate::native_contract::token::public_types::{
    AccountSignatures, Ed25519Signature, Identifier, Signature, SignaturePayload,
    SignaturePayloadV0,
};
use core::cmp::Ordering;
use soroban_env_common::{CheckedEnv, Symbol, TryFromVal, TryIntoVal};
use std::cmp::min;

const MAX_ACCOUNT_SIGNATURE_WEIGHT: u32 = u8::MAX as u32;
const MAX_ACCOUNT_SIGNATURES: u32 = 20;

fn check_ed25519_auth(
    e: &Host,
    auth: Ed25519Signature,
    function: Symbol,
    args: Vec,
) -> Result<(), Error> {
    let msg = SignaturePayloadV0 {
        function,
        contract: BytesN::<32>::try_from_val(e, e.get_current_contract()?)?,
        network: Bytes::try_from_val(e, e.get_ledger_network_passphrase()?)?,
        args,
    };
    let msg_bin = e.serialize_to_bytes(SignaturePayload::V0(msg).try_into_val(e)?)?;

    e.verify_sig_ed25519(msg_bin, auth.public_key.into(), auth.signature.into())?;
    Ok(())
}

fn check_account_auth(
    e: &Host,
    auth: AccountSignatures,
    function: Symbol,
    args: Vec,
) -> Result<(), Error> {
    let msg = SignaturePayloadV0 {
        function,
        contract: BytesN::<32>::try_from_val(e, e.get_current_contract()?)?,
        network: Bytes::try_from_val(e, e.get_ledger_network_passphrase()?)?,
        args,
    };
    let msg_bin = e.serialize_to_bytes(SignaturePayload::V0(msg).try_into_val(e)?)?;

    let mut weight = 0u32;
    let sigs = &auth.signatures;
    // Check if there is too many signatures: there shouldn't be more
    // signatures then the amount of account signers.
    if sigs.len()? > MAX_ACCOUNT_SIGNATURES {
        return Err(Error::ContractError);
    }
    let mut prev_pk: Option<BytesN<32>> = None;
    for i in 0..sigs.len()? {
        let sig: Ed25519Signature = sigs.get(i)?;
        // Cannot take multiple signatures from the same key
        if let Some(prev) = prev_pk {
            if prev.compare(&sig.public_key)? != Ordering::Less {
                return Err(Error::ContractError);
            }
        }

        e.verify_sig_ed25519(
            msg_bin.clone(),
            sig.public_key.clone().into(),
            sig.signature.into(),
        )?;
        let signer_weight_rv = e.account_get_signer_weight(
            auth.account_id.clone().into(),
            sig.public_key.clone().into(),
        )?;
        // Clamp signature weight to be at most 255. This is consistent with
        // classic tx signature weight computations in Core.
        let signer_weight: u32 = min(signer_weight_rv.try_into()?, MAX_ACCOUNT_SIGNATURE_WEIGHT);
        weight += signer_weight;
        prev_pk = Some(sig.public_key);
    }

    let threshold_rv = e.account_get_medium_threshold(auth.account_id.into())?;
    if weight < threshold_rv.try_into()? {
        Err(Error::ContractError)
    } else {
        Ok(())
    }
}

pub fn check_auth(
    e: &Host,
    auth: Signature,
    nonce: BigInt,
    function: Symbol,
    args: Vec,
) -> Result<(), Error> {
    match auth {
        Signature::Contract => {
            if nonce.compare(&BigInt::from_u64(e, 0)?)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                e.get_invoking_contract()?;
                Ok(())
            }
        }
        Signature::Ed25519(kea) => {
            let stored_nonce =
                read_and_increment_nonce(e, Identifier::Ed25519(kea.public_key.clone()))?;
            if nonce.compare(&stored_nonce)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                check_ed25519_auth(e, kea, function, args)?;
                Ok(())
            }
        }
        Signature::Account(kaa) => {
            let stored_nonce =
                read_and_increment_nonce(e, Identifier::Account(kaa.account_id.clone()))?;
            if nonce.compare(&stored_nonce)? != Ordering::Equal {
                Err(Error::ContractError)
            } else {
                check_account_auth(e, kaa, function, args)?;
                Ok(())
            }
        }
    }
}
