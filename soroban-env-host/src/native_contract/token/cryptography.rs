use crate::host::Host;
use crate::native_contract::base_types::{Bytes, BytesN, Vec};
use crate::native_contract::token::nonce::read_and_increment_nonce;
use crate::native_contract::token::public_types::{
    AccountSignatures, Ed25519Signature, Identifier, Signature, SignaturePayload,
    SignaturePayloadV0,
};
use crate::{err, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{ThresholdIndexes, Uint256};
use soroban_env_common::{CheckedEnv, InvokerType, Symbol, TryFromVal, TryIntoVal};

use super::error::ContractError;

const MAX_ACCOUNT_SIGNATURES: u32 = 20;

// Metering: covered by components
fn check_ed25519_auth(
    e: &Host,
    auth: Ed25519Signature,
    name: Symbol,
    args: Vec,
) -> Result<(), HostError> {
    let msg = SignaturePayloadV0 {
        name,
        contract: BytesN::<32>::try_from_val(e, e.get_current_contract()?)?,
        network: Bytes::try_from_val(e, e.get_ledger_network_passphrase()?)?,
        args,
    };
    let msg_bin = e.serialize_to_bytes(SignaturePayload::V0(msg).try_into_val(e)?)?;

    e.verify_sig_ed25519(msg_bin, auth.public_key.into(), auth.signature.into())?;
    Ok(())
}

// Metering: *mostly* covered by components.
fn check_account_auth(
    e: &Host,
    auth: AccountSignatures,
    name: Symbol,
    args: Vec,
) -> Result<(), HostError> {
    let msg = SignaturePayloadV0 {
        name,
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
        return Err(err!(
            e,
            ContractError::AuthenticationError,
            "too many account signers: {} > {}",
            sigs.len()?,
            MAX_ACCOUNT_SIGNATURES
        ));
    }
    let account = e.load_account(auth.account_id)?;
    let mut prev_pk: Option<BytesN<32>> = None;
    for i in 0..sigs.len()? {
        let sig: Ed25519Signature = sigs.get(i)?;
        // Cannot take multiple signatures from the same key
        if let Some(prev) = prev_pk {
            if prev.compare(&sig.public_key)? != Ordering::Less {
                return Err(err!(
                    e,
                    ContractError::AuthenticationError,
                    "public keys are not ordered: {} > {}",
                    prev,
                    sig.public_key
                ));
            }
        }

        e.verify_sig_ed25519(
            msg_bin.clone(),
            sig.public_key.clone().into(),
            sig.signature.into(),
        )?;

        let signer_weight =
            e.get_signer_weight_from_account(Uint256(sig.public_key.to_array()?), &account)?;
        // 0 weight indicates that signer doesn't belong to this account. Treat
        // this as an error to indicate a bug in signatures, even if another
        // signers would have enough weight.
        if signer_weight == 0 {
            return Err(err!(
                e,
                ContractError::AuthenticationError,
                "signer '{}' does not belong to account",
                sig.public_key
            ));
        }
        // Overflow isn't possible here as
        // 255 * MAX_ACCOUNT_SIGNATURES is < u32::MAX.
        weight += signer_weight as u32;
        prev_pk = Some(sig.public_key);
    }
    let threshold = account.thresholds.0[ThresholdIndexes::Med as usize];
    if weight < threshold as u32 {
        Err(err!(
            e,
            ContractError::AuthenticationError,
            "signature weight is lower than threshold: {} < {}",
            weight,
            threshold as u32
        ))
    } else {
        Ok(())
    }
}

// Metering: *mostly* covered by components.
pub fn check_auth(
    e: &Host,
    auth: Signature,
    nonce: i128,
    function: Symbol,
    args: Vec,
) -> Result<(), HostError> {
    match auth {
        Signature::Invoker => {
            if nonce != 0 {
                Err(err!(
                    e,
                    ContractError::NonceError,
                    "non-zero invoker nonce: {}",
                    nonce
                ))
            } else {
                let invoker_type: InvokerType = Host::get_invoker_type(&e)?.try_into()?;
                match invoker_type {
                    InvokerType::Account => e.get_invoking_account()?,
                    InvokerType::Contract => e.get_invoking_contract()?,
                };
                Ok(())
            }
        }
        Signature::Ed25519(kea) => {
            let stored_nonce =
                read_and_increment_nonce(e, Identifier::Ed25519(kea.public_key.clone()))?;
            if nonce != stored_nonce {
                Err(err!(
                    e,
                    ContractError::NonceError,
                    "incorrect nonce: expected {}, got {}",
                    stored_nonce,
                    nonce
                ))
            } else {
                check_ed25519_auth(e, kea, function, args)?;
                Ok(())
            }
        }
        Signature::Account(kaa) => {
            let stored_nonce =
                read_and_increment_nonce(e, Identifier::Account(kaa.account_id.clone()))?;
            if nonce != stored_nonce {
                Err(err!(
                    e,
                    ContractError::NonceError,
                    "incorrect nonce: expected {}, got {}",
                    stored_nonce,
                    nonce
                ))
            } else {
                check_account_auth(e, kaa, function, args)?;
                Ok(())
            }
        }
    }
}
