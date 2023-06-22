use crate::auth::{AuthorizedFunction, AuthorizedInvocation};
// This is a built-in account 'contract'. This is not actually a contract, as
// it doesn't need to be directly invoked. But semantically this is analagous
// to a generic smart wallet contract that supports authentication and blanket
// context authorization.
use crate::host::{frame::ContractReentryMode, Host};
use crate::native_contract::{base_types::BytesN, contract_error::ContractError};
use crate::{err, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{
    self, ContractIdPreimage, Hash, ScErrorCode, ScErrorType, ThresholdIndexes, Uint256,
};
use soroban_env_common::{Env, EnvBase, Symbol, TryFromVal, TryIntoVal, Val};

use crate::native_contract::base_types::Vec as HostVec;

const MAX_ACCOUNT_SIGNATURES: u32 = 20;

use soroban_env_common::xdr::AccountId;
use soroban_native_sdk_macros::contracttype;

use super::base_types::Address;
use super::common_types::ContractExecutable;

pub const ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME: &str = "__check_auth";

#[derive(Clone)]
#[contracttype]
pub struct ContractAuthorizationContext {
    pub contract: Address,
    pub fn_name: Symbol,
    pub args: HostVec,
}

#[derive(Clone)]
#[contracttype]
pub struct CreateContractHostFnContext {
    pub executable: ContractExecutable,
    pub salt: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
enum AuthorizationContext {
    Contract(ContractAuthorizationContext),
    CreateContractHostFn(CreateContractHostFnContext),
}

#[derive(Clone)]
#[contracttype]
pub struct AccountEd25519Signature {
    pub public_key: BytesN<32>,
    pub signature: BytesN<64>,
}

impl AuthorizationContext {
    fn from_authorized_fn(host: &Host, function: &AuthorizedFunction) -> Result<Self, HostError> {
        match function {
            AuthorizedFunction::ContractFn(contract_fn) => {
                let args = HostVec::try_from_val(host, &contract_fn.args)?;
                Ok(AuthorizationContext::Contract(
                    ContractAuthorizationContext {
                        contract: contract_fn.contract_address.try_into_val(host)?,
                        fn_name: contract_fn.function_name,
                        args,
                    },
                ))
            }
            AuthorizedFunction::CreateContractHostFn(args) => {
                let wasm_hash = match &args.executable {
                    xdr::ContractExecutable::Wasm(wasm_hash) => {
                        BytesN::<32>::from_slice(host, wasm_hash.as_slice())?
                    }
                    xdr::ContractExecutable::Token => {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected auth context for create contract host fn",
                            &[],
                        ))
                    }
                };
                let salt = match &args.contract_id_preimage {
                    ContractIdPreimage::Address(id_from_addr) => {
                        BytesN::<32>::from_slice(host, id_from_addr.salt.as_slice())?
                    }
                    ContractIdPreimage::Asset(_) => {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected auth context for create contract host fn",
                            &[],
                        ))
                    }
                };
                Ok(AuthorizationContext::CreateContractHostFn(
                    CreateContractHostFnContext {
                        executable: ContractExecutable::Wasm(wasm_hash),
                        salt,
                    },
                ))
            }
        }
    }
}

fn invocation_tree_to_auth_contexts(
    host: &Host,
    invocation: &AuthorizedInvocation,
    out_contexts: &mut HostVec,
) -> Result<(), HostError> {
    out_contexts.push(&AuthorizationContext::from_authorized_fn(
        host,
        &invocation.function,
    )?)?;
    for sub_invocation in &invocation.sub_invocations {
        invocation_tree_to_auth_contexts(host, sub_invocation, out_contexts)?;
    }
    Ok(())
}

pub(crate) fn check_account_contract_auth(
    host: &Host,
    account_contract: &Hash,
    signature_payload: &[u8; 32],
    signature_args: &Vec<Val>,
    invocation: &AuthorizedInvocation,
) -> Result<(), HostError> {
    let payload_obj = host.bytes_new_from_slice(signature_payload)?;
    let signature_args_vec = HostVec::try_from_val(host, signature_args)?;
    let mut auth_context_vec = HostVec::new(host)?;
    invocation_tree_to_auth_contexts(host, invocation, &mut auth_context_vec)?;
    Ok(host
        .call_n_internal(
            account_contract,
            ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME.try_into_val(host)?,
            &[
                payload_obj.into(),
                signature_args_vec.into(),
                auth_context_vec.into(),
            ],
            // Allow self reentry for this function in order to be able to do
            // wallet admin ops using the auth framework itself.
            ContractReentryMode::SelfAllowed,
            true,
        )?
        .try_into()?)
}

pub(crate) fn check_account_authentication(
    host: &Host,
    account_id: AccountId,
    payload: &[u8],
    signatures: &Vec<Val>,
) -> Result<(), HostError> {
    // Check if there is too many signatures: there shouldn't be more
    // signatures then the amount of account signers.
    let len = signatures.len();
    if len > MAX_ACCOUNT_SIGNATURES as usize {
        return Err(err!(
            host,
            ContractError::AuthenticationError,
            "too many account signers",
            len
        ));
    }
    let payload_obj = host.bytes_new_from_slice(payload)?;
    let account = host.load_account(account_id)?;
    let mut prev_pk: Option<BytesN<32>> = None;
    let mut weight = 0u32;
    for sig in signatures {
        let sig = AccountEd25519Signature::try_from_val(host, sig)?;
        // Cannot take multiple signatures from the same key
        if let Some(prev) = prev_pk {
            if prev.compare(&sig.public_key)? != Ordering::Less {
                return Err(err!(
                    host,
                    ContractError::AuthenticationError,
                    "public keys are not ordered",
                    prev,
                    sig.public_key
                ));
            }
        }

        host.verify_sig_ed25519(
            sig.public_key.clone().into(),
            payload_obj,
            sig.signature.into(),
        )?;

        let signer_weight =
            host.get_signer_weight_from_account(Uint256(sig.public_key.to_array()?), &account)?;
        // 0 weight indicates that signer doesn't belong to this account. Treat
        // this as an error to indicate a bug in signatures, even if another
        // signers would have enough weight.
        if signer_weight == 0 {
            return Err(err!(
                host,
                ContractError::AuthenticationError,
                "signer does not belong to account",
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
            host,
            ContractError::AuthenticationError,
            "signature weight is lower than threshold",
            weight,
            threshold as u32
        ))
    } else {
        Ok(())
    }
}
