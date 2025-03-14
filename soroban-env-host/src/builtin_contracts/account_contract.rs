// This is a built-in account 'contract'. This is not actually a contract, as
// it doesn't need to be directly invoked. But semantically this is analagous
// to a generic smart wallet contract that supports authentication and blanket
// context authorization.
use crate::{
    auth::{AuthorizedFunction, AuthorizedInvocation},
    builtin_contracts::{
        base_types::{Address, BytesN, Vec as HostVec},
        common_types::ContractExecutable,
        contract_error::ContractError,
    },
    err,
    host::{
        frame::{CallParams, ContractReentryMode},
        Host,
    },
    xdr::{
        self, AccountId, ContractIdPreimage, Hash, ScErrorCode, ScErrorType, ThresholdIndexes,
        Uint256,
    },
    Env, EnvBase, HostError, Symbol, TryFromVal, TryIntoVal, Val,
};
use core::cmp::Ordering;

const MAX_ACCOUNT_SIGNATURES: u32 = 20;

use soroban_builtin_sdk_macros::contracttype;

pub const ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME: &str = "__check_auth";

#[derive(Clone)]
#[contracttype]
pub(crate) struct ContractAuthorizationContext {
    pub(crate) contract: Address,
    pub(crate) fn_name: Symbol,
    pub(crate) args: HostVec,
}

#[derive(Clone)]
#[contracttype]
pub(crate) struct CreateContractHostFnContext {
    pub(crate) executable: ContractExecutable,
    pub(crate) salt: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub(crate) struct CreateContractWithConstructorHostFnContext {
    pub(crate) executable: ContractExecutable,
    pub(crate) salt: BytesN<32>,
    pub(crate) constructor_args: HostVec,
}

#[derive(Clone)]
#[contracttype]
pub(crate) enum AuthorizationContext {
    Contract(ContractAuthorizationContext),
    CreateContractHostFn(CreateContractHostFnContext),
    CreateContractWithCtorHostFn(CreateContractWithConstructorHostFnContext),
}

#[derive(Clone)]
#[contracttype]
pub(crate) struct AccountEd25519Signature {
    pub(crate) public_key: BytesN<32>,
    pub(crate) signature: BytesN<64>,
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
                    xdr::ContractExecutable::StellarAsset => return Err(host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InvalidInput,
                        "StellarAsset executable is not allowed when authorizing create_contract host fn",
                        &[],
                    )),
                };
                let salt = match &args.contract_id_preimage {
                    ContractIdPreimage::Address(id_from_addr) => {
                        BytesN::<32>::from_slice(host, id_from_addr.salt.as_slice())?
                    }
                    ContractIdPreimage::Asset(_) => return Err(host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InvalidInput,
                        "asset preimage is not allowed when authorizing create_contract host fn",
                        &[],
                    )),
                };
                if args.constructor_args.is_empty() {
                    Ok(AuthorizationContext::CreateContractHostFn(
                        CreateContractHostFnContext {
                            executable: ContractExecutable::Wasm(wasm_hash),
                            salt,
                        },
                    ))
                } else {
                    let args_vec = host.scvals_to_val_vec(&args.constructor_args.as_slice())?;
                    Ok(AuthorizationContext::CreateContractWithCtorHostFn(
                        CreateContractWithConstructorHostFnContext {
                            executable: ContractExecutable::Wasm(wasm_hash),
                            salt,
                            constructor_args: args_vec.try_into_val(host)?,
                        },
                    ))
                }
            }
        }
    }
}

// metering: covered
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

// metering: covered
pub(crate) fn check_account_contract_auth(
    host: &Host,
    account_contract: &Hash,
    signature_payload: &[u8; 32],
    signature: Val,
    invocation: &AuthorizedInvocation,
) -> Result<(), HostError> {
    let payload_obj = host.bytes_new_from_slice(signature_payload)?;
    let mut auth_context_vec = HostVec::new(host)?;
    invocation_tree_to_auth_contexts(host, invocation, &mut auth_context_vec)?;
    Ok(host
        .call_n_internal(
            account_contract,
            ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME.try_into_val(host)?,
            &[payload_obj.into(), signature, auth_context_vec.into()],
            CallParams {
                // Allow self reentry for this function in order to be able to do
                // wallet admin ops using the auth framework itself.
                reentry_mode: ContractReentryMode::SelfAllowed,
                internal_host_call: true,
                treat_missing_function_as_noop: false,
            },
        )?
        .try_into()?)
}

// metering: covered
pub(crate) fn check_account_authentication(
    host: &Host,
    account_id: AccountId,
    payload: &[u8],
    signature: Val,
) -> Result<(), HostError> {
    let signatures: HostVec = signature.try_into_val(host)?;
    // Check if there is too many signatures: there shouldn't be more
    // signatures then the amount of account signers.
    let len = signatures.len()?;
    if len > MAX_ACCOUNT_SIGNATURES {
        return Err(err!(
            host,
            ContractError::AuthenticationError,
            "too many account signers",
            len
        ));
    }
    if len == 0 {
        return Err(host.error(
            ContractError::AuthenticationError.into(),
            "no account signatures found",
            &[],
        ));
    }
    let payload_obj = host.bytes_new_from_slice(payload)?;
    let account = host.load_account(account_id)?;
    let mut prev_pk: Option<BytesN<32>> = None;
    let mut weight = 0u32;
    for i in 0..len {
        let sig: AccountEd25519Signature = signatures.get(i)?;
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
        // 255 * MAX_ACCOUNT_SIGNATURES is < u32::MAX,
        // but to future-proof the code we do saturating_add.
        weight = weight.saturating_add(signer_weight as u32);
        prev_pk = Some(sig.public_key);
    }
    // This should always work but again, we err on side
    // of future-proofing against changed assumptions.
    let Some(threshold) = account.thresholds.0.get(ThresholdIndexes::Med as usize) else {
        return Err(host.error(
            (ScErrorType::Auth, ScErrorCode::InternalError).into(),
            "unexpected thresholds-array size",
            &[],
        ));
    };
    if weight < *threshold as u32 {
        Err(err!(
            host,
            ContractError::AuthenticationError,
            "signature weight is lower than threshold",
            weight,
            *threshold as u32
        ))
    } else {
        Ok(())
    }
}
