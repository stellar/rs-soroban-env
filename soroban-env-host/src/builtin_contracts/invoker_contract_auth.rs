use crate::{
    auth::{AuthorizedFunction, AuthorizedInvocation, ContractFunction},
    budget::AsBudget,
    builtin_contracts::{
        account_contract::{
            ContractAuthorizationContext, CreateContractHostFnContext,
            CreateContractWithConstructorHostFnContext,
        },
        base_types::Vec as ContractTypeVec,
        common_types::ContractExecutable,
    },
    host::metered_clone::{MeteredClone, MeteredContainer},
    host_object::HostVec,
    xdr::{
        self, ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgsV2, ScAddress,
        ScErrorCode, ScErrorType,
    },
    Host, HostError, TryFromVal, TryIntoVal, Val,
};
use soroban_builtin_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub(crate) struct SubContractInvocation {
    pub context: ContractAuthorizationContext,
    pub sub_invocations: ContractTypeVec, // Vec of InvokerContractAuthEntry
}

/// Contract type used as the element of `authorize_as_curr_contract` host
/// function argument vector.
/// Defines sub-contract call authorizations performed on behalf of the current
/// contract.
#[derive(Clone)]
#[contracttype]
pub(crate) enum InvokerContractAuthEntry {
    Contract(SubContractInvocation),
    CreateContractHostFn(CreateContractHostFnContext),
    CreateContractWithCtorHostFn(CreateContractWithConstructorHostFnContext),
}

// metering: covered
impl InvokerContractAuthEntry {
    fn to_authorized_invocation(
        &self,
        host: &Host,
        invoker_contract_addr: &ScAddress,
    ) -> Result<AuthorizedInvocation, HostError> {
        match &self {
            InvokerContractAuthEntry::Contract(contract_invocation) => {
                let function = AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: contract_invocation.context.contract.as_object(),
                    function_name: contract_invocation.context.fn_name,
                    args: host.visit_obj(
                        contract_invocation.context.args.as_object(),
                        |v: &HostVec| v.to_vec(host.budget_ref()),
                    )?,
                });
                let mut sub_invocations: Vec<AuthorizedInvocation> = vec![];
                host.visit_obj(
                    contract_invocation.sub_invocations.as_object(),
                    |v: &HostVec| {
                        Vec::<AuthorizedInvocation>::charge_bulk_init_cpy(
                            v.len() as u64,
                            host.as_budget(),
                        )?;
                        sub_invocations.reserve(v.len());
                        for val in v.iter() {
                            let entry = InvokerContractAuthEntry::try_from_val(host, val)?;
                            sub_invocations
                                .push(entry.to_authorized_invocation(host, invoker_contract_addr)?);
                        }
                        Ok(())
                    },
                )?;
                Ok(AuthorizedInvocation::new(function, sub_invocations))
            }
            InvokerContractAuthEntry::CreateContractHostFn(create_contract_fn) => {
                let wasm_hash = match &create_contract_fn.executable {
                    ContractExecutable::Wasm(b) => {
                        host.hash_from_bytesobj_input("wasm_ref", b.as_object())?
                    }
                    ContractExecutable::StellarAsset => {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected authorized StellarAsset contract creation",
                            &[],
                        ));
                    }
                };
                let function = AuthorizedFunction::CreateContractHostFn(CreateContractArgsV2 {
                    contract_id_preimage: ContractIdPreimage::Address(
                        ContractIdPreimageFromAddress {
                            address: invoker_contract_addr.metered_clone(host)?,
                            salt: host.u256_from_bytesobj_input(
                                "salt",
                                create_contract_fn.salt.as_object(),
                            )?,
                        },
                    ),
                    executable: xdr::ContractExecutable::Wasm(wasm_hash),
                    constructor_args: Default::default(),
                });
                Ok(AuthorizedInvocation::new(function, vec![]))
            }
            InvokerContractAuthEntry::CreateContractWithCtorHostFn(create_contract_fn) => {
                let wasm_hash = match &create_contract_fn.executable {
                    ContractExecutable::Wasm(b) => {
                        host.hash_from_bytesobj_input("wasm_ref", b.as_object())?
                    }
                    ContractExecutable::StellarAsset => {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected authorized StellarAsset contract creation",
                            &[],
                        ));
                    }
                };
                let function = AuthorizedFunction::CreateContractHostFn(CreateContractArgsV2 {
                    contract_id_preimage: ContractIdPreimage::Address(
                        ContractIdPreimageFromAddress {
                            address: invoker_contract_addr.metered_clone(host)?,
                            salt: host.u256_from_bytesobj_input(
                                "salt",
                                create_contract_fn.salt.as_object(),
                            )?,
                        },
                    ),
                    executable: xdr::ContractExecutable::Wasm(wasm_hash),
                    constructor_args: host
                        .vecobject_to_scval_vec(create_contract_fn.constructor_args.as_object())?,
                });
                Ok(AuthorizedInvocation::new(function, vec![]))
            }
        }
    }
}

// metering: covered
pub(crate) fn invoker_contract_auth_to_authorized_invocation(
    host: &Host,
    invoker_contract_addr: &ScAddress,
    invoker_auth_entry: Val,
) -> Result<AuthorizedInvocation, HostError> {
    let entry = InvokerContractAuthEntry::try_from_val(host, &invoker_auth_entry)?;
    entry.to_authorized_invocation(host, invoker_contract_addr)
}
