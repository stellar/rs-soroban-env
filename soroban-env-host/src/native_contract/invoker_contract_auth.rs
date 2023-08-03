use super::account_contract::{ContractAuthorizationContext, CreateContractHostFnContext};
use super::common_types::ContractExecutable;
use crate::budget::AsBudget;
use crate::host::metered_clone::{MeteredClone, MeteredContainer};
use crate::host_object::HostVec;
use crate::{
    auth::{AuthorizedFunction, AuthorizedInvocation, ContractFunction},
    native_contract::base_types::Vec as ContractTypeVec,
    Host, HostError,
};
use soroban_env_common::xdr::{
    self, ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgs, ScAddress,
    ScErrorCode, ScErrorType,
};
use soroban_env_common::{TryFromVal, TryIntoVal, Val};
use soroban_native_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub struct SubContractInvocation {
    pub context: ContractAuthorizationContext,
    pub sub_invocations: ContractTypeVec, // Vec of InvokerContractAuthEntry
}

/// Contract type used as the element of `authorize_as_curr_contract` host
/// function argument vector.
/// Defines sub-contract call authorizations performed on behalf of the current
/// contract.
#[derive(Clone)]
#[contracttype]
pub enum InvokerContractAuthEntry {
    Contract(SubContractInvocation),
    CreateContractHostFn(CreateContractHostFnContext),
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
                    ContractExecutable::Token => {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected authorized token creation",
                            &[],
                        ));
                    }
                };
                let function = AuthorizedFunction::CreateContractHostFn(CreateContractArgs {
                    contract_id_preimage: ContractIdPreimage::Address(
                        ContractIdPreimageFromAddress {
                            address: invoker_contract_addr.metered_clone(host.budget_ref())?,
                            salt: host.u256_from_bytesobj_input(
                                "salt",
                                create_contract_fn.salt.as_object(),
                            )?,
                        },
                    ),
                    executable: xdr::ContractExecutable::Wasm(wasm_hash),
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
