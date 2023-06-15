use super::account_contract::{
    ContractAuthorizationContext, ContractExecutable, CreateContractHostFnContext,
};
use crate::host::metered_clone::MeteredClone;
use crate::host_object::HostVec;
use crate::{
    auth::{AuthorizedFunction, AuthorizedInvocation, ContractFunction},
    native_contract::base_types::Vec as ContractTypeVec,
    Host, HostError,
};
use soroban_env_common::xdr::{
    ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgs, ScAddress,
    ScContractExecutable,
};
use soroban_env_common::{RawVal, TryFromVal, TryIntoVal};
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
                let sub_invocations: Vec<InvokerContractAuthEntry> = host.visit_obj(
                    contract_invocation.sub_invocations.as_object(),
                    |v: &HostVec| {
                        v.iter()
                            .map(|val| InvokerContractAuthEntry::try_from_val(host, val))
                            .collect::<Result<Vec<InvokerContractAuthEntry>, HostError>>()
                    },
                )?;
                let sub_invocations = sub_invocations
                    .into_iter()
                    .map(|i| i.to_authorized_invocation(host, invoker_contract_addr))
                    .collect::<Result<Vec<AuthorizedInvocation>, HostError>>()?;
                Ok(AuthorizedInvocation::new(function, sub_invocations))
            }
            InvokerContractAuthEntry::CreateContractHostFn(create_contract_fn) => {
                let wasm_hash = match &create_contract_fn.executable {
                    ContractExecutable::Wasm(b) => {
                        host.hash_from_bytesobj_input("wasm_ref", b.as_object())?
                    }
                };
                let function = AuthorizedFunction::CreateContractHostFn(CreateContractArgs {
                    contract_id_preimage: ContractIdPreimage::Address(
                        ContractIdPreimageFromAddress {
                            address: invoker_contract_addr.metered_clone(host.budget_ref())?,
                            salt: host.uint256_from_bytesobj_input(
                                "salt",
                                create_contract_fn.salt.as_object(),
                            )?,
                        },
                    ),
                    executable: ScContractExecutable::WasmRef(wasm_hash),
                });
                Ok(AuthorizedInvocation::new(function, vec![]))
            }
        }
    }
}

pub(crate) fn invoker_contract_auth_to_authorized_invocation(
    host: &Host,
    invoker_contract_addr: &ScAddress,
    invoker_auth_entry: RawVal,
) -> Result<AuthorizedInvocation, HostError> {
    let entry = InvokerContractAuthEntry::try_from_val(host, &invoker_auth_entry)?;
    entry.to_authorized_invocation(host, invoker_contract_addr)
}
