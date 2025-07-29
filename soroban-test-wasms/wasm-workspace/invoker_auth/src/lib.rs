#![no_std]

use soroban_sdk::{
    auth::ContractContext, contract, contractimpl, contracttype, symbol_short, vec, Address,
    BytesN, Env, Error, IntoVal, Symbol, Val, Vec,
};

// We define custom contract types (instead of using the SDK ones) that
// resemble `InvokerContractAuthEntry` but also have some unsupported options
// for greater test coverage.

#[derive(Clone)]
#[contracttype]
pub enum ContractExecutable {
    Wasm(BytesN<32>),
    // This shouldn't be supported.
    StellarAsset,
}

#[derive(Clone)]
#[contracttype]
pub struct CreateContractHostFnContext {
    pub executable: ContractExecutable,
    pub salt: BytesN<32>,
}

#[derive(Clone)]
#[contracttype]
pub struct CreateContractWithConstructorHostFnContext {
    pub executable: ContractExecutable,
    pub salt: BytesN<32>,
    pub constructor_args: Vec<Val>,
}

#[derive(Clone)]
#[contracttype]
pub enum InvokerContractAuthEntry {
    Contract(SubContractInvocation),
    CreateContractHostFn(CreateContractHostFnContext),
    CreateContractWithCtorHostFn(CreateContractWithConstructorHostFnContext),
    // A non-existent enum option
    CreateContractV3Fake(CreateContractHostFnContext),
}

#[derive(Clone)]
#[contracttype]
pub struct SubContractInvocation {
    pub context: ContractContext,
    pub sub_invocations: Vec<InvokerContractAuthEntry>,
}

#[contract]
struct InvokerAuthContract;

#[derive(Clone)]
#[contracttype]
struct InvokerAuthNode {
    pub contract: Address,
    pub arg: i32,
    pub children: Vec<InvokerAuthNode>,
}

#[derive(Clone)]
#[contracttype]
struct InvokerCallNode {
    pub contract: Address,
    pub try_call: bool,
    // Indicates if a contract call should fail at one of these points:
    // 1 - right after adding invoker auth entries
    // 2 - after authorizing `authorize` addresses
    // 3 - after invoking all children
    // Any other value means no failure.
    pub failure_point: u32,
    pub authorize: Vec<(Address, i32)>,
    pub auth: Vec<InvokerAuthNode>,
    pub children: Vec<InvokerCallNode>,
}

#[contractimpl]
impl InvokerAuthContract {
    // Function for testing complex call trees with multiple invoker auths
    // Note, that this only authorizes the first successful 'child' call via
    // `authorize_as_current_contract`, all the subsequent child calls should
    // either not require auth from the indirect invoker, or will fail.
    pub fn auth_fn(env: Env, call: InvokerCallNode) {
        let mut auth_entries = vec![&env];
        for auth in call.auth.iter() {
            auth_entries.push_back(auth_to_invoker_auth_entry(&env, &auth));
        }

        // NB: This is not a function normally found in the SDK, this is just
        // added locally for building this contract. It's the same as
        // `authorize_as_current_contract`, but accepts a VecObject.
        env.authorize_as_current_contract_vec(auth_entries.to_object());
        if call.failure_point == 1 {
            panic!("fail");
        }

        for (addr, val) in &call.authorize {
            addr.require_auth_for_args((val,).into_val(&env));
        }

        if call.failure_point == 2 {
            panic!("fail");
        }
        let mut was_success = false;
        for child in &call.children {
            if child.try_call {
                let res = env.try_invoke_contract::<(), Error>(
                    &child.contract,
                    &symbol_short!("auth_fn"),
                    (child.clone(),).into_val(&env),
                );
                if !was_success && res.is_err() {
                    env.authorize_as_current_contract_vec(auth_entries.to_object());
                } else {
                    was_success = true;
                }
            } else {
                env.invoke_contract::<()>(
                    &child.contract,
                    &symbol_short!("auth_fn"),
                    (child.clone(),).into_val(&env),
                );
                was_success = true;
            }
        }
        if call.failure_point == 3 {
            panic!("fail");
        }
    }

    // Various failure scenarios that should result in a recoverable error.
    pub fn invoker_auth_with_sac_create(env: Env) {
        let auth_entry =
            InvokerContractAuthEntry::CreateContractHostFn(CreateContractHostFnContext {
                executable: ContractExecutable::StellarAsset,
                salt: [0; 32].into_val(&env),
            });
        env.authorize_as_current_contract_vec(vec![&env, auth_entry].to_object());
    }

    pub fn invoker_auth_with_sac_create_v2(env: Env) {
        let auth_entry = InvokerContractAuthEntry::CreateContractWithCtorHostFn(
            CreateContractWithConstructorHostFnContext {
                executable: ContractExecutable::StellarAsset,
                salt: [0; 32].into_val(&env),
                constructor_args: vec![&env],
            },
        );
        env.authorize_as_current_contract_vec(vec![&env, auth_entry].to_object());
    }

    pub fn invoker_auth_with_bad_type(env: Env) {
        let auth_entry =
            InvokerContractAuthEntry::CreateContractV3Fake(CreateContractHostFnContext {
                executable: ContractExecutable::Wasm([0; 32].into_val(&env)),
                salt: [0; 32].into_val(&env),
            });
        env.authorize_as_current_contract_vec(vec![&env, auth_entry].to_object());
    }
}

fn auth_to_invoker_auth_entry(env: &Env, auth: &InvokerAuthNode) -> InvokerContractAuthEntry {
    let mut sub_invocations = vec![env];
    for child in auth.children.iter() {
        sub_invocations.push_back(auth_to_invoker_auth_entry(env, &child));
    }
    InvokerContractAuthEntry::Contract(SubContractInvocation {
        context: ContractContext {
            contract: auth.contract.clone(),
            fn_name: Symbol::new(&env, "auth_fn"),
            args: (auth.arg,).into_val(env),
        },
        sub_invocations,
    })
}
