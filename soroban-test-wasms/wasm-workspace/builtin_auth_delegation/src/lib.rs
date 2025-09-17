#![no_std]
use soroban_sdk::{
    auth::Context, contract, contractimpl, contracttype, Address, BytesN, Env, Error, IntoVal,
    Symbol, Vec,
};

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    DelegatedSigner(Address),
    FailAuth,
}

// While this is called 'Signature', it's used for testing various
// invariants of the delegated account auth, so this are not meant to resemble
// the signature types in the real custom accounts.
#[derive(Clone)]
#[contracttype]
pub enum Signature {
    // Verifies the expected signature payload
    Payload(BytesN<32>),
    // Verify the expected context
    Context(Vec<Context>),
    // 'Built-in' delegated signer (using `delegate_account_auth` host fn).
    // This should not normally be passed via signatures due to existence of
    // `get_delegated_signers_for_current_auth_check`, we do this to cover
    // calling `delegate_account_auth` in invalid fashion.
    BuiltinDelegated(Address),
    // Verify the expected list of delegated signers
    ExpectedDelegatedSigners(Vec<Address>),
    // Call `require_auth` on the given address to test interaction with auth
    // nesting (shouldn't be used in practice).
    DirectDelegated(Address),
    // Call `try_indirect_auth` on the given address, which in turn calls
    // `require_auth`. The bool indicates whether we expect the call to succeed.
    // This is used for rollback testing, as well as additional auth nesting
    // testing.
    TryIndirectDelegated((Address, bool)),
    // Make `__check_auth` fail unconditionally for the given address. This is
    // used in conjunction with `TryIndirectDelegated` to test rollbacks.
    MakeAuthFail((Address, bool)),
}

#[contract]
struct AccountWithDelegation;

#[contractimpl]
impl AccountWithDelegation {
    pub fn update_signers(env: Env, delegated_signers: Vec<Address>) {
        for addr in delegated_signers.iter() {
            env.storage()
                .instance()
                .set(&DataKey::DelegatedSigner(addr.clone()), &true);
        }
    }

    pub fn call_delegate_account_auth(env: Env) {
        env.current_contract_address().delegate_account_auth();
    }

    pub fn call_get_delegated_signers(env: Env) {
        env.get_delegated_signers_for_current_auth_check();
    }

    pub fn auth_fn(_env: Env, address: Address, _val: u32) {
        address.require_auth();
    }

    pub fn indirect_auth(env: Env, _signature_payload: BytesN<32>) {
        env.current_contract_address().require_auth();
    }

    pub fn make_auth_fail(env: Env, fail: bool) {
        env.storage().instance().set(&DataKey::FailAuth, &fail);
    }

    #[allow(non_snake_case)]
    pub fn __check_auth(
        env: Env,
        signature_payload: BytesN<32>,
        signatures: Vec<Signature>,
        auth_context: Vec<Context>,
    ) {
        if let Some(true) = env.storage().instance().get(&DataKey::FailAuth) {
            panic!();
        }

        for signature in signatures.iter() {
            match signature {
                Signature::Payload(payload) => {
                    assert!(payload == signature_payload);
                }
                Signature::Context(ctx) => {
                    assert!(ctx == auth_context);
                }
                Signature::BuiltinDelegated(signer) => {
                    assert!(env
                        .storage()
                        .instance()
                        .has(&DataKey::DelegatedSigner(signer.clone())));
                    signer.delegate_account_auth();
                }
                Signature::ExpectedDelegatedSigners(expected) => {
                    assert!(expected == env.get_delegated_signers_for_current_auth_check());
                }
                Signature::DirectDelegated(signer) => {
                    assert!(env
                        .storage()
                        .instance()
                        .has(&DataKey::DelegatedSigner(signer.clone())));
                    signer.require_auth_for_args((&signature_payload,).into_val(&env));
                }
                Signature::TryIndirectDelegated((signer, expect_ok)) => {
                    assert!(env
                        .storage()
                        .instance()
                        .has(&DataKey::DelegatedSigner(signer.clone())));
                    let res = env.try_invoke_contract::<(), Error>(
                        &signer,
                        &Symbol::new(&env, "indirect_auth"),
                        (&signature_payload,).into_val(&env),
                    );
                    assert_eq!(res.is_ok(), expect_ok);
                }
                Signature::MakeAuthFail((address, fail)) => {
                    env.invoke_contract::<()>(
                        &address,
                        &Symbol::new(&env, "make_auth_fail"),
                        (&fail,).into_val(&env),
                    );
                }
            }
        }
    }
}
