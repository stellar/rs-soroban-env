use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};
use soroban_builtin_sdk_macros::contracttype;
use soroban_env_common::Compare;
use soroban_test_wasms::DELEGATED_AUTH_TEST_CONTRACT;

use crate::builtin_contracts::account_contract::{
    AuthorizationContext, ContractAuthorizationContext,
};
use crate::builtin_contracts::base_types::{BytesN, Vec as HostVec};
use crate::{
    builtin_contracts::base_types::Address,
    xdr::{
        HashIdPreimage, HashIdPreimageSorobanAuthorization,
        HashIdPreimageSorobanAuthorizationWithAddress, InvokeContractArgs, ScBytes, ScErrorCode,
        ScErrorType, ScVal, ScVec, SorobanAddressCredentials,
        SorobanAddressCredentialsWithDelegates, SorobanAuthorizationEntry,
        SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials,
        SorobanDelegateSignature,
    },
    Env, Host, HostError, LedgerInfo, Symbol, TryFromVal, TryIntoVal, Val,
};

#[derive(Clone)]
#[contracttype]
enum DataKey {
    DelegatedSigner(Address),
    CryptoSigner(BytesN<32>),
}

// While this is called 'Signature', it's used for testing various
// invariants of the delegated account auth, so this are not meant to resemble
// the signature types in the real custom accounts.
// See docs for `builtin_auth_delegation` contract for details on each variant.
#[derive(Clone)]
#[contracttype]
enum Signature {
    Payload(BytesN<32>),
    Context(HostVec),
    BuiltinDelegated(Address),
    ExpectedDelegatedSigners(HostVec),
    DirectDelegated(Address),
    TryIndirectDelegated((Address, bool)),
    MakeAuthFail((Address, bool)),
}

// A helper for building the auth entries for the test contract. This builds
// a correct set of auth entries for the provided signers by default, but also
// allows tweaking some build parameters to create incorrect auth entries.
struct AuthBuilder {
    host: Host,
    address: Address,
    signatures: Vec<
        Box<
            dyn Fn(
                &Host,
                &[u8; 32],
                &mut Vec<SorobanAuthorizationEntry>,
                &mut Vec<SorobanDelegateSignature>,
            ) -> Signature,
        >,
    >,

    has_builtin_delegated_signers: bool,
    // Normally we want to shuffle signatures for better coverage, but some
    // tests need a stable order to test rollbacks.
    preserve_signature_order: bool,
    // Various flags that allow building incorrect or customized auth entries
    // for the error testing.
    use_preimage_with_address: Option<bool>,
    force_credential_with_delegates: bool,
    shuffle_builtin_delegated_signers: bool,
}

impl AuthBuilder {
    fn new(host: &Host, address: &Address) -> Self {
        Self {
            host: host.clone(),
            address: address.clone(),
            signatures: vec![],
            has_builtin_delegated_signers: false,
            use_preimage_with_address: None,
            force_credential_with_delegates: false,
            shuffle_builtin_delegated_signers: false,
            preserve_signature_order: false,
        }
    }

    fn expect_valid_payload(mut self) -> Self {
        self.signatures.push(Box::new(|host, payload, _, _| {
            Signature::Payload(BytesN::<32>::from_slice(host, payload).unwrap())
        }));
        self
    }

    fn add_invalid_payload(mut self) -> Self {
        self.signatures.push(Box::new(|host, _, _, _| {
            let mut bad_payload = [0u8; 32];
            host.with_test_prng(|rng| {
                rng.fill(&mut bad_payload);
                Ok(())
            })
            .unwrap();
            Signature::Payload(BytesN::<32>::from_slice(host, &bad_payload).unwrap())
        }));
        self
    }

    fn expect_context(mut self, context: Vec<AuthorizationContext>) -> Self {
        self.signatures.push(Box::new(move |host, _, _, _| {
            let ctx = HostVec::from_t_slice(host, &context).unwrap();
            Signature::Context(ctx)
        }));
        self
    }

    fn add_builtin_delegated_sig(mut self, address: &Address) -> Self {
        let address = address.clone();
        self.signatures.push(Box::new(move |_, _, _, _| {
            Signature::BuiltinDelegated(address.clone())
        }));
        self
    }

    fn preserve_signature_order(mut self) -> Self {
        self.preserve_signature_order = true;
        self
    }

    fn sign_builtin_delegated(mut self, auth_builder: AuthBuilder) -> Self {
        self.has_builtin_delegated_signers = true;
        self.signatures
            .push(Box::new(move |_, payload, auth_entries, delegates| {
                let (signature, nested_delegates, nested_auth) = auth_builder.sign(payload);
                delegates.push(SorobanDelegateSignature {
                    address: auth_builder.address.to_sc_address().unwrap(),
                    signature,
                    nested_delegates: nested_delegates.try_into().unwrap(),
                });
                auth_entries.extend_from_slice(&nested_auth);
                Signature::BuiltinDelegated(auth_builder.address.clone())
            }));
        self
    }

    fn sign_direct_delegated(mut self, auth_builder: AuthBuilder) -> Self {
        let address = self.address.clone();
        self.signatures
            .push(Box::new(move |_, payload, auth_entries, _| {
                let additional_auth = auth_builder.build_auth_entries(
                    &address,
                    "__check_auth",
                    &[ScVal::Bytes(ScBytes(
                        payload.as_slice().try_into().unwrap(),
                    ))],
                );
                auth_entries.extend_from_slice(&additional_auth);
                Signature::DirectDelegated(auth_builder.address.clone())
            }));
        self
    }

    fn sign_try_indirect_delegated(mut self, auth_builder: AuthBuilder, expect_ok: bool) -> Self {
        self.signatures
            .push(Box::new(move |_, payload, auth_entries, _| {
                let nested_auth = auth_builder.build_auth_entries(
                    &auth_builder.address,
                    "indirect_auth",
                    &[ScVal::Bytes(ScBytes(
                        payload.as_slice().try_into().unwrap(),
                    ))],
                );
                auth_entries.extend_from_slice(&nested_auth);
                Signature::TryIndirectDelegated((auth_builder.address.clone(), expect_ok))
            }));
        self
    }

    fn expect_delegated_signers(mut self, signers: &[&Address]) -> Self {
        let mut signers: Vec<Address> = signers.iter().map(|s| (*s).clone()).collect();
        signers.sort_by(|a, b| self.host.compare(&a.as_object(), &b.as_object()).unwrap());

        self.signatures.push(Box::new(move |host, _, _, _| {
            let signers = HostVec::from_t_slice(host, &signers).unwrap();
            Signature::ExpectedDelegatedSigners(signers)
        }));
        self
    }

    fn make_auth_fail(mut self, address: &Address, fail: bool) -> Self {
        let address = address.clone();
        self.signatures.push(Box::new(move |_, _, _, _| {
            Signature::MakeAuthFail((address.clone(), fail))
        }));
        self
    }

    fn force_preimage_with_address(mut self, use_preimage_with_address: bool) -> Self {
        self.use_preimage_with_address = Some(use_preimage_with_address);
        self
    }

    fn force_credential_with_delegates(mut self) -> Self {
        self.force_credential_with_delegates = true;
        self
    }

    fn shuffle_builtin_delegated_signers(mut self) -> Self {
        self.shuffle_builtin_delegated_signers = true;
        self
    }

    fn build_top_level(
        &self,
        contract_address: &Address,
        arg: u32,
    ) -> Vec<SorobanAuthorizationEntry> {
        let mut auth_entries = self.build_auth_entries(
            contract_address,
            "auth_fn",
            &[
                ScVal::Address(self.address.to_sc_address().unwrap()),
                arg.into(),
            ],
        );
        self.host
            .with_test_prng(|rng| {
                auth_entries.shuffle(rng);
                Ok(())
            })
            .unwrap();
        auth_entries
    }

    fn build_auth_entries(
        &self,
        contract_address: &Address,
        fn_name: &str,
        args: &[ScVal],
    ) -> Vec<SorobanAuthorizationEntry> {
        let root_invocation = SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                contract_address: contract_address.to_sc_address().unwrap(),
                function_name: fn_name.try_into().unwrap(),
                args: args.try_into().unwrap(),
            }),
            sub_invocations: Default::default(),
        };
        let nonce: i64 = self.host.with_test_prng(|rng| Ok(rng.gen())).unwrap();
        let signature_expiration_ledger = 1000;
        let network_id = self
            .host
            .with_ledger_info(|li: &LedgerInfo| Ok(li.network_id))
            .unwrap()
            .try_into()
            .unwrap();
        let use_preimage_with_address = if let Some(v) = self.use_preimage_with_address {
            v
        } else {
            self.has_builtin_delegated_signers
        };
        let signature_payload_preimage = if use_preimage_with_address {
            HashIdPreimage::SorobanAuthorizationWithAddress(
                HashIdPreimageSorobanAuthorizationWithAddress {
                    network_id,
                    invocation: root_invocation.clone(),
                    address: self.address.to_sc_address().unwrap(),
                    nonce,
                    signature_expiration_ledger,
                },
            )
        } else {
            HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                network_id,
                invocation: root_invocation.clone(),
                nonce,
                signature_expiration_ledger,
            })
        };
        let signature_payload = self
            .host
            .metered_hash_xdr(&signature_payload_preimage)
            .unwrap();
        let (signature, delegates, mut auth_entries) = self.sign(&signature_payload);

        let credentials = SorobanAddressCredentials {
            address: self.address.to_sc_address().unwrap(),
            nonce,
            signature,
            signature_expiration_ledger: 1000,
        };
        let curr_auth_entry = if delegates.is_empty() && !self.force_credential_with_delegates {
            SorobanAuthorizationEntry {
                credentials: SorobanCredentials::Address(credentials),
                root_invocation,
            }
        } else {
            SorobanAuthorizationEntry {
                credentials: SorobanCredentials::AddressWithDelegates(
                    SorobanAddressCredentialsWithDelegates {
                        address_credentials: credentials,
                        delegates: delegates.try_into().unwrap(),
                    },
                ),
                root_invocation,
            }
        };
        auth_entries.push(curr_auth_entry);

        auth_entries
    }

    fn sign(
        &self,
        payload: &[u8; 32],
    ) -> (
        ScVal,
        Vec<SorobanDelegateSignature>,
        Vec<SorobanAuthorizationEntry>,
    ) {
        let mut signatures = vec![];
        let mut auth_entries = vec![];
        let mut builtin_delegated_signers = vec![];
        for s in &self.signatures {
            signatures.push((*s)(
                &self.host,
                payload,
                &mut auth_entries,
                &mut builtin_delegated_signers,
            ));
        }
        let mut signature_vals: Vec<Val> = signatures
            .iter()
            .map(|s| s.try_into_val(&self.host).unwrap())
            .collect();
        if !self.preserve_signature_order {
            self.host
                .with_test_prng(|rng| {
                    signature_vals.shuffle(rng);
                    Ok(())
                })
                .unwrap();
        }
        let signature = ScVal::Vec(Some(ScVec(
            self.host
                .vals_to_scval_vec(signature_vals.as_slice())
                .unwrap(),
        )));

        builtin_delegated_signers.sort_by(|a, b| a.address.cmp(&b.address));
        if self.shuffle_builtin_delegated_signers {
            let len = builtin_delegated_signers.len();
            assert!(len >= 2);
            builtin_delegated_signers.swap(len - 2, len - 1);
        }
        (signature, builtin_delegated_signers, auth_entries)
    }
}

struct DelegatedAuthTest {
    host: Host,
    contracts: Vec<Address>,
}

const AUTH_FN_ARG: u32 = 123;

impl DelegatedAuthTest {
    fn setup(contract_cnt: usize) -> Self {
        // These tests don't use observations, so we use a unique rng seed for
        // each test run to get more coverage.
        let mut seed = [0u8; 32];
        thread_rng().fill(&mut seed);
        let host = Host::test_host_with_recording_footprint();
        dbg!(&host.get_ledger_protocol_version());
        host.set_base_prng_seed(seed).unwrap();
        host.enable_invocation_metering();
        host.enable_debug().unwrap();

        host.with_mut_ledger_info(|li| {
            li.sequence_number = 100;
            li.max_entry_ttl = 10000;
        })
        .unwrap();

        let mut contracts = vec![];
        for _ in 0..contract_cnt {
            let contract_id_obj = host.register_test_contract_wasm(DELEGATED_AUTH_TEST_CONTRACT);
            contracts.push(contract_id_obj.try_into_val(&host).unwrap());
        }

        Self { host, contracts }
    }

    fn new_auth_builder(&self, contract_id: usize) -> AuthBuilder {
        AuthBuilder::new(&self.host, &self.contracts[contract_id])
    }

    fn update_delegated_signers(&self, contract_id: usize, delegated_signers: &[usize]) {
        let delegated = delegated_signers
            .iter()
            .map(|i| self.contracts[*i].clone())
            .collect::<Vec<_>>();
        let delegated = HostVec::from_t_slice(&self.host, &delegated).unwrap();
        self.host
            .call(
                self.contracts[contract_id].as_object(),
                Symbol::try_from_val(&self.host, &"update_signers").unwrap(),
                test_vec![&self.host, delegated].into(),
            )
            .unwrap();
    }

    fn call_delegate_account_auth(&self, contract_id: usize) -> Result<Val, HostError> {
        self.host.call(
            self.contracts[contract_id].as_object(),
            Symbol::try_from_val(&self.host, &"call_delegate_account_auth").unwrap(),
            test_vec![&self.host].into(),
        )
    }

    fn call_get_delegated_signers(&self, contract_id: usize) -> Result<Val, HostError> {
        self.host.call(
            self.contracts[contract_id].as_object(),
            Symbol::try_from_val(&self.host, &"call_get_delegated_signers").unwrap(),
            test_vec![&self.host].into(),
        )
    }

    fn call_auth_fn(&self, auth_builder: AuthBuilder) -> Result<Val, HostError> {
        let auth_entries = auth_builder.build_top_level(&self.contracts[0], AUTH_FN_ARG);
        // Some tests create invalid auth entries, we want to assert on that
        // as well, so we don't unwrap here.
        self.host.set_authorization_entries(auth_entries)?;
        let res = self.host.call(
            self.contracts[0].as_object(),
            Symbol::try_from_val(&self.host, &"auth_fn").unwrap(),
            test_vec![&self.host, &auth_builder.address, AUTH_FN_ARG].into(),
        );
        res
    }

    fn auth_fn_context(&self, arg_contract_id: usize, arg: u32) -> Vec<AuthorizationContext> {
        vec![AuthorizationContext::Contract(
            ContractAuthorizationContext {
                contract: self.contracts[0].clone(),
                fn_name: Symbol::try_from_val(&self.host, &"auth_fn").unwrap(),
                args: test_vec![&self.host, &self.contracts[arg_contract_id], arg],
            },
        )]
    }
}

#[test]
fn test_delegated_auth_functions_not_callable_outside_check_auth() {
    let test = DelegatedAuthTest::setup(1);
    assert!(HostError::result_matches_err(
        test.call_delegate_account_auth(0),
        (ScErrorType::Context, ScErrorCode::InvalidAction)
    ));
    assert!(HostError::result_matches_err(
        test.call_get_delegated_signers(0),
        (ScErrorType::Context, ScErrorCode::InvalidAction)
    ))
}

#[test]
fn test_credential_with_delegates_uses_preimage_with_address() {
    let test = DelegatedAuthTest::setup(3);
    test.update_delegated_signers(1, &[2]);

    // Success scenarios
    // No delegates, use the no-delegates credential and the preimage without address.
    assert!(test
        .call_auth_fn(test.new_auth_builder(1).expect_valid_payload())
        .is_ok());
    // No delegates, but use the credentials with delegates and the preimage
    // with address.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .force_credential_with_delegates()
                .force_preimage_with_address(true)
                .expect_valid_payload()
        )
        .is_ok());
    // // One delegate, use the credential with delegates and the preimage with
    // // address.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .expect_valid_payload()
                .sign_builtin_delegated(test.new_auth_builder(2))
        )
        .is_ok());

    // Failure scenarios
    // No delegates, use the no-delegates credential and the preimage with
    // address.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .force_preimage_with_address(true)
                .expect_valid_payload()
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // No delegates, use the address with delegates credential and the preimage
    // without address.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .force_credential_with_delegates()
                .force_preimage_with_address(false)
                .expect_valid_payload()
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // 1 delegate, use the address with delegates credential and the preimage
    // without address.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .force_preimage_with_address(false)
                .expect_valid_payload()
                .sign_builtin_delegated(test.new_auth_builder(2))
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_duplicate_and_unsorted_delegated_signers_are_not_allowed() {
    let test = DelegatedAuthTest::setup(7);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4]);
    test.update_delegated_signers(4, &[5, 6]);
    // Make sure the base test setup works for the full valid signer tree.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2).sign_builtin_delegated(
                        test.new_auth_builder(4)
                            .sign_builtin_delegated(test.new_auth_builder(5))
                            .sign_builtin_delegated(test.new_auth_builder(6))
                    )
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
        )
        .is_ok());
    // Just have the top-level signer twice.
    // Note, that here and in all the other failure cases below, the error is
    // `InvalidInput` because the duplicate is detected during parsing the auth
    // entries before running the contract function itself.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(2))
                .sign_builtin_delegated(test.new_auth_builder(2)) // duplicate
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));

    // Add a duplicate delegate signer at the top level with other tree nodes
    // present.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2).sign_builtin_delegated(
                        test.new_auth_builder(4)
                            .sign_builtin_delegated(test.new_auth_builder(5))
                            .sign_builtin_delegated(test.new_auth_builder(6))
                    )
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
                .sign_builtin_delegated(test.new_auth_builder(2)) // duplicate
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));
    // Add a duplicate delegate signer at a nested level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(3))
                .sign_builtin_delegated(
                    test.new_auth_builder(2).sign_builtin_delegated(
                        test.new_auth_builder(4)
                            .sign_builtin_delegated(test.new_auth_builder(5))
                            .sign_builtin_delegated(test.new_auth_builder(6))
                            .sign_builtin_delegated(test.new_auth_builder(5)) // duplicate
                    )
                )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));

    // Incorrect order of delegate signers at the top level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(2))
                .sign_builtin_delegated(test.new_auth_builder(3))
                .shuffle_builtin_delegated_signers()
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));

    // Incorrect order of delegate signers at the top level with other
    // nested signers present.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2).sign_builtin_delegated(
                        test.new_auth_builder(4)
                            .sign_builtin_delegated(test.new_auth_builder(5))
                            .sign_builtin_delegated(test.new_auth_builder(6))
                    )
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
                .shuffle_builtin_delegated_signers()
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));

    // Incorrect order of delegate signers at a nested level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(3))
                .sign_builtin_delegated(
                    test.new_auth_builder(2).sign_builtin_delegated(
                        test.new_auth_builder(4)
                            .sign_builtin_delegated(test.new_auth_builder(5))
                            .sign_builtin_delegated(test.new_auth_builder(6))
                            .shuffle_builtin_delegated_signers()
                    )
                )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidInput)
    ));
}

#[test]
fn test_signature_payload_forwarded_to_delegated_signers() {
    let test = DelegatedAuthTest::setup(6);
    test.update_delegated_signers(1, &[2]);
    test.update_delegated_signers(2, &[3, 4]);
    test.update_delegated_signers(3, &[5]);
    test.update_delegated_signers(4, &[]);
    test.update_delegated_signers(5, &[]);

    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .expect_valid_payload()
                .sign_builtin_delegated(test.new_auth_builder(2).expect_valid_payload())
        )
        .is_ok());
    // Make sure that invalid payload in delegated signer results in
    // auth failure (i.e. that payload verification actually works).
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(2).add_invalid_payload())
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Test with a tree of delegated signers.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .expect_valid_payload()
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .expect_valid_payload()
                        .sign_builtin_delegated(
                            test.new_auth_builder(3)
                                .expect_valid_payload()
                                .sign_builtin_delegated(
                                    test.new_auth_builder(5).expect_valid_payload()
                                )
                        )
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                )
        )
        .is_ok());

    // Expect invalid payload in the deepest delegated signer.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .expect_valid_payload()
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .expect_valid_payload()
                        .sign_builtin_delegated(
                            test.new_auth_builder(3)
                                .expect_valid_payload()
                                .sign_builtin_delegated(
                                    test.new_auth_builder(5).add_invalid_payload()
                                )
                        )
                )
                .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_context_forwarded_to_delegated_signers() {
    let test = DelegatedAuthTest::setup(7);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4]);
    test.update_delegated_signers(4, &[5, 6]);

    let auth_fn_context = test.auth_fn_context(1, AUTH_FN_ARG);
    // Success scenarios
    // Single delegated signer
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .expect_context(auth_fn_context.clone())
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .expect_context(auth_fn_context.clone())
                )
        )
        .is_ok());
    // Tree of delegated signers
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .expect_context(auth_fn_context.clone())
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .expect_context(auth_fn_context.clone())
                        .sign_builtin_delegated(
                            test.new_auth_builder(4)
                                .expect_context(auth_fn_context.clone())
                                .sign_builtin_delegated(
                                    test.new_auth_builder(5)
                                        .expect_context(auth_fn_context.clone())
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(6)
                                        .expect_context(auth_fn_context.clone())
                                )
                        )
                )
                .sign_builtin_delegated(
                    test.new_auth_builder(3)
                        .expect_context(auth_fn_context.clone())
                )
        )
        .is_ok());

    // Failure scenarios
    let mut bad_contexts = vec![
        // Wrong number of context entries (populated below)
        auth_fn_context.clone(),
        // Wrong argument
        test.auth_fn_context(1, AUTH_FN_ARG + 1),
        // Wrong contract
        test.auth_fn_context(0, AUTH_FN_ARG),
    ];
    bad_contexts[0].push(auth_fn_context[0].clone());

    for bad_context in &bad_contexts {
        // Wrong argument in the context in the direct delegated signer
        assert!(HostError::result_matches_err(
            test.call_auth_fn(
                test.new_auth_builder(1)
                    .expect_context(auth_fn_context.clone())
                    .sign_builtin_delegated(
                        test.new_auth_builder(2).expect_context(bad_context.clone())
                    )
            ),
            (ScErrorType::Auth, ScErrorCode::InvalidAction)
        ));
        // Wrong argument in a nested delegated signer
        assert!(HostError::result_matches_err(
            test.call_auth_fn(
                test.new_auth_builder(1)
                    .expect_context(auth_fn_context.clone())
                    .sign_builtin_delegated(
                        test.new_auth_builder(2)
                            .expect_context(auth_fn_context.clone())
                            .sign_builtin_delegated(
                                test.new_auth_builder(4).expect_context(bad_context.clone())
                            )
                    )
            ),
            (ScErrorType::Auth, ScErrorCode::InvalidAction)
        ));
    }
}

#[test]
fn test_builtin_delegated_signer_reentrance_not_allowed() {
    let test = DelegatedAuthTest::setup(5);
    // Allow 'self' as delegated signer in the contract configuration, that's
    // not disallowed by the contract.
    test.update_delegated_signers(1, &[1, 2]);
    test.update_delegated_signers(2, &[1, 2, 3, 4]);

    // Make sure we can still use non-reentrant delegated signers.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(3))
                    .sign_builtin_delegated(test.new_auth_builder(4))
            )
        )
        .is_ok());

    // Self re-entrance of the top-level delegated signer.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(1)) // Re-entrance
                .sign_builtin_delegated(test.new_auth_builder(2))
        ),
        // Note, that since the top-level error comes from the require_auth,
        // it's got to be the auth error, even though reentrance is a context
        // error by itself (it's just logged in the diagnostics).
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // Self re-entrance of a nested delegated signer.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(2)) // Re-entrance
                    .sign_builtin_delegated(test.new_auth_builder(3))
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Indirect re-entrance of the top-level delegated signer.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(1)) // Re-entrance to 1
                    .sign_builtin_delegated(test.new_auth_builder(3))
                    .sign_builtin_delegated(test.new_auth_builder(4))
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Re-entrance into top-level contract via `require_auth` delegation.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_direct_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(1)) // Re-entrance to 1
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Re-entrance into inner contract via `require_auth` delegation.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_direct_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(2)) // Re-entrance to 2
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Re-entrance via indirect `require_auth` delegation (via an additional
    // contract call)
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1).sign_try_indirect_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(1)), // Re-entrance to 1
                // Expect failure of this particular call, but the overall
                // invocation still succeeds.
                false
            )
        )
        .is_ok());
}

#[test]
fn test_builtin_delegated_signers_must_be_present_in_payload() {
    let test = DelegatedAuthTest::setup(6);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4, 5]);

    // Success scenario with all required delegated signers present.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4))
                        .sign_builtin_delegated(test.new_auth_builder(5))
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
        )
        .is_ok());

    // Request a delegated signer that is not present in the payload at the
    // top-level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .add_builtin_delegated_sig(&test.contracts[2]) // Not present in payload
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Request a delegated signer that is not present in the payload at the
    // top-level with other valid signers present.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4))
                        .sign_builtin_delegated(test.new_auth_builder(5))
                )
                .add_builtin_delegated_sig(&test.contracts[3]) // Not present in payload
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Request a delegated signer that is not present in the payload at a
    // nested level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .add_builtin_delegated_sig(&test.contracts[4]) // Not present in payload
                    .sign_builtin_delegated(test.new_auth_builder(5))
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_delegate_account_auth_is_not_reused() {
    let test = DelegatedAuthTest::setup(6);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4, 5]);

    // Reuse top-level delegate
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4))
                        .sign_builtin_delegated(test.new_auth_builder(5))
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
                .add_builtin_delegated_sig(&test.contracts[3]) // Reuse delegate 3
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    // Reuse inner delegate
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4))
                        .sign_builtin_delegated(test.new_auth_builder(5))
                        .add_builtin_delegated_sig(&test.contracts[4]) // Reuse delegate 4
                )
                .sign_builtin_delegated(test.new_auth_builder(3))
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_delegated_signers_getter() {
    let test = DelegatedAuthTest::setup(12);
    test.update_delegated_signers(1, &[2, 3, 4]);
    test.update_delegated_signers(2, &[5, 6]);
    test.update_delegated_signers(5, &[7]);
    test.update_delegated_signers(6, &[8, 9, 10, 11]);
    // No delegated signers
    assert!(test
        .call_auth_fn(test.new_auth_builder(1).expect_delegated_signers(&[]))
        .is_ok());
    // Some valid delegated signers at different tree levels
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(
                            test.new_auth_builder(5).expect_delegated_signers(&[],)
                        )
                        .sign_builtin_delegated(
                            test.new_auth_builder(6)
                                .sign_builtin_delegated(
                                    test.new_auth_builder(8).expect_delegated_signers(&[])
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(10).expect_delegated_signers(&[])
                                )
                                .expect_delegated_signers(&[
                                    &test.contracts[8],
                                    &test.contracts[10],
                                ])
                        )
                )
                .sign_builtin_delegated(test.new_auth_builder(4).expect_delegated_signers(&[]))
                .expect_delegated_signers(&[&test.contracts[2], &test.contracts[4],])
        )
        .is_ok());

    // Check that getters works at all levels with maximum valid tree.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(
                            test.new_auth_builder(5)
                                .sign_builtin_delegated(
                                    test.new_auth_builder(7).expect_delegated_signers(&[])
                                )
                                .expect_delegated_signers(&[&test.contracts[7]],)
                        )
                        .sign_builtin_delegated(
                            test.new_auth_builder(6)
                                .sign_builtin_delegated(
                                    test.new_auth_builder(8).expect_delegated_signers(&[])
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(9).expect_delegated_signers(&[])
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(10).expect_delegated_signers(&[])
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(11).expect_delegated_signers(&[])
                                )
                                .expect_delegated_signers(&[
                                    &test.contracts[8],
                                    &test.contracts[9],
                                    &test.contracts[10],
                                    &test.contracts[11],
                                ])
                        )
                )
                .sign_builtin_delegated(test.new_auth_builder(3).expect_delegated_signers(&[]))
                .sign_builtin_delegated(test.new_auth_builder(4).expect_delegated_signers(&[]))
                .expect_delegated_signers(&[
                    &test.contracts[2],
                    &test.contracts[3],
                    &test.contracts[4],
                ])
        )
        .is_ok());

    // Make sure we're actually verifying the expected delegated signers.
    // No actual delegated signers, but some expected.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .expect_delegated_signers(&[&test.contracts[2]])
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // Some actual delegated signers, but none expected.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(2))
                .expect_delegated_signers(&[])
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // Mismatched expected delegated signers.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_builtin_delegated(test.new_auth_builder(2))
                .expect_delegated_signers(&[&test.contracts[3]])
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    // Mismatched expected delegated signers at a nested level.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .sign_builtin_delegated(test.new_auth_builder(5))
                    .sign_builtin_delegated(
                        test.new_auth_builder(6)
                            .expect_delegated_signers(&[&test.contracts[6], &test.contracts[6]],)
                    )
            ),
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_builtin_delegates_within_nested_auth() {
    let test = DelegatedAuthTest::setup(9);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4, 5]);
    test.update_delegated_signers(5, &[6]);
    test.update_delegated_signers(6, &[7, 8]);

    // Direct `require_auth` with a single delegate
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_direct_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                        .expect_valid_payload()
                )
                .expect_valid_payload()
        )
        .is_ok());

    // Direct `require_auth` with delegates at the top level with more nested delegates
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .sign_direct_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                        .sign_builtin_delegated(
                            test.new_auth_builder(5).sign_builtin_delegated(
                                test.new_auth_builder(6)
                                    .sign_builtin_delegated(
                                        test.new_auth_builder(7).expect_valid_payload()
                                    )
                                    .sign_builtin_delegated(
                                        test.new_auth_builder(8).expect_valid_payload()
                                    )
                            )
                        )
                        .expect_valid_payload()
                )
                .sign_builtin_delegated(test.new_auth_builder(3).expect_valid_payload())
        )
        .is_ok());

    // Built-in delegate calls `require_auth` with its own built-in delegate
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .sign_direct_delegated(
                        test.new_auth_builder(5)
                            .sign_builtin_delegated(
                                test.new_auth_builder(6)
                                    .sign_builtin_delegated(
                                        test.new_auth_builder(7).expect_valid_payload()
                                    )
                                    .sign_builtin_delegated(
                                        test.new_auth_builder(8).expect_valid_payload()
                                    )
                            )
                            .expect_valid_payload()
                    )
                    .expect_valid_payload()
            )
        )
        .is_ok());

    // Double `require_auth` nesting with built-in delegates
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1).sign_direct_delegated(
                test.new_auth_builder(2).sign_direct_delegated(
                    test.new_auth_builder(5)
                        .sign_direct_delegated(
                            test.new_auth_builder(6)
                                .sign_builtin_delegated(
                                    test.new_auth_builder(7).expect_valid_payload()
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(8).expect_valid_payload()
                                )
                                .expect_valid_payload()
                        )
                        .expect_valid_payload()
                )
            )
        )
        .is_ok());

    // Add invalid payload to some of the trees to make sure that auth is
    // actually enforced.
    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1)
                .sign_direct_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                        .add_invalid_payload()
                )
                .expect_valid_payload()
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));

    assert!(HostError::result_matches_err(
        test.call_auth_fn(
            test.new_auth_builder(1).sign_direct_delegated(
                test.new_auth_builder(2).sign_direct_delegated(
                    test.new_auth_builder(5)
                        .sign_direct_delegated(
                            test.new_auth_builder(6)
                                .sign_builtin_delegated(
                                    test.new_auth_builder(7).expect_valid_payload()
                                )
                                .sign_builtin_delegated(
                                    test.new_auth_builder(8).add_invalid_payload()
                                )
                                .expect_valid_payload()
                        )
                        .expect_valid_payload()
                )
            )
        ),
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_builtin_delegates_within_nested_auth_and_rollbacks() {
    let test = DelegatedAuthTest::setup(9);
    test.update_delegated_signers(1, &[2, 3]);
    test.update_delegated_signers(2, &[4, 5]);
    test.update_delegated_signers(5, &[6]);
    test.update_delegated_signers(6, &[7, 8]);

    // Fail the first indirect call with built-in delegates, succeed the second
    // one.
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .preserve_signature_order()
                .make_auth_fail(&test.contracts[2], true)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4)),
                    false
                )
                .make_auth_fail(&test.contracts[2], false)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                        .expect_valid_payload(),
                    true
                )
                .expect_valid_payload()
        )
        .is_ok());

    // Like the previous scenario, but fail the inner delegate
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .preserve_signature_order()
                .make_auth_fail(&test.contracts[4], true)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4)),
                    false
                )
                .make_auth_fail(&test.contracts[4], false)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .sign_builtin_delegated(test.new_auth_builder(4).expect_valid_payload())
                        .expect_valid_payload(),
                    true
                )
                .expect_valid_payload()
        )
        .is_ok());

    // Fail a deep call
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1)
                .preserve_signature_order()
                .make_auth_fail(&test.contracts[8], true)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .preserve_signature_order()
                        .expect_valid_payload()
                        .sign_direct_delegated(
                            test.new_auth_builder(5)
                                .preserve_signature_order()
                                .expect_valid_payload()
                                .sign_builtin_delegated(
                                    test.new_auth_builder(6)
                                        .preserve_signature_order()
                                        .sign_builtin_delegated(
                                            test.new_auth_builder(7).expect_valid_payload()
                                        )
                                        .sign_builtin_delegated(
                                            test.new_auth_builder(8).expect_valid_payload()
                                        )
                                )
                        ),
                    false
                )
                .make_auth_fail(&test.contracts[8], false)
                .sign_try_indirect_delegated(
                    test.new_auth_builder(2)
                        .preserve_signature_order()
                        .expect_valid_payload()
                        .sign_direct_delegated(
                            test.new_auth_builder(5)
                                .preserve_signature_order()
                                .expect_valid_payload()
                                .sign_builtin_delegated(
                                    test.new_auth_builder(6)
                                        .preserve_signature_order()
                                        .sign_builtin_delegated(
                                            test.new_auth_builder(7).expect_valid_payload()
                                        )
                                        .sign_builtin_delegated(
                                            test.new_auth_builder(8).expect_valid_payload()
                                        )
                                )
                        ),
                    true
                )
                .expect_valid_payload()
        )
        .is_ok());

    // Fail an indirect call delegate within a built-in delegate
    assert!(test
        .call_auth_fn(
            test.new_auth_builder(1).sign_builtin_delegated(
                test.new_auth_builder(2)
                    .preserve_signature_order()
                    .expect_valid_payload()
                    .make_auth_fail(&test.contracts[7], true)
                    .sign_try_indirect_delegated(
                        test.new_auth_builder(5)
                            .preserve_signature_order()
                            .expect_valid_payload()
                            .sign_builtin_delegated(
                                test.new_auth_builder(6)
                                    .sign_builtin_delegated(test.new_auth_builder(7))
                            ),
                        false
                    )
                    .make_auth_fail(&test.contracts[7], false)
                    .sign_try_indirect_delegated(
                        test.new_auth_builder(5)
                            .expect_valid_payload()
                            .sign_builtin_delegated(
                                test.new_auth_builder(6)
                                    .sign_builtin_delegated(
                                        test.new_auth_builder(7).expect_valid_payload()
                                    )
                                    .expect_valid_payload()
                            ),
                        true
                    )
            )
        )
        .is_ok());
}
