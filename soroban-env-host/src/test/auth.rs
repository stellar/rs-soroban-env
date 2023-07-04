use ed25519_dalek::Keypair;
use rand::{thread_rng, Rng};
use soroban_env_common::xdr::{
    AccountId, ContractDataDurability, HashIdPreimage, HashIdPreimageSorobanAuthorization,
    PublicKey, ScAddress, ScNonceKey, ScSymbol, ScVal, ScVec, SorobanAddressCredentials,
    SorobanAuthorizationEntry, SorobanAuthorizedContractFunction, SorobanAuthorizedFunction,
    SorobanAuthorizedInvocation, SorobanCredentials, Uint256,
};
use soroban_native_sdk_macros::contracttype;
use soroban_test_wasms::AUTH_TEST_CONTRACT;

use crate::auth::RecordedAuthPayload;
use crate::budget::AsBudget;
use crate::native_contract::base_types::Address;
use crate::native_contract::testutils::{
    create_account, generate_keypair, sign_payload_for_account,
};
use crate::{host_vec, Host, LedgerInfo};
use soroban_env_common::{AddressObject, Env, Symbol, SymbolStr, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::Vec as HostVec;

#[derive(Clone)]
#[contracttype]
pub struct ContractTreeNode {
    pub contract: Address,
    pub need_auth: HostVec,
    pub children: HostVec,
}

struct AuthTest {
    host: Host,
    keys: Vec<Keypair>,
    contracts: Vec<Address>,
    last_nonces: Vec<Vec<i64>>,
}

struct SetupNode {
    contract_address: Address,
    need_auth: Vec<bool>,
    children: Vec<SetupNode>,
}

struct SignNode {
    contract_address: Address,
    fn_name: Symbol,
    args: ScVec,
    children: Vec<SignNode>,
}

impl SetupNode {
    fn new(contract_address: &Address, need_auth: Vec<bool>, children: Vec<SetupNode>) -> Self {
        Self {
            contract_address: contract_address.clone(),
            need_auth,
            children,
        }
    }
}

impl SignNode {
    fn new(
        contract_address: &Address,
        fn_name: Symbol,
        args: ScVec,
        children: Vec<SignNode>,
    ) -> Self {
        Self {
            contract_address: contract_address.clone(),
            fn_name,
            args,
            children,
        }
    }

    fn tree_fn(contract_id: &Address, children: Vec<SignNode>) -> Self {
        Self {
            contract_address: contract_id.clone(),
            children,
            fn_name: Symbol::try_from_small_str("tree_fn").unwrap(),
            args: ScVec::default(),
        }
    }
}

// This test uses `AuthContract::tree_fn` to build various authorization trees.
impl AuthTest {
    fn setup(signer_cnt: usize, contract_cnt: usize) -> Self {
        let host = Host::test_host_with_recording_footprint();
        // TODO: remove the `reset_unlimited` and instead reset inputs wherever appropriate
        // to respect the budget limit.
        host.as_budget().reset_unlimited().unwrap();
        host.enable_debug().unwrap();

        host.with_mut_ledger_info(|li| {
            li.sequence_number = 100;
            li.max_entry_expiration = 10000;
        })
        .unwrap();
        let mut accounts = vec![];
        for _ in 0..signer_cnt {
            accounts.push(generate_keypair());
        }
        for account in &accounts {
            create_account(
                &host,
                &AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
                    account.public.to_bytes(),
                ))),
                vec![(&account, 1)],
                100_000_000,
                1,
                [1, 0, 0, 0],
                None,
                None,
                0,
            )
        }
        let mut contracts = vec![];
        for _ in 0..contract_cnt {
            let contract_id_obj = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);
            contracts.push(contract_id_obj.try_into_val(&host).unwrap());
        }
        Self {
            host,
            keys: accounts,
            contracts,
            last_nonces: vec![],
        }
    }

    // Runs the test for the given setup in enforcing mode. `sign_payloads`
    // contain should contain a vec of `SignNode` inputs for every signer in the
    // setup. The nodes then are mapped to the respective `SorobanAuthorizationEntry` entries.
    fn tree_test_enforcing(
        &mut self,
        root: &SetupNode,
        sign_payloads: Vec<Vec<SignNode>>,
        success: bool,
    ) {
        self.test_enforcing(
            root.contract_address.clone(),
            Symbol::try_from_small_str("tree_fn").unwrap(),
            host_vec![
                &self.host,
                self.get_addresses(),
                self.convert_setup_tree(&root)
            ],
            sign_payloads,
            success,
        );
    }

    fn test_enforcing(
        &mut self,
        contract_address: Address,
        fn_name: Symbol,
        args: HostVec,
        sign_payloads: Vec<Vec<SignNode>>,
        success: bool,
    ) {
        let mut contract_auth = vec![];
        self.last_nonces.clear();
        for address_id in 0..self.keys.len() {
            let sc_address = self.key_to_sc_address(&self.keys[address_id]);
            let mut curr_nonces = vec![];
            for sign_root in &sign_payloads[address_id] {
                let nonce = thread_rng().gen_range(0, i64::MAX);
                curr_nonces.push(nonce);
                let root_invocation = self.convert_sign_node(sign_root);
                let payload_preimage =
                    HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                        network_id: self
                            .host
                            .with_ledger_info(|li: &LedgerInfo| Ok(li.network_id))
                            .unwrap()
                            .try_into()
                            .unwrap(),
                        invocation: root_invocation.clone(),
                        nonce,
                        signature_expiration_ledger: 1000,
                    });
                let payload = self.host.metered_hash_xdr(&payload_preimage).unwrap();
                let signature_args = host_vec![
                    &self.host,
                    sign_payload_for_account(&self.host, &self.keys[address_id], &payload)
                ];
                contract_auth.push(SorobanAuthorizationEntry {
                    credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                        address: sc_address.clone(),
                        nonce,
                        signature_args: self
                            .host
                            .call_args_to_scvec(signature_args.into())
                            .unwrap(),
                        signature_expiration_ledger: 1000,
                    }),
                    root_invocation,
                });
            }
            self.last_nonces.push(curr_nonces);
        }

        self.host.set_authorization_entries(contract_auth).unwrap();
        assert_eq!(
            self.host
                .call(contract_address.into(), fn_name, args.into(),)
                .is_ok(),
            success
        );
    }

    fn read_nonce_expiration(&self, address: &Address, nonce: i64) -> Option<u32> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey { nonce });
        let nonce_key = self.host.storage_key_for_address(
            address.to_sc_address().unwrap(),
            nonce_key_scval,
            ContractDataDurability::Temporary,
        );
        self.host
            .with_mut_storage(|storage| {
                if !storage.has(&nonce_key, self.host.budget_ref())? {
                    return Ok(None);
                }
                let entry = storage.get(&nonce_key, self.host.budget_ref())?;
                match &entry.data {
                    soroban_env_common::xdr::LedgerEntryData::ContractData(contract_data) => {
                        Ok(Some(contract_data.expiration_ledger_seq))
                    }
                    _ => panic!("unexpected entry"),
                }
            })
            .unwrap()
    }

    // Runs `tree_fn` corresponding to the provided setup in recordind mode and
    // returns the recorded payloads.
    fn tree_run_recording(&self, root: &SetupNode) -> Vec<RecordedAuthPayload> {
        let addresses = self.get_addresses();
        let tree = self.convert_setup_tree(&root);
        self.run_recording(
            &root.contract_address,
            Symbol::try_from_small_str("tree_fn").unwrap(),
            host_vec![&self.host, addresses, tree],
        )
    }

    fn run_recording(
        &self,
        contract_address: &Address,
        fn_name: Symbol,
        args: HostVec,
    ) -> Vec<RecordedAuthPayload> {
        self.host.switch_to_recording_auth().unwrap();
        self.host
            .call(contract_address.clone().into(), fn_name, args.into())
            .unwrap();
        self.host.get_recorded_auth_payloads().unwrap()
    }

    fn key_to_sc_address(&self, key: &Keypair) -> ScAddress {
        ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
            key.public.to_bytes(),
        ))))
    }

    fn key_to_address(&self, key: &Keypair) -> AddressObject {
        let sc_address = self.key_to_sc_address(key);
        self.host.add_host_object(sc_address).unwrap()
    }

    fn get_addresses(&self) -> HostVec {
        let mut addresses = HostVec::new(&self.host).unwrap();
        for key in &self.keys {
            addresses
                .push_val(self.key_to_address(key).to_val())
                .unwrap();
        }

        addresses
    }

    fn convert_sign_node(&self, root: &SignNode) -> SorobanAuthorizedInvocation {
        let mut sub_invocations = vec![];
        for c in &root.children {
            sub_invocations.push(self.convert_sign_node(c));
        }

        // TODO: more and better Symbol<->SymbolStr<->ScSymbol conversions.
        let function_name: SymbolStr = root.fn_name.try_into_val(&self.host).unwrap();
        let function_name: ScSymbol = ScSymbol(function_name.to_string().try_into().unwrap());

        SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(SorobanAuthorizedContractFunction {
                contract_address: root.contract_address.to_sc_address().unwrap(),
                function_name,
                args: root.args.clone(),
            }),
            sub_invocations: sub_invocations.try_into().unwrap(),
        }
    }

    fn convert_setup_tree(&self, root: &SetupNode) -> ContractTreeNode {
        let mut children = HostVec::new(&self.host).unwrap();
        for c in &root.children {
            children.push(&self.convert_setup_tree(c)).unwrap();
        }
        let mut need_auth = HostVec::new(&self.host).unwrap();
        for n in &root.need_auth {
            need_auth.push(n).unwrap();
        }
        ContractTreeNode {
            contract: root.contract_address.clone(),
            need_auth,
            children,
        }
    }

    fn verify_nonces_consumed(&self, consumed_per_address: Vec<usize>) {
        for address_id in 0..self.last_nonces.len() {
            let mut consumed = 0;
            let address: Address = self
                .key_to_address(&self.keys[address_id])
                .try_into_val(&self.host)
                .unwrap();
            for nonce in &self.last_nonces[address_id] {
                if let Some(expiration_ledger) = self.read_nonce_expiration(&address, *nonce) {
                    assert_eq!(expiration_ledger, 1000);
                    consumed += 1;
                }
            }
            assert_eq!(consumed, consumed_per_address[address_id]);
        }
    }
}

#[test]
fn test_single_authorized_call() {
    let mut test = AuthTest::setup(1, 2);
    let setup = SetupNode::new(&test.contracts[0], vec![true], vec![]);
    let expected_sign_payloads = vec![vec![SignNode::tree_fn(&test.contracts[0], vec![])]];
    assert_eq!(
        test.tree_run_recording(&setup),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[0])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&expected_sign_payloads[0][0])
        }]
    );

    // Correct call
    test.tree_test_enforcing(&setup, expected_sign_payloads, true);
    test.verify_nonces_consumed(vec![1]);

    assert_eq!(
        test.tree_run_recording(&setup),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[0])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[0], vec![]))
        }]
    );

    // Correct call with an extra top-level payload
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(&test.contracts[1], vec![]),
            SignNode::tree_fn(&test.contracts[0], vec![]),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Correct call with extra sub-contract payload
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![SignNode::tree_fn(&test.contracts[1], vec![])],
        )]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Failing scenarios
    // Nothing signed
    test.tree_test_enforcing(&setup, vec![vec![]], false);
    // Wrong contract
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(&test.contracts[1], vec![])]],
        false,
    );
    // Smoke test nonces for one failing scenario - there is really no way
    // nonces are incremented unless the rollback logic is broken.
    test.verify_nonces_consumed(vec![0]);
    // Correct contract called from a wrong contract
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[1],
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        )]],
        false,
    );
}

#[test]
fn test_single_authorized_call_for_multiple_addresses() {
    let mut test = AuthTest::setup(2, 2);
    let setup = SetupNode::new(&test.contracts[0], vec![true, true], vec![]);
    let expected_sign_payloads = vec![
        vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        vec![SignNode::tree_fn(&test.contracts[0], vec![])],
    ];
    assert_eq!(
        test.tree_run_recording(&setup),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&expected_sign_payloads[0][0])
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&expected_sign_payloads[1][0])
            }
        ]
    );

    // Correct call
    test.tree_test_enforcing(&setup, expected_sign_payloads, true);
    test.verify_nonces_consumed(vec![1, 1]);

    // Failing scenarios
    // Partially signed
    test.tree_test_enforcing(
        &setup,
        vec![vec![], vec![SignNode::tree_fn(&test.contracts[0], vec![])]],
        false,
    );
    // Wrong contract
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(&test.contracts[1], vec![])],
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        ],
        false,
    );
    // Correct contract called from a wrong contract
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(
                &test.contracts[1],
                vec![SignNode::tree_fn(&test.contracts[0], vec![])],
            )],
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        ],
        false,
    );
}

#[test]
fn test_single_authorization_for_one_address_among_multiple() {
    let mut test = AuthTest::setup(2, 2);
    let setup = SetupNode::new(&test.contracts[0], vec![false, true], vec![]);
    let expected_sign_payloads = vec![vec![], vec![SignNode::tree_fn(&test.contracts[0], vec![])]];
    assert_eq!(
        test.tree_run_recording(&setup),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[1])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&expected_sign_payloads[1][0])
        }]
    );

    // Correct call
    test.tree_test_enforcing(&setup, expected_sign_payloads, true);
    test.verify_nonces_consumed(vec![0, 1]);
    // Correct call with both signatures, but only second address auth is needed
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![0, 1]);
    // Correct call with an unused extra contract signature
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(&test.contracts[1], vec![])],
            vec![SignNode::tree_fn(&test.contracts[0], vec![])],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![0, 1]);

    // Failing scenarios
    // Wrong address signed (first instead of second)
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(&test.contracts[0], vec![])], vec![]],
        false,
    );
}

#[test]
fn test_single_authorized_call_tree() {
    let mut test = AuthTest::setup(1, 5);
    // Tree edges (everything needs to be authorized): A->(B, C), B->(D, C), C->(E, D)
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                ],
            ),
        ],
    );
    assert_eq!(
        test.tree_run_recording(&setup),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[0])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                        ],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                        ],
                    ),
                ],
            ))
        }]
    );

    // Correct call
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
            ],
        )]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Correct call with some extra authorizations
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                        ],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                        ],
                    ),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                ],
            ),
            SignNode::tree_fn(&test.contracts[0], vec![]),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Failing scenarios
    // Incorrect call due to a missing invocation
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                ),
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
        )]],
        false,
    );

    // Incorrect call due to mismatching node
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![
                SignNode::tree_fn(
                    &test.contracts[4], // Should have been 1
                    vec![
                        // Invocation order shouldn't matter here and purposely
                        // doesn't match the call tree order
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
        )]],
        false,
    );

    // Incorrect call due to extra empty root node - this is where the order
    // matters
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(&test.contracts[0], vec![]),
            SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            // Invocation order shouldn't matter here and purposely
                            // doesn't match the call tree order
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                        ],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                        ],
                    ),
                ],
            ),
        ]],
        false,
    );
}

#[test]
fn test_two_authorized_trees() {
    let mut test = AuthTest::setup(1, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                ],
            ),
        ],
    );

    assert_eq!(
        test.tree_run_recording(&setup),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[1],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                ),)
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),)
            }
        ]
    );

    // Correct call
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[1],
                vec![
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                ],
            ),
            SignNode::tree_fn(
                &test.contracts[2],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            ),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![2]);

    // Failing scenarios
    // Top-level authorization instead of 2 trees
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
        )]],
        false,
    );
}

#[test]
fn test_three_authorized_trees() {
    let mut test = AuthTest::setup(1, 5);

    let setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![false],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                ],
            ),
        ],
    );

    assert_eq!(
        test.tree_run_recording(&setup),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[3], vec![]),)
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[2], vec![]),)
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),)
            }
        ]
    );

    // Correct call
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(&test.contracts[2], vec![]),
            SignNode::tree_fn(
                &test.contracts[2],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            ),
            SignNode::tree_fn(&test.contracts[3], vec![]),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![3]);

    // Failing scenarios
    // Top-level tree instead of 3 trees
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
        )]],
        false,
    );

    // 2 trees instead of 3
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[1],
                vec![
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                ],
            ),
            SignNode::tree_fn(
                &test.contracts[2],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            ),
        ]],
        false,
    );

    // Incorrect order - this is a bit of an edge case, but if there are
    // multiple invocations that are absolutely identical, we just resolve
    // them one-by-one in the same order as they are provided in the transaction
    // and the order here is different from the actual call order (contract 2
    // without sub-invocations happens *before* the contract 2 call with
    // sub-invocations, while the payload comes *after* it).
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[2],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            ),
            SignNode::tree_fn(&test.contracts[2], vec![]),
            SignNode::tree_fn(&test.contracts[3], vec![]),
        ]],
        false,
    );
}

#[test]
fn test_multi_address_trees() {
    let mut test = AuthTest::setup(3, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true, false, true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true, true, false],
                vec![
                    SetupNode::new(&test.contracts[3], vec![false, true, true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true, false, false], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true, true, false],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true, false, true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true, true, true], vec![]),
                ],
            ),
        ],
    );

    assert_eq!(
        test.tree_run_recording(&setup),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(
                            &test.contracts[1],
                            vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                        ),
                        SignNode::tree_fn(
                            &test.contracts[2],
                            vec![
                                SignNode::tree_fn(&test.contracts[4], vec![]),
                                SignNode::tree_fn(&test.contracts[3], vec![]),
                            ],
                        ),
                    ],
                ),)
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[2])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ))
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[1],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),)
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[2],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),)
            },
        ]
    );

    // Correct call
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                        ],
                    ),
                ],
            )],
            vec![
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),
            ],
            vec![SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            )],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![1, 2, 1]);

    // Failing scenarios
    // Missing one call for address[2]
    test.tree_test_enforcing(
        &setup,
        vec![
            vec![SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[4], vec![]),
                        ],
                    ),
                ],
            )],
            vec![
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),
                SignNode::tree_fn(
                    &test.contracts[2],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),
            ],
            vec![SignNode::tree_fn(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[4], vec![]),
                ],
            )],
        ],
        false,
    );
}

#[test]
fn test_out_of_order_auth() {
    let mut test = AuthTest::setup(1, 2);
    let call_args = host_vec![
        &test.host,
        test.key_to_address(&test.keys[0]),
        &test.contracts[1]
    ];
    assert_eq!(
        test.run_recording(
            &test.contracts[0],
            Symbol::try_from_small_str("order_fn").unwrap(),
            host_vec![
                &test.host,
                test.key_to_address(&test.keys[0]),
                test.contracts[1]
            ],
        ),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::new(
                    &test.contracts[1],
                    Symbol::try_from_small_str("do_auth").unwrap(),
                    test.host
                        .call_args_to_scvec(
                            host_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32]
                                .into(),
                        )
                        .unwrap(),
                    vec![],
                ))
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::new(
                    &test.contracts[0],
                    Symbol::try_from_small_str("order_fn").unwrap(),
                    test.host
                        .call_args_to_scvec(
                            host_vec![
                                &test.host,
                                test.key_to_address(&test.keys[0]),
                                test.contracts[1]
                            ]
                            .into(),
                        )
                        .unwrap(),
                    vec![],
                ))
            },
        ]
    );

    // Successful scenario - order_fn and do_auth need separate authorized trees
    // because do_auth happens before order_fn.
    test.test_enforcing(
        test.contracts[0].clone(),
        Symbol::try_from_small_str("order_fn").unwrap(),
        call_args.clone(),
        vec![vec![
            SignNode::new(
                &test.contracts[0],
                Symbol::try_from_small_str("order_fn").unwrap(),
                test.host
                    .call_args_to_scvec(
                        host_vec![
                            &test.host,
                            test.key_to_address(&test.keys[0]),
                            test.contracts[1]
                        ]
                        .into(),
                    )
                    .unwrap(),
                vec![],
            ),
            SignNode::new(
                &test.contracts[1],
                Symbol::try_from_small_str("do_auth").unwrap(),
                test.host
                    .call_args_to_scvec(
                        host_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32].into(),
                    )
                    .unwrap(),
                vec![],
            ),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![2]);

    test.test_enforcing(
        test.contracts[0].clone(),
        Symbol::try_from_small_str("order_fn").unwrap(),
        call_args,
        vec![vec![SignNode::new(
            &test.contracts[0],
            Symbol::try_from_small_str("order_fn").unwrap(),
            test.host
                .call_args_to_scvec(
                    host_vec![
                        &test.host,
                        test.key_to_address(&test.keys[0]),
                        test.contracts[1]
                    ]
                    .into(),
                )
                .unwrap(),
            vec![SignNode::new(
                &test.contracts[1],
                Symbol::try_from_small_str("do_auth").unwrap(),
                test.host
                    .call_args_to_scvec(
                        host_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32].into(),
                    )
                    .unwrap(),
                vec![],
            )],
        )]],
        false,
    );
}

#[test]
fn test_invoker_subcontract_auth() {
    let mut test = AuthTest::setup(0, 6);

    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true],
                vec![
                    SetupNode::new(
                        &test.contracts[4],
                        vec![true],
                        vec![SetupNode::new(&test.contracts[3], vec![true], vec![])],
                    ),
                    // Contract will authorize this, but `require_auth` isn't
                    // called - that's fine as we don't require exhausting
                    // every authorization.
                    SetupNode::new(&test.contracts[3], vec![false], vec![]),
                ],
            ),
        ],
    );

    let tree = test.convert_setup_tree(&setup);
    let args = host_vec![&test.host, tree];
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn").unwrap();
    assert_eq!(
        test.run_recording(&test.contracts[5], fn_name.clone(), args.clone(),),
        vec![]
    );

    test.test_enforcing(
        test.contracts[5].clone(),
        fn_name.clone(),
        args.clone(),
        vec![],
        true,
    )
}

#[test]
fn test_invoker_subcontract_with_gaps() {
    let mut test = AuthTest::setup(0, 4);

    // The direct contract call doesn't need to require auth - the contract
    // still authorizes the following sub-contract calls only.
    let top_gap_setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![SetupNode::new(&test.contracts[1], vec![true], vec![])],
    );

    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn").unwrap();

    test.test_enforcing(
        test.contracts[3].clone(),
        fn_name.clone(),
        host_vec![&test.host, test.convert_setup_tree(&top_gap_setup)],
        vec![],
        true,
    );

    // Gap in the middle is not allowed (contract authorizes 1->2, while we
    // require auth only for 2).
    let mid_gap_setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![SetupNode::new(
            &test.contracts[1],
            vec![false],
            vec![SetupNode::new(&test.contracts[2], vec![true], vec![])],
        )],
    );

    test.test_enforcing(
        test.contracts[3].clone(),
        fn_name.clone(),
        host_vec![&test.host, test.convert_setup_tree(&mid_gap_setup)],
        vec![],
        false,
    )
}
