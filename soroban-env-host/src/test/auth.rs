use ed25519_dalek::SigningKey;
use rand::Rng;
use soroban_builtin_sdk_macros::contracttype;
use soroban_env_common::xdr::{
    AccountId, ContractDataDurability, HashIdPreimage, HashIdPreimageSorobanAuthorization,
    InvokeContractArgs, PublicKey, ScAddress, ScBytes, ScErrorCode, ScErrorType, ScNonceKey,
    ScSymbol, ScVal, SorobanAddressCredentials, SorobanAuthorizationEntry,
    SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials, Uint256, VecM,
};
use soroban_test_wasms::{
    AUTH_TEST_CONTRACT, CONDITIONAL_ACCOUNT_TEST_CONTRACT, DELEGATED_ACCOUNT_TEST_CONTRACT,
};

use crate::auth::RecordedAuthPayload;
use crate::budget::AsBudget;
use crate::builtin_contracts::base_types::Address;
use crate::builtin_contracts::testutils::{
    create_account, generate_signing_key, sign_payload_for_account, signing_key_to_account_id,
};
use crate::{Host, LedgerInfo};
use soroban_env_common::{AddressObject, Env, Symbol, SymbolStr, TryFromVal, TryIntoVal};

use crate::builtin_contracts::base_types::Vec as HostVec;

use pretty_assertions::assert_eq;

#[derive(Clone)]
#[contracttype]
pub struct ContractTreeNode {
    pub(crate) contract: Address,
    pub(crate) need_auth: HostVec,
    pub(crate) children: HostVec,
    pub(crate) try_call: bool,
}

struct AuthTest {
    host: Host,
    keys: Vec<SigningKey>,
    contracts: Vec<Address>,
    last_nonces: Vec<Vec<i64>>,
}

struct SetupNode {
    contract_address: Address,
    need_auth: Vec<bool>,
    children: Vec<SetupNode>,
    try_call: bool,
}

struct SignNode {
    contract_address: Address,
    fn_name: Symbol,
    args: VecM<ScVal>,
    children: Vec<SignNode>,
}

impl SetupNode {
    fn new(contract_address: &Address, need_auth: Vec<bool>, children: Vec<SetupNode>) -> Self {
        Self {
            contract_address: contract_address.clone(),
            need_auth,
            children,
            try_call: false,
        }
    }

    fn new_try_call(
        contract_address: &Address,
        need_auth: Vec<bool>,
        children: Vec<SetupNode>,
    ) -> Self {
        Self {
            contract_address: contract_address.clone(),
            need_auth,
            children,
            try_call: true,
        }
    }
}

impl SignNode {
    fn new(
        contract_address: &Address,
        fn_name: Symbol,
        args: VecM<ScVal>,
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
            args: VecM::default(),
        }
    }

    fn tree_fn_stored(contract_id: &Address, children: Vec<SignNode>) -> Self {
        Self {
            contract_address: contract_id.clone(),
            children,
            fn_name: Symbol::try_from_small_str("stree_fn").unwrap(),
            args: VecM::default(),
        }
    }
}

// This test uses `AuthContract::tree_fn` to build various authorization trees.
impl AuthTest {
    fn setup_with_contract(signer_cnt: usize, contract_cnt: usize, contract_wasm: &[u8]) -> Self {
        let host = Host::test_host_with_recording_footprint();
        // TODO: remove the `reset_unlimited` and instead reset inputs wherever appropriate
        // to respect the budget limit.
        host.as_budget().reset_unlimited().unwrap();
        host.enable_debug().unwrap();

        host.with_mut_ledger_info(|li| {
            li.sequence_number = 100;
            li.max_entry_ttl = 10000;
        })
        .unwrap();
        let mut accounts = vec![];
        for _ in 0..signer_cnt {
            accounts.push(generate_signing_key(&host));
        }
        for signing_key in &accounts {
            create_account(
                &host,
                &signing_key_to_account_id(signing_key),
                vec![(&signing_key, 1)],
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
            let contract_id_obj = host.register_test_contract_wasm(contract_wasm);
            contracts.push(contract_id_obj.try_into_val(&host).unwrap());
        }
        Self {
            host,
            keys: accounts,
            contracts,
            last_nonces: vec![],
        }
    }

    fn setup(signer_cnt: usize, contract_cnt: usize) -> Self {
        Self::setup_with_contract(signer_cnt, contract_cnt, AUTH_TEST_CONTRACT)
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
        self.tree_test_enforcing_with_addresses(self.get_addresses(), root, sign_payloads, success);
    }

    fn tree_test_enforcing_with_addresses(
        &mut self,
        addresses: HostVec,
        root: &SetupNode,
        sign_payloads: Vec<Vec<SignNode>>,
        success: bool,
    ) {
        self.test_enforcing(
            root.contract_address.clone(),
            Symbol::try_from_small_str("tree_fn").unwrap(),
            test_vec![&self.host, addresses, self.convert_setup_tree(&root)],
            sign_payloads,
            success,
        );
    }

    // Like tree_test_enforcing, but read the set of the addreses from storage
    // for the first authorization.
    // Covers scenarios when `require_auth` is called for the addresses computed
    // programmatically (as opposed to passing them as inputs);
    fn tree_test_enforcing_with_stored_addresses(
        &mut self,
        root: &SetupNode,
        sign_payloads: Vec<Vec<SignNode>>,
        success: bool,
    ) {
        self.store_addresses(root);
        self.test_enforcing(
            root.contract_address.clone(),
            Symbol::try_from_small_str("stree_fn").unwrap(),
            test_vec![
                &self.host,
                self.get_addresses(),
                self.convert_setup_tree(&root)
            ],
            sign_payloads,
            success,
        );
    }

    fn store_addresses(&self, root: &SetupNode) {
        self.host
            .call(
                root.contract_address.clone().into(),
                Symbol::try_from_small_str("store").unwrap(),
                test_vec![&self.host, self.get_addresses()].into(),
            )
            .unwrap();
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
                let nonce = self
                    .host
                    .with_test_prng(|chacha| Ok(chacha.gen_range(0..=i64::MAX)))
                    .unwrap();
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
                let signature_args = test_vec![
                    &self.host,
                    sign_payload_for_account(&self.host, &self.keys[address_id], &payload)
                ];
                contract_auth.push(SorobanAuthorizationEntry {
                    credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                        address: sc_address.clone(),
                        nonce,
                        signature: ScVal::Vec(Some(
                            self.host
                                .vecobject_to_scval_vec(signature_args.into())
                                .unwrap()
                                .into(),
                        )),
                        signature_expiration_ledger: 1000,
                    }),
                    root_invocation,
                });
            }
            self.last_nonces.push(curr_nonces);
        }

        self.host.set_authorization_entries(contract_auth).unwrap();
        let res = self
            .host
            .call(contract_address.into(), fn_name, args.into());
        if res.is_err() && success {
            eprintln!(
                "invocation failed while success expected, error: {:?}",
                res.clone().err().unwrap()
            );
        }
        assert_eq!(res.is_ok(), success);
    }

    fn read_nonce_live_until(&self, address: &Address, nonce: i64) -> Option<u32> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey { nonce });
        let nonce_key = self
            .host
            .storage_key_for_address(
                address.to_sc_address().unwrap(),
                nonce_key_scval,
                ContractDataDurability::Temporary,
            )
            .unwrap();
        self.host
            .with_mut_storage(|storage| {
                if !storage.has(&nonce_key, self.host.budget_ref())? {
                    return Ok(None);
                }
                let (_, live_until_ledger) =
                    storage.get_with_live_until_ledger(&nonce_key, self.host.budget_ref())?;
                Ok(live_until_ledger)
            })
            .unwrap()
    }

    // Runs `tree_fn` corresponding to the provided setup in recordind mode and
    // returns the recorded payloads.
    fn tree_run_recording(
        &self,
        root: &SetupNode,
        disable_non_root_auth: bool,
    ) -> Vec<RecordedAuthPayload> {
        let addresses = self.get_addresses();
        let tree = self.convert_setup_tree(&root);
        self.run_recording(
            &root.contract_address,
            Symbol::try_from_small_str("tree_fn").unwrap(),
            test_vec![&self.host, addresses, tree],
            disable_non_root_auth,
        )
    }

    fn tree_run_recording_with_stored_addresses(
        &self,
        root: &SetupNode,
        disable_non_root_auth: bool,
    ) -> Vec<RecordedAuthPayload> {
        self.store_addresses(root);
        let addresses = self.get_addresses();
        let tree = self.convert_setup_tree(&root);
        self.run_recording(
            &root.contract_address,
            Symbol::try_from_small_str("stree_fn").unwrap(),
            test_vec![&self.host, addresses, tree],
            disable_non_root_auth,
        )
    }

    fn tree_run_recording_with_custom_addresses(
        &self,
        addresses: HostVec,
        root: &SetupNode,
        disable_non_root_auth: bool,
    ) -> Vec<RecordedAuthPayload> {
        let tree = self.convert_setup_tree(&root);
        self.run_recording(
            &root.contract_address,
            Symbol::try_from_small_str("tree_fn").unwrap(),
            test_vec![&self.host, addresses, tree],
            disable_non_root_auth,
        )
    }

    fn run_recording(
        &self,
        contract_address: &Address,
        fn_name: Symbol,
        args: HostVec,
        disable_non_root_auth: bool,
    ) -> Vec<RecordedAuthPayload> {
        self.host
            .switch_to_recording_auth(disable_non_root_auth)
            .unwrap();
        self.host
            .call(contract_address.clone().into(), fn_name, args.into())
            .unwrap();
        self.host.get_recorded_auth_payloads().unwrap()
    }

    fn test_recording_and_enforcing_no_auth(
        &mut self,
        contract_address: Address,
        fn_name: Symbol,
        args: HostVec,
        success: bool,
    ) {
        self.host.switch_to_recording_auth(true).unwrap();
        assert_eq!(
            self.host
                .call(
                    contract_address.clone().into(),
                    fn_name,
                    args.clone().into()
                )
                .is_ok(),
            success
        );

        self.test_enforcing(contract_address.clone(), fn_name, args, vec![], success);
    }

    fn key_to_sc_address(&self, key: &SigningKey) -> ScAddress {
        ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
            key.verifying_key().to_bytes(),
        ))))
    }

    fn key_to_address(&self, key: &SigningKey) -> AddressObject {
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
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
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
            try_call: root.try_call,
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
                if let Some(live_until_ledger) = self.read_nonce_live_until(&address, *nonce) {
                    assert_eq!(live_until_ledger, 1000);
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
        test.tree_run_recording(&setup, true),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[0])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&expected_sign_payloads[0][0]),
        }]
    );

    // Correct call
    test.tree_test_enforcing(&setup, expected_sign_payloads, true);
    test.verify_nonces_consumed(vec![1]);

    assert_eq!(
        test.tree_run_recording(&setup, true),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[0])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[0], vec![])),
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
    // nonces are consumed unless the rollback logic is broken.
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

    // Account doesn't exist.
    test.host
        .with_mut_storage(|storage| {
            let account_id = signing_key_to_account_id(&test.keys[0]);
            let key = test.host.to_account_key(account_id)?;
            // Note, that this represents 'correct footprint, missing value' scenario.
            // Incorrect footprint scenario is not covered (it's not auth specific).
            storage.del(&key, test.host.budget_ref())
        })
        .unwrap();

    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(&test.contracts[0], vec![])]],
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
        test.tree_run_recording(&setup, true),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&expected_sign_payloads[0][0]),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&expected_sign_payloads[1][0]),
            },
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
        test.tree_run_recording(&setup, true),
        vec![RecordedAuthPayload {
            address: Some(test.key_to_sc_address(&test.keys[1])),
            nonce: Some(0),
            invocation: test.convert_sign_node(&expected_sign_payloads[1][0]),
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
        test.tree_run_recording(&setup, true),
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
            )),
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
                        SignNode::tree_fn(&test.contracts[2], vec![]),
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
fn test_disjoint_tree_not_allowed() {
    let mut test = AuthTest::setup(1, 2);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![SetupNode::new(&test.contracts[1], vec![true], vec![])],
    );
    // Correct call
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[0],
            vec![SignNode::tree_fn(&test.contracts[1], vec![])],
        )]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Incorrect call - tree has been split into two separate nodes.
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(&test.contracts[0], vec![]),
            SignNode::tree_fn(&test.contracts[1], vec![]),
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
        // Root call is not authorized, disable non-root auth check.
        test.tree_run_recording(&setup, false),
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
                ),),
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
                ),),
            },
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
fn test_two_authorized_trees_with_rollback() {
    let mut test = AuthTest::setup(1, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![
            SetupNode::new_try_call(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new_try_call(
                &test.contracts[2],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                ],
            ),
        ],
    );

    // Both subtrees fail, no nonces consumed.
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[1],
                vec![
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    // Missing child payload
                ],
            ),
            SignNode::tree_fn(
                &test.contracts[2],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    // Missing child payload
                ],
            ),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![0]);

    // First tree fails, second succeeds and consumes nonce
    test.tree_test_enforcing(
        &setup,
        vec![vec![
            SignNode::tree_fn(
                &test.contracts[1],
                vec![
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    // Missing child payload
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
    test.verify_nonces_consumed(vec![1]);

    // Second tree fails, first succeeds and consumes nonce
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
                    // Missing child payload
                ],
            ),
        ]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Only the valid second tree is passed and consumes nonce.
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[2],
            vec![
                SignNode::tree_fn(&test.contracts[3], vec![]),
                SignNode::tree_fn(&test.contracts[4], vec![]),
            ],
        )]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // No trees passed at all.
    test.tree_test_enforcing(&setup, vec![vec![]], true);
    test.verify_nonces_consumed(vec![0]);
}

#[test]
fn test_rolled_back_auth_entry_can_be_reused() {
    let mut test = AuthTest::setup(1, 5);
    // Setup up two trees that are almost identical besides one deep call.
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![
            SetupNode::new_try_call(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true], vec![]),
                    SetupNode::new(&test.contracts[4], vec![true], vec![]),
                ],
            ),
        ],
    );

    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[1],
            vec![
                SignNode::tree_fn(&test.contracts[3], vec![]),
                SignNode::tree_fn(&test.contracts[4], vec![]),
            ],
        )]],
        true,
    );
    test.verify_nonces_consumed(vec![1]);

    // Failure - the payload provided only for the first call and the
    // second tree doesn't allow failures.
    test.tree_test_enforcing(
        &setup,
        vec![vec![SignNode::tree_fn(
            &test.contracts[1],
            vec![
                SignNode::tree_fn(&test.contracts[3], vec![]),
                SignNode::tree_fn(&test.contracts[2], vec![]),
            ],
        )]],
        false,
    );
}

#[test]
fn test_disable_non_root_recording_auth() {
    let test = AuthTest::setup(1, 3);
    test.host.switch_to_recording_auth(true).unwrap();
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![false],
        vec![
            SetupNode::new(&test.contracts[1], vec![true], vec![]),
            SetupNode::new(&test.contracts[2], vec![true], vec![]),
        ],
    );
    let addresses = test.get_addresses();
    let tree = test.convert_setup_tree(&setup);
    let err = test
        .host
        .call(
            setup.contract_address.clone().into(),
            Symbol::try_from_small_str("tree_fn").unwrap(),
            test_vec![&test.host, addresses, tree].into(),
        )
        .err()
        .unwrap();
    assert!(err.error.is_type(ScErrorType::Auth));
    assert!(err.error.is_code(ScErrorCode::InvalidAction));

    // Now enable non root auth - the call should succeed.
    test.host.switch_to_recording_auth(false).unwrap();
    assert!(test
        .host
        .call(
            setup.contract_address.into(),
            Symbol::try_from_small_str("tree_fn").unwrap(),
            test_vec![&test.host, addresses, tree].into(),
        )
        .is_ok());
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
        // Root call is not authorized, disable non-root auth check.
        test.tree_run_recording(&setup, false),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[3], vec![])),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(&test.contracts[2], vec![])),
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
                ),),
            },
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
        // One of the root calls is not authorized, disable non-root auth check.
        test.tree_run_recording(&setup, false),
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
                ),),
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
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[1],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[2],
                    vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                ),),
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
    let call_args = test_vec![
        &test.host,
        test.key_to_address(&test.keys[0]),
        &test.contracts[1]
    ];
    assert_eq!(
        test.run_recording(
            &test.contracts[0],
            Symbol::try_from_small_str("order_fn").unwrap(),
            test_vec![
                &test.host,
                test.key_to_address(&test.keys[0]),
                test.contracts[1]
            ],
            // Root auth happens after the sub-contract call, so don't disable
            // non-root auth.
            false,
        ),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::new(
                    &test.contracts[1],
                    Symbol::try_from_small_str("do_auth").unwrap(),
                    test.host
                        .vecobject_to_scval_vec(
                            test_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32]
                                .into(),
                        )
                        .unwrap(),
                    vec![],
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::new(
                    &test.contracts[0],
                    Symbol::try_from_small_str("order_fn").unwrap(),
                    test.host
                        .vecobject_to_scval_vec(
                            test_vec![
                                &test.host,
                                test.key_to_address(&test.keys[0]),
                                test.contracts[1]
                            ]
                            .into(),
                        )
                        .unwrap(),
                    vec![],
                )),
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
                    .vecobject_to_scval_vec(
                        test_vec![
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
                    .vecobject_to_scval_vec(
                        test_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32].into(),
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
                .vecobject_to_scval_vec(
                    test_vec![
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
                    .vecobject_to_scval_vec(
                        test_vec![&test.host, test.key_to_address(&test.keys[0]), 10_u32].into(),
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
    let args = test_vec![&test.host, &tree, &tree];
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn").unwrap();
    test.test_recording_and_enforcing_no_auth(
        test.contracts[5].clone(),
        fn_name,
        args.clone(),
        true,
    );
}

#[test]
fn test_invoker_subcontract_auth_with_duplicates() {
    let mut test = AuthTest::setup(0, 6);

    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
        ],
    );

    let tree = test.convert_setup_tree(&setup);
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn").unwrap();
    test.test_recording_and_enforcing_no_auth(
        test.contracts[5].clone(),
        fn_name,
        test_vec![&test.host, &tree, &tree],
        true,
    );
    // Remove the top-level duplicate node from contract's authorizations - the call
    // must fail now.
    let incomplete_tree = test.convert_setup_tree(&SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![SetupNode::new(
            &test.contracts[1],
            vec![true],
            vec![
                SetupNode::new(&test.contracts[2], vec![true], vec![]),
                SetupNode::new(&test.contracts[2], vec![true], vec![]),
            ],
        )],
    ));
    test.test_recording_and_enforcing_no_auth(
        test.contracts[5].clone(),
        fn_name,
        test_vec![&test.host, &tree, &incomplete_tree],
        false,
    );
    // Remove the inner duplicate node from contract's authorizations - the call
    // must fail now.
    let incomplete_tree = test.convert_setup_tree(&SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[1],
                vec![true],
                vec![
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                    // Missing one auth here.
                ],
            ),
        ],
    ));
    test.test_recording_and_enforcing_no_auth(
        test.contracts[5].clone(),
        fn_name,
        test_vec![&test.host, &tree, &incomplete_tree],
        false,
    );
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
    let top_gap_tree = test.convert_setup_tree(&top_gap_setup);
    test.test_enforcing(
        test.contracts[3].clone(),
        fn_name,
        test_vec![&test.host, &top_gap_tree, &top_gap_tree],
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
    let mid_gap_tree = test.convert_setup_tree(&mid_gap_setup);
    test.test_enforcing(
        test.contracts[3].clone(),
        fn_name,
        test_vec![&test.host, &mid_gap_tree, &mid_gap_tree],
        vec![],
        false,
    );
}

#[test]
fn test_invoker_subcontract_auth_with_rollbacks() {
    let mut test = AuthTest::setup(0, 7);

    // Setup the call tree that allows contract failures at different levels.
    let call_setup = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            // Allow auth failure right after the root call (root call itself is authorized by
            // default).
            SetupNode::new_try_call(&test.contracts[1], vec![true], vec![]),
            SetupNode::new(&test.contracts[2], vec![true], vec![]),
            // Add a nested call that might fail in sub-calls.
            SetupNode::new_try_call(
                &test.contracts[3],
                vec![true],
                vec![SetupNode::new(&test.contracts[4], vec![true], vec![])],
            ),
            // Add a nested call that is expected to succeed.
            SetupNode::new(
                &test.contracts[3],
                vec![true],
                vec![
                    // Allow failure deeper in the stack.
                    SetupNode::new_try_call(&test.contracts[4], vec![true], vec![]),
                    SetupNode::new(&test.contracts[5], vec![true], vec![]),
                ],
            ),
        ],
    );

    let call_tree = test.convert_setup_tree(&call_setup);
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn").unwrap();

    // Authorize the whole tree - should succeed.
    test.test_recording_and_enforcing_no_auth(
        test.contracts[6].clone(),
        fn_name,
        test_vec![&test.host, &call_tree, &call_tree],
        true,
    );
    // Skip authorizing the fallible calls - this should still succeed.
    let tree_without_optional_auth = test.convert_setup_tree(&SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            SetupNode::new(&test.contracts[2], vec![true], vec![]),
            // Note, that this should be matched to try_call for contract 3 first, then
            // rolled back due to failure, then used again for 3->5 auth.
            SetupNode::new(
                &test.contracts[3],
                vec![true],
                vec![SetupNode::new(&test.contracts[5], vec![true], vec![])],
            ),
        ],
    ));
    test.test_recording_and_enforcing_no_auth(
        test.contracts[6].clone(),
        fn_name,
        test_vec![&test.host, &call_tree, &tree_without_optional_auth],
        true,
    );
    // Authorize only fallible calls, but not infallible - now invocation should fail.
    let tree_without_mandatory_auth = test.convert_setup_tree(&SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            // Allow auth failure right after the root call (root call itself is authorized by
            // default).
            SetupNode::new(&test.contracts[1], vec![true], vec![]),
            // Add a nested call that might fail in sub-calls.
            SetupNode::new(
                &test.contracts[3],
                vec![true],
                vec![SetupNode::new(&test.contracts[4], vec![true], vec![])],
            ),
        ],
    ));
    test.test_recording_and_enforcing_no_auth(
        test.contracts[6].clone(),
        fn_name,
        test_vec![&test.host, &call_tree, &tree_without_mandatory_auth],
        false,
    );
    // Add a mix of fallible and mandatory auth, with mandatory auth missing.
    let tree_with_incorrect_auth = test.convert_setup_tree(&SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![
            // Allow auth failure right after the root call (root call itself is authorized by
            // default).
            SetupNode::new(&test.contracts[1], vec![true], vec![]),
            SetupNode::new(&test.contracts[2], vec![true], vec![]),
            // Add a nested call that is expected to succeed.
            SetupNode::new(
                &test.contracts[3],
                vec![true],
                vec![
                    // Allow failure deeper in the stack.
                    SetupNode::new_try_call(&test.contracts[4], vec![true], vec![]),
                    // Incorrect auth - this needs to be contract 5 to pass.
                    SetupNode::new(&test.contracts[2], vec![true], vec![]),
                ],
            ),
        ],
    ));
    test.test_recording_and_enforcing_no_auth(
        test.contracts[6].clone(),
        fn_name,
        test_vec![&test.host, &call_tree, &tree_with_incorrect_auth],
        false,
    );
}

#[test]
fn test_invoker_subcontract_auth_is_not_reused() {
    let mut test = AuthTest::setup(0, 4);

    let call_tree = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![SetupNode::new(&test.contracts[1], vec![true], vec![])],
    );
    let failing_tree = SetupNode::new(
        &test.contracts[0],
        vec![true],
        vec![SetupNode::new(&test.contracts[2], vec![true], vec![])],
    );

    let args = test_vec![
        &test.host,
        test.convert_setup_tree(&call_tree),
        test.convert_setup_tree(&failing_tree)
    ];
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_no_reuse").unwrap();

    test.test_recording_and_enforcing_no_auth(
        test.contracts[3].clone(),
        fn_name,
        args.clone(),
        true,
    );
}

#[test]
fn test_invoker_subcontract_auth_without_subcontract_calls() {
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
                vec![SetupNode::new(
                    &test.contracts[4],
                    vec![true],
                    vec![SetupNode::new(&test.contracts[3], vec![true], vec![])],
                )],
            ),
        ],
    );

    let tree = test.convert_setup_tree(&setup);
    let args = test_vec![&test.host, tree];
    let fn_name = Symbol::try_from_val(&test.host, &"invoker_auth_fn_no_call").unwrap();
    test.test_recording_and_enforcing_no_auth(
        test.contracts[5].clone(),
        fn_name,
        args.clone(),
        true,
    );
}

#[test]
fn test_require_auth_within_check_auth() {
    let test = AuthTest::setup_with_contract(1, 2, DELEGATED_ACCOUNT_TEST_CONTRACT);
    let auth_contract: Address = test
        .host
        .register_test_contract_wasm(AUTH_TEST_CONTRACT)
        .try_into_val(&test.host)
        .unwrap();

    // Account 1 is used to authorize calls on behalf of account 0.
    test.host
        .call(
            test.contracts[0].as_object(),
            Symbol::try_from_small_str("init").unwrap(),
            test_vec![&test.host, test.contracts[1]].as_object(),
        )
        .unwrap();
    // Classic account is used to authorize calls on behalf of account 1.
    test.host
        .call(
            test.contracts[1].as_object(),
            Symbol::try_from_small_str("init").unwrap(),
            test_vec![&test.host, test.key_to_address(&test.keys[0])].as_object(),
        )
        .unwrap();
    let network_id: crate::xdr::Hash = test
        .host
        .with_ledger_info(|li: &LedgerInfo| Ok(li.network_id))
        .unwrap()
        .try_into()
        .unwrap();

    let mut auth_entries = vec![];
    // Payload for account 0 is just the normal contract call payload.
    let account_0_invocation = SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
            contract_address: auth_contract.to_sc_address().unwrap(),
            function_name: "do_auth".try_into().unwrap(),
            args: vec![
                ScVal::Address(test.contracts[0].to_sc_address().unwrap()),
                ScVal::U32(123),
            ]
            .try_into()
            .unwrap(),
        }),
        sub_invocations: VecM::default(),
    };
    let payload_preimage_for_account_0 =
        HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
            network_id: network_id.clone(),
            invocation: account_0_invocation.clone(),
            nonce: 1111,
            signature_expiration_ledger: 1000,
        });
    auth_entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: test.contracts[0].to_sc_address().unwrap(),
            nonce: 1111,
            signature: ScVal::Void,
            signature_expiration_ledger: 1000,
        }),
        root_invocation: account_0_invocation,
    });
    let account_0_payload_hash = test
        .host
        .metered_hash_xdr(&payload_preimage_for_account_0)
        .unwrap();

    // Account 1 needs to authorize `__check_auth` for account 0 with its
    // payload hash.
    let account_1_invocation = SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
            contract_address: test.contracts[0].to_sc_address().unwrap(),
            function_name: "__check_auth".try_into().unwrap(),
            args: vec![ScVal::Bytes(ScBytes(
                account_0_payload_hash.try_into().unwrap(),
            ))]
            .try_into()
            .unwrap(),
        }),
        sub_invocations: VecM::default(),
    };
    let payload_preimage_for_account_1 =
        HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
            network_id: network_id.clone(),
            invocation: account_1_invocation.clone(),
            nonce: 2222,
            signature_expiration_ledger: 2000,
        });

    auth_entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: test.contracts[1].to_sc_address().unwrap(),
            nonce: 2222,
            signature: ScVal::Void,
            signature_expiration_ledger: 2000,
        }),
        root_invocation: account_1_invocation,
    });

    let account_1_payload_hash = test
        .host
        .metered_hash_xdr(&payload_preimage_for_account_1)
        .unwrap();
    // Classic account needs to authorize `__check_auth` for account 1 with its
    // payload hash.
    let classic_account_invocation = SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
            contract_address: test.contracts[1].to_sc_address().unwrap(),
            function_name: "__check_auth".try_into().unwrap(),
            args: vec![ScVal::Bytes(ScBytes(
                account_1_payload_hash.try_into().unwrap(),
            ))]
            .try_into()
            .unwrap(),
        }),
        sub_invocations: VecM::default(),
    };
    let payload_preimage_for_classic_account =
        HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
            network_id: network_id.clone(),
            invocation: classic_account_invocation.clone(),
            nonce: 3333,
            signature_expiration_ledger: 3000,
        });

    let classic_account_payload_hash = test
        .host
        .metered_hash_xdr(&payload_preimage_for_classic_account)
        .unwrap();

    // Only one actual signature is needed (for the classic account).
    let signature_args = test_vec![
        &test.host,
        sign_payload_for_account(&test.host, &test.keys[0], &classic_account_payload_hash)
    ];
    // Push an auth entry with bad nonce to fail auth at the deepest level.
    auth_entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: test.key_to_sc_address(&test.keys[0]),
            nonce: 4444,
            signature: ScVal::Vec(Some(
                test.host
                    .vecobject_to_scval_vec(signature_args.clone().into())
                    .unwrap()
                    .into(),
            )),
            signature_expiration_ledger: 3000,
        }),
        root_invocation: classic_account_invocation.clone(),
    });

    test.host
        .set_authorization_entries(auth_entries.clone())
        .unwrap();
    let err = test
        .host
        .call(
            auth_contract.as_object(),
            Symbol::try_from_small_str("do_auth").unwrap(),
            test_vec![&test.host, test.contracts[0], 123_u32].as_object(),
        )
        .err()
        .unwrap();
    assert!(err.error.is_type(ScErrorType::Auth));
    assert!(err.error.is_code(ScErrorCode::InvalidAction));

    // Add the correct entry - now the call should pass
    auth_entries.pop();
    auth_entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: test.key_to_sc_address(&test.keys[0]),
            nonce: 3333,
            signature: ScVal::Vec(Some(
                test.host
                    .vecobject_to_scval_vec(signature_args.into())
                    .unwrap()
                    .into(),
            )),
            signature_expiration_ledger: 3000,
        }),
        root_invocation: classic_account_invocation,
    });

    test.host.set_authorization_entries(auth_entries).unwrap();
    test.host
        .call(
            auth_contract.as_object(),
            Symbol::try_from_small_str("do_auth").unwrap(),
            test_vec![&test.host, test.contracts[0], 123_u32].as_object(),
        )
        .unwrap();
    assert_eq!(
        test.read_nonce_live_until(&test.contracts[0], 1111),
        Some(1000)
    );
    assert_eq!(
        test.read_nonce_live_until(&test.contracts[1], 2222),
        Some(2000)
    );
    assert_eq!(
        test.read_nonce_live_until(
            &test
                .key_to_address(&test.keys[0])
                .try_into_val(&test.host)
                .unwrap(),
            3333,
        ),
        Some(3000)
    );
}

#[test]
fn test_require_auth_for_self_within_check_auth() {
    let test = AuthTest::setup_with_contract(1, 1, DELEGATED_ACCOUNT_TEST_CONTRACT);
    let auth_contract: Address = test
        .host
        .register_test_contract_wasm(AUTH_TEST_CONTRACT)
        .try_into_val(&test.host)
        .unwrap();

    // Use account as its own owner.
    test.host
        .call(
            test.contracts[0].as_object(),
            Symbol::try_from_small_str("init").unwrap(),
            test_vec![&test.host, test.contracts[0]].as_object(),
        )
        .unwrap();

    let mut auth_entries = vec![];
    // Add a single auth entry.
    auth_entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: test.contracts[0].to_sc_address().unwrap(),
            nonce: 1111,
            signature: ScVal::Void,
            signature_expiration_ledger: 1000,
        }),
        root_invocation: SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                contract_address: auth_contract.to_sc_address().unwrap(),
                function_name: "do_auth".try_into().unwrap(),
                args: vec![
                    ScVal::Address(test.contracts[0].to_sc_address().unwrap()),
                    ScVal::U32(123),
                ]
                .try_into()
                .unwrap(),
            }),
            sub_invocations: VecM::default(),
        },
    });
    let err = test
        .host
        .call(
            auth_contract.as_object(),
            Symbol::try_from_small_str("do_auth").unwrap(),
            test_vec![&test.host, test.contracts[0], 123_u32].as_object(),
        )
        .err()
        .unwrap();
    // Make sure we're just getting an auth error and not ending up in some
    // context/recursion error states.
    assert!(err.error.is_type(ScErrorType::Auth));
    assert!(err.error.is_code(ScErrorCode::InvalidAction));
}

#[test]
fn test_multi_address_auth_with_stored_addresses() {
    let mut test = AuthTest::setup(3, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true, true, true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true, false, false],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true, true, false], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true, true, false], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true, true, false],
                vec![
                    SetupNode::new(&test.contracts[4], vec![false, false, false], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true, true, false], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[1],
                vec![true, false, false],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true, true, false], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true, true, false], vec![]),
                ],
            ),
        ],
    );
    assert_eq!(
        test.tree_run_recording_with_stored_addresses(&setup, true),
        vec![
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn_stored(
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
                            vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                        ),
                        SignNode::tree_fn(
                            &test.contracts[1],
                            vec![
                                SignNode::tree_fn(&test.contracts[3], vec![]),
                                SignNode::tree_fn(&test.contracts[2], vec![]),
                            ],
                        ),
                    ],
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn_stored(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(
                            &test.contracts[2],
                            vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                        ),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[2])),
                nonce: Some(0),
                invocation: test
                    .convert_sign_node(&SignNode::tree_fn_stored(&test.contracts[0], vec![])),
            },
        ]
    );

    // Correct call
    test.tree_test_enforcing_with_stored_addresses(
        &setup,
        vec![
            vec![SignNode::tree_fn_stored(
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
                        vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                        ],
                    ),
                ],
            )],
            vec![SignNode::tree_fn_stored(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                    ),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                ],
            )],
            vec![SignNode::tree_fn_stored(&test.contracts[0], vec![])],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![1, 1, 1]);

    // Failing scenarios
    // Disjoint sub-tree for the second account.
    test.tree_test_enforcing_with_stored_addresses(
        &setup,
        vec![
            vec![SignNode::tree_fn_stored(
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
                        vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                    ),
                    SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                        ],
                    ),
                ],
            )],
            vec![SignNode::tree_fn_stored(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                ],
            )],
            vec![SignNode::tree_fn_stored(&test.contracts[0], vec![])],
        ],
        false,
    );

    // Disjoint top-level tree for the first account
    test.tree_test_enforcing_with_stored_addresses(
        &setup,
        vec![
            vec![
                SignNode::tree_fn_stored(
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
                            vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                        ),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[1],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                ),
            ],
            vec![SignNode::tree_fn_stored(
                &test.contracts[0],
                vec![
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                    SignNode::tree_fn(
                        &test.contracts[2],
                        vec![SignNode::tree_fn(&test.contracts[3], vec![])],
                    ),
                    SignNode::tree_fn(&test.contracts[3], vec![]),
                    SignNode::tree_fn(&test.contracts[2], vec![]),
                ],
            )],
            vec![SignNode::tree_fn_stored(&test.contracts[0], vec![])],
        ],
        false,
    );
}

#[test]
fn test_same_auth_tree_with_duplicate_addresses() {
    let mut test = AuthTest::setup(2, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true, true, true, true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true, true, true, true],
                vec![
                    SetupNode::new(&test.contracts[3], vec![true, true, true, true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true, true, true, true], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![true, true, true, true],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true, true, true, true], vec![]),
                    SetupNode::new(&test.contracts[3], vec![true, true, true, true], vec![]),
                ],
            ),
        ],
    );

    let addresses = test_vec![
        &test.host,
        test.key_to_address(&test.keys[0]).to_val(),
        test.key_to_address(&test.keys[1]).to_val(),
        test.key_to_address(&test.keys[1]).to_val(),
        test.key_to_address(&test.keys[0]).to_val(),
    ];
    assert_eq!(
        test.tree_run_recording_with_custom_addresses(addresses.clone(), &setup, true),
        vec![
            RecordedAuthPayload {
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
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
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
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
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
                )),
            },
            RecordedAuthPayload {
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
                )),
            },
        ]
    );

    // Correct call
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![
                SignNode::tree_fn(
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
                ),
                SignNode::tree_fn(
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
                ),
            ],
            vec![
                SignNode::tree_fn(
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
                ),
                SignNode::tree_fn(
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
                ),
            ],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![2, 2]);

    // Failing scenarios
    // Just one auth per address
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![SignNode::tree_fn(
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
            )],
            vec![SignNode::tree_fn(
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
            )],
        ],
        false,
    );

    // Incorrectly merged trees - it's not allowed to use the same auth tree
    // multiple times per frame
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![
                SignNode::tree_fn(
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
                        // Sub-tree calls from the second entry - while
                        // technically the address has authorized the same call
                        // tree twice, we don't allow matching it twice in the
                        // same frame.
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
                ),
                SignNode::tree_fn(&test.contracts[0], vec![]),
            ],
            vec![
                SignNode::tree_fn(
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
                ),
                SignNode::tree_fn(
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
                ),
            ],
        ],
        false,
    );
}

#[test]
fn test_different_auth_trees_with_duplicate_addresses() {
    let mut test = AuthTest::setup(2, 5);
    let setup = SetupNode::new(
        &test.contracts[0],
        vec![true, true, true, true],
        vec![
            SetupNode::new(
                &test.contracts[1],
                vec![true, false, true, false],
                vec![
                    SetupNode::new(&test.contracts[3], vec![false, true, true, true], vec![]),
                    SetupNode::new(&test.contracts[2], vec![true, true, true, false], vec![]),
                ],
            ),
            SetupNode::new(
                &test.contracts[2],
                vec![false, true, true, false],
                vec![
                    SetupNode::new(&test.contracts[4], vec![true, false, true, false], vec![]),
                    SetupNode::new(&test.contracts[3], vec![false, false, true, true], vec![]),
                ],
            ),
        ],
    );

    let addresses = test_vec![
        &test.host,
        test.key_to_address(&test.keys[0]).to_val(),
        test.key_to_address(&test.keys[1]).to_val(),
        test.key_to_address(&test.keys[1]).to_val(),
        test.key_to_address(&test.keys[0]).to_val(),
    ];
    assert_eq!(
        test.tree_run_recording_with_custom_addresses(addresses.clone(), &setup, true),
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
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[1])),
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
                )),
            },
            RecordedAuthPayload {
                address: Some(test.key_to_sc_address(&test.keys[0])),
                nonce: Some(0),
                invocation: test.convert_sign_node(&SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                )),
            },
        ]
    );

    // Correct call
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(
                            &test.contracts[1],
                            vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                        ),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
            vec![
                SignNode::tree_fn(
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
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                ),
            ],
        ],
        true,
    );
    test.verify_nonces_consumed(vec![2, 2]);

    // Failing scenarios
    // Incorrectly merged trees
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        // We need to authorize both 0->[1 skipped]->3 and
                        // 0->1->3 call stacks, where 1 is the same contract. As
                        // it can't be authorized and not authorized at the same
                        // time, this can't succeed.
                        SignNode::tree_fn(
                            &test.contracts[1],
                            vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                        ),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![SignNode::tree_fn(&test.contracts[4], vec![])],
                ),
            ],
            vec![
                SignNode::tree_fn(
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
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                    ],
                ),
            ],
        ],
        false,
    );

    // Another incorrect tree mix
    test.tree_test_enforcing_with_addresses(
        addresses.clone(),
        &setup,
        vec![
            vec![
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                    ],
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(
                            &test.contracts[1],
                            vec![SignNode::tree_fn(&test.contracts[2], vec![])],
                        ),
                        SignNode::tree_fn(&test.contracts[4], vec![]),
                    ],
                ),
            ],
            vec![
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![SignNode::tree_fn(
                        &test.contracts[1],
                        vec![
                            SignNode::tree_fn(&test.contracts[2], vec![]),
                            SignNode::tree_fn(&test.contracts[3], vec![]),
                        ],
                    )],
                ),
                SignNode::tree_fn(
                    &test.contracts[0],
                    vec![
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        SignNode::tree_fn(&test.contracts[3], vec![]),
                        SignNode::tree_fn(&test.contracts[2], vec![]),
                        // That's the third auth for the contract 2 and we only
                        // call 2 just twice.
                        SignNode::tree_fn(
                            &test.contracts[2],
                            vec![
                                SignNode::tree_fn(&test.contracts[3], vec![]),
                                SignNode::tree_fn(&test.contracts[4], vec![]),
                            ],
                        ),
                    ],
                ),
            ],
        ],
        false,
    );
}

#[test]
fn test_rollback_with_conditional_custom_account_auth() {
    let test = AuthTest::setup(0, 2);
    let account_obj = test
        .host
        .register_test_contract_wasm(CONDITIONAL_ACCOUNT_TEST_CONTRACT);
    let account = Address::try_from_val(&test.host, &account_obj).unwrap();

    let auth_entry_prototype = SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            address: account.to_sc_address().unwrap(),
            nonce: 0,
            signature: ScVal::Void,
            signature_expiration_ledger: 1000,
        }),
        root_invocation: SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                contract_address: test.contracts[1].to_sc_address().unwrap(),
                function_name: "do_auth".try_into().unwrap(),
                args: vec![
                    ScVal::Address(account.to_sc_address().unwrap()),
                    ScVal::U32(123),
                ]
                .try_into()
                .unwrap(),
            }),
            sub_invocations: Default::default(),
        },
    };
    let create_auth_entry = |nonce: i64| {
        let mut entry = auth_entry_prototype.clone();
        if let SorobanCredentials::Address(creds) = &mut entry.credentials {
            creds.nonce = nonce;
        } else {
            unreachable!();
        };
        entry
    };

    let do_call = |auth_entries| {
        test.host.set_authorization_entries(auth_entries).unwrap();
        test.host.call(
            test.contracts[0].clone().into(),
            Symbol::try_from_val(&test.host, &"conditional_auth").unwrap(),
            test_vec![&test.host, &account, &test.contracts[1]].into(),
        )
    };

    // No auth entries at all - fail.
    assert!(do_call(vec![])
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));
    // One auth entry - should succeed and consume one nonce (from the second
    // call in `conditional_auth` fn).
    assert!(do_call(vec![create_auth_entry(111)]).is_ok());
    assert_eq!(test.read_nonce_live_until(&account, 111), Some(1000));

    // Two auth entries - only one will be used in the second call.
    assert!(do_call(vec![create_auth_entry(222), create_auth_entry(333)]).is_ok());
    assert_eq!(test.read_nonce_live_until(&account, 222), Some(1000));
    assert_eq!(test.read_nonce_live_until(&account, 333), None);

    // Call `allow` to allow first auth to pass.
    test.host
        .call(
            account.clone().into(),
            Symbol::try_from_small_str("allow").unwrap(),
            test_vec![&test.host].into(),
        )
        .unwrap();
    assert!(do_call(vec![
        create_auth_entry(444),
        create_auth_entry(555),
        create_auth_entry(666),
    ])
    .is_ok());
    assert_eq!(test.read_nonce_live_until(&account, 444), Some(1000));
    assert_eq!(test.read_nonce_live_until(&account, 555), Some(1000));
    // Third call still can't succeed and won't consume nonce.
    assert_eq!(test.read_nonce_live_until(&account, 666), None);
}
