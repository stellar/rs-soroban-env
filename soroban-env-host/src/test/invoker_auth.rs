#![allow(dead_code)]
#![allow(unused_variables)]
use soroban_builtin_sdk_macros::contracttype;
use soroban_test_wasms::INVOKER_AUTH_TEST_CONTRACT;

use crate::builtin_contracts::base_types::Vec as HostVec;
use crate::{
    builtin_contracts::base_types::Address,
    xdr::{ScErrorCode, ScErrorType},
    Host, HostError, Val,
};
use soroban_env_common::{Env, Symbol, TryFromVal, TryIntoVal};

#[contracttype]
struct InvokerAuthNodeContractType {
    pub contract: Address,
    pub arg: i32,
    pub children: HostVec,
}

#[contracttype]
struct InvokerCallNodeContractType {
    pub contract: Address,
    pub try_call: bool,
    pub failure_point: u32,
    pub authorize: HostVec,
    pub auth: HostVec,
    pub children: HostVec,
}

#[derive(Clone)]
struct InvokerAuthNode {
    contract: usize,
    arg: i32,
    children: Vec<InvokerAuthNode>,
}

#[derive(Clone)]
struct InvokerCallNode {
    contract: usize,
    try_call: bool,
    fail: bool,
    authorize: Vec<(usize, i32)>,
    auth: Vec<InvokerAuthNode>,
    children: Vec<InvokerCallNode>,
}

impl InvokerAuthNode {
    fn to_val(&self, test: &InvokerAuthTest) -> Val {
        let mut children = HostVec::new(&test.host).unwrap();
        for child in &self.children {
            children.push(&child.to_val(test)).unwrap();
        }
        InvokerAuthNodeContractType {
            contract: test.contracts[self.contract].clone(),
            arg: self.arg,
            children,
        }
        .try_into_val(&test.host)
        .unwrap()
    }
}

impl InvokerCallNode {
    fn no_fail(
        contract_id: usize,
        auth: Vec<InvokerAuthNode>,
        authorize: Vec<(usize, i32)>,
        children: Vec<InvokerCallNode>,
    ) -> Self {
        Self {
            contract: contract_id,
            try_call: false,
            fail: false,
            authorize,
            auth,
            children,
        }
    }

    fn try_no_fail(
        contract_id: usize,
        auth: Vec<InvokerAuthNode>,
        authorize: Vec<(usize, i32)>,
        children: Vec<InvokerCallNode>,
    ) -> Self {
        Self {
            contract: contract_id,
            try_call: true,
            fail: false,
            authorize,
            auth,
            children,
        }
    }

    fn fail(
        contract_id: usize,
        auth: Vec<InvokerAuthNode>,
        authorize: Vec<(usize, i32)>,
        children: Vec<InvokerCallNode>,
    ) -> Self {
        Self {
            contract: contract_id,
            try_call: false,
            fail: true,
            authorize,
            auth,
            children,
        }
    }

    fn try_fail(
        contract_id: usize,
        auth: Vec<InvokerAuthNode>,
        authorize: Vec<(usize, i32)>,
        children: Vec<InvokerCallNode>,
    ) -> Self {
        Self {
            contract: contract_id,
            try_call: true,
            fail: true,
            authorize,
            auth,
            children,
        }
    }

    fn to_val(&self, test: &InvokerAuthTest, failure_point_mask: u32, call_id: &mut u32) -> Val {
        let failure_point = (failure_point_mask >> (*call_id * 2)) & 0b11;

        let mut authorize = HostVec::new(&test.host).unwrap();
        for (contract_id, val) in &self.authorize {
            authorize
                .push(&test_vec![&test.host, &test.contracts[*contract_id], *val])
                .unwrap();
        }
        let mut auth = HostVec::new(&test.host).unwrap();
        for a in &self.auth {
            auth.push(&a.to_val(test)).unwrap();
        }
        let mut children = HostVec::new(&test.host).unwrap();
        for child in &self.children {
            *call_id += 1;
            children
                .push(&child.to_val(test, failure_point_mask, call_id))
                .unwrap();
        }
        InvokerCallNodeContractType {
            contract: test.contracts[self.contract].clone(),
            try_call: self.try_call,
            failure_point,
            authorize,
            auth,
            children,
        }
        .try_into_val(&test.host)
        .unwrap()
    }

    fn generate_failure_point_masks(&self, masks: &Vec<u32>, call_id: &mut u32) -> Vec<u32> {
        let mut new_masks = vec![];
        for mask in masks {
            if self.fail {
                for i in 1..=3 {
                    new_masks.push(*mask | (i << (*call_id * 2)));
                }
            } else {
                new_masks.push(*mask);
            }
        }
        for child in &self.children {
            *call_id += 1;
            new_masks = child.generate_failure_point_masks(&new_masks, call_id);
        }
        assert!(new_masks.len() <= 1000);
        new_masks
    }

    fn generate_failure_scenarios(&self, test: &InvokerAuthTest) -> Vec<Val> {
        let mut call_id = 0;
        let failure_point_masks = self.generate_failure_point_masks(&vec![0], &mut call_id);
        let mut res = vec![];
        for failure_point_mask in failure_point_masks {
            let mut call_id = 0;
            res.push(self.to_val(test, failure_point_mask, &mut call_id));
        }
        res
    }
}

struct InvokerAuthTest {
    host: Host,
    contracts: Vec<Address>,
}

impl InvokerAuthTest {
    fn setup(contract_cnt: usize) -> Self {
        let host = Host::test_host_with_recording_footprint();
        host.enable_invocation_metering();
        host.enable_debug().unwrap();

        host.with_mut_ledger_info(|li| {
            li.sequence_number = 100;
            li.max_entry_ttl = 10000;
        })
        .unwrap();

        let mut contracts = vec![];
        for _ in 0..contract_cnt {
            let contract_id_obj = host.register_test_contract_wasm(INVOKER_AUTH_TEST_CONTRACT);
            contracts.push(contract_id_obj.try_into_val(&host).unwrap());
        }
        Self { host, contracts }
    }

    fn test_fn_call(
        &self,
        contract_address: &Address,
        fn_name: &Symbol,
        args: &HostVec,
        expected_error: Option<(ScErrorType, ScErrorCode)>,
    ) {
        // Make sure we're always in the enforcing auth mode.
        self.host.set_authorization_entries(vec![]).unwrap();
        let res = self.host.call(
            contract_address.clone().into(),
            fn_name.clone(),
            args.clone().into(),
        );
        if let Some((error_type, error_code)) = expected_error {
            assert!(HostError::result_matches_err(res, (error_type, error_code)));
        } else {
            if !res.is_ok() {
                dbg!(res.clone().err().unwrap());
            }
            assert!(res.is_ok());
        }
    }

    fn test_auth_fn(
        &self,
        call: &InvokerCallNode,
        expected_error: Option<(ScErrorType, ScErrorCode)>,
    ) {
        let scenarios = call.generate_failure_scenarios(&self);
        for scenario in scenarios {
            self.test_fn_call(
                &self.contracts[0],
                &Symbol::try_from_val(&self.host, &"auth_fn").unwrap(),
                &test_vec![&self.host, scenario],
                expected_error,
            );
        }
    }
}

#[test]
fn test_invalid_invoker_auth_format() {
    let test = InvokerAuthTest::setup(1);
    let contract = &test.contracts[0];
    let args = test_vec![&test.host];
    // These two calls try to authorize SAC deployment, which is not allowed
    // and is handled in the auth library.
    test.test_fn_call(
        contract,
        &Symbol::try_from_val(&test.host, &"invoker_auth_with_sac_create").unwrap(),
        &args,
        Some((ScErrorType::Auth, ScErrorCode::InvalidInput)),
    );
    test.test_fn_call(
        contract,
        &Symbol::try_from_val(&test.host, &"invoker_auth_with_sac_create_v2").unwrap(),
        &args,
        Some((ScErrorType::Auth, ScErrorCode::InvalidInput)),
    );
    // This call passes a contract type that can't be parsed as
    // InvokerContractAuthEntry, so the error comes from the generic contract type
    // parsing code.
    test.test_fn_call(
        contract,
        &Symbol::try_from_val(&test.host, &"invoker_auth_with_bad_type").unwrap(),
        &args,
        Some((ScErrorType::Value, ScErrorCode::InvalidInput)),
    );
}

#[test]
fn test_invoker_auth_with_multiple_authorizers() {
    let test = InvokerAuthTest::setup(6);
    let auth_0 = vec![
        InvokerAuthNode {
            contract: 2,
            arg: 2,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 3,
            arg: 3,
            children: vec![
                InvokerAuthNode {
                    contract: 4,
                    arg: 4,
                    children: vec![],
                },
                InvokerAuthNode {
                    contract: 5,
                    arg: 5,
                    children: vec![],
                },
            ],
        },
    ];
    let auth_1 = vec![
        InvokerAuthNode {
            contract: 4,
            arg: 4,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 5,
            arg: 5,
            children: vec![],
        },
    ];
    let success_call = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            auth_1.clone(),
            vec![(0, 1)],
            vec![
                InvokerCallNode::no_fail(
                    3,
                    vec![],
                    vec![(0, 3), (1, 3)],
                    vec![
                        InvokerCallNode::no_fail(4, vec![], vec![(0, 4), (1, 4), (3, 4)], vec![]),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (1, 5), (3, 5)], vec![]),
                    ],
                ),
                InvokerCallNode::no_fail(2, vec![], vec![(0, 2), (1, 2)], vec![]),
            ],
        )],
    );

    let success_call_no_direct_invoker_auth = InvokerCallNode::no_fail(
        0,
        auth_0,
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            auth_1,
            vec![],
            vec![
                InvokerCallNode::no_fail(
                    3,
                    vec![],
                    vec![(0, 3)],
                    vec![
                        InvokerCallNode::no_fail(4, vec![], vec![(0, 4), (1, 4)], vec![]),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (1, 5)], vec![]),
                    ],
                ),
                InvokerCallNode::no_fail(2, vec![], vec![(0, 2)], vec![]),
            ],
        )],
    );

    test.test_auth_fn(&success_call_no_direct_invoker_auth, None);

    let mut success_call_with_extra_auth = success_call.clone();
    success_call_with_extra_auth.auth.push(InvokerAuthNode {
        contract: 2,
        arg: 3,
        children: vec![],
    });
    success_call_with_extra_auth.children[0].auth[0]
        .children
        .push(InvokerAuthNode {
            contract: 5,
            arg: 6,
            children: vec![],
        });
    test.test_auth_fn(&success_call_with_extra_auth, None);

    let mut failed_extra_auth_call = success_call.clone();
    failed_extra_auth_call.children[0]
        .children
        .push(InvokerCallNode::no_fail(2, vec![], vec![(0, 2)], vec![]));
    test.test_auth_fn(
        &failed_extra_auth_call,
        Some((ScErrorType::Auth, ScErrorCode::InvalidAction)),
    );

    let mut failed_extra_authorize_call = success_call.clone();
    failed_extra_authorize_call.children[0].children[0]
        .authorize
        // One more auth required from contract 0.
        .push((0, 3));
    test.test_auth_fn(
        &failed_extra_authorize_call,
        Some((ScErrorType::Auth, ScErrorCode::InvalidAction)),
    );

    let mut failed_non_invoker_auth_call = success_call.clone();
    failed_non_invoker_auth_call.children[0].children[0].children[0]
        .authorize
        // Auth required from contract 2 (not an invoker).
        .push((2, 4));
    test.test_auth_fn(
        &failed_non_invoker_auth_call,
        Some((ScErrorType::Auth, ScErrorCode::InvalidAction)),
    );
}

#[test]
fn test_failure_after_adding_invoker_auth() {
    let test = InvokerAuthTest::setup(4);
    let auth_0 = vec![InvokerAuthNode {
        contract: 2,
        arg: 2,
        children: vec![InvokerAuthNode {
            contract: 3,
            arg: 3,
            children: vec![],
        }],
    }];
    let unused_auth = vec![
        InvokerAuthNode {
            contract: 0,
            arg: 0,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 1,
            arg: 1,
            children: vec![],
        },
    ];

    let failure_call_without_children = InvokerCallNode::fail(0, auth_0.clone(), vec![], vec![]);
    test.test_auth_fn(
        &failure_call_without_children,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let failure_call_with_children = InvokerCallNode::fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            vec![],
            vec![(0, 1)],
            vec![InvokerCallNode::no_fail(
                2,
                vec![],
                vec![(0, 2), (1, 2)],
                vec![],
            )],
        )],
    );
    test.test_auth_fn(
        &failure_call_with_children,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let failure_call_with_children_and_unused_auth = InvokerCallNode::fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            unused_auth.clone(),
            vec![(0, 1)],
            vec![InvokerCallNode::no_fail(
                2,
                unused_auth.clone(),
                vec![(0, 2), (1, 2)],
                vec![],
            )],
        )],
    );
    test.test_auth_fn(
        &failure_call_with_children_and_unused_auth,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let failure_call_no_direct_auth = InvokerCallNode::fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            vec![],
            vec![],
            vec![InvokerCallNode::no_fail(2, vec![], vec![(0, 2)], vec![])],
        )],
    );
    test.test_auth_fn(
        &failure_call_no_direct_auth,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let auth_1 = vec![InvokerAuthNode {
        contract: 3,
        arg: 3,
        children: vec![],
    }];
    let deep_failure_call = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::fail(
            1,
            auth_1.clone(),
            vec![(0, 1)],
            vec![InvokerCallNode::no_fail(
                2,
                vec![],
                vec![(0, 2), (1, 2)],
                vec![InvokerCallNode::no_fail(
                    3,
                    vec![],
                    vec![(0, 3), (1, 3)],
                    vec![],
                )],
            )],
        )],
    );
    test.test_auth_fn(
        &deep_failure_call,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let multiple_failure_calls = InvokerCallNode::fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::fail(
            1,
            auth_1.clone(),
            vec![(0, 1)],
            vec![InvokerCallNode::no_fail(
                2,
                vec![],
                vec![(0, 2), (1, 2)],
                vec![InvokerCallNode::no_fail(
                    3,
                    vec![],
                    vec![(0, 3), (1, 3)],
                    vec![],
                )],
            )],
        )],
    );
    test.test_auth_fn(
        &multiple_failure_calls,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );

    let multiple_failure_calls_with_unused_auth = InvokerCallNode::fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::fail(
            1,
            auth_1,
            vec![(0, 1)],
            vec![InvokerCallNode::no_fail(
                2,
                unused_auth.clone(),
                vec![(0, 2), (1, 2)],
                vec![InvokerCallNode::no_fail(
                    3,
                    unused_auth.clone(),
                    vec![(0, 3), (1, 3)],
                    vec![],
                )],
            )],
        )],
    );
    test.test_auth_fn(
        &multiple_failure_calls_with_unused_auth,
        Some((ScErrorType::WasmVm, ScErrorCode::InvalidAction)),
    );
}

#[test]
fn test_invoker_auth_tree_with_rollbacks() {
    let test = InvokerAuthTest::setup(6);
    let auth_0 = vec![
        InvokerAuthNode {
            contract: 2,
            arg: 2,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 3,
            arg: 3,
            children: vec![
                InvokerAuthNode {
                    contract: 4,
                    arg: 4,
                    children: vec![],
                },
                InvokerAuthNode {
                    contract: 5,
                    arg: 5,
                    children: vec![],
                },
            ],
        },
    ];
    let auth_1 = vec![
        InvokerAuthNode {
            contract: 4,
            arg: 4,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 5,
            arg: 5,
            children: vec![],
        },
    ];

    let unused_auth = vec![
        InvokerAuthNode {
            contract: 0,
            arg: 0,
            children: vec![],
        },
        InvokerAuthNode {
            contract: 1,
            arg: 1,
            children: vec![],
        },
    ];

    let success_call_with_one_rollback = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            auth_1.clone(),
            vec![(0, 1)],
            vec![
                InvokerCallNode::no_fail(
                    3,
                    vec![],
                    vec![(0, 3), (1, 3)],
                    vec![
                        InvokerCallNode::try_fail(5, vec![], vec![(0, 5), (1, 5), (3, 5)], vec![]),
                        InvokerCallNode::no_fail(4, vec![], vec![(0, 4), (1, 4), (3, 4)], vec![]),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (1, 5), (3, 5)], vec![]),
                    ],
                ),
                InvokerCallNode::no_fail(2, vec![], vec![(0, 2), (1, 2)], vec![]),
            ],
        )],
    );

    test.test_auth_fn(&success_call_with_one_rollback, None);

    let success_call_with_multiple_rollbacks = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            unused_auth.clone(),
            vec![(0, 1)],
            vec![
                InvokerCallNode::try_fail(
                    3,
                    unused_auth.clone(),
                    vec![(0, 3), (1, 3)],
                    vec![
                        InvokerCallNode::no_fail(4, vec![], vec![(0, 4), (3, 4)], vec![]),
                        InvokerCallNode::try_fail(
                            5,
                            unused_auth.clone(),
                            vec![(0, 5), (3, 5)],
                            vec![],
                        ),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (3, 5)], vec![]),
                    ],
                ),
                InvokerCallNode::try_fail(2, unused_auth.clone(), vec![(0, 2), (1, 2)], vec![]),
                InvokerCallNode::no_fail(2, unused_auth.clone(), vec![(0, 2), (1, 2)], vec![]),
                InvokerCallNode::no_fail(
                    3,
                    unused_auth.clone(),
                    vec![(0, 3), (1, 3)],
                    vec![
                        InvokerCallNode::try_fail(
                            4,
                            unused_auth.clone(),
                            vec![(0, 4), (3, 4)],
                            vec![],
                        ),
                        InvokerCallNode::no_fail(
                            4,
                            unused_auth.clone(),
                            vec![(0, 4), (3, 4)],
                            vec![],
                        ),
                        InvokerCallNode::try_fail(5, vec![], vec![(0, 5), (3, 5)], vec![]),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (3, 5)], vec![]),
                    ],
                ),
            ],
        )],
    );
    test.test_auth_fn(&success_call_with_multiple_rollbacks, None);

    let success_call_with_multiple_rollbacks_and_auth_failures = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![InvokerCallNode::no_fail(
            1,
            unused_auth.clone(),
            vec![(0, 1)],
            vec![
                InvokerCallNode::try_no_fail(
                    3,
                    unused_auth.clone(),
                    // Second auth will fail, only call with 3 is authorized.
                    vec![(0, 3), (0, 4)],
                    vec![],
                ),
                InvokerCallNode::no_fail(
                    3,
                    unused_auth.clone(),
                    vec![(0, 3), (1, 3)],
                    vec![
                        InvokerCallNode::try_no_fail(
                            4,
                            unused_auth.clone(),
                            // Second auth will fail, contract 1 didn't
                            // authorize this.
                            vec![(0, 4), (1, 5)],
                            vec![],
                        ),
                        InvokerCallNode::no_fail(
                            4,
                            unused_auth.clone(),
                            vec![(0, 4), (3, 5)],
                            vec![],
                        ),
                        InvokerCallNode::try_fail(5, vec![], vec![(0, 5), (3, 5)], vec![]),
                        InvokerCallNode::no_fail(5, vec![], vec![(0, 5), (3, 5)], vec![]),
                    ],
                ),
                // Second auth for contract 0 will fail, only call with 2 is
                // authorized.
                InvokerCallNode::try_no_fail(
                    2,
                    unused_auth.clone(),
                    vec![(0, 2), (1, 2), (0, 3)],
                    vec![],
                ),
                InvokerCallNode::no_fail(2, vec![], vec![(0, 2), (1, 2)], vec![]),
            ],
        )],
    );
    test.test_auth_fn(
        &success_call_with_multiple_rollbacks_and_auth_failures,
        None,
    );
}

#[test]
fn test_multi_invoker_auth_deep_stack_with_rollbacks() {
    let test = InvokerAuthTest::setup(6);
    let auth_0 = vec![InvokerAuthNode {
        contract: 2,
        arg: 2,
        children: vec![InvokerAuthNode {
            contract: 3,
            arg: 3,
            children: vec![InvokerAuthNode {
                contract: 4,
                arg: 4,
                children: vec![InvokerAuthNode {
                    contract: 5,
                    arg: 5,
                    children: vec![],
                }],
            }],
        }],
    }];
    let auth_1 = vec![auth_0[0].children[0].clone()];
    let auth_2 = vec![auth_1[0].children[0].clone()];
    let auth_3 = vec![auth_2[0].children[0].clone()];
    let call_with_rollbacks = InvokerCallNode::no_fail(
        0,
        auth_0.clone(),
        vec![],
        vec![
            InvokerCallNode::try_fail(
                1,
                auth_1.clone(),
                vec![(0, 1)],
                vec![InvokerCallNode::no_fail(
                    2,
                    auth_2.clone(),
                    vec![(0, 2), (1, 2)],
                    vec![InvokerCallNode::no_fail(
                        3,
                        auth_3.clone(),
                        vec![(0, 3), (1, 3)],
                        vec![InvokerCallNode::no_fail(
                            4,
                            vec![],
                            vec![(0, 4), (1, 4), (2, 4), (3, 4)],
                            vec![InvokerCallNode::no_fail(
                                5,
                                vec![],
                                vec![(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)],
                                vec![],
                            )],
                        )],
                    )],
                )],
            ),
            InvokerCallNode::no_fail(
                1,
                auth_1.clone(),
                vec![(0, 1)],
                vec![InvokerCallNode::no_fail(
                    2,
                    auth_2.clone(),
                    vec![(0, 2), (1, 2)],
                    vec![
                        InvokerCallNode::try_fail(
                            3,
                            auth_3.clone(),
                            vec![(0, 3), (1, 3)],
                            vec![InvokerCallNode::no_fail(
                                4,
                                vec![],
                                vec![(0, 4), (1, 4), (2, 4), (3, 4)],
                                vec![InvokerCallNode::no_fail(
                                    5,
                                    vec![],
                                    vec![(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)],
                                    vec![],
                                )],
                            )],
                        ),
                        InvokerCallNode::no_fail(
                            3,
                            auth_3.clone(),
                            vec![(0, 3), (1, 3)],
                            vec![InvokerCallNode::no_fail(
                                4,
                                vec![],
                                vec![(0, 4), (1, 4), (2, 4), (3, 4)],
                                vec![
                                    InvokerCallNode::try_fail(
                                        5,
                                        vec![],
                                        vec![(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)],
                                        vec![],
                                    ),
                                    InvokerCallNode::no_fail(
                                        5,
                                        vec![],
                                        vec![(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)],
                                        vec![],
                                    ),
                                ],
                            )],
                        ),
                    ],
                )],
            ),
        ],
    );
    test.test_auth_fn(&call_with_rollbacks, None);
}
