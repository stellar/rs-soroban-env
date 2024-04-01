use crate::simulation::{
    simulate_extend_ttl_op, simulate_invoke_host_function_op, simulate_restore_op,
    ExtendTtlOpSimulationResult, LedgerEntryDiff, RestoreOpSimulationResult,
    SimulationAdjustmentConfig, SimulationAdjustmentFactor,
};
use crate::testutils::{
    ledger_entry_to_ledger_key, temp_entry, MockSnapshotSource, CURRENT_PROTOCOL_VERSION,
};
use crate::NetworkConfig;
use pretty_assertions::assert_eq;
use soroban_env_host::e2e_testutils::{
    account_entry, auth_contract_invocation, bytes_sc_val, create_contract_auth,
    default_ledger_info, get_account_id, get_contract_id_preimage, get_wasm_hash, get_wasm_key,
    ledger_entry, upload_wasm_host_fn, wasm_entry, wasm_entry_non_validated,
    AuthContractInvocationNode, CreateContractData,
};
use soroban_env_host::fees::{FeeConfiguration, RentFeeConfiguration};
use soroban_env_host::xdr::{
    ContractCostParamEntry, ContractCostParams, ContractCostType, ContractDataDurability,
    ContractDataEntry, ExtensionPoint, LedgerEntry, LedgerEntryData, LedgerFootprint, LedgerKey,
    LedgerKeyContractData, ScAddress, ScErrorCode, ScErrorType, ScNonceKey, ScVal,
    SorobanAddressCredentials, SorobanAuthorizationEntry, SorobanCredentials, SorobanResources,
    SorobanTransactionData,
};
use soroban_env_host::HostError;
use soroban_test_wasms::{ADD_I32, AUTH_TEST_CONTRACT};
use std::rc::Rc;
use tap::prelude::*;

fn default_network_config() -> NetworkConfig {
    let default_entry = ContractCostParamEntry {
        ext: ExtensionPoint::V0,
        const_term: 0,
        linear_term: 0,
    };
    let mut cpu_cost_params = vec![default_entry.clone(); ContractCostType::variants().len()];
    let mut mem_cost_params = vec![default_entry; ContractCostType::variants().len()];
    for i in 0..ContractCostType::variants().len() {
        let v = i as i64;
        cpu_cost_params[i].const_term = (v + 1) * 1000;
        cpu_cost_params[i].linear_term = v << 7;
        mem_cost_params[i].const_term = (v + 1) * 500;
        mem_cost_params[i].linear_term = v << 6;
    }
    let ledger_info = default_ledger_info();

    NetworkConfig {
        fee_configuration: FeeConfiguration {
            fee_per_instruction_increment: 10,
            fee_per_read_entry: 20,
            fee_per_write_entry: 30,
            fee_per_read_1kb: 40,
            fee_per_write_1kb: 50,
            fee_per_historical_1kb: 60,
            fee_per_contract_event_1kb: 70,
            fee_per_transaction_size_1kb: 80,
        },
        rent_fee_configuration: RentFeeConfiguration {
            fee_per_write_1kb: 50,
            fee_per_write_entry: 30,
            persistent_rent_rate_denominator: 100,
            temporary_rent_rate_denominator: 1000,
        },
        tx_max_instructions: 100_000_000,
        tx_memory_limit: 40_000_000,
        cpu_cost_params: ContractCostParams(cpu_cost_params.try_into().unwrap()),
        memory_cost_params: ContractCostParams(mem_cost_params.try_into().unwrap()),
        min_temp_entry_ttl: ledger_info.min_temp_entry_ttl,
        min_persistent_entry_ttl: ledger_info.min_persistent_entry_ttl,
        max_entry_ttl: ledger_info.max_entry_ttl,
    }
}

fn test_adjustment_config() -> SimulationAdjustmentConfig {
    SimulationAdjustmentConfig {
        instructions: SimulationAdjustmentFactor::new(1.1, 100_000),
        read_bytes: SimulationAdjustmentFactor::new(1.2, 500),
        write_bytes: SimulationAdjustmentFactor::new(1.3, 300),
        tx_size: SimulationAdjustmentFactor::new(1.4, 1000),
        refundable_fee: SimulationAdjustmentFactor::new(1.5, 100_000),
    }
}

fn nonce_key(address: ScAddress, nonce: i64) -> LedgerKey {
    LedgerKey::ContractData(LedgerKeyContractData {
        contract: address,
        key: ScVal::LedgerKeyNonce(ScNonceKey { nonce }),
        durability: ContractDataDurability::Temporary,
    })
}

fn nonce_entry(address: ScAddress, nonce: i64) -> LedgerEntry {
    ledger_entry(LedgerEntryData::ContractData(ContractDataEntry {
        ext: ExtensionPoint::V0,
        contract: address,
        key: ScVal::LedgerKeyNonce(ScNonceKey { nonce }),
        durability: ContractDataDurability::Temporary,
        val: ScVal::Void,
    }))
}

#[test]
fn test_simulate_upload_wasm() {
    let source_account = get_account_id([123; 32]);
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let snapshot_source =
        Rc::new(MockSnapshotSource::from_entries(vec![], ledger_info.sequence_number).unwrap());

    let res = simulate_invoke_host_function_op(
        snapshot_source.clone(),
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        upload_wasm_host_fn(ADD_I32),
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );

    assert_eq!(res.auth, vec![]);
    assert!(res.contract_events.is_empty());
    assert!(res.diagnostic_events.is_empty());

    let (expected_instructions, expected_write_bytes, expected_resource_fee, expected_mem_bytes) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (1505493, 636, 33063, 752745)
        } else {
            (1644789, 684, 35548, 822393)
        };
    assert_eq!(
        res.transaction_data,
        Some(SorobanTransactionData {
            ext: ExtensionPoint::V0,
            resources: SorobanResources {
                footprint: LedgerFootprint {
                    read_only: Default::default(),
                    read_write: vec![get_wasm_key(ADD_I32)].try_into().unwrap()
                },
                instructions: expected_instructions,
                read_bytes: 0,
                write_bytes: expected_write_bytes,
            },
            resource_fee: expected_resource_fee,
        })
    );
    assert_eq!(res.simulated_instructions, expected_instructions);
    assert_eq!(res.simulated_memory, expected_mem_bytes);
    assert_eq!(
        res.modified_entries,
        vec![LedgerEntryDiff {
            state_before: None,
            state_after: Some(wasm_entry(ADD_I32))
        }]
    );

    let res_with_adjustments = simulate_invoke_host_function_op(
        snapshot_source,
        &network_config,
        &test_adjustment_config(),
        &ledger_info,
        upload_wasm_host_fn(ADD_I32),
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert_eq!(
        res_with_adjustments.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );

    assert_eq!(res_with_adjustments.auth, res.auth);
    assert_eq!(res_with_adjustments.contract_events, res.contract_events);
    assert_eq!(
        res_with_adjustments.diagnostic_events,
        res.diagnostic_events
    );
    assert_eq!(
        res_with_adjustments.simulated_instructions,
        res.simulated_instructions
    );
    assert_eq!(res_with_adjustments.simulated_memory, res.simulated_memory);
    let expected_adjusted_resource_fee = if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION
    {
        133367
    } else {
        135867
    };
    assert_eq!(
        res_with_adjustments.transaction_data,
        Some(SorobanTransactionData {
            ext: ExtensionPoint::V0,
            resources: SorobanResources {
                footprint: LedgerFootprint {
                    read_only: Default::default(),
                    read_write: vec![get_wasm_key(ADD_I32)].try_into().unwrap()
                },
                instructions: (expected_instructions as f64 * 1.1) as u32,
                read_bytes: 0,
                write_bytes: expected_write_bytes + 300,
            },
            resource_fee: expected_adjusted_resource_fee,
        })
    );
}

#[test]
fn test_simulation_returns_insufficient_budget_error() {
    let source_account = get_account_id([123; 32]);
    let ledger_info = default_ledger_info();
    let mut network_config = default_network_config();
    network_config.tx_max_instructions = 100_000;
    let snapshot_source =
        Rc::new(MockSnapshotSource::from_entries(vec![], ledger_info.sequence_number).unwrap());

    let res = simulate_invoke_host_function_op(
        snapshot_source.clone(),
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        upload_wasm_host_fn(ADD_I32),
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    assert_eq!(res.auth, vec![]);
    assert!(res.contract_events.is_empty());
    assert!(res.diagnostic_events.is_empty());

    assert_eq!(res.transaction_data, None);
    assert_eq!(res.simulated_instructions, 111516);
    assert_eq!(res.simulated_memory, 45006);
    assert_eq!(res.modified_entries, vec![]);
}

#[test]
fn test_simulation_returns_logic_error() {
    let source_account = get_account_id([123; 32]);
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let snapshot_source =
        Rc::new(MockSnapshotSource::from_entries(vec![], ledger_info.sequence_number).unwrap());
    let bad_wasm = [0; 1000];

    let res = simulate_invoke_host_function_op(
        snapshot_source.clone(),
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        upload_wasm_host_fn(&bad_wasm),
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    assert_eq!(res.auth, vec![]);
    assert!(res.contract_events.is_empty());
    assert!(!res.diagnostic_events.is_empty());

    assert_eq!(res.transaction_data, None);
    assert_eq!(res.simulated_instructions, 154568);
    assert_eq!(res.simulated_memory, 77284);
    assert_eq!(res.modified_entries, vec![]);
}

#[test]
fn test_simulate_create_contract() {
    let source_account = get_account_id([123; 32]);
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let contract = CreateContractData::new([1; 32], ADD_I32);

    let snapshot_source = Rc::new(
        MockSnapshotSource::from_entries(
            vec![(
                contract.wasm_entry,
                Some(ledger_info.sequence_number + 1000),
            )],
            ledger_info.sequence_number,
        )
        .unwrap(),
    );

    let res = simulate_invoke_host_function_op(
        snapshot_source.clone(),
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        contract.host_fn.clone(),
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(contract.contract_address)
    );

    assert_eq!(
        res.auth,
        vec![create_contract_auth(
            &get_contract_id_preimage(&contract.deployer, &[1; 32]),
            ADD_I32,
        )]
    );
    assert!(res.contract_events.is_empty());
    assert!(res.diagnostic_events.is_empty());
    let (expected_instructions, expected_read_bytes, expected_resource_fee, expected_mem_bytes) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (929703, 636, 6490, 464846)
        } else {
            (1014327, 684, 6577, 507158)
        };
    assert_eq!(
        res.transaction_data,
        Some(SorobanTransactionData {
            ext: ExtensionPoint::V0,
            resources: SorobanResources {
                footprint: LedgerFootprint {
                    read_only: vec![contract.wasm_key.clone()].try_into().unwrap(),
                    read_write: vec![contract.contract_key.clone()].try_into().unwrap()
                },
                instructions: expected_instructions,
                read_bytes: expected_read_bytes,
                write_bytes: 104,
            },
            resource_fee: expected_resource_fee,
        })
    );
    assert_eq!(res.simulated_instructions, expected_instructions);
    assert_eq!(res.simulated_memory, expected_mem_bytes);
    assert_eq!(
        res.modified_entries,
        vec![LedgerEntryDiff {
            state_before: None,
            state_after: Some(contract.contract_entry)
        }]
    );
}

#[test]
fn test_simulate_invoke_contract_with_auth() {
    let contracts = vec![
        CreateContractData::new([1; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([2; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([3; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([4; 32], AUTH_TEST_CONTRACT),
    ];

    let tree = AuthContractInvocationNode {
        address: contracts[0].contract_address.clone(),
        children: vec![
            AuthContractInvocationNode {
                address: contracts[1].contract_address.clone(),
                children: vec![AuthContractInvocationNode {
                    address: contracts[2].contract_address.clone(),
                    children: vec![AuthContractInvocationNode {
                        address: contracts[3].contract_address.clone(),
                        children: vec![],
                    }],
                }],
            },
            AuthContractInvocationNode {
                address: contracts[2].contract_address.clone(),
                children: vec![
                    AuthContractInvocationNode {
                        address: contracts[1].contract_address.clone(),
                        children: vec![],
                    },
                    AuthContractInvocationNode {
                        address: contracts[3].contract_address.clone(),
                        children: vec![],
                    },
                ],
            },
        ],
    };
    let source_account = get_account_id([123; 32]);
    let other_account = get_account_id([124; 32]);
    let host_fn = auth_contract_invocation(
        vec![
            ScAddress::Account(source_account.clone()),
            ScAddress::Account(other_account.clone()),
        ],
        tree.clone(),
    );
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let snapshot_source = Rc::new(
        MockSnapshotSource::from_entries(
            vec![
                (
                    contracts[0].wasm_entry.clone(),
                    Some(ledger_info.sequence_number + 100),
                ),
                (
                    contracts[0].contract_entry.clone(),
                    Some(ledger_info.sequence_number + 1000),
                ),
                (
                    contracts[1].contract_entry.clone(),
                    Some(ledger_info.sequence_number + 1000),
                ),
                (
                    contracts[2].contract_entry.clone(),
                    Some(ledger_info.sequence_number + 1000),
                ),
                (
                    contracts[3].contract_entry.clone(),
                    Some(ledger_info.sequence_number + 1000),
                ),
                // Source account doesn't need to be accessed
                (account_entry(&other_account), None),
            ],
            ledger_info.sequence_number,
        )
        .unwrap(),
    );

    let res = simulate_invoke_host_function_op(
        snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        host_fn,
        None,
        &source_account,
        [1; 32],
        true,
    )
    .unwrap();
    assert_eq!(res.invoke_result.unwrap(), ScVal::Void);

    let other_account_address = ScAddress::Account(other_account.clone());
    // This value is stable thanks to hardcoded RNG seed.
    let other_account_nonce = 1039859045797838027;
    let expected_auth_tree = tree.into_authorized_invocation();
    assert_eq!(
        res.auth,
        vec![
            SorobanAuthorizationEntry {
                credentials: SorobanCredentials::SourceAccount,
                root_invocation: expected_auth_tree.clone(),
            },
            SorobanAuthorizationEntry {
                credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                    address: other_account_address.clone(),
                    nonce: other_account_nonce,
                    signature_expiration_ledger: 0,
                    signature: ScVal::Void,
                }),
                root_invocation: expected_auth_tree,
            }
        ]
    );
    assert!(res.contract_events.is_empty());
    assert!(!res.diagnostic_events.is_empty());

    let (expected_instructions, expected_read_bytes, expected_resource_fee, expected_mem_bytes) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (38210172, 7492, 39363, 19104958)
        } else {
            (41268119, 7540, 42423, 20633919)
        };

    assert_eq!(
        res.transaction_data,
        Some(SorobanTransactionData {
            ext: ExtensionPoint::V0,
            resources: SorobanResources {
                footprint: LedgerFootprint {
                    read_only: vec![
                        ledger_entry_to_ledger_key(&account_entry(&other_account)).unwrap(),
                        contracts[0].contract_key.clone(),
                        contracts[1].contract_key.clone(),
                        contracts[2].contract_key.clone(),
                        contracts[3].contract_key.clone(),
                        contracts[0].wasm_key.clone(),
                    ]
                    .tap_mut(|v| v.sort())
                    .try_into()
                    .unwrap(),
                    read_write: vec![nonce_key(
                        other_account_address.clone(),
                        other_account_nonce
                    )]
                    .try_into()
                    .unwrap()
                },
                instructions: expected_instructions,
                read_bytes: expected_read_bytes,
                write_bytes: 76,
            },
            resource_fee: expected_resource_fee,
        })
    );
    assert_eq!(res.simulated_instructions, expected_instructions);
    assert_eq!(res.simulated_memory, expected_mem_bytes);
    assert_eq!(
        res.modified_entries,
        vec![LedgerEntryDiff {
            state_before: None,
            state_after: Some(nonce_entry(other_account_address, other_account_nonce))
        }]
    );
}

#[test]
fn test_simulate_extend_ttl_op() {
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let contract_entry = CreateContractData::new([111; 32], ADD_I32).contract_entry;
    let entries = vec![
        (
            wasm_entry(ADD_I32),
            Some(ledger_info.sequence_number + 100_000),
        ),
        (
            wasm_entry(AUTH_TEST_CONTRACT),
            Some(ledger_info.sequence_number + 100),
        ),
        (contract_entry, Some(ledger_info.sequence_number + 500_000)),
        (
            wasm_entry_non_validated(b"123"),
            Some(ledger_info.sequence_number + 1_000_000),
        ),
        (temp_entry(b"321"), Some(ledger_info.sequence_number + 100)),
        (
            temp_entry(b"123"),
            Some(ledger_info.sequence_number + 100_000),
        ),
        (
            temp_entry(b"456"),
            Some(ledger_info.sequence_number + 1_000_000),
        ),
    ];
    let mut keys: Vec<LedgerKey> = entries
        .iter()
        .map(|e| ledger_entry_to_ledger_key(&e.0).unwrap())
        .collect();
    let snapshot_source =
        MockSnapshotSource::from_entries(entries, ledger_info.sequence_number).unwrap();

    let no_op_extension = simulate_extend_ttl_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
        100,
    )
    .unwrap();
    assert_eq!(
        no_op_extension,
        ExtendTtlOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: Default::default()
                    },
                    instructions: 0,
                    read_bytes: 0,
                    write_bytes: 0,
                },
                resource_fee: 280,
            }
        }
    );

    let extension_for_some_entries = simulate_extend_ttl_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
        100_001,
    )
    .unwrap();
    let (expected_read_bytes, expected_resource_fee) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (7712, 339313)
        } else {
            (7808, 341657)
        };
    assert_eq!(
        extension_for_some_entries,
        ExtendTtlOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: vec![
                            keys[0].clone(),
                            keys[1].clone(),
                            keys[4].clone(),
                            keys[5].clone(),
                        ]
                        .tap_mut(|v| v.sort())
                        .try_into()
                        .unwrap(),
                        read_write: Default::default()
                    },
                    instructions: 0,
                    read_bytes: expected_read_bytes,
                    write_bytes: 0,
                },
                resource_fee: expected_resource_fee,
            }
        }
    );

    let extension_for_all_entries = simulate_extend_ttl_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
        1_000_001,
    )
    .unwrap();
    let (expected_read_bytes, expected_resource_fee) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (7944, 3697000)
        } else {
            (8040, 3741533)
        };
    assert_eq!(
        extension_for_all_entries,
        ExtendTtlOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: keys.clone().tap_mut(|v| v.sort()).try_into().unwrap(),
                        read_write: Default::default()
                    },
                    instructions: 0,
                    read_bytes: expected_read_bytes,
                    write_bytes: 0,
                },
                resource_fee: expected_resource_fee,
            }
        }
    );

    // Non-existent entry should be just skipped.
    keys.push(get_wasm_key(b"abc"));
    let extension_for_all_entries_with_non_existent = simulate_extend_ttl_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
        1_000_001,
    )
    .unwrap();
    assert_eq!(
        extension_for_all_entries,
        extension_for_all_entries_with_non_existent
    );

    let extension_for_all_entries_with_adjustment = simulate_extend_ttl_op(
        &snapshot_source,
        &network_config,
        &test_adjustment_config(),
        &ledger_info,
        &keys,
        1_000_001,
    )
    .unwrap();
    let expected_resource_fee = if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
        5545310
    } else {
        5612108
    };
    assert_eq!(
        extension_for_all_entries_with_adjustment,
        ExtendTtlOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: extension_for_all_entries
                        .transaction_data
                        .resources
                        .footprint,
                    instructions: 0,
                    read_bytes: (expected_read_bytes as f64 * 1.2) as u32,
                    write_bytes: 0,
                },
                resource_fee: expected_resource_fee,
            }
        }
    );
}

#[test]
fn test_simulate_restore_op() {
    let mut ledger_info = default_ledger_info();
    let network_config = default_network_config();
    let contract_entry = CreateContractData::new([111; 32], ADD_I32).contract_entry;
    let entries = vec![
        (
            wasm_entry(ADD_I32),
            Some(ledger_info.sequence_number + 100_000),
        ),
        (
            wasm_entry(AUTH_TEST_CONTRACT),
            Some(ledger_info.sequence_number + 100),
        ),
        (contract_entry, Some(ledger_info.sequence_number + 500_000)),
        (
            wasm_entry_non_validated(b"123"),
            Some(ledger_info.sequence_number + 1_000_000),
        ),
    ];
    let keys: Vec<LedgerKey> = entries
        .iter()
        .map(|e| ledger_entry_to_ledger_key(&e.0).unwrap())
        .collect();
    let snapshot_source =
        MockSnapshotSource::from_entries(entries, ledger_info.sequence_number).unwrap();

    let no_op_restoration = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
    )
    .unwrap();

    assert_eq!(
        no_op_restoration,
        RestoreOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: Default::default()
                    },
                    instructions: 0,
                    read_bytes: 0,
                    write_bytes: 0,
                },
                resource_fee: 279,
            }
        }
    );

    let init_seq_num = ledger_info.sequence_number;
    ledger_info.sequence_number = init_seq_num + 100_001;
    let restoration_for_some_entries = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
    )
    .unwrap();
    let (expected_rw_bytes, expected_resource_fee) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (7568, 370692)
        } else {
            (7664, 375389)
        };
    assert_eq!(
        restoration_for_some_entries,
        RestoreOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: vec![keys[0].clone(), keys[1].clone(),]
                            .tap_mut(|v| v.sort())
                            .try_into()
                            .unwrap()
                    },
                    instructions: 0,
                    read_bytes: expected_rw_bytes,
                    write_bytes: expected_rw_bytes,
                },
                resource_fee: expected_resource_fee,
            }
        }
    );

    ledger_info.sequence_number = init_seq_num + 1_000_001;
    let extension_for_all_entries = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &keys,
    )
    .unwrap();
    let (expected_rw_bytes, expected_resource_fee) =
        if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION {
            (7728, 378736)
        } else {
            (7824, 383433)
        };
    assert_eq!(
        extension_for_all_entries,
        RestoreOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: keys.clone().tap_mut(|v| v.sort()).try_into().unwrap()
                    },
                    instructions: 0,
                    read_bytes: expected_rw_bytes,
                    write_bytes: expected_rw_bytes,
                },
                resource_fee: expected_resource_fee,
            }
        }
    );

    let extension_for_all_entries_with_adjustment = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &test_adjustment_config(),
        &ledger_info,
        &keys,
    )
    .unwrap();
    let expected_adjusted_resource_fee = if ledger_info.protocol_version == CURRENT_PROTOCOL_VERSION
    {
        567785
    } else {
        574827
    };
    assert_eq!(
        extension_for_all_entries_with_adjustment,
        RestoreOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: keys.clone().tap_mut(|v| v.sort()).try_into().unwrap()
                    },
                    instructions: 0,
                    read_bytes: (expected_rw_bytes as f64 * 1.2) as u32,
                    write_bytes: (expected_rw_bytes as f64 * 1.3) as u32,
                },
                resource_fee: expected_adjusted_resource_fee,
            }
        }
    );
}

#[test]
fn test_simulate_restore_op_returns_error_for_temp_entries() {
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();

    let snapshot_source = MockSnapshotSource::from_entries(
        vec![(temp_entry(b"123"), Some(ledger_info.sequence_number - 10))],
        ledger_info.sequence_number,
    )
    .unwrap();

    let res = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &[ledger_entry_to_ledger_key(&temp_entry(b"123")).unwrap()],
    );
    assert!(res.is_err());
}

#[test]
fn test_simulate_restore_op_returns_error_for_non_existent_entry() {
    let ledger_info = default_ledger_info();
    let network_config = default_network_config();

    let snapshot_source =
        MockSnapshotSource::from_entries(vec![], ledger_info.sequence_number).unwrap();

    let res = simulate_restore_op(
        &snapshot_source,
        &network_config,
        &SimulationAdjustmentConfig::no_adjustments(),
        &ledger_info,
        &[get_wasm_key(b"123")],
    );
    assert!(res.is_err());
}
