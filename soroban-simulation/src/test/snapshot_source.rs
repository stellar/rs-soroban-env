use crate::network_config::NetworkConfig;
use crate::simulation::{RestoreOpSimulationResult, SimulationAdjustmentConfig};
use crate::snapshot_source::{AutoRestoringSnapshotSource, SimulationSnapshotSource};
use crate::testutils::{ledger_entry_to_ledger_key, temp_entry, MockSnapshotSource};
use pretty_assertions::assert_eq;
use soroban_env_host::e2e_testutils::{
    account_entry, get_account_id, ledger_entry, wasm_entry_non_validated,
};
use soroban_env_host::fees::{FeeConfiguration, RentFeeConfiguration};
use soroban_env_host::storage::SnapshotSource;
use soroban_env_host::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
    AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountEntryExtensionV3, ExtensionPoint,
    LedgerEntryData, LedgerFootprint, Liabilities, SequenceNumber, Signer, SignerKey,
    SorobanResources, SorobanTransactionData, SponsorshipDescriptor, Thresholds, TimePoint,
    Uint256,
};
use soroban_env_host::LedgerInfo;
use std::rc::Rc;

#[test]
fn test_automatic_restoration() {
    let ledger_seq = 300;
    let snapshot = Rc::new(
        MockSnapshotSource::from_entries(
            vec![
                (wasm_entry_non_validated(b"1"), Some(100)), // persistent, expired
                (wasm_entry_non_validated(b"2"), Some(299)), // persistent, expired
                (wasm_entry_non_validated(b"3"), Some(300)), // persistent, live
                (wasm_entry_non_validated(b"4"), Some(400)), // persistent, live
                (temp_entry(b"5"), Some(299)),               // temp, removed
                (temp_entry(b"6"), Some(300)),               // temp, live
                (temp_entry(b"7"), Some(400)),               // temp, live
            ],
            ledger_seq,
        )
        .unwrap(),
    );
    let ledger_info = LedgerInfo {
        sequence_number: ledger_seq,
        min_persistent_entry_ttl: 1000,
        ..Default::default()
    };
    let network_config = NetworkConfig {
        fee_configuration: FeeConfiguration {
            fee_per_instruction_increment: 8000,
            fee_per_read_entry: 500,
            fee_per_write_entry: 1000,
            fee_per_read_1kb: 30,
            fee_per_write_1kb: 60,
            fee_per_historical_1kb: 200,
            fee_per_contract_event_1kb: 500,
            fee_per_transaction_size_1kb: 300,
        },
        rent_fee_configuration: RentFeeConfiguration {
            fee_per_write_1kb: 5000,
            fee_per_write_entry: 1000,
            persistent_rent_rate_denominator: 10,
            temporary_rent_rate_denominator: 100,
        },
        ..Default::default()
    };
    let auto_restoring_snapshot = AutoRestoringSnapshotSource::new(snapshot, &ledger_info).unwrap();
    assert_eq!(
        auto_restoring_snapshot
            .simulate_restore_keys_op(
                &network_config,
                &SimulationAdjustmentConfig::no_adjustments(),
                &ledger_info
            )
            .unwrap(),
        None
    );

    let restored_entry_expiration = ledger_seq + 1000 - 1;

    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"1111")).unwrap()
            ))
            .unwrap(),
        None
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"1")).unwrap()
            ))
            .unwrap(),
        Some((
            Rc::new(wasm_entry_non_validated(b"1")),
            Some(restored_entry_expiration)
        ))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"2")).unwrap()
            ))
            .unwrap(),
        Some((
            Rc::new(wasm_entry_non_validated(b"2")),
            Some(restored_entry_expiration)
        ))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"3")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry_non_validated(b"3")), Some(300)))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"4")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry_non_validated(b"4")), Some(400)))
    );

    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&temp_entry(b"5")).unwrap()
            ))
            .unwrap(),
        None
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&temp_entry(b"6")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(temp_entry(b"6")), Some(300)))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&temp_entry(b"7")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(temp_entry(b"7")), Some(400)))
    );

    assert_eq!(
        auto_restoring_snapshot
            .simulate_restore_keys_op(
                &network_config,
                &SimulationAdjustmentConfig::no_adjustments(),
                &ledger_info
            )
            .unwrap(),
        Some(RestoreOpSimulationResult {
            transaction_data: SorobanTransactionData {
                ext: ExtensionPoint::V0,
                resources: SorobanResources {
                    footprint: LedgerFootprint {
                        read_only: Default::default(),
                        read_write: vec![
                            ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"1")).unwrap(),
                            ledger_entry_to_ledger_key(&wasm_entry_non_validated(b"2")).unwrap(),
                        ]
                        .try_into()
                        .unwrap()
                    },
                    instructions: 0,
                    read_bytes: 112,
                    write_bytes: 112,
                },
                resource_fee: 62192,
            }
        })
    );

    auto_restoring_snapshot.reset_restored_keys();
    assert_eq!(
        auto_restoring_snapshot
            .simulate_restore_keys_op(
                &network_config,
                &SimulationAdjustmentConfig::no_adjustments(),
                &ledger_info
            )
            .unwrap(),
        None
    );
}

#[test]
fn test_simulation_snapshot_source_creates_account_extensions() {
    let account_1 = get_account_id([111; 32]);
    let account_2 = get_account_id([222; 32]);
    let account_3 = get_account_id([33; 32]);
    let mut account_without_extensions = account_entry(&account_1);
    match &mut account_without_extensions.data {
        LedgerEntryData::Account(acc) => {
            acc.signers = vec![
                Signer {
                    key: SignerKey::Ed25519(Uint256([1; 32])),
                    weight: 1,
                },
                Signer {
                    key: SignerKey::Ed25519(Uint256([2; 32])),
                    weight: 2,
                },
            ]
            .try_into()
            .unwrap();
        }
        _ => (),
    }
    let mut account_with_ext_v2 = account_entry(&account_2);
    match &mut account_with_ext_v2.data {
        LedgerEntryData::Account(acc) => {
            acc.signers = vec![Signer {
                key: SignerKey::Ed25519(Uint256([1; 32])),
                weight: 1,
            }]
            .try_into()
            .unwrap();
            acc.ext = AccountEntryExt::V1(AccountEntryExtensionV1 {
                liabilities: Liabilities {
                    buying: 123,
                    selling: 456,
                },
                ext: AccountEntryExtensionV1Ext::V2(AccountEntryExtensionV2 {
                    num_sponsored: 5,
                    num_sponsoring: 6,
                    signer_sponsoring_i_ds: Default::default(),
                    ext: AccountEntryExtensionV2Ext::V0,
                }),
            });
        }
        _ => (),
    }
    let mut account_with_ext_v3 = account_entry(&account_3);
    match &mut account_with_ext_v3.data {
        LedgerEntryData::Account(acc) => {
            acc.ext = AccountEntryExt::V1(AccountEntryExtensionV1 {
                liabilities: Liabilities {
                    buying: 789,
                    selling: 987,
                },
                ext: AccountEntryExtensionV1Ext::V2(AccountEntryExtensionV2 {
                    num_sponsored: 2,
                    num_sponsoring: 3,
                    signer_sponsoring_i_ds: Default::default(),
                    ext: AccountEntryExtensionV2Ext::V3(AccountEntryExtensionV3 {
                        ext: ExtensionPoint::V0,
                        seq_ledger: 150,
                        seq_time: TimePoint(111),
                    }),
                }),
            });
        }
        _ => (),
    };
    let inner_snapshot = MockSnapshotSource::from_entries(
        vec![
            (account_without_extensions.clone(), None),
            (account_with_ext_v2.clone(), None),
            (account_with_ext_v3.clone(), None),
        ],
        300,
    )
    .unwrap();
    let snapshot = SimulationSnapshotSource::new(&inner_snapshot);
    assert_eq!(
        snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&account_entry(&get_account_id([0; 32]))).unwrap()
            ))
            .unwrap(),
        None
    );
    assert_eq!(
        snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&account_without_extensions).unwrap()
            ))
            .unwrap(),
        Some((
            Rc::new(ledger_entry(LedgerEntryData::Account(AccountEntry {
                account_id: get_account_id([111; 32]),
                balance: 10_000_000,
                seq_num: SequenceNumber(0),
                num_sub_entries: 0,
                inflation_dest: None,
                flags: 0,
                home_domain: Default::default(),
                thresholds: Thresholds([1, 0, 0, 0]),
                signers: vec![
                    Signer {
                        key: SignerKey::Ed25519(Uint256([1; 32])),
                        weight: 1,
                    },
                    Signer {
                        key: SignerKey::Ed25519(Uint256([2; 32])),
                        weight: 2,
                    },
                ]
                .try_into()
                .unwrap(),
                ext: AccountEntryExt::V1(AccountEntryExtensionV1 {
                    liabilities: Liabilities {
                        buying: 0,
                        selling: 0,
                    },
                    ext: AccountEntryExtensionV1Ext::V2(AccountEntryExtensionV2 {
                        num_sponsored: 0,
                        num_sponsoring: 0,
                        signer_sponsoring_i_ds: vec![SponsorshipDescriptor(None); 2]
                            .try_into()
                            .unwrap(),
                        ext: AccountEntryExtensionV2Ext::V3(AccountEntryExtensionV3 {
                            ext: ExtensionPoint::V0,
                            seq_ledger: 0,
                            seq_time: TimePoint(0),
                        }),
                    }),
                }),
            }))),
            None
        ))
    );

    assert_eq!(
        snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&account_with_ext_v2).unwrap()
            ))
            .unwrap(),
        Some((
            Rc::new(ledger_entry(LedgerEntryData::Account(AccountEntry {
                account_id: get_account_id([222; 32]),
                balance: 10_000_000,
                seq_num: SequenceNumber(0),
                num_sub_entries: 0,
                inflation_dest: None,
                flags: 0,
                home_domain: Default::default(),
                thresholds: Thresholds([1, 0, 0, 0]),
                signers: vec![Signer {
                    key: SignerKey::Ed25519(Uint256([1; 32])),
                    weight: 1,
                }]
                .try_into()
                .unwrap(),
                ext: AccountEntryExt::V1(AccountEntryExtensionV1 {
                    liabilities: Liabilities {
                        buying: 123,
                        selling: 456,
                    },
                    ext: AccountEntryExtensionV1Ext::V2(AccountEntryExtensionV2 {
                        num_sponsored: 5,
                        num_sponsoring: 6,
                        signer_sponsoring_i_ds: Default::default(),
                        ext: AccountEntryExtensionV2Ext::V3(AccountEntryExtensionV3 {
                            ext: ExtensionPoint::V0,
                            seq_ledger: 0,
                            seq_time: TimePoint(0),
                        }),
                    }),
                }),
            }))),
            None
        ))
    );
    assert_eq!(
        snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&account_with_ext_v3).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(account_with_ext_v3), None))
    );
}
