use crate::network_config::NetworkConfig;
use crate::simulation::{RestoreOpSimulationResult, SimulationAdjustmentConfig};
use crate::snapshot_source::AutoRestoringSnapshotSource;
use crate::testutils::{ledger_entry_to_ledger_key, temp_entry, MockSnapshotSource};
use pretty_assertions::assert_eq;
use soroban_env_host::e2e_testutils::wasm_entry;
use soroban_env_host::fees::{FeeConfiguration, RentFeeConfiguration};
use soroban_env_host::storage::SnapshotSource;
use soroban_env_host::xdr::{
    ExtensionPoint, LedgerFootprint, SorobanResources, SorobanTransactionData,
};
use soroban_env_host::LedgerInfo;
use std::rc::Rc;

#[test]
fn test_automatic_restoration() {
    let ledger_seq = 300;
    let snapshot = Rc::new(
        MockSnapshotSource::from_entries(
            vec![
                (wasm_entry(b"1"), Some(100)), // persistent, expired
                (wasm_entry(b"2"), Some(299)), // persistent, expired
                (wasm_entry(b"3"), Some(300)), // persistent, live
                (wasm_entry(b"4"), Some(400)), // persistent, live
                (temp_entry(b"5"), Some(299)), // temp, removed
                (temp_entry(b"6"), Some(300)), // temp, live
                (temp_entry(b"7"), Some(400)), // temp, live
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
                ledger_entry_to_ledger_key(&wasm_entry(b"1111")).unwrap()
            ))
            .unwrap(),
        None
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry(b"1")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry(b"1")), Some(restored_entry_expiration)))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry(b"2")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry(b"2")), Some(restored_entry_expiration)))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry(b"3")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry(b"3")), Some(300)))
    );
    assert_eq!(
        auto_restoring_snapshot
            .get(&Rc::new(
                ledger_entry_to_ledger_key(&wasm_entry(b"4")).unwrap()
            ))
            .unwrap(),
        Some((Rc::new(wasm_entry(b"4")), Some(400)))
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
                            ledger_entry_to_ledger_key(&wasm_entry(b"1")).unwrap(),
                            ledger_entry_to_ledger_key(&wasm_entry(b"2")).unwrap(),
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
