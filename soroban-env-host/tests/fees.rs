use soroban_env_host::fees::{
    compute_rent_fee, compute_transaction_resource_fee, compute_write_fee_per_1kb,
    FeeConfiguration, LedgerEntryRentChange, RentFeeConfiguration, TransactionResources,
    WriteFeeConfiguration,
};

#[test]
fn resource_fee_computation() {
    // No resources
    assert_eq!(
        compute_transaction_resource_fee(
            &TransactionResources {
                instructions: 0,
                read_entries: 0,
                write_entries: 0,
                read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                transaction_size_bytes: 0,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 100,
                fee_per_read_entry: 100,
                fee_per_write_entry: 100,
                fee_per_read_1kb: 100,
                fee_per_write_1kb: 100,
                fee_per_historical_1kb: 100,
                fee_per_contract_event_1kb: 100,
                fee_per_transaction_size_1kb: 100,
            },
        ),
        // 30 comes from TX_BASE_RESULT_SIZE
        (30, 0)
    );

    // Minimal resources
    assert_eq!(
        compute_transaction_resource_fee(
            &TransactionResources {
                instructions: 1,
                read_entries: 1,
                write_entries: 1,
                read_bytes: 1,
                write_bytes: 1,
                contract_events_size_bytes: 1,
                transaction_size_bytes: 1,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 100,
                fee_per_read_entry: 100,
                fee_per_write_entry: 100,
                fee_per_read_1kb: 100,
                fee_per_write_1kb: 100,
                fee_per_historical_1kb: 100,
                fee_per_contract_event_1kb: 100,
                fee_per_transaction_size_1kb: 100,
            },
        ),
        // 2 entry read + 1 write + 30 from TX_BASE_RESULT_SIZE + 1 for
        // everything else
        (334, 1)
    );

    // Different resource/fee values
    assert_eq!(
        compute_transaction_resource_fee(
            &TransactionResources {
                instructions: 10_123_456,
                read_entries: 30,
                write_entries: 10,
                read_bytes: 25_600,
                write_bytes: 10_340,
                contract_events_size_bytes: 321_654,
                transaction_size_bytes: 35_721,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 1000,
                fee_per_read_entry: 2000,
                fee_per_write_entry: 4000,
                fee_per_read_1kb: 1500,
                fee_per_write_1kb: 3000,
                fee_per_historical_1kb: 300,
                fee_per_contract_event_1kb: 200,
                fee_per_transaction_size_1kb: 900,
            },
        ),
        (1_242_089, 62824)
    );

    // Integer limits
    assert_eq!(
        compute_transaction_resource_fee(
            &TransactionResources {
                instructions: u32::MAX,
                read_entries: u32::MAX,
                write_entries: u32::MAX,
                read_bytes: u32::MAX,
                write_bytes: u32::MAX,
                contract_events_size_bytes: u32::MAX,
                transaction_size_bytes: u32::MAX,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: i64::MAX,
                fee_per_read_entry: i64::MAX,
                fee_per_write_entry: i64::MAX,
                fee_per_read_1kb: i64::MAX,
                fee_per_write_1kb: i64::MAX,
                fee_per_historical_1kb: i64::MAX,
                fee_per_contract_event_1kb: i64::MAX,
                fee_per_transaction_size_1kb: i64::MAX,
            },
        ),
        // The refundable (events) fee is not i64::MAX because we do division
        // after multiplication and hence it's i64::MAX / 1024.
        // Hitting the integer size limits shouldn't be an issue in practice;
        // we need to just make sure there are no overflows.
        (i64::MAX, 9_007_199_254_740_992)
    );
}

#[test]
fn test_rent_bump_fees_with_only_bump() {
    let fee_config = RentFeeConfiguration {
        fee_per_write_1kb: 1000,
        persistent_rent_rate_denominator: 10_000,
        temporary_rent_rate_denominator: 100_000,
    };

    // Minimal size
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 1,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            50_000,
        ),
        // 1 * 1000 * 200_000 / (10_000 * 1024)
        20
    );

    // Minimal ledgers
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 10 * 1024,
                new_size_bytes: 10 * 1024,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_001,
            }],
            &fee_config,
            50_000,
        ),
        // ceil(10 * 1024 * 1000 * 1 / (10_000 * 1024))
        1
    );

    // Minimal ledgers & size
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 1,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_001,
            }],
            &fee_config,
            50_000,
        ),
        // ceil(1 * 1000 * 1 / (10_000 * 1024))
        1
    );

    // No size change
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 10 * 1024,
                new_size_bytes: 10 * 1024,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            50_000,
        ),
        // 10 * 1024 * 1000 * 200_000 / (10_000 * 1024)
        200_000
    );

    // Size decrease
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 10 * 1024,
                new_size_bytes: 5 * 1024,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            50_000,
        ),
        // 5 * 1024 * 1000 * 200_000 / (10_000 * 1024)
        100_000
    );

    // Temp storage rate
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: false,
                old_size_bytes: 10 * 1024,
                new_size_bytes: 10 * 1024,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            50_000,
        ),
        // 10 * 1024 * 1000 * 200_000 / (100_000 * 1024)
        20_000
    );

    // Multiple entries
    assert_eq!(
        compute_rent_fee(
            &vec![
                LedgerEntryRentChange {
                    is_persistent: false,
                    old_size_bytes: 10 * 1024,
                    new_size_bytes: 10 * 1024,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 300_000,
                },
                LedgerEntryRentChange {
                    is_persistent: true,
                    old_size_bytes: 10 * 1024,
                    new_size_bytes: 10 * 1024,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 300_000,
                },
                LedgerEntryRentChange {
                    is_persistent: true,
                    old_size_bytes: 1,
                    new_size_bytes: 1,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 100_001,
                },
                LedgerEntryRentChange {
                    is_persistent: true,
                    old_size_bytes: 1,
                    new_size_bytes: 1,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 300_000,
                }
            ],
            &fee_config,
            50_000,
        ),
        // 20_000 + 200_000 + 1 + 20
        220_021
    );
}

#[test]
fn test_rent_bump_fees_with_only_size_change() {
    let fee_config = RentFeeConfiguration {
        fee_per_write_1kb: 1000,
        persistent_rent_rate_denominator: 10_000,
        temporary_rent_rate_denominator: 100_000,
    };

    // Large size increase
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            25_000,
        ),
        // 99_999 * 1000 * (100_000 - 25_000 + 1) / (10_000 * 1024)
        732_425
    );

    // Large size increase, temp storage
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: false,
                old_size_bytes: 1,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            25_000,
        ),
        // 99_999 * 1000 * (100_000 - 25_000 + 1) / (1_000 * 1024)
        73_243
    );

    // Small size increase
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 99_999,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            25_000,
        ),
        // ceil(1 * 1000 * (100_000 - 25_000 + 1) / (10_000 * 1024))
        8
    );

    // Small ledger difference
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            99_999,
        ),
        // ceil(99_999 * 1000 * (100_000 - 99_999 + 1) / (10_000 * 1024))
        20
    );

    // Multiple entries
    assert_eq!(
        compute_rent_fee(
            &vec![
                LedgerEntryRentChange {
                    is_persistent: true,
                    old_size_bytes: 1,
                    new_size_bytes: 100_000,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 100_000,
                },
                LedgerEntryRentChange {
                    is_persistent: false,
                    old_size_bytes: 1,
                    new_size_bytes: 100_000,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 100_000,
                }
            ],
            &fee_config,
            25_000,
        ),
        // 732_425 + 73_243
        805_668
    );
}

#[test]
fn test_rent_bump_with_size_change_and_bump() {
    let fee_config = RentFeeConfiguration {
        fee_per_write_1kb: 1000,
        persistent_rent_rate_denominator: 10_000,
        temporary_rent_rate_denominator: 100_000,
    };

    // Persistent entry
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            25_000,
        ),
        // 100_000 * 1000 * 200_000 / (10_000 * 1024) +
        // 99_999 * 1000 * (100_000 - 25_000 + 1) / (10_000 * 1024)
        2_685_550
    );

    // Temp entry
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: false,
                old_size_bytes: 1,
                new_size_bytes: 100_000,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 300_000,
            }],
            &fee_config,
            25_000,
        ),
        // 100_000 * 1000 * 200_000 / (10_000 * 1024) +
        // 99_999 * 1000 * (100_000 - 25_000 + 1) / (10_000 * 1024)
        268_556
    );

    // Multiple entries
    assert_eq!(
        compute_rent_fee(
            &vec![
                LedgerEntryRentChange {
                    is_persistent: true,
                    old_size_bytes: 1,
                    new_size_bytes: 100_000,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 300_000,
                },
                LedgerEntryRentChange {
                    is_persistent: false,
                    old_size_bytes: 1,
                    new_size_bytes: 100_000,
                    old_expiration_ledger: 100_000,
                    new_expiration_ledger: 300_000,
                }
            ],
            &fee_config,
            25_000,
        ),
        // 2_685_550 + 268_556
        2_954_106
    );

    // Small increments
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 1,
                new_size_bytes: 2,
                old_expiration_ledger: 100_000,
                new_expiration_ledger: 100_001,
            }],
            &fee_config,
            99_999,
        ),
        // ceil(2 * 1000 * 1 / (10_000 * 1024)) +
        // ceil(1 * 1000 * (100_000 - 99_999 + 1) / (10_000 * 1024))
        2
    );
}

#[test]
fn test_rent_bump_without_old_entry() {
    let fee_config = RentFeeConfiguration {
        fee_per_write_1kb: 1000,
        persistent_rent_rate_denominator: 10_000,
        temporary_rent_rate_denominator: 100_000,
    };

    // Persistent storage
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: true,
                old_size_bytes: 0,
                new_size_bytes: 100_000,
                old_expiration_ledger: 0,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            25_000,
        ),
        // 100_000 * 1000 * (100_000 - 25_000) / (10_000 * 1024)
        732_432
    );

    // Temp storage
    assert_eq!(
        compute_rent_fee(
            &vec![LedgerEntryRentChange {
                is_persistent: false,
                old_size_bytes: 0,
                new_size_bytes: 100_000,
                old_expiration_ledger: 0,
                new_expiration_ledger: 100_000,
            }],
            &fee_config,
            25_000,
        ),
        // 100_000 * 1000 * (100_000 - 25_000) / (10_000 * 1024)
        73_244
    );
}

#[test]
fn test_compute_write_fee() {
    let fee_config = WriteFeeConfiguration {
        bucket_list_target_size_bytes: 100_000,
        write_fee_1kb_bucket_list_low: 100,
        write_fee_1kb_bucket_list_high: 10_000,
        bucket_list_write_fee_growth_factor: 50,
    };
    // Empty bucket list
    assert_eq!(compute_write_fee_per_1kb(0, &fee_config), 100);
    // Partially filled bucket list
    assert_eq!(
        compute_write_fee_per_1kb(50_000, &fee_config),
        100 + (10_000 - 100) / 2
    );
    assert_eq!(compute_write_fee_per_1kb(56_789, &fee_config), 5723);
    // Full bucket list
    assert_eq!(compute_write_fee_per_1kb(100_000, &fee_config), 10_000);
    // Bucket list bigger than target
    assert_eq!(
        compute_write_fee_per_1kb(150_000, &fee_config),
        10_000 + 50 * (10_000 - 100) / 2
    );
    // Bucket list several times bigger than target
    assert_eq!(
        compute_write_fee_per_1kb(580_000, &fee_config),
        10_000 + 2_376_000
    );

    let large_fee_config = WriteFeeConfiguration {
        bucket_list_target_size_bytes: 100_000_000_000_000,
        write_fee_1kb_bucket_list_low: 1_000_000,
        write_fee_1kb_bucket_list_high: 1_000_000_000,
        bucket_list_write_fee_growth_factor: 50,
    };
    // Large bucket list size and fees, half-filled bucket list
    assert_eq!(
        compute_write_fee_per_1kb(50_000_000_000_000, &large_fee_config),
        1_000_000 + (1_000_000_000 - 1_000_000) / 2
    );
    // Large bucket list size and fees, over target bucket list
    assert_eq!(
        compute_write_fee_per_1kb(150_000_000_000_000, &large_fee_config),
        1_000_000_000 + 50 * (1_000_000_000 - 1_000_000) / 2
    );
}
