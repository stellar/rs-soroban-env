use soroban_env_host::fees::{
    compute_transaction_resource_fee, FeeConfiguration, TransactionResources,
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
                metadata_size_bytes: 0,
                transaction_size_bytes: 0,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 100,
                fee_per_read_entry: 100,
                fee_per_write_entry: 100,
                fee_per_read_1kb: 100,
                fee_per_write_1kb: 100,
                fee_per_historical_1kb: 100,
                fee_per_metadata_1kb: 100,
                fee_per_propagate_1kb: 100,
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
                metadata_size_bytes: 1,
                transaction_size_bytes: 1,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 100,
                fee_per_read_entry: 100,
                fee_per_write_entry: 100,
                fee_per_read_1kb: 100,
                fee_per_write_1kb: 100,
                fee_per_historical_1kb: 100,
                fee_per_metadata_1kb: 100,
                fee_per_propagate_1kb: 100,
            },
        ),
        // 2 entry read + 1 write + 30 from TX_BASE_RESULT_SIZE + 1 for
        // everything else
        (335, 1)
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
                metadata_size_bytes: 321_654,
                transaction_size_bytes: 35_721,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: 1000,
                fee_per_read_entry: 2000,
                fee_per_write_entry: 4000,
                fee_per_read_1kb: 1500,
                fee_per_write_1kb: 3000,
                fee_per_historical_1kb: 300,
                fee_per_metadata_1kb: 200,
                fee_per_propagate_1kb: 900,
            },
        ),
        (1304913, 62824)
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
                metadata_size_bytes: u32::MAX,
                transaction_size_bytes: u32::MAX,
            },
            &FeeConfiguration {
                fee_per_instruction_increment: i64::MAX,
                fee_per_read_entry: i64::MAX,
                fee_per_write_entry: i64::MAX,
                fee_per_read_1kb: i64::MAX,
                fee_per_write_1kb: i64::MAX,
                fee_per_historical_1kb: i64::MAX,
                fee_per_metadata_1kb: i64::MAX,
                fee_per_propagate_1kb: i64::MAX,
            },
        ),
        // The refundable (metadata) fee is not i64::MAX because we do division
        // after multiplication and hence it's i64::MAX / 1024.
        // Hitting the integer size limits shouldn't be an issue in practice;
        // we need to just make sure there are no overflows.
        (i64::MAX, 9007199254740992)
    );
}
