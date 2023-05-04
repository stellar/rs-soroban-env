/// This module defines the fee computation protocol for Soroban.
///
/// This is technically not part of the Soroban host and is provided here for
/// the sake of sharing between the systems that run Soroban host (such as
/// Stellar core or Soroban RPC service).
const INSTRUCTIONS_INCREMENT: i64 = 10000;
const DATA_SIZE_1KB_INCREMENT: i64 = 1024;
const TX_BASE_RESULT_SIZE: u32 = 300;

/// These are the resource upper bounds specified by the Soroban transaction.
pub struct TransactionResources {
    /// Number of CPU instructions.
    pub instructions: u32,
    /// Number of ledger entries the transaction reads.
    pub read_entries: u32,
    /// Number of ledger entries the transaction writes (these are also counted
    /// as entries that are being read for the sake of the respective fees).
    pub write_entries: u32,
    /// Number of bytes read from ledger.
    pub read_bytes: u32,
    /// Number of bytes written to ledger.
    pub write_bytes: u32,
    /// Size of the metadata that transaction emits. Consists of the size of
    /// the events XDR, the size of writeable entries XDR before the transaction
    /// is applied, the size of writeable entries XDR after the transaction is
    /// applied.
    pub metadata_size_bytes: u32,
    /// Size of the transaction XDR.
    pub transaction_size_bytes: u32,
}

/// Fee-related network configuration.
///
/// This should be normally loaded from the ledger.
pub struct FeeConfiguration {
    /// Fee per `INSTRUCTIONS_INCREMENT=10000` instructions.
    pub fee_per_instruction_increment: i64,
    /// Fee per 1 entry read from ledger.
    pub fee_per_read_entry: i64,
    /// Fee per 1 entry written to ledger.
    pub fee_per_write_entry: i64,
    /// Fee per 1KB read from ledger.
    pub fee_per_read_1kb: i64,
    /// Fee per 1KB written to ledger.
    pub fee_per_write_1kb: i64,
    /// Fee per 1KB written to history (the history write size is based on
    /// transaction size and `TX_BASE_RESULT_SIZE`).
    pub fee_per_historical_1kb: i64,
    /// Fee per 1KB of metadata written.
    pub fee_per_metadata_1kb: i64,
    /// Fee per 1KB propagate to the network (the propagated size is equal to
    /// the transaction size).
    pub fee_per_propagate_1kb: i64,
}

/// Computes the resource fee for a transaction based on the resource
/// consumption and the fee-related network configuration.
///
/// Returns a pair of `(fee, refundable_fee)`, where refundable fee is also
/// included into the final fee.
pub fn compute_transaction_resource_fee(
    tx_resources: &TransactionResources,
    fee_config: &FeeConfiguration,
) -> (i64, i64) {
    let compute_fee = compute_fee_per_increment(
        tx_resources.instructions,
        fee_config.fee_per_instruction_increment,
        INSTRUCTIONS_INCREMENT,
    );
    let ledger_read_entry_fee: i64 = fee_config.fee_per_read_entry.saturating_mul(
        tx_resources
            .read_entries
            .saturating_add(tx_resources.write_entries)
            .into(),
    );
    let ledger_write_entry_fee = fee_config
        .fee_per_write_entry
        .saturating_mul(tx_resources.write_entries.into());
    let ledger_read_bytes_fee = compute_fee_per_increment(
        tx_resources.read_bytes,
        fee_config.fee_per_read_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );
    let ledger_write_bytes_fee = compute_fee_per_increment(
        tx_resources.write_bytes,
        fee_config.fee_per_write_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let historical_fee = compute_fee_per_increment(
        tx_resources
            .transaction_size_bytes
            .saturating_add(TX_BASE_RESULT_SIZE),
        fee_config.fee_per_historical_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let meta_data_fee = compute_fee_per_increment(
        tx_resources.metadata_size_bytes,
        fee_config.fee_per_metadata_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let bandwidth_fee = compute_fee_per_increment(
        tx_resources.transaction_size_bytes,
        fee_config.fee_per_propagate_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let refundable_fee = meta_data_fee;
    let non_refundable_fee = compute_fee
        .saturating_add(ledger_read_entry_fee)
        .saturating_add(ledger_write_entry_fee)
        .saturating_add(ledger_read_bytes_fee)
        .saturating_add(ledger_write_bytes_fee)
        .saturating_add(historical_fee)
        .saturating_add(bandwidth_fee);
    let res = (
        refundable_fee.saturating_add(non_refundable_fee),
        refundable_fee,
    );
    res
}

fn compute_fee_per_increment(resource_value: u32, fee_rate: i64, increment: i64) -> i64 {
    let resource_val: i64 = resource_value.into();
    num_integer::div_ceil(resource_val.saturating_mul(fee_rate), increment)
}
