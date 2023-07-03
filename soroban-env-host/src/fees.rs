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

/// Change in a single ledger entry with parameters relevant for rent fee
/// computations.
/// 
/// This represents the entry state before and after transaction has been
/// applied.
pub struct LedgerEntryRentChange {
    /// Whether this is persistent or temporary entry.
    pub is_persistent: bool,
    /// Size of the entry in bytes before it has been modified.
    /// `0` for newly-created entires.
    pub old_size_bytes: u32,
    /// Size of the entry in bytes after it has been modified.
    pub new_size_bytes: u32,
    /// Expiration ledger of the entry before it has been modified.
    /// `0` for newly-created entires.
    pub old_expiration_ledger: u32,
    /// Expiration ledger of the entry after it has been modified.
    pub new_expiration_ledger: u32,
}

/// Rent fee-related network configuration.
///
/// This should be normally loaded from the ledger.
pub struct RentFeeConfiguration {
    /// Fee per 1KB written to ledger.
    /// This is the same field as in `FeeConfiguration`.
    pub fee_per_write_1kb: i64,
    /// Denominator for the total rent fee for persistent storage.
    /// 
    /// This can be thought of as the number of ledgers of rent that costs as
    /// much, as writing the entry for the first time (i.e. if the value is
    /// `1000`, then we would charge the entry write fee for every 1000 ledgers
    /// of rent).
    pub persistent_rent_rate_denomniator: i64,
    /// Denominator for the total rent fee for temporary storage.
    /// 
    /// This has the same semantics as `persistent_rent_rate_denomniator`.
    pub temporary_rent_rate_denomniator: i64,
}

/// Computes the resource fee for a transaction based on the resource
/// consumption and the fee-related network configuration.
///
/// This can handle unsantized user inputs.
/// 
/// Returns a pair of `(non_refundable_fee, refundable_fee)` that represent
/// non-refundable and refundable resource fee components respectively.
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

    (non_refundable_fee, refundable_fee)
}

/// Computes the total rent-related fee for the provided ledger entry changes.
/// 
/// The rent-related fees consist of the fees for rent bumps and fees for 
/// increasing the entry size (with or without rent bump).
/// 
/// This cannot handle unsantized inputs and relies on sane configuration and
/// ledger changes. This is due to the fact that rent is managed automatically
/// wihtout user-provided inputs.
pub fn compute_rent_fee(
    changed_entries: &Vec<LedgerEntryRentChange>,
    fee_config: &RentFeeConfiguration,
    current_ledger_seq: u32,
) -> i64 {
    let mut fee = 0;
    for e in changed_entries {
        fee += rent_fee_per_entry_change(e, fee_config, current_ledger_seq);
    }
    fee
}

fn rent_fee_per_entry_change(
    entry_change: &LedgerEntryRentChange,
    fee_config: &RentFeeConfiguration,
    current_ledger: u32,
) -> i64 {
    let mut fee = 0;
    // Pay for the rent extension (if any).
    if entry_change.new_expiration_ledger > entry_change.old_expiration_ledger {
        fee += rent_fee_for_size_and_ledgers(
            entry_change.is_persistent,
            // New portion of rent is payed for the new size of the entry.
            entry_change.new_size_bytes,
            // Rent should be covered until `old_expiration_ledger` (or start 
            // from the current ledger for new entries), so don't include it
            // into the number of rent ledgers.
            entry_change.new_expiration_ledger
                - entry_change.old_expiration_ledger.max(current_ledger - 1),
            fee_config,
        );
    }
    // Pay for the entry size increase (if any).
    if entry_change.new_size_bytes > entry_change.old_size_bytes && entry_change.old_size_bytes > 0
    {
        fee += rent_fee_for_size_and_ledgers(
            entry_change.is_persistent,
            // Pay only for the size increase.
            entry_change.new_size_bytes - entry_change.old_size_bytes,
            // Cover ledger interval [current; old], as (old, new] is already
            // covered above for the whole new size.
            entry_change.old_expiration_ledger - current_ledger + 1,
            fee_config,
        );
    }

    fee
}

fn rent_fee_for_size_and_ledgers(
    is_persistent: bool,
    entry_size: u32,
    rent_ledgers: u32,
    fee_config: &RentFeeConfiguration,
) -> i64 {
    // Multiplication can overflow here - unlike fee computation this can rely
    // on sane input parameters as rent fee computation does not depend on any
    // user inputs.
    let num = entry_size as i64 * fee_config.fee_per_write_1kb as i64 * rent_ledgers as i64;
    let storage_coef = if is_persistent {
        fee_config.persistent_rent_rate_denomniator
    } else {
        fee_config.temporary_rent_rate_denomniator
    };
    let denom = DATA_SIZE_1KB_INCREMENT * storage_coef;
    num_integer::div_ceil(num, denom)
}

fn compute_fee_per_increment(resource_value: u32, fee_rate: i64, increment: i64) -> i64 {
    let resource_val: i64 = resource_value.into();
    num_integer::div_ceil(resource_val.saturating_mul(fee_rate), increment)
}
