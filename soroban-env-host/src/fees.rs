/// This module defines the fee computation protocol for Soroban.
///
/// This is technically not part of the Soroban host and is provided here for
/// the sake of sharing between the systems that run Soroban host (such as
/// Stellar core or Soroban RPC service).
///
/// Rough estimate of the base size of any transaction result in the archives
/// (independent of the transaction envelope size).
pub const TX_BASE_RESULT_SIZE: u32 = 300;
/// Estimate for any `TtlEntry` ledger entry
pub const TTL_ENTRY_SIZE: u32 = 48;

pub const INSTRUCTIONS_INCREMENT: i64 = 10000;
pub const DATA_SIZE_1KB_INCREMENT: i64 = 1024;

// minimum effective write fee per 1KB
pub const MINIMUM_WRITE_FEE_PER_1KB: i64 = 1000;

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
    /// Size of the contract events XDR.
    pub contract_events_size_bytes: u32,
    /// Size of the transaction XDR.
    pub transaction_size_bytes: u32,
}

/// Fee-related network configuration.
///
/// This should be normally loaded from the ledger, with exception of the
/// `fee_per_write_1kb`, that has to be computed via `compute_write_fee_per_1kb`
/// function.

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FeeConfiguration {
    /// Fee per `INSTRUCTIONS_INCREMENT=10000` instructions.
    pub fee_per_instruction_increment: i64,
    /// Fee per 1 entry read from ledger.
    pub fee_per_read_entry: i64,
    /// Fee per 1 entry written to ledger.
    pub fee_per_write_entry: i64,
    /// Fee per 1KB read from ledger.
    pub fee_per_read_1kb: i64,
    /// Fee per 1KB written to ledger. This has to be computed via
    /// `compute_write_fee_per_1kb`.
    pub fee_per_write_1kb: i64,
    /// Fee per 1KB written to history (the history write size is based on
    /// transaction size and `TX_BASE_RESULT_SIZE`).
    pub fee_per_historical_1kb: i64,
    /// Fee per 1KB of contract events written.
    pub fee_per_contract_event_1kb: i64,
    /// Fee per 1KB of transaction size.
    pub fee_per_transaction_size_1kb: i64,
}

/// Network configuration used to determine the ledger write fee.
///
/// This should be normally loaded from the ledger.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct WriteFeeConfiguration {
    // Write fee grows linearly until bucket list reaches this size.
    pub bucket_list_target_size_bytes: i64,
    // Fee per 1KB write when the bucket list is empty.
    pub write_fee_1kb_bucket_list_low: i64,
    // Fee per 1KB write when the bucket list has reached
    // `bucket_list_target_size_bytes`.
    pub write_fee_1kb_bucket_list_high: i64,
    // Write fee multiplier for any additional data past the first
    // `bucket_list_target_size_bytes`.
    pub bucket_list_write_fee_growth_factor: u32,
}

/// Change in a single ledger entry with parameters relevant for rent fee
/// computations.
///
/// This represents the entry state before and after transaction has been
/// applied.
pub struct LedgerEntryRentChange {
    /// Whether this is persistent or temporary entry.
    pub is_persistent: bool,
    /// Size of the entry in bytes before it has been modified, including the
    /// key.
    /// `0` for newly-created entires.
    pub old_size_bytes: u32,
    /// Size of the entry in bytes after it has been modified, including the
    /// key.
    pub new_size_bytes: u32,
    /// Live until ledger of the entry before it has been modified.
    /// Should be less than the current ledger for newly-created entires.
    pub old_live_until_ledger: u32,
    /// Live until ledger of the entry after it has been modified.
    pub new_live_until_ledger: u32,
}

/// Rent fee-related network configuration.
///
/// This should be normally loaded from the ledger, with exception of the
/// `fee_per_write_1kb`, that has to be computed via `compute_write_fee_per_1kb`
/// function.

#[derive(Debug, Default, PartialEq, Eq)]
pub struct RentFeeConfiguration {
    /// Fee per 1KB written to ledger.
    /// This is the same field as in `FeeConfiguration` and it has to be
    /// computed via `compute_write_fee_per_1kb`.
    pub fee_per_write_1kb: i64,
    /// Fee per 1 entry written to ledger.
    /// This is the same field as in `FeeConfiguration`.
    pub fee_per_write_entry: i64,
    /// Denominator for the total rent fee for persistent storage.
    ///
    /// This can be thought of as the number of ledgers of rent that costs as
    /// much, as writing the entry for the first time (i.e. if the value is
    /// `1000`, then we would charge the entry write fee for every 1000 ledgers
    /// of rent).
    pub persistent_rent_rate_denominator: i64,
    /// Denominator for the total rent fee for temporary storage.
    ///
    /// This has the same semantics as `persistent_rent_rate_denominator`.
    pub temporary_rent_rate_denominator: i64,
}

/// Computes the resource fee for a transaction based on the resource
/// consumption and the fee-related network configuration.
///
/// This can handle unsantized user inputs for `tx_resources`, but expects
/// sane configuration.
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

    let events_fee = compute_fee_per_increment(
        tx_resources.contract_events_size_bytes,
        fee_config.fee_per_contract_event_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let bandwidth_fee = compute_fee_per_increment(
        tx_resources.transaction_size_bytes,
        fee_config.fee_per_transaction_size_1kb,
        DATA_SIZE_1KB_INCREMENT,
    );

    let refundable_fee = events_fee;
    let non_refundable_fee = compute_fee
        .saturating_add(ledger_read_entry_fee)
        .saturating_add(ledger_write_entry_fee)
        .saturating_add(ledger_read_bytes_fee)
        .saturating_add(ledger_write_bytes_fee)
        .saturating_add(historical_fee)
        .saturating_add(bandwidth_fee);

    (non_refundable_fee, refundable_fee)
}

// Helper for clamping values to the range of positive i64, with
// invalid cases mapped to i64::MAX.
trait ClampFee {
    fn clamp_fee(self) -> i64;
}

impl ClampFee for i64 {
    fn clamp_fee(self) -> i64 {
        if self < 0 {
            // Negatives shouldn't be possible -- they're banned in the logic
            // that sets most of the configs, and we're only using i64 for XDR
            // sake, ultimately I think compatibility with java which only has
            // signed types -- anyway we're assuming i64::MAX is more likely the
            // safest in-band default-value for erroneous cses, since it's more
            // likely to fail a tx, than to open a "0 cost tx" DoS vector.
            i64::MAX
        } else {
            self
        }
    }
}

impl ClampFee for i128 {
    fn clamp_fee(self) -> i64 {
        if self < 0 {
            i64::MAX
        } else {
            i64::try_from(self).unwrap_or(i64::MAX)
        }
    }
}

/// Computes the effective write fee per 1 KB of data written to ledger.
///
/// The computed fee should be used in fee configuration for
/// `compute_transaction_resource_fee` function.
///
/// This depends only on the current ledger (more specifically, bucket list)
/// size.
pub fn compute_write_fee_per_1kb(
    bucket_list_size_bytes: i64,
    fee_config: &WriteFeeConfiguration,
) -> i64 {
    let fee_rate_multiplier = fee_config
        .write_fee_1kb_bucket_list_high
        .saturating_sub(fee_config.write_fee_1kb_bucket_list_low)
        .clamp_fee();
    let mut write_fee_per_1kb: i64;
    if bucket_list_size_bytes < fee_config.bucket_list_target_size_bytes {
        // Convert multipliers to i128 to make sure we can handle large bucket list
        // sizes.
        write_fee_per_1kb = num_integer::div_ceil(
            (fee_rate_multiplier as i128).saturating_mul(bucket_list_size_bytes as i128),
            (fee_config.bucket_list_target_size_bytes as i128).max(1),
        )
        .clamp_fee();
        // no clamp_fee here
        write_fee_per_1kb =
            write_fee_per_1kb.saturating_add(fee_config.write_fee_1kb_bucket_list_low);
    } else {
        write_fee_per_1kb = fee_config.write_fee_1kb_bucket_list_high;
        let bucket_list_size_after_reaching_target =
            bucket_list_size_bytes.saturating_sub(fee_config.bucket_list_target_size_bytes);
        let post_target_fee = num_integer::div_ceil(
            (fee_rate_multiplier as i128)
                .saturating_mul(bucket_list_size_after_reaching_target as i128)
                .saturating_mul(fee_config.bucket_list_write_fee_growth_factor as i128),
            (fee_config.bucket_list_target_size_bytes as i128).max(1),
        )
        .clamp_fee();
        write_fee_per_1kb = write_fee_per_1kb.saturating_add(post_target_fee);
    }

    write_fee_per_1kb.max(MINIMUM_WRITE_FEE_PER_1KB)
}

/// Computes the total rent-related fee for the provided ledger entry changes.
///
/// The rent-related fees consist of the fees for TTL extensions and fees for
/// increasing the entry size (with or without TTL extensions).
///
/// This cannot handle unsantized inputs and relies on sane configuration and
/// ledger changes. This is due to the fact that rent is managed automatically
/// wihtout user-provided inputs.
pub fn compute_rent_fee(
    changed_entries: &[LedgerEntryRentChange],
    fee_config: &RentFeeConfiguration,
    current_ledger_seq: u32,
) -> i64 {
    let mut fee: i64 = 0;
    let mut extended_entries: i64 = 0;
    let mut extended_entry_key_size_bytes: u32 = 0;
    for e in changed_entries {
        fee = fee.saturating_add(rent_fee_per_entry_change(e, fee_config, current_ledger_seq));
        if e.old_live_until_ledger < e.new_live_until_ledger {
            extended_entries = extended_entries.saturating_add(1);
            extended_entry_key_size_bytes =
                extended_entry_key_size_bytes.saturating_add(TTL_ENTRY_SIZE);
        }
    }
    // The TTL extensions need to be written to the ledger. As they have
    // constant size, we can charge for writing them independently of the actual
    // entry size.
    fee = fee.saturating_add(
        fee_config
            .fee_per_write_entry
            .saturating_mul(extended_entries),
    );
    fee = fee.saturating_add(compute_fee_per_increment(
        extended_entry_key_size_bytes,
        fee_config.fee_per_write_1kb,
        DATA_SIZE_1KB_INCREMENT,
    ));

    fee
}

// Size of half-open range (lo, hi], or None if lo>hi
fn exclusive_ledger_diff(lo: u32, hi: u32) -> Option<u32> {
    hi.checked_sub(lo)
}

// Size of closed range [lo, hi] or None if lo>hi
fn inclusive_ledger_diff(lo: u32, hi: u32) -> Option<u32> {
    exclusive_ledger_diff(lo, hi).map(|diff| diff.saturating_add(1))
}

impl LedgerEntryRentChange {
    fn entry_is_new(&self) -> bool {
        self.old_size_bytes == 0 && self.old_live_until_ledger == 0
    }

    fn extension_ledgers(&self, current_ledger: u32) -> Option<u32> {
        let ledger_before_extension = if self.entry_is_new() {
            current_ledger.saturating_sub(1)
        } else {
            self.old_live_until_ledger
        };
        exclusive_ledger_diff(ledger_before_extension, self.new_live_until_ledger)
    }

    fn prepaid_ledgers(&self, current_ledger: u32) -> Option<u32> {
        if self.entry_is_new() {
            None
        } else {
            inclusive_ledger_diff(current_ledger, self.old_live_until_ledger)
        }
    }

    fn size_increase(&self) -> Option<u32> {
        self.new_size_bytes.checked_sub(self.old_size_bytes)
    }
}

fn rent_fee_per_entry_change(
    entry_change: &LedgerEntryRentChange,
    fee_config: &RentFeeConfiguration,
    current_ledger: u32,
) -> i64 {
    let mut fee: i64 = 0;
    // If there was a difference-in-expiration, pay for the new ledger range
    // at the new size.
    if let Some(rent_ledgers) = entry_change.extension_ledgers(current_ledger) {
        fee = fee.saturating_add(rent_fee_for_size_and_ledgers(
            entry_change.is_persistent,
            entry_change.new_size_bytes,
            rent_ledgers,
            fee_config,
        ));
    }

    // If there were some ledgers already paid for at an old size, and the size
    // of the entry increased, those pre-paid ledgers need to pay top-up fees to
    // account for the change in size.
    if let (Some(rent_ledgers), Some(entry_size)) = (
        entry_change.prepaid_ledgers(current_ledger),
        entry_change.size_increase(),
    ) {
        fee = fee.saturating_add(rent_fee_for_size_and_ledgers(
            entry_change.is_persistent,
            entry_size,
            rent_ledgers,
            fee_config,
        ));
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
    let num = (entry_size as i64)
        .saturating_mul(fee_config.fee_per_write_1kb)
        .saturating_mul(rent_ledgers as i64);
    let storage_coef = if is_persistent {
        fee_config.persistent_rent_rate_denominator
    } else {
        fee_config.temporary_rent_rate_denominator
    };
    let denom = DATA_SIZE_1KB_INCREMENT.saturating_mul(storage_coef);
    num_integer::div_ceil(num, denom.max(1))
}

fn compute_fee_per_increment(resource_value: u32, fee_rate: i64, increment: i64) -> i64 {
    let resource_val: i64 = resource_value.into();
    num_integer::div_ceil(resource_val.saturating_mul(fee_rate), increment.max(1))
}
