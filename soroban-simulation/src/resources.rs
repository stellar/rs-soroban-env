use super::snapshot_source::SnapshotSourceWithArchive;
use crate::network_config::NetworkConfig;
use crate::simulation::{SimulationAdjustmentConfig, SimulationAdjustmentFactor};
use anyhow::{anyhow, ensure, Context, Result};

use soroban_env_host::{
    e2e_invoke::{extract_rent_changes, LedgerEntryChange},
    fees::{
        compute_rent_fee, compute_transaction_resource_fee, LedgerEntryRentChange,
        TransactionResources,
    },
    ledger_info::get_key_durability,
    storage::SnapshotSource,
    xdr::{
        BytesM, ContractDataDurability, DecoratedSignature, Duration, ExtensionPoint, Hash,
        LedgerBounds, LedgerFootprint, LedgerKey, Limits, Memo, MuxedAccount, MuxedAccountMed25519,
        Operation, OperationBody, Preconditions, PreconditionsV2, ReadXdr, SequenceNumber,
        Signature, SignatureHint, SignerKey, SignerKeyEd25519SignedPayload, SorobanResources,
        SorobanTransactionData, TimeBounds, TimePoint, Transaction, TransactionExt,
        TransactionV1Envelope, Uint256, WriteXdr,
    },
    LedgerInfo, DEFAULT_XDR_RW_LIMITS,
};
use std::convert::TryInto;
use std::rc::Rc;

impl SimulationAdjustmentConfig {
    fn adjust_resources(&self, resources: &mut SorobanResources) {
        resources.instructions = self.instructions.adjust_u32(resources.instructions);
        resources.read_bytes = self.read_bytes.adjust_u32(resources.read_bytes);
        resources.write_bytes = self.write_bytes.adjust_u32(resources.write_bytes);
    }
}

impl SimulationAdjustmentFactor {
    fn adjust_u32(&self, value: u32) -> u32 {
        // `0` typically means that resource hasn't been used at all,
        // so adjusting it with an additive factor would likely waste
        // resources unnecessarily.
        if value == 0 {
            return 0;
        }
        value.saturating_add(self.additive_factor).max(
            ((value as f64) * self.multiplicative_factor)
                .clamp(0.0, u32::MAX as f64)
                .floor() as u32,
        )
    }

    fn adjust_i64(&self, value: i64) -> i64 {
        // `0` typically means that resource hasn't been used at all,
        // so adjusting it with an additive factor would likely waste
        // resources unnecessarily.
        if value == 0 {
            return 0;
        }
        value.saturating_add(self.additive_factor as i64).max(
            ((value as f64) * self.multiplicative_factor)
                .clamp(0.0, i64::MAX as f64)
                .floor() as i64,
        )
    }
}

pub(crate) fn compute_resource_fee(
    network_config: &NetworkConfig,
    ledger_info: &LedgerInfo,
    resources: &TransactionResources,
    rent_changes: &[LedgerEntryRentChange],
    adjustment_config: &SimulationAdjustmentConfig,
) -> i64 {
    let (non_refundable_fee, refundable_fee) =
        compute_transaction_resource_fee(resources, &network_config.fee_configuration);
    let rent_fee = compute_rent_fee(
        rent_changes,
        &network_config.rent_fee_configuration,
        ledger_info.sequence_number,
    );
    let refundable_fee = adjustment_config
        .refundable_fee
        .adjust_i64(refundable_fee.saturating_add(rent_fee));
    non_refundable_fee.saturating_add(refundable_fee)
}

pub(crate) fn compute_adjusted_transaction_resources(
    operation: OperationBody,
    simulated_operation_resources: &mut SorobanResources,
    adjustment_config: &SimulationAdjustmentConfig,
    contract_events_and_return_value_size: u32,
) -> Result<TransactionResources> {
    adjustment_config.adjust_resources(simulated_operation_resources);
    Ok(TransactionResources {
        instructions: simulated_operation_resources.instructions,
        read_entries: (simulated_operation_resources.footprint.read_only.len()
            + simulated_operation_resources.footprint.read_write.len())
            as u32,
        write_entries: simulated_operation_resources.footprint.read_write.len() as u32,
        read_bytes: simulated_operation_resources.read_bytes,
        write_bytes: simulated_operation_resources.write_bytes,
        transaction_size_bytes: adjustment_config.tx_size.adjust_u32(
            estimate_max_transaction_size_for_operation(operation, &simulated_operation_resources)
                .context("could not compute the maximum transaction size for operation")?,
        ),
        contract_events_size_bytes: contract_events_and_return_value_size,
    })
}

pub(crate) fn simulate_invoke_host_function_op_resources(
    ledger_changes: &[LedgerEntryChange],
    simulated_instructions: u32,
) -> Result<(SorobanResources, Vec<LedgerEntryRentChange>)> {
    let mut read_bytes = 0;
    let mut write_bytes = 0;
    let mut read_only_footprint = vec![];
    let mut read_write_footprint = vec![];
    for ledger_change in ledger_changes {
        read_bytes += ledger_change.old_entry_size_bytes;
        // It should be safe to decode values returned encoded by host without limits.
        let key = LedgerKey::from_xdr(&ledger_change.encoded_key, Limits::none())?;
        if ledger_change.read_only {
            read_only_footprint.push(key);
        } else {
            read_write_footprint.push(key);
            if let Some(new_value) = &ledger_change.encoded_new_value {
                write_bytes += new_value.len() as u32;
            }
        }
    }
    read_only_footprint.sort();
    read_write_footprint.sort();
    let footprint = LedgerFootprint {
        read_only: read_only_footprint.try_into()?,
        read_write: read_write_footprint.try_into()?,
    };
    let resources = SorobanResources {
        footprint,
        instructions: simulated_instructions,
        read_bytes,
        write_bytes,
    };
    let rent_changes = extract_rent_changes(ledger_changes);
    Ok((resources, rent_changes))
}

pub(crate) fn simulate_extend_ttl_op_resources(
    keys_to_extend: &[LedgerKey],
    snapshot: &impl SnapshotSource,
    current_ledger_seq: u32,
    extend_to: u32,
) -> Result<(SorobanResources, Vec<LedgerEntryRentChange>)> {
    let mut read_bytes = 0;
    let mut rent_changes = Vec::<LedgerEntryRentChange>::with_capacity(keys_to_extend.len());
    let mut extended_keys = Vec::<LedgerKey>::with_capacity(keys_to_extend.len());

    let new_live_until_ledger = current_ledger_seq + extend_to;
    for key in keys_to_extend {
        let durability = get_key_durability(key).ok_or_else(|| anyhow!("Can't extend TTL for ledger entry with key `{:?}`. Only entries with TTL (contract data or code entries) can have it extended", key))?;
        let entry_with_live_until = snapshot.get(&Rc::new(key.clone()))?;
        let Some((entry, live_until)) = entry_with_live_until else {
            continue;
        };
        let entry_size: u32 = entry.to_xdr(DEFAULT_XDR_RW_LIMITS)?.len().try_into()?;
        let current_live_until_ledger = live_until.ok_or_else(|| {
            anyhow!("Internal error: missing TTL for ledger key that must have TTL: `{key:?}`")
        })?;
        if new_live_until_ledger <= current_live_until_ledger {
            continue;
        }
        extended_keys.push(key.clone());
        read_bytes += entry_size;
        rent_changes.push(LedgerEntryRentChange {
            is_persistent: durability == ContractDataDurability::Persistent,
            old_size_bytes: entry_size,
            new_size_bytes: entry_size,
            old_live_until_ledger: current_live_until_ledger,
            new_live_until_ledger,
        });
    }
    extended_keys.sort();
    let resources = SorobanResources {
        footprint: LedgerFootprint {
            read_only: extended_keys.try_into()?,
            read_write: Default::default(),
        },
        instructions: 0,
        read_bytes,
        write_bytes: 0,
    };
    Ok((resources, rent_changes))
}

pub(crate) fn simulate_restore_op_resources(
    keys_to_restore: &[LedgerKey],
    snapshot_source: &impl SnapshotSourceWithArchive,
    ledger_info: &LedgerInfo,
) -> Result<(SorobanResources, Vec<LedgerEntryRentChange>)> {
    let restored_live_until_ledger = ledger_info
        .min_live_until_ledger_checked(ContractDataDurability::Persistent)
        .ok_or_else(|| {
            anyhow!("minimum persistent live until ledger overflows - ledger info is misconfigured")
        })?;
    let mut restored_bytes = 0_u32;
    let mut rent_changes: Vec<LedgerEntryRentChange> = Vec::with_capacity(keys_to_restore.len());
    let mut restored_keys = Vec::<LedgerKey>::with_capacity(keys_to_restore.len());

    for key in keys_to_restore {
        let durability = get_key_durability(key);
        ensure!(
            durability == Some(ContractDataDurability::Persistent),
            "Can't restore a ledger entry with key: {key:?}. Only persistent ledger entries with TTL can be restored."
        );
        let entry_with_live_until = snapshot_source
            .get_including_archived(&Rc::new(key.clone()))?
            .ok_or_else(|| anyhow!("Missing entry to restore for key {key:?}"))?;
        let (entry, live_until) = entry_with_live_until;

        let current_live_until_ledger = live_until.ok_or_else(|| {
            anyhow!("Internal error: missing TTL for ledger key that must have TTL: `{key:?}`")
        })?;

        if current_live_until_ledger >= ledger_info.sequence_number {
            continue;
        }
        restored_keys.push(key.clone());

        let entry_size: u32 = entry.to_xdr(DEFAULT_XDR_RW_LIMITS)?.len().try_into()?;
        restored_bytes = restored_bytes.saturating_add(entry_size);
        rent_changes.push(LedgerEntryRentChange {
            is_persistent: true,
            old_size_bytes: 0,
            new_size_bytes: entry_size,
            old_live_until_ledger: 0,
            new_live_until_ledger: restored_live_until_ledger,
        });
    }
    restored_keys.sort();
    let resources = SorobanResources {
        footprint: LedgerFootprint {
            read_only: Default::default(),
            read_write: restored_keys.try_into()?,
        },
        instructions: 0,
        read_bytes: restored_bytes,
        write_bytes: restored_bytes,
    };
    Ok((resources, rent_changes))
}

fn estimate_max_transaction_size_for_operation(
    operation: OperationBody,
    resources: &SorobanResources,
) -> Result<u32> {
    let source = MuxedAccount::MuxedEd25519(MuxedAccountMed25519 {
        id: 0,
        ed25519: Uint256([0; 32]),
    });
    let bytes64: BytesM<64> = vec![0; 64].try_into()?;
    let signatures: Vec<DecoratedSignature> = vec![
        DecoratedSignature {
            hint: SignatureHint([0; 4]),
            signature: Signature(bytes64.clone()),
        };
        20
    ];
    let signer_key = SignerKey::Ed25519SignedPayload(SignerKeyEd25519SignedPayload {
        ed25519: Uint256([0; 32]),
        payload: bytes64.clone(),
    });
    let envelope = TransactionV1Envelope {
        tx: Transaction {
            source_account: source.clone(),
            fee: 0,
            seq_num: SequenceNumber(0),
            cond: Preconditions::V2(PreconditionsV2 {
                time_bounds: Some(TimeBounds {
                    min_time: TimePoint(0),
                    max_time: TimePoint(0),
                }),
                ledger_bounds: Some(LedgerBounds {
                    min_ledger: 0,
                    max_ledger: 0,
                }),
                min_seq_num: Some(SequenceNumber(0)),
                min_seq_age: Duration(0),
                min_seq_ledger_gap: 0,
                extra_signers: vec![signer_key.clone(), signer_key].try_into()?,
            }),
            memo: Memo::Hash(Hash([0; 32])),
            operations: vec![Operation {
                source_account: Some(source),
                body: operation,
            }]
            .try_into()?,
            ext: TransactionExt::V1(SorobanTransactionData {
                resources: SorobanResources {
                    footprint: resources.footprint.clone(),
                    instructions: 0,
                    read_bytes: 0,
                    write_bytes: 0,
                },
                resource_fee: 0,
                ext: ExtensionPoint::V0,
            }),
        },
        signatures: signatures.try_into()?,
    };

    let envelope_xdr = envelope.to_xdr(DEFAULT_XDR_RW_LIMITS)?;
    let envelope_size = envelope_xdr.len();
    Ok(envelope_size.try_into()?)
}
