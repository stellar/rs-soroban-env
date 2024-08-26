use crate::network_config::NetworkConfig;
use crate::resources::{
    compute_adjusted_transaction_resources, compute_resource_fee, simulate_extend_ttl_op_resources,
    simulate_invoke_host_function_op_resources, simulate_restore_op_resources,
};
use crate::snapshot_source::{
    SimulationSnapshotSource, SimulationSnapshotSourceWithArchive, SnapshotSourceWithArchive,
};
use anyhow::Result;
use soroban_env_host::{
    e2e_invoke::invoke_host_function_in_recording_mode,
    e2e_invoke::LedgerEntryChange,
    storage::SnapshotSource,
    xdr::{
        AccountId, ContractEvent, DiagnosticEvent, HostFunction, InvokeHostFunctionOp, LedgerKey,
        OperationBody, ScVal, SorobanAuthorizationEntry, SorobanResources, SorobanTransactionData,
    },
    xdr::{ExtendFootprintTtlOp, ExtensionPoint, LedgerEntry, ReadXdr, RestoreFootprintOp},
    HostError, LedgerInfo, DEFAULT_XDR_RW_LIMITS,
};
use std::rc::Rc;

/// Configures the adjustment of a simulated value (e.g. resource or fee).
/// The value is adjusted to be
/// `max(value * multiplicative_factor, value + additive_factor)`
pub struct SimulationAdjustmentFactor {
    pub multiplicative_factor: f64,
    pub additive_factor: u32,
}

/// Configuration for adjusting the resources and fees for a simulated
/// transaction.
pub struct SimulationAdjustmentConfig {
    pub instructions: SimulationAdjustmentFactor,
    pub read_bytes: SimulationAdjustmentFactor,
    pub write_bytes: SimulationAdjustmentFactor,
    pub tx_size: SimulationAdjustmentFactor,
    pub refundable_fee: SimulationAdjustmentFactor,
}

/// Represents the state of a `LedgerEntry` before and after the
/// transaction execution.
/// `None` represents that entry was not present or removed.
#[derive(Eq, PartialEq, Debug)]
pub struct LedgerEntryDiff {
    pub state_before: Option<LedgerEntry>,
    pub state_after: Option<LedgerEntry>,
}

/// Result of simulating `InvokeHostFunctionOp` operation.
#[derive(Debug)]
pub struct InvokeHostFunctionSimulationResult {
    /// Result value of the invoked function or error returned for invocation.
    pub invoke_result: std::result::Result<ScVal, HostError>,
    /// Authorization data, either passed through from the call (when provided),
    /// or recorded during the invocation.
    pub auth: Vec<SorobanAuthorizationEntry>,
    /// All the events that contracts emitted during invocation.
    /// Empty for failed invocations.
    pub contract_events: Vec<ContractEvent>,
    /// Diagnostic events recorded during simulation.
    /// This is populated when diagnostics is enabled and even when the
    /// invocation fails.
    pub diagnostic_events: Vec<DiagnosticEvent>,
    /// Soroban transaction extension containing simulated resources and
    /// the estimated resource fee.
    /// `None` for failed invocations.
    pub transaction_data: Option<SorobanTransactionData>,
    /// The number of CPU instructions metered during the simulation,
    /// without any adjustments applied.
    /// This is expected to not match `transaction_data` in case if
    /// instructions are adjusted via `SimulationAdjustmentConfig`.
    pub simulated_instructions: u32,
    /// The number of memory bytes metered during the simulation,
    /// without any adjustments applied.
    pub simulated_memory: u32,
    /// Differences for any RW entries that have been modified during
    /// the transaction execution.
    /// Empty for failed invocations.
    pub modified_entries: Vec<LedgerEntryDiff>,
}

/// Result of simulating `ExtendFootprintTtlOp` operation.
#[derive(Eq, PartialEq, Debug)]
pub struct ExtendTtlOpSimulationResult {
    /// Soroban transaction extension containing simulated resources and
    /// the estimated resource fee.
    pub transaction_data: SorobanTransactionData,
}

/// Result of simulating `RestoreFootprintOp` operation.
#[derive(Eq, PartialEq, Debug)]
pub struct RestoreOpSimulationResult {
    /// Soroban transaction extension containing simulated resources and
    /// the estimated resource fee.
    pub transaction_data: SorobanTransactionData,
}

/// Simulates `InvokeHostFunctionOp` operation specified via its
/// relevant payload parts.
///
/// The operation is defined by the host function itself (`host_fn`)
/// and optionally signed `auth_entries`. In case if `auth_entries` are
/// omitted, the simulation will use recording authorization mode and
/// return non-signed recorded authorization entries.
///
/// The rest of parameters define the ledger state (`snapshot_source`,
/// `network_config`, `ledger_info`), simulation adjustment
/// configuration (`adjustment_config`), and transaction execution
/// parameters (`source_account`, `base_prng_seed`).
///
/// `enable_diagnostics` enables recording of `diagnostic_events` in the
/// response.
///
/// This function makes the best effort at returning non-Err result even
/// for failed invocations. It should only fail if ledger is
/// mis-configured (e.g. when computed fees cause overflows).
#[allow(clippy::too_many_arguments)]
pub fn simulate_invoke_host_function_op(
    snapshot_source: Rc<dyn SnapshotSource>,
    network_config: &NetworkConfig,
    adjustment_config: &SimulationAdjustmentConfig,
    ledger_info: &LedgerInfo,
    host_fn: HostFunction,
    auth_entries: Option<Vec<SorobanAuthorizationEntry>>,
    source_account: &AccountId,
    base_prng_seed: [u8; 32],
    enable_diagnostics: bool,
) -> Result<InvokeHostFunctionSimulationResult> {
    let snapshot_source = Rc::new(SimulationSnapshotSource::new_from_rc(snapshot_source));
    let budget = network_config.create_budget()?;
    let mut diagnostic_events = vec![];
    let recording_result = invoke_host_function_in_recording_mode(
        &budget,
        enable_diagnostics,
        &host_fn,
        source_account,
        auth_entries,
        ledger_info.clone(),
        snapshot_source.clone(),
        base_prng_seed,
        &mut diagnostic_events,
    );
    let invoke_result = match &recording_result {
        Ok(r) => r.invoke_result.clone(),
        Err(e) => Err(e.clone()),
    };
    // We try to fill the simulation result as much as possible:
    // diagnostics can be populated unconditionally, and we can always
    // store the budget measurements.
    let mut simulation_result = InvokeHostFunctionSimulationResult {
        // Don't distinguish between the errors that happen during invocation vs
        // during setup as that seems too granular.
        invoke_result,
        simulated_instructions: budget.get_cpu_insns_consumed()?.try_into()?,
        simulated_memory: budget.get_mem_bytes_consumed()?.try_into()?,
        diagnostic_events,
        // Fields that should only be populated for successful invocations.
        auth: vec![],
        contract_events: vec![],
        transaction_data: None,
        modified_entries: vec![],
    };
    let Ok(recording_result) = recording_result else {
        return Ok(simulation_result);
    };
    if recording_result.invoke_result.is_err() {
        return Ok(simulation_result);
    }
    // Fill the remaining fields only for successful invocations.
    simulation_result.auth = recording_result.auth;
    simulation_result.contract_events = recording_result.contract_events;
    simulation_result.modified_entries =
        extract_modified_entries(&*snapshot_source, &recording_result.ledger_changes)?;

    let (mut resources, rent_changes) = simulate_invoke_host_function_op_resources(
        &recording_result.ledger_changes,
        simulation_result.simulated_instructions,
    )?;
    let operation = OperationBody::InvokeHostFunction(InvokeHostFunctionOp {
        host_function: host_fn,
        auth: simulation_result.auth.clone().try_into()?,
    });
    let transaction_resources = compute_adjusted_transaction_resources(
        operation,
        &mut resources,
        adjustment_config,
        recording_result.contract_events_and_return_value_size,
    )?;
    let resource_fee = compute_resource_fee(
        network_config,
        &ledger_info,
        &transaction_resources,
        &rent_changes,
        adjustment_config,
    );
    simulation_result.transaction_data = Some(create_transaction_data(resources, resource_fee));

    Ok(simulation_result)
}

/// Simulates `ExtendFootprintTtlOp` operation specified via its
/// relevant payload parts.
///
/// The operation is defined by the `keys_to_extend` and the
/// `extend_to`. The TTL for the provided keys will be extended to
/// become `ledger_info.sequence_number + extend_to`. Entries that
/// don't exist in the `snapshot_source` and entries that already
/// have TTL bigger than the requested extension will be ignored
/// and excluded from the simulation results.
///
/// The rest of parameters define the ledger state (`snapshot_source`,
/// `network_config`, `ledger_info`) and simulation adjustment
/// configuration (`adjustment_config`).
///
/// This may only return error in case if ledger is mis-configured.
pub fn simulate_extend_ttl_op(
    snapshot_source: &impl SnapshotSource,
    network_config: &NetworkConfig,
    adjustment_config: &SimulationAdjustmentConfig,
    ledger_info: &LedgerInfo,
    keys_to_extend: &[LedgerKey],
    extend_to: u32,
) -> Result<ExtendTtlOpSimulationResult> {
    let snapshot_source = SimulationSnapshotSource::new(snapshot_source);
    let (mut resources, rent_changes) = simulate_extend_ttl_op_resources(
        keys_to_extend,
        &snapshot_source,
        ledger_info.sequence_number,
        extend_to,
    )?;
    let operation = OperationBody::ExtendFootprintTtl(ExtendFootprintTtlOp {
        ext: ExtensionPoint::V0,
        extend_to,
    });
    let transaction_resources =
        compute_adjusted_transaction_resources(operation, &mut resources, adjustment_config, 0)?;
    let resource_fee = compute_resource_fee(
        network_config,
        &ledger_info,
        &transaction_resources,
        &rent_changes,
        adjustment_config,
    );
    Ok(ExtendTtlOpSimulationResult {
        transaction_data: create_transaction_data(resources, resource_fee),
    })
}

/// Simulates `RestoreFootprintTtlOp` operation specified via its
/// relevant payload parts.
///
/// The operation is defined by the specified `keys_to_restore`. The
/// keys will be restored with TTL set to
/// `ledger_info.sequence_number + ledger_info.min_persistent_entry_ttl - 1`.
/// Live entries will be ignored and excluded from the simulation results.
///
/// The rest of parameters define the ledger state (`snapshot_source`,
/// `network_config`, `ledger_info`) and simulation adjustment
/// configuration (`adjustment_config`). Note, that the `snapshot_source`
/// has to be able to provide the access to the archived entries.
///
/// This will return error if a key can't be restored due to either having
/// incorrect type/durability (e.g. a temp entry), if a key is missing from
/// `snapshot_source`, or in case of ledger mis-configuration.
pub fn simulate_restore_op(
    snapshot_source: &impl SnapshotSourceWithArchive,
    network_config: &NetworkConfig,
    adjustment_config: &SimulationAdjustmentConfig,
    ledger_info: &LedgerInfo,
    keys_to_restore: &[LedgerKey],
) -> Result<RestoreOpSimulationResult> {
    let snapshot_source = SimulationSnapshotSourceWithArchive::new(snapshot_source);
    let (mut resources, rent_changes) =
        simulate_restore_op_resources(keys_to_restore, &snapshot_source, ledger_info)?;
    let operation = OperationBody::RestoreFootprint(RestoreFootprintOp {
        ext: ExtensionPoint::V0,
    });
    let transaction_resources =
        compute_adjusted_transaction_resources(operation, &mut resources, adjustment_config, 0)?;
    let resource_fee = compute_resource_fee(
        network_config,
        &ledger_info,
        &transaction_resources,
        &rent_changes,
        adjustment_config,
    );
    Ok(RestoreOpSimulationResult {
        transaction_data: create_transaction_data(resources, resource_fee),
    })
}

impl SimulationAdjustmentFactor {
    pub fn new(multiplicative_factor: f64, additive_factor: u32) -> Self {
        Self {
            multiplicative_factor,
            additive_factor,
        }
    }

    pub fn no_adjustment() -> Self {
        Self {
            multiplicative_factor: 1.0,
            additive_factor: 0,
        }
    }
}

impl SimulationAdjustmentConfig {
    pub fn no_adjustments() -> Self {
        Self {
            instructions: SimulationAdjustmentFactor::no_adjustment(),
            read_bytes: SimulationAdjustmentFactor::no_adjustment(),
            write_bytes: SimulationAdjustmentFactor::no_adjustment(),
            tx_size: SimulationAdjustmentFactor::no_adjustment(),
            refundable_fee: SimulationAdjustmentFactor::no_adjustment(),
        }
    }

    pub fn default_adjustment() -> Self {
        Self {
            instructions: SimulationAdjustmentFactor::new(1.04, 50_000),
            read_bytes: SimulationAdjustmentFactor::no_adjustment(),
            write_bytes: SimulationAdjustmentFactor::no_adjustment(),
            // It's safe to have pretty significant adjustment for tx size, as
            // unused fee will be refunded.
            tx_size: SimulationAdjustmentFactor::new(1.1, 500),
            refundable_fee: SimulationAdjustmentFactor::new(1.15, 0),
        }
    }
}

fn create_transaction_data(
    resources: SorobanResources,
    resource_fee: i64,
) -> SorobanTransactionData {
    SorobanTransactionData {
        resources,
        resource_fee,
        ext: ExtensionPoint::V0,
    }
}

fn extract_modified_entries(
    snapshot: &(impl SnapshotSource + ?Sized),
    ledger_changes: &[LedgerEntryChange],
) -> Result<Vec<LedgerEntryDiff>> {
    let mut diffs = vec![];
    for c in ledger_changes {
        if c.read_only {
            continue;
        }
        let key = LedgerKey::from_xdr(c.encoded_key.clone(), DEFAULT_XDR_RW_LIMITS)?;
        let state_before = snapshot.get(&Rc::new(key))?.map(|v| v.0.as_ref().clone());
        let state_after = match &c.encoded_new_value {
            Some(v) => Some(LedgerEntry::from_xdr(v.clone(), DEFAULT_XDR_RW_LIMITS)?),
            None => None,
        };
        diffs.push(LedgerEntryDiff {
            state_before,
            state_after,
        });
    }
    Ok(diffs)
}
