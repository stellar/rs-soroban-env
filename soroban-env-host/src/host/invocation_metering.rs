use soroban_env_common::Env;

use crate::{
    builtin_contracts::account_contract::ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME,
    e2e_invoke::{encode_contract_events, entry_size_for_rent},
    fees::{FeeConfiguration, DATA_SIZE_1KB_INCREMENT, INSTRUCTIONS_INCREMENT, TTL_ENTRY_SIZE},
    ledger_info::get_key_durability,
    storage::{is_persistent_key, AccessType, Storage},
    xdr::{
        ContractDataDurability, ContractId, HostFunction, LedgerKey, ScAddress, ScErrorCode,
        ScErrorType, ScSymbol,
    },
    AddressObject, Symbol, SymbolStr, TryFromVal,
};

use super::{metered_xdr::metered_write_xdr, Host, HostError};

/// Represents the resources measured during an invocation.
///
/// This resembles the resources necessary to build a Soroban transaction and
/// compute its fee with a few exceptions (specifically, the transaction size
/// and the return value size).
///
/// This is almost the same as `InvocationResources`, but uses signed types
/// everywhere because sub-invocations may actually have 'negative' resource
/// consumption (e.g. if a contract call reduces the size of the entry that has
/// previously been modified to a larger size).
#[derive(Default, Clone, Debug, Eq, PartialEq)]
pub struct InvocationResources {
    /// Number of modelled CPU instructions.
    pub instructions: i64,
    /// Size of modelled memory in bytes.
    ///
    /// Note, that the used memory does not affect the fees. It only has an
    /// upper bound.
    pub mem_bytes: i64,
    /// Number of entries that need to be read from the disk.
    ///
    /// This is the total number of restored Soroban ledger entries and
    /// non-Soroban entries (such as 'classic' account balances).
    ///
    /// Live Soroban state is stored in-memory and most of the time this
    /// is going to be 0 or almost 0.
    pub disk_read_entries: u32,
    /// Number of in-memory ledger entries accessed by the invocation.
    ///
    /// This includes all the live Soroban entries, i.e. most of the entries
    /// that a contract interacts with.
    ///
    /// Note, that this value does not affect the fees. It only has an upper
    /// bound.
    pub memory_read_entries: u32,
    /// Number of entries that need to be written to the ledger due to
    /// modification.
    pub write_entries: u32,
    /// Total number of bytes that need to be read from disk.
    ///
    /// This is the total size of restored Soroban ledger entries and
    /// non-Soroban entries (such as 'classic' account balances).
    ///
    /// Live Soroban state is stored in-memory and most of the time this
    /// is going to be 0 or almost 0.
    pub disk_read_bytes: u32,
    /// Total number of bytes that need to be written to the ledger.
    pub write_bytes: u32,
    /// Total size of the contract events emitted.
    pub contract_events_size_bytes: u32,
    /// Cumulative rent bump of all the persistent entries in 'ledger-bytes'.
    /// 'Ledger-byte' is a rent bump of 1 byte for 1 ledger. Rent fee is
    /// proportional to the total amount of 'ledger-bytes'.
    pub persistent_rent_ledger_bytes: i64,
    /// Number of persistent entries that had their rent bumped.
    pub persistent_entry_rent_bumps: u32,
    /// Cumulative rent bump of all the temporary entries in 'ledger-bytes'.
    /// 'Ledger-byte' is a rent bump of 1 byte for 1 ledger. Rent fee is
    /// proportional to the total amount of 'ledger-bytes'.    
    pub temporary_rent_ledger_bytes: i64,
    /// Number of temporary entries that had their rent bumped.
    pub temporary_entry_rent_bumps: u32,
}

/// Represents the resources measured during an invocation.
///
/// This resembles the resources necessary to build a Soroban transaction and
/// compute its fee with a few exceptions (specifically, the transaction size
/// and the return value size).
#[derive(Default, Clone, Debug, Eq, PartialEq)]
pub struct SubInvocationResources {
    /// Number of modelled CPU instructions.
    pub instructions: i64,
    /// Size of modelled memory in bytes.
    ///
    /// Note, that the used memory does not affect the fees. It only has an
    /// upper bound.
    pub mem_bytes: i64,
    /// Number of entries that need to be read from the disk.
    ///
    /// This is the total number of restored Soroban ledger entries and
    /// non-Soroban entries (such as 'classic' account balances).
    ///
    /// Live Soroban state is stored in-memory and most of the time this
    /// is going to be 0 or almost 0.
    pub disk_read_entries: i32,
    /// Number of in-memory ledger entries accessed by the invocation.
    ///
    /// This includes all the live Soroban entries, i.e. most of the entries
    /// that a contract interacts with.
    ///
    /// Note, that this value does not affect the fees. It only has an upper
    /// bound.
    pub memory_read_entries: i32,
    /// Number of entries that need to be written to the ledger due to
    /// modification.
    pub write_entries: i32,
    /// Total number of bytes that need to be read from disk.
    ///
    /// This is the total size of restored Soroban ledger entries and
    /// non-Soroban entries (such as 'classic' account balances).
    ///
    /// Live Soroban state is stored in-memory and most of the time this
    /// is going to be 0 or almost 0.
    pub disk_read_bytes: i32,
    /// Total number of bytes that need to be written to the ledger.
    pub write_bytes: i32,
    /// Total size of the contract events emitted.
    pub contract_events_size_bytes: i32,
    /// Cumulative rent bump of all the persistent entries in 'ledger-bytes'.
    /// 'Ledger-byte' is a rent bump of 1 byte for 1 ledger. Rent fee is
    /// proportional to the total amount of 'ledger-bytes'.
    pub persistent_rent_ledger_bytes: i64,
    /// Number of persistent entries that had their rent bumped.
    pub persistent_entry_rent_bumps: i32,
    /// Cumulative rent bump of all the temporary entries in 'ledger-bytes'.
    /// 'Ledger-byte' is a rent bump of 1 byte for 1 ledger. Rent fee is
    /// proportional to the total amount of 'ledger-bytes'.    
    pub temporary_rent_ledger_bytes: i64,
    /// Number of temporary entries that had their rent bumped.
    pub temporary_entry_rent_bumps: i32,
}

impl From<SubInvocationResources> for InvocationResources {
    fn from(sub: SubInvocationResources) -> Self {
        Self {
            instructions: sub.instructions,
            mem_bytes: sub.mem_bytes,
            disk_read_entries: sub.disk_read_entries.max(0) as u32,
            memory_read_entries: sub.memory_read_entries.max(0) as u32,
            write_entries: sub.write_entries.max(0) as u32,
            disk_read_bytes: sub.disk_read_bytes.max(0) as u32,
            write_bytes: sub.write_bytes.max(0) as u32,
            contract_events_size_bytes: sub.contract_events_size_bytes.max(0) as u32,
            persistent_rent_ledger_bytes: sub.persistent_rent_ledger_bytes,
            persistent_entry_rent_bumps: sub.persistent_entry_rent_bumps.max(0) as u32,
            temporary_rent_ledger_bytes: sub.temporary_rent_ledger_bytes,
            temporary_entry_rent_bumps: sub.temporary_entry_rent_bumps.max(0) as u32,
        }
    }
}

impl SubInvocationResources {
    fn subtract(mut self, other: &SubInvocationResources) -> Self {
        self.instructions = self.instructions.saturating_sub(other.instructions);
        self.mem_bytes = self.mem_bytes.saturating_sub(other.mem_bytes);
        self.disk_read_entries = self
            .disk_read_entries
            .saturating_sub(other.disk_read_entries);
        self.memory_read_entries = self
            .memory_read_entries
            .saturating_sub(other.memory_read_entries);
        self.write_entries = self.write_entries.saturating_sub(other.write_entries);
        self.disk_read_bytes = self.disk_read_bytes.saturating_sub(other.disk_read_bytes);
        self.write_bytes = self.write_bytes.saturating_sub(other.write_bytes);
        self.contract_events_size_bytes = self
            .contract_events_size_bytes
            .saturating_sub(other.contract_events_size_bytes);
        self.persistent_rent_ledger_bytes = self
            .persistent_rent_ledger_bytes
            .saturating_sub(other.persistent_rent_ledger_bytes);
        self.persistent_entry_rent_bumps = self
            .persistent_entry_rent_bumps
            .saturating_sub(other.persistent_entry_rent_bumps);
        self.temporary_rent_ledger_bytes = self
            .temporary_rent_ledger_bytes
            .saturating_sub(other.temporary_rent_ledger_bytes);
        self.temporary_entry_rent_bumps = self
            .temporary_entry_rent_bumps
            .saturating_sub(other.temporary_entry_rent_bumps);
        self
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DetailedInvocationResources {
    /// The type of the invocation (e.g. contract call, Wasm upload).
    pub invocation: MeteringInvocation,
    /// Resources measured during the invocation.
    pub resources: SubInvocationResources,
    /// Resources for sub-calls made during the invocation, if any.
    /// Note, that not all the resources commute, i.e. the sum of the resources
    pub sub_call_resources: Vec<DetailedInvocationResources>,
}

/// Detailed estimate of the transaction fees in stroops based on the
/// `InvocationResources`.
///
/// Since `InvocationResources` don't account for certain metered resources,
/// these are omitted from the estimate as well.
#[derive(Default, Clone, Debug, Eq, PartialEq)]
pub struct FeeEstimate {
    /// Total fee (sum of all the remaining fields).
    pub total: i64,
    /// Fee for instructions.
    pub instructions: i64,
    /// Fee for ledger entry reads.
    pub disk_read_entries: i64,
    /// Fee for ledger entry writes.
    pub write_entries: i64,
    /// Fee for the overall size of ledger disk reads.
    pub disk_read_bytes: i64,
    /// Fee for the overall size of ledger writes.
    pub write_bytes: i64,
    /// Fee for the contract events emitted.
    pub contract_events: i64,
    /// Rent fee for the persistent entries.
    pub persistent_entry_rent: i64,
    /// Rent fee for the temporary entries.
    pub temporary_entry_rent: i64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DetailedFeeEstimate {
    /// The type of the invocation (e.g. contract call, Wasm upload).
    pub invocation: MeteringInvocation,
    /// Fee estimate for the invocation.
    pub fee_estimate: FeeEstimate,
    /// Fee estimates for sub-calls made during the invocation, if any.
    pub sub_call_fee_estimates: Vec<DetailedFeeEstimate>,
}

impl InvocationResources {
    /// Estimates the fees necessary for the current resources based on the
    /// provided fee configuration.
    ///
    /// This is only an estimate and it can't be used for the actual transaction
    /// submission (simulation using the Soroban RPC should be used instead).
    ///
    /// The quality of the estimate depends on the provided fee configuration,
    /// so it must resemble the target network as close as possible.
    pub fn estimate_fees(
        &self,
        fee_config: &FeeConfiguration,
        fee_per_rent_1kb: i64,
        persistent_rent_rate_denominator: i64,
        temporary_rent_rate_denominator: i64,
    ) -> FeeEstimate {
        let instructions = compute_fee_per_increment(
            self.instructions,
            fee_config.fee_per_instruction_increment,
            INSTRUCTIONS_INCREMENT,
        );
        let disk_read_entries = fee_config.fee_per_disk_read_entry.saturating_mul(
            self.disk_read_entries
                .saturating_add(self.write_entries)
                .into(),
        );
        let write_entries = fee_config
            .fee_per_write_entry
            .saturating_mul(self.write_entries.into());
        let disk_read_bytes = compute_fee_per_increment(
            self.disk_read_bytes.into(),
            fee_config.fee_per_disk_read_1kb,
            DATA_SIZE_1KB_INCREMENT,
        );
        let write_bytes = compute_fee_per_increment(
            self.write_bytes.into(),
            fee_config.fee_per_write_1kb,
            DATA_SIZE_1KB_INCREMENT,
        );
        let contract_events = compute_fee_per_increment(
            self.contract_events_size_bytes.into(),
            fee_config.fee_per_contract_event_1kb,
            DATA_SIZE_1KB_INCREMENT,
        );

        let mut persistent_entry_ttl_entry_writes = fee_config
            .fee_per_write_entry
            .saturating_mul(self.persistent_entry_rent_bumps.into());
        persistent_entry_ttl_entry_writes =
            persistent_entry_ttl_entry_writes.saturating_add(compute_fee_per_increment(
                (TTL_ENTRY_SIZE as i64).saturating_mul(self.persistent_entry_rent_bumps.into()),
                fee_config.fee_per_write_1kb,
                DATA_SIZE_1KB_INCREMENT,
            ));
        let mut temp_entry_ttl_entry_writes = fee_config
            .fee_per_write_entry
            .saturating_mul(self.temporary_entry_rent_bumps.into());
        temp_entry_ttl_entry_writes =
            temp_entry_ttl_entry_writes.saturating_add(compute_fee_per_increment(
                (TTL_ENTRY_SIZE as i64).saturating_mul(self.temporary_entry_rent_bumps.into()),
                fee_config.fee_per_write_1kb,
                DATA_SIZE_1KB_INCREMENT,
            ));

        let persistent_entry_rent = compute_fee_per_increment(
            self.persistent_rent_ledger_bytes,
            fee_per_rent_1kb,
            DATA_SIZE_1KB_INCREMENT.saturating_mul(persistent_rent_rate_denominator),
        )
        .saturating_add(persistent_entry_ttl_entry_writes);
        let temporary_entry_rent = compute_fee_per_increment(
            self.temporary_rent_ledger_bytes,
            fee_per_rent_1kb,
            DATA_SIZE_1KB_INCREMENT.saturating_mul(temporary_rent_rate_denominator),
        )
        .saturating_add(temp_entry_ttl_entry_writes);
        let total = instructions
            .saturating_add(disk_read_entries)
            .saturating_add(write_entries)
            .saturating_add(disk_read_bytes)
            .saturating_add(write_bytes)
            .saturating_add(contract_events)
            .saturating_add(persistent_entry_rent)
            .saturating_add(temporary_entry_rent);
        FeeEstimate {
            total,
            instructions,
            disk_read_entries,
            write_entries,
            disk_read_bytes,
            write_bytes,
            contract_events,
            persistent_entry_rent,
            temporary_entry_rent,
        }
    }
}

impl DetailedInvocationResources {
    /// Estimates the fees necessary for the resources for this invocation, as
    /// well as its sub-invocations, based on the provided fee configuration.
    ///
    /// This is only an estimate and it can't be used for the actual transaction
    /// submission (simulation using the Soroban RPC should be used instead).
    ///
    /// The quality of the estimate depends on the provided fee configuration,
    /// so it must resemble the target network as close as possible.
    pub fn estimate_fees(
        &self,
        fee_config: &FeeConfiguration,
        fee_per_rent_1kb: i64,
        persistent_rent_rate_denominator: i64,
        temporary_rent_rate_denominator: i64,
    ) -> DetailedFeeEstimate {
        let resources: InvocationResources = self.resources.clone().into();
        let fee_estimate = resources.estimate_fees(
            fee_config,
            fee_per_rent_1kb,
            persistent_rent_rate_denominator,
            temporary_rent_rate_denominator,
        );
        let sub_call_fee_estimates = self
            .sub_call_resources
            .iter()
            .map(|r| {
                r.estimate_fees(
                    fee_config,
                    fee_per_rent_1kb,
                    persistent_rent_rate_denominator,
                    temporary_rent_rate_denominator,
                )
            })
            .collect();
        DetailedFeeEstimate {
            invocation: self.invocation.clone(),
            fee_estimate,
            sub_call_fee_estimates,
        }
    }
}

/// A helper for metering the resources only within a logical host invocation
/// without finalizing the host.
///
/// The 'logical' invocations are the typical entry points for the unit tests,
/// such as invocations based on `HostFunction` XDR, lifecycle operations
/// (registering Wasm, creating a contract instance), direct contract calls
/// etc.
#[derive(Clone, Default)]
pub(crate) struct InvocationMeter {
    enabled: bool,
    stack_depth: u32,
    storage_snapshot: Storage,
    detailed_invocation_resources: Option<DetailedInvocationResources>,
}

/// Identifies the type of invocation being metered.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum MeteringInvocation {
    /// A contract function call. Contains the contract address and the name
    /// of the function being called.
    InvokeContract(ScAddress, ScSymbol),
    /// Wasm upload that happened as a top-level invocation (i.e. this will not
    /// be recorded for Wasm uploads initiated from within a contract).
    /// The top-level Wasm uploads may only happen in transactions or in test
    /// setup functions.
    WasmUploadEntryPoint,
    /// Contract creation that happened as a top-level invocation (i.e. this
    /// will not be recorded for contract creation initiated from within a contract).
    CreateContractEntryPoint,
}

impl MeteringInvocation {
    pub(crate) fn from_host_function(hf: &HostFunction) -> Self {
        match hf {
            HostFunction::InvokeContract(invoke_args) => MeteringInvocation::InvokeContract(
                invoke_args.contract_address.clone(),
                invoke_args.function_name.clone(),
            ),
            HostFunction::UploadContractWasm(_) => MeteringInvocation::WasmUploadEntryPoint,
            HostFunction::CreateContract(_) | HostFunction::CreateContractV2(_) => {
                MeteringInvocation::CreateContractEntryPoint
            }
        }
    }

    pub(crate) fn contract_invocation_with_address_obj(
        host: &Host,
        address: AddressObject,
        function_name: Symbol,
    ) -> Self {
        let mut address_xdr = ScAddress::Contract(Default::default());
        let mut function_name_xdr = ScSymbol::default();
        host.with_debug_mode(|| {
            address_xdr = host.visit_obj(address, |a: &ScAddress| Ok(a.clone()))?;
            function_name_xdr = SymbolStr::try_from_val(host, &function_name)?
                .to_string()
                .as_str()
                .try_into()
                .map_err(|_| {
                    host.err(
                        ScErrorType::Value,
                        ScErrorCode::InternalError,
                        "can't convert Symbol to ScSymbol",
                        &[],
                    )
                })?;
            Ok(())
        });
        MeteringInvocation::InvokeContract(address_xdr, function_name_xdr)
    }

    pub(crate) fn contract_invocation(
        host: &Host,
        contract_id: &ContractId,
        function_name: Symbol,
    ) -> Self {
        let address = ScAddress::Contract(contract_id.clone());
        let mut function_name_xdr = ScSymbol::default();
        host.with_debug_mode(|| {
            function_name_xdr = SymbolStr::try_from_val(host, &function_name)?
                .to_string()
                .as_str()
                .try_into()
                .map_err(|_| {
                    host.err(
                        ScErrorType::Value,
                        ScErrorCode::InternalError,
                        "can't convert Symbol to ScSymbol",
                        &[],
                    )
                })?;
            Ok(())
        });
        MeteringInvocation::InvokeContract(address, function_name_xdr)
    }

    pub(crate) fn check_auth_invocation(host: &Host, address: AddressObject) -> Self {
        let mut address_xdr = ScAddress::Contract(Default::default());
        let function_name = ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME
            .try_into()
            .unwrap_or_default();
        host.with_debug_mode(|| {
            address_xdr = host.visit_obj(address, |a: &ScAddress| Ok(a.clone()))?;
            Ok(())
        });
        MeteringInvocation::InvokeContract(address_xdr, function_name)
    }
}
/// Scope guard for `InvocationMeter` that automatically finishes the metered
/// invocation when it goes out of scope.
pub(crate) struct InvocationMeterScope<'a> {
    host: &'a Host,
}

impl<'a> InvocationMeterScope<'a> {
    fn new(host: &'a Host) -> Self {
        Self { host }
    }
}
impl Drop for InvocationMeterScope<'_> {
    fn drop(&mut self) {
        if let Ok(mut meter) = self.host.try_borrow_invocation_meter_mut() {
            let _res = meter.pop_invocation(self.host);
            _res.unwrap();
        }
    }
}

impl InvocationMeter {
    // Gets the metered resources for the last metered invocation.
    pub(crate) fn get_root_invocation_resources(&self) -> Option<InvocationResources> {
        self.detailed_invocation_resources
            .as_ref()
            .map(|r| r.resources.clone().into())
    }

    // Gets the detailed, per-invocation metered resources for the last
    // top-level invocation.
    pub(crate) fn get_detailed_invocation_resources(&self) -> Option<DetailedInvocationResources> {
        self.detailed_invocation_resources.clone()
    }

    fn push_invocation<'a>(
        &mut self,
        host: &'a Host,
        invocation: MeteringInvocation,
    ) -> Result<Option<InvocationMeterScope<'a>>, HostError> {
        if !self.enabled {
            return Ok(None);
        }
        if self.stack_depth == 0 {
            // Reset all the state relevant to the invocation resource metering.
            host.budget_ref().reset()?;
            host.try_borrow_events_mut()?.clear();
            // Note, that the storage itself shouldn't be reset, as it's treated
            // as the ledger state before invocation.
            host.try_borrow_storage_mut()?.reset_footprint();
            self.storage_snapshot = host.try_borrow_storage()?.clone();
            self.stack_depth = 1;
            self.detailed_invocation_resources = Some(DetailedInvocationResources {
                invocation,
                resources: host.snapshot_current_resources(&self.storage_snapshot),
                sub_call_resources: vec![],
            });
            return Ok(Some(InvocationMeterScope::new(host)));
        }
        let mut parent_invocation_resources =
            self.detailed_invocation_resources.as_mut().ok_or_else(|| {
                host.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "missing invocation resources for non-root invocation",
                    &[],
                )
            })?;

        for _ in 0..(self.stack_depth - 1) {
            parent_invocation_resources = parent_invocation_resources
                .sub_call_resources
                .last_mut()
                .ok_or_else(|| {
                    host.err(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                        "incorrect stack depth for invocation metering",
                        &[],
                    )
                })?;
        }
        // In tests we sometimes end up with multiple invocations that do
        // effectively the same thing, e.g. a test helper that registers a
        // contract will call a host function that creates contract, both can
        // be an entry point for the invocation metering. In these cases we
        // don't start a new invocation scope.
        if parent_invocation_resources.invocation == invocation {
            return Ok(None);
        }

        // We only meter cross-contract calls after the top-level invocation, so
        // inner calls to upload Wasm or create contract from within a contract
        // should not spawn a new invocation scope.
        if !matches!(invocation, MeteringInvocation::InvokeContract(_, _)) {
            return Ok(None);
        }
        parent_invocation_resources
            .sub_call_resources
            .push(DetailedInvocationResources {
                invocation: invocation.clone(),
                resources: host.snapshot_current_resources(&self.storage_snapshot),
                sub_call_resources: vec![],
            });

        self.stack_depth += 1;
        return Ok(Some(InvocationMeterScope::new(host)));
    }

    fn pop_invocation(&mut self, host: &Host) -> Result<(), HostError> {
        if self.stack_depth == 0 {
            return Ok(());
        }
        let mut current_invocation_resources =
            self.detailed_invocation_resources.as_mut().ok_or_else(|| {
                host.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "missing invocation resources for non-root invocation",
                    &[],
                )
            })?;

        for _ in 0..(self.stack_depth - 1) {
            current_invocation_resources = current_invocation_resources
                .sub_call_resources
                .last_mut()
                .ok_or_else(|| {
                    host.err(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                        "incorrect stack depth for invocation metering",
                        &[],
                    )
                })?;
        }
        current_invocation_resources.resources = host
            .snapshot_current_resources(&self.storage_snapshot)
            .subtract(&current_invocation_resources.resources);

        // If we're popping the root invocation in test environment, we need to
        // emulate the write-back to the module cache (typically done by the
        // embedding environment) of any new contracts added during the
        // invocation.
        #[cfg(any(test, feature = "testutils"))]
        if self.stack_depth == 1 {
            // Subtle: we use `with_shadow_mode` instead of `with_debug_mode`
            // here because this needs to happen even if debug mode is off and
            // also because we need to have some side effects that are not
            // desired in debug mode.
            host.budget_ref()
                .with_shadow_mode(|| host.ensure_module_cache_contains_host_storage_contracts());
        }

        self.stack_depth -= 1;
        Ok(())
    }
}

impl Host {
    /// Tries to start a metered invocation, when invocation metering is enabled.
    ///
    /// The returned object has to stay alive while the invocation is active.
    ///
    /// If there is already an invocation active, returns `None`.
    pub(crate) fn maybe_meter_invocation(
        &self,
        invocation: MeteringInvocation,
    ) -> Option<InvocationMeterScope<'_>> {
        // Note: we're using the standard `try_borrow_mut` instead of a helper
        // generated with `impl_checked_borrow_helpers` in order to not spam
        // the logs with failures. It is expected for metering_scope to be
        // borrowed.
        if let Ok(mut scope) = self.0.invocation_meter.try_borrow_mut() {
            let res = scope.push_invocation(self, invocation);
            if let Ok(maybe_scope) = res {
                maybe_scope
            } else {
                #[cfg(any(test, feature = "testutils"))]
                {
                    res.unwrap();
                }
                None
            }
        } else {
            None
        }
    }

    /// Enables invocation metering (it's disabled by default).
    pub fn enable_invocation_metering(&self) {
        // This only works when debug mode is enabled. SDK enables debug mode
        // by default, so this should be no-op for most of the current users.
        self.enable_debug().unwrap();
        if let Ok(mut meter) = self.0.invocation_meter.try_borrow_mut() {
            meter.enabled = true;
        }
    }

    fn snapshot_current_resources(
        &self,
        init_storage_snapshot: &Storage,
    ) -> SubInvocationResources {
        let mut invocation_resources = SubInvocationResources::default();
        let budget = self.budget_ref();
        invocation_resources.instructions =
            budget.get_cpu_insns_consumed().unwrap_or_default() as i64;
        invocation_resources.mem_bytes = budget.get_mem_bytes_consumed().unwrap_or_default() as i64;

        // Resource measurement is best-effort, though we don't expect this to
        // ever fail.
        self.with_debug_mode(|| {
            let _res = self.try_snapshot_storage_and_event_resources(
                init_storage_snapshot,
                &mut invocation_resources,
            );
            #[cfg(test)]
            _res.unwrap();
            Ok(())
        });

        invocation_resources
    }

    fn try_snapshot_storage_and_event_resources(
        &self,
        init_storage_snapshot: &Storage,
        invocation_resources: &mut SubInvocationResources,
    ) -> Result<(), HostError> {
        let mut curr_storage = self.try_borrow_storage_mut()?;
        let curr_footprint = curr_storage.footprint.clone();

        let curr_ledger_seq: u32 = self.get_ledger_sequence()?.into();
        for (key, curr_access_type) in curr_footprint.0.iter(self.budget_ref())? {
            let maybe_init_entry = init_storage_snapshot.get_from_map(key, self)?;
            let mut init_entry_size_for_rent = 0;
            let mut init_live_until_ledger = curr_ledger_seq;
            let mut is_disk_read = match key.as_ref() {
                LedgerKey::ContractData(_) | LedgerKey::ContractCode(_) => false,
                _ => true,
            };
            if let Some((init_entry, init_entry_live_until)) = maybe_init_entry {
                if let Some(live_until) = init_entry_live_until {
                    if live_until >= curr_ledger_seq {
                        // Only bump `init_live_until_ledger` to a value higher than the current
                        // ledger in order to get the appropriate rent bump amount.
                        init_live_until_ledger = live_until;
                    } else {
                        // If the entry is persistent and it has expired, then
                        // we deal with the autorestore and thus need to mark
                        // the entry as disk read.
                        is_disk_read = is_persistent_key(key.as_ref());
                    }
                }

                let mut buf = Vec::<u8>::new();
                metered_write_xdr(self.budget_ref(), init_entry.as_ref(), &mut buf)?;
                if is_disk_read {
                    invocation_resources.disk_read_bytes += buf.len() as i32;
                }
                init_entry_size_for_rent =
                    entry_size_for_rent(self.budget_ref(), &init_entry, buf.len() as u32)?;
            }
            let mut entry_size = 0;
            let mut new_entry_size_for_rent = 0;
            let mut entry_live_until_ledger = None;
            let maybe_entry = curr_storage.try_get_full(key, self, None)?;
            if let Some((entry, entry_live_until)) = maybe_entry {
                let mut buf = Vec::<u8>::new();
                metered_write_xdr(self.budget_ref(), entry.as_ref(), &mut buf)?;
                entry_size = buf.len() as u32;
                new_entry_size_for_rent =
                    entry_size_for_rent(self.budget_ref(), &entry, entry_size)?;
                entry_live_until_ledger = entry_live_until;
            }
            if is_disk_read {
                invocation_resources.disk_read_entries += 1;
            } else {
                invocation_resources.memory_read_entries += 1;
            }
            if matches!(curr_access_type, AccessType::ReadWrite) {
                invocation_resources.write_entries += 1;
                invocation_resources.write_bytes += entry_size as i32;
            }

            if let Some(new_live_until) = entry_live_until_ledger {
                let extension_ledgers = (new_live_until - init_live_until_ledger) as i64;
                let rent_size_delta = if new_entry_size_for_rent > init_entry_size_for_rent {
                    (new_entry_size_for_rent - init_entry_size_for_rent) as i64
                } else {
                    0
                };
                let existing_ledgers = (init_live_until_ledger - curr_ledger_seq) as i64;
                let rent_ledger_bytes = existing_ledgers * rent_size_delta
                    + extension_ledgers * (new_entry_size_for_rent as i64);
                if rent_ledger_bytes > 0 {
                    match get_key_durability(key.as_ref()) {
                        Some(ContractDataDurability::Temporary) => {
                            invocation_resources.temporary_rent_ledger_bytes += rent_ledger_bytes;
                            invocation_resources.temporary_entry_rent_bumps += 1;
                        }
                        Some(ContractDataDurability::Persistent) => {
                            invocation_resources.persistent_rent_ledger_bytes += rent_ledger_bytes;
                            invocation_resources.persistent_entry_rent_bumps += 1;
                        }
                        None => (),
                    }
                }
            }
        }
        let events = self.try_borrow_events()?.externalize(self)?;
        let encoded_contract_events = encode_contract_events(self.budget_ref(), &events)?;
        for event in &encoded_contract_events {
            invocation_resources.contract_events_size_bytes += event.len() as i32;
        }
        Ok(())
    }
}

fn compute_fee_per_increment(resource_value: i64, fee_rate: i64, increment: i64) -> i64 {
    num_integer::div_ceil(resource_value.saturating_mul(fee_rate), increment.max(1))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        xdr::{ContractId, Hash},
        Symbol, TryFromVal, TryIntoVal,
    };
    use expect_test::expect;
    use soroban_test_wasms::CONTRACT_STORAGE;

    fn assert_resources_equal_to_budget(host: &Host) {
        assert_eq!(
            host.get_last_invocation_resources().unwrap().instructions as u64,
            host.budget_ref().get_cpu_insns_consumed().unwrap()
        );
        assert_eq!(
            host.get_last_invocation_resources().unwrap().mem_bytes as u64,
            host.budget_ref().get_mem_bytes_consumed().unwrap()
        );
    }

    // run `UPDATE_EXPECT=true cargo test` to update this test.
    // The exact values don't matter too much here (unless the diffs are
    // produced without a protocol upgrade), but the presence/absence of certain
    // resources is important (comments clarify which ones).
    #[test]
    fn test_invocation_resource_metering() {
        let host = Host::test_host_with_recording_footprint();
        host.enable_invocation_metering();
        host.enable_debug().unwrap();
        host.with_mut_ledger_info(|li| {
            li.sequence_number = 100;
            li.max_entry_ttl = 10000;
            li.min_persistent_entry_ttl = 1000;
            li.min_temp_entry_ttl = 16;
        })
        .unwrap();

        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
        // We meter the whole registration procedure here (upload + create
        // contract), so 2 writes/bumps are expected.
        expect![[r#"
            InvocationResources {
                instructions: 4199640,
                mem_bytes: 2863204,
                disk_read_entries: 0,
                memory_read_entries: 2,
                write_entries: 2,
                disk_read_bytes: 0,
                write_bytes: 3132,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 80531388,
                persistent_entry_rent_bumps: 2,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        expect![[r#"
            DetailedInvocationResources {
                invocation: CreateContractEntryPoint,
                resources: SubInvocationResources {
                    instructions: 4199640,
                    mem_bytes: 2863204,
                    disk_read_entries: 0,
                    memory_read_entries: 2,
                    write_entries: 2,
                    disk_read_bytes: 0,
                    write_bytes: 3132,
                    contract_events_size_bytes: 0,
                    persistent_rent_ledger_bytes: 80531388,
                    persistent_entry_rent_bumps: 2,
                    temporary_rent_ledger_bytes: 0,
                    temporary_entry_rent_bumps: 0,
                },
                sub_call_resources: [],
            }"#]]
        .assert_eq(
            format!(
                "{:#?}",
                host.get_detailed_last_invocation_resources().unwrap()
            )
            .as_str(),
        );
        assert_resources_equal_to_budget(&host);

        let key = Symbol::try_from_small_str("key_1").unwrap();

        // Has with no entries - no writes/rent bumps expected.
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"has_persistent").unwrap(),
                test_vec![&host, key].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 316637,
                mem_bytes: 1134859,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        expect![[r#"
            DetailedInvocationResources {
                invocation: InvokeContract(
                    Contract(
                        ContractId(
                            Hash(ba863dea340f907c97f640ecbe669125e9f8f3b63ed1f4ed0f30073b869e5441),
                        ),
                    ),
                    ScSymbol(
                        StringM(has_persistent),
                    ),
                ),
                resources: SubInvocationResources {
                    instructions: 316637,
                    mem_bytes: 1134859,
                    disk_read_entries: 0,
                    memory_read_entries: 3,
                    write_entries: 0,
                    disk_read_bytes: 0,
                    write_bytes: 0,
                    contract_events_size_bytes: 0,
                    persistent_rent_ledger_bytes: 0,
                    persistent_entry_rent_bumps: 0,
                    temporary_rent_ledger_bytes: 0,
                    temporary_entry_rent_bumps: 0,
                },
                sub_call_resources: [],
            }"#]]
        .assert_eq(
            format!(
                "{:#?}",
                host.get_detailed_last_invocation_resources().unwrap()
            )
            .as_str(),
        );
        assert_resources_equal_to_budget(&host);

        // 1 persistent write together with the respective initial rent bump.
        let _ = &host
            .try_call(
                contract_id,
                Symbol::try_from_val(&host, &"put_persistent").unwrap(),
                test_vec![&host, key, 1234_u64].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 320246,
                mem_bytes: 1135322,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 1,
                disk_read_bytes: 0,
                write_bytes: 84,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 83916,
                persistent_entry_rent_bumps: 1,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Another has check, should have more data read than the first one.
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"has_persistent").unwrap(),
                test_vec![&host, key].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 315936,
                mem_bytes: 1134707,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // 1 temporary entry write with the initial rent bump.
        let _ = &host
            .try_call(
                contract_id,
                Symbol::try_from_val(&host, &"put_temporary").unwrap(),
                test_vec![&host, key, 1234_u64].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 322157,
                mem_bytes: 1135678,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 1,
                disk_read_bytes: 0,
                write_bytes: 84,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 1260,
                temporary_entry_rent_bumps: 1,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Has check, same amount of data is read as for persistent has check.
        let _ = &host
            .try_call(
                contract_id,
                Symbol::try_from_val(&host, &"has_temporary").unwrap(),
                test_vec![&host, key].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 316476,
                mem_bytes: 1134775,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Extend persistent entry, 1 persistent extension is expected.
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"extend_persistent").unwrap(),
                test_vec![&host, key, &5000_u32, &5000_u32].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 317701,
                mem_bytes: 1135127,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 336084,
                persistent_entry_rent_bumps: 1,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Extend temp entry, 1 persistent extension is expected.
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"extend_temporary").unwrap(),
                test_vec![&host, key, &3000_u32, &3000_u32].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 318103,
                mem_bytes: 1135127,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 250740,
                temporary_entry_rent_bumps: 1,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Try extending entry for a non-existent key, this should fail.
        let non_existent_key = Symbol::try_from_small_str("non_exist").unwrap();
        let res = &host.call(
            contract_id,
            Symbol::try_from_val(&host, &"extend_persistent").unwrap(),
            test_vec![&host, non_existent_key, &5000_u32, &5000_u32].into(),
        );
        assert!(res.is_err());
        expect![[r#"
            InvocationResources {
                instructions: 317540,
                mem_bytes: 1135195,
                disk_read_entries: 0,
                memory_read_entries: 3,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Advance the ledger sequence to get the contract instance and Wasm
        // to expire.
        host.with_mut_ledger_info(|li| {
            li.sequence_number += li.min_persistent_entry_ttl;
        })
        .unwrap();
        // `has_persistent`` check has to trigger auto-restore for 2 entries (
        // the contract instance/code).
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"has_persistent").unwrap(),
                test_vec![&host, key].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 320711,
                mem_bytes: 1135662,
                disk_read_entries: 2,
                memory_read_entries: 1,
                write_entries: 2,
                disk_read_bytes: 3132,
                write_bytes: 3132,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 80531388,
                persistent_entry_rent_bumps: 2,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);

        // Advance the ledger further to make the persistent key to expire as
        // well.
        host.with_mut_ledger_info(|li| {
            li.sequence_number += 5000 - li.min_persistent_entry_ttl + 1;
        })
        .unwrap();
        // 3 entries will be autorestored now.
        let _ = &host
            .call(
                contract_id,
                Symbol::try_from_val(&host, &"has_persistent").unwrap(),
                test_vec![&host, key].into(),
            )
            .unwrap();
        expect![[r#"
            InvocationResources {
                instructions: 323248,
                mem_bytes: 1136109,
                disk_read_entries: 3,
                memory_read_entries: 0,
                write_entries: 3,
                disk_read_bytes: 3216,
                write_bytes: 3216,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 80615304,
                persistent_entry_rent_bumps: 3,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }"#]]
        .assert_eq(format!("{:#?}", host.get_last_invocation_resources().unwrap()).as_str());
        assert_resources_equal_to_budget(&host);
    }

    #[test]
    fn test_resource_fee_estimation() {
        // No resources
        assert_eq!(
            InvocationResources {
                instructions: 0,
                mem_bytes: 100_000,
                disk_read_entries: 0,
                memory_read_entries: 100,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events_size_bytes: 0,
                persistent_rent_ledger_bytes: 0,
                persistent_entry_rent_bumps: 0,
                temporary_rent_ledger_bytes: 0,
                temporary_entry_rent_bumps: 0,
            }
            .estimate_fees(
                &FeeConfiguration {
                    fee_per_instruction_increment: 100,
                    fee_per_disk_read_entry: 100,
                    fee_per_write_entry: 100,
                    fee_per_disk_read_1kb: 100,
                    fee_per_write_1kb: 100,
                    fee_per_historical_1kb: 100,
                    fee_per_contract_event_1kb: 100,
                    fee_per_transaction_size_1kb: 100,
                },
                100,
                1,
                1
            ),
            FeeEstimate {
                total: 0,
                instructions: 0,
                disk_read_entries: 0,
                write_entries: 0,
                disk_read_bytes: 0,
                write_bytes: 0,
                contract_events: 0,
                persistent_entry_rent: 0,
                temporary_entry_rent: 0,
            }
        );

        // Minimal resources
        assert_eq!(
            InvocationResources {
                instructions: 1,
                mem_bytes: 100_000,
                disk_read_entries: 1,
                memory_read_entries: 100,
                write_entries: 1,
                disk_read_bytes: 1,
                write_bytes: 1,
                contract_events_size_bytes: 1,
                persistent_rent_ledger_bytes: 1,
                persistent_entry_rent_bumps: 1,
                temporary_rent_ledger_bytes: 1,
                temporary_entry_rent_bumps: 1
            }
            .estimate_fees(
                &FeeConfiguration {
                    fee_per_instruction_increment: 100,
                    fee_per_disk_read_entry: 100,
                    fee_per_write_entry: 100,
                    fee_per_disk_read_1kb: 100,
                    fee_per_write_1kb: 100,
                    fee_per_historical_1kb: 100,
                    fee_per_contract_event_1kb: 100,
                    fee_per_transaction_size_1kb: 100,
                },
                100,
                1,
                1
            ),
            FeeEstimate {
                total: 516,
                instructions: 1,
                disk_read_entries: 200,
                write_entries: 100,
                disk_read_bytes: 1,
                write_bytes: 1,
                contract_events: 1,
                persistent_entry_rent: 106,
                temporary_entry_rent: 106
            }
        );

        // Different resource/fee values, based on the values from
        // fees::resource_fee_computation test.
        assert_eq!(
            InvocationResources {
                instructions: 10_123_456,
                mem_bytes: 100_000,
                disk_read_entries: 30,
                memory_read_entries: 100,
                write_entries: 10,
                disk_read_bytes: 25_600,
                write_bytes: 10_340,
                contract_events_size_bytes: 321_654,
                persistent_rent_ledger_bytes: 1_000_000_000,
                persistent_entry_rent_bumps: 3,
                temporary_rent_ledger_bytes: 4_000_000_000,
                temporary_entry_rent_bumps: 6
            }
            .estimate_fees(
                &FeeConfiguration {
                    fee_per_instruction_increment: 1000,
                    fee_per_disk_read_entry: 2000,
                    fee_per_write_1kb: 3000,
                    fee_per_write_entry: 4000,
                    fee_per_disk_read_1kb: 1500,
                    fee_per_historical_1kb: 300,
                    fee_per_contract_event_1kb: 200,
                    fee_per_transaction_size_1kb: 900,
                },
                6000,
                1000,
                2000
            ),
            FeeEstimate {
                // 1_200_139 + event fees + rent fees
                total: 18_878_354,
                instructions: 1_012_346,
                disk_read_entries: 80000,
                write_entries: 40000,
                disk_read_bytes: 37500,
                write_bytes: 30293,
                contract_events: 62824,
                persistent_entry_rent: 5871797,
                temporary_entry_rent: 11743594
            }
        );

        // Integer limits
        assert_eq!(
            InvocationResources {
                instructions: i64::MAX,
                mem_bytes: i64::MAX,
                disk_read_entries: u32::MAX,
                memory_read_entries: 100,
                write_entries: u32::MAX,
                disk_read_bytes: u32::MAX,
                write_bytes: u32::MAX,
                contract_events_size_bytes: u32::MAX,
                persistent_rent_ledger_bytes: i64::MAX,
                persistent_entry_rent_bumps: u32::MAX,
                temporary_rent_ledger_bytes: i64::MAX,
                temporary_entry_rent_bumps: u32::MAX
            }
            .estimate_fees(
                &FeeConfiguration {
                    fee_per_instruction_increment: i64::MAX,
                    fee_per_disk_read_entry: i64::MAX,
                    fee_per_write_entry: i64::MAX,
                    fee_per_disk_read_1kb: i64::MAX,
                    fee_per_write_1kb: i64::MAX,
                    fee_per_historical_1kb: i64::MAX,
                    fee_per_contract_event_1kb: i64::MAX,
                    fee_per_transaction_size_1kb: i64::MAX,
                },
                i64::MAX,
                i64::MAX,
                i64::MAX
            ),
            FeeEstimate {
                total: i64::MAX,
                instructions: 922337203685478,
                disk_read_entries: i64::MAX,
                write_entries: i64::MAX,
                disk_read_bytes: 9007199254740992,
                write_bytes: 9007199254740992,
                contract_events: 9007199254740992,
                persistent_entry_rent: i64::MAX,
                temporary_entry_rent: i64::MAX
            }
        );
    }

    #[test]
    fn test_estimate_detailed_fees() {
        let resources = DetailedInvocationResources {
            invocation: MeteringInvocation::InvokeContract(
                ScAddress::Contract(ContractId(Hash([1; 32]))),
                "foo".try_into().unwrap(),
            ),
            resources: SubInvocationResources {
                instructions: 10_123_456,
                mem_bytes: 100_000,
                disk_read_entries: 30,
                memory_read_entries: 100,
                write_entries: 10,
                disk_read_bytes: 25_600,
                write_bytes: 10_340,
                contract_events_size_bytes: 321_654,
                persistent_rent_ledger_bytes: 1_000_000_000,
                persistent_entry_rent_bumps: 3,
                temporary_rent_ledger_bytes: 4_000_000_000,
                temporary_entry_rent_bumps: 6,
            },
            sub_call_resources: vec![
                DetailedInvocationResources {
                    invocation: MeteringInvocation::WasmUploadEntryPoint,
                    resources: SubInvocationResources {
                        instructions: 1,
                        mem_bytes: 200_000,
                        disk_read_entries: 1,
                        memory_read_entries: 100,
                        write_entries: 1,
                        disk_read_bytes: 1,
                        write_bytes: 1,
                        contract_events_size_bytes: 1,
                        persistent_rent_ledger_bytes: 1,
                        persistent_entry_rent_bumps: 1,
                        temporary_rent_ledger_bytes: 1,
                        temporary_entry_rent_bumps: 1,
                    },
                    sub_call_resources: vec![DetailedInvocationResources {
                        invocation: MeteringInvocation::CreateContractEntryPoint,
                        resources: SubInvocationResources {
                            instructions: 0,
                            mem_bytes: 300_000,
                            disk_read_entries: 0,
                            memory_read_entries: 100,
                            write_entries: 0,
                            disk_read_bytes: 0,
                            write_bytes: 0,
                            contract_events_size_bytes: 0,
                            persistent_rent_ledger_bytes: 0,
                            persistent_entry_rent_bumps: 0,
                            temporary_rent_ledger_bytes: 0,
                            temporary_entry_rent_bumps: 0,
                        },
                        sub_call_resources: vec![],
                    }],
                },
                DetailedInvocationResources {
                    invocation: MeteringInvocation::InvokeContract(
                        ScAddress::Contract(ContractId(Hash([2; 32]))),
                        "bar".try_into().unwrap(),
                    ),
                    resources: SubInvocationResources {
                        instructions: 10_000,
                        mem_bytes: 500_000,
                        contract_events_size_bytes: 100,
                        // All the storage metrics may be negative in case if
                        // a contract call deletes previously modified entries.
                        // We just treat them as zeroes for fee estimation.
                        disk_read_entries: -1,
                        memory_read_entries: -2,
                        write_entries: -3,
                        disk_read_bytes: -4,
                        write_bytes: -5,
                        persistent_rent_ledger_bytes: -6,
                        persistent_entry_rent_bumps: -7,
                        temporary_rent_ledger_bytes: -8,
                        temporary_entry_rent_bumps: -9,
                    },
                    sub_call_resources: vec![],
                },
            ],
        };

        let fee_estimate = resources.estimate_fees(
            &FeeConfiguration {
                fee_per_instruction_increment: 1000,
                fee_per_disk_read_entry: 2000,
                fee_per_write_1kb: 3000,
                fee_per_write_entry: 4000,
                fee_per_disk_read_1kb: 1500,
                fee_per_historical_1kb: 300,
                fee_per_contract_event_1kb: 200,
                fee_per_transaction_size_1kb: 900,
            },
            6000,
            1000,
            2000,
        );
        expect![[r#"
            DetailedFeeEstimate {
                invocation: InvokeContract(
                    Contract(
                        ContractId(
                            Hash(0101010101010101010101010101010101010101010101010101010101010101),
                        ),
                    ),
                    ScSymbol(
                        StringM(foo),
                    ),
                ),
                fee_estimate: FeeEstimate {
                    total: 18878354,
                    instructions: 1012346,
                    disk_read_entries: 80000,
                    write_entries: 40000,
                    disk_read_bytes: 37500,
                    write_bytes: 30293,
                    contract_events: 62824,
                    persistent_entry_rent: 5871797,
                    temporary_entry_rent: 11743594,
                },
                sub_call_fee_estimates: [
                    DetailedFeeEstimate {
                        invocation: WasmUploadEntryPoint,
                        fee_estimate: FeeEstimate {
                            total: 16291,
                            instructions: 1,
                            disk_read_entries: 4000,
                            write_entries: 4000,
                            disk_read_bytes: 2,
                            write_bytes: 3,
                            contract_events: 1,
                            persistent_entry_rent: 4142,
                            temporary_entry_rent: 4142,
                        },
                        sub_call_fee_estimates: [
                            DetailedFeeEstimate {
                                invocation: CreateContractEntryPoint,
                                fee_estimate: FeeEstimate {
                                    total: 0,
                                    instructions: 0,
                                    disk_read_entries: 0,
                                    write_entries: 0,
                                    disk_read_bytes: 0,
                                    write_bytes: 0,
                                    contract_events: 0,
                                    persistent_entry_rent: 0,
                                    temporary_entry_rent: 0,
                                },
                                sub_call_fee_estimates: [],
                            },
                        ],
                    },
                    DetailedFeeEstimate {
                        invocation: InvokeContract(
                            Contract(
                                ContractId(
                                    Hash(0202020202020202020202020202020202020202020202020202020202020202),
                                ),
                            ),
                            ScSymbol(
                                StringM(bar),
                            ),
                        ),
                        fee_estimate: FeeEstimate {
                            total: 1020,
                            instructions: 1000,
                            disk_read_entries: 0,
                            write_entries: 0,
                            disk_read_bytes: 0,
                            write_bytes: 0,
                            contract_events: 20,
                            persistent_entry_rent: 0,
                            temporary_entry_rent: 0,
                        },
                        sub_call_fee_estimates: [],
                    },
                ],
            }"#]]
        .assert_eq(format!("{:#?}", fee_estimate).as_str());
    }
}
