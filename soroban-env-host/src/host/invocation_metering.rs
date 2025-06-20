use std::cell::RefMut;

use soroban_env_common::Env;

use crate::{
    e2e_invoke::{encode_contract_events, entry_size_for_rent},
    fees::{FeeConfiguration, DATA_SIZE_1KB_INCREMENT, INSTRUCTIONS_INCREMENT, TTL_ENTRY_SIZE},
    ledger_info::get_key_durability,
    storage::{is_persistent_key, AccessType, Storage},
    xdr::{ContractDataDurability, LedgerKey, ScErrorCode, ScErrorType},
};

use super::{metered_xdr::metered_write_xdr, Host, HostError};

/// Represents the resources measured during an invocation.
///
/// This resembles the resources necessary to build a Soroban transaction and
/// compute its fee with a few exceptions (specifically, the transaction size
/// and the return value size).
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

/// A helper for metering the resources only within a logical host invocation
/// without finalizing the host.
///
/// The 'logical' invocations are the typical entry points for the unit tests,
/// such as invocations based on `HostFunction` XDR, lifecycle operations
/// (registering Wasm, creating a contract instance), direct contract calls
/// etc.
#[derive(Default, Clone)]
pub(crate) struct InvocationMeter {
    active: bool,
    enabled: bool,
    storage_snapshot: Option<Storage>,
    invocation_resources: Option<InvocationResources>,
}

/// Scope guard for `InvocationMeter` that automatically finishes the metered
/// invocation when it goes out of scope.
pub(crate) struct InvocationMeterScope<'a> {
    meter: RefMut<'a, InvocationMeter>,
    host: &'a Host,
}

impl Drop for InvocationMeterScope<'_> {
    fn drop(&mut self) {
        self.meter.finish_invocation(self.host);
    }
}

impl InvocationMeter {
    /// Gets the metered resources for the last metered invocation (if any).
    pub(crate) fn get_invocation_resources(&self) -> Option<InvocationResources> {
        self.invocation_resources.clone()
    }

    fn start_invocation<'a>(
        mut scope: RefMut<'a, InvocationMeter>,
        host: &'a Host,
    ) -> Result<Option<InvocationMeterScope<'a>>, HostError> {
        if scope.active || !scope.enabled {
            return Ok(None);
        }
        scope.storage_snapshot = Some(host.try_borrow_storage()?.clone());
        // Reset all the state relevant to the invocation resources. Note, that
        // the storage itself shouldn't be reset, as it's treated as the ledger
        // state before invocation.
        host.try_borrow_storage_mut()?.reset_footprint();
        host.try_borrow_events_mut()?.clear();
        host.budget_ref().reset()?;
        Ok(Some(InvocationMeterScope { meter: scope, host }))
    }

    fn finish_invocation(&mut self, host: &Host) -> () {
        self.active = false;
        let mut invocation_resources = InvocationResources::default();
        let budget = host.budget_ref();
        invocation_resources.instructions =
            budget.get_cpu_insns_consumed().unwrap_or_default() as i64;
        invocation_resources.mem_bytes = budget.get_mem_bytes_consumed().unwrap_or_default() as i64;

        let measure_res = budget.with_observable_shadow_mode(|| {
            self.try_measure_resources(&mut invocation_resources, host)
        });

        if measure_res.is_ok() {
            self.invocation_resources = Some(invocation_resources);
        } else {
            self.invocation_resources = None;
        }

        // Emulate the write-back to the module cache (typically done by the
        // embedding environment) of any new contracts added during the
        // invocation.
        budget.with_shadow_mode(|| host.ensure_module_cache_contains_host_storage_contracts());

        self.storage_snapshot = None;
    }

    fn try_measure_resources(
        &mut self,
        invocation_resources: &mut InvocationResources,
        host: &Host,
    ) -> Result<(), HostError> {
        let prev_storage = self.storage_snapshot.as_mut().ok_or_else(|| {
            host.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "missing a storage snapshot in metering scope, `open` must be called before `close`",
                &[],
            )
        })?;

        let mut curr_storage = host.try_borrow_storage_mut()?;
        let footprint = curr_storage.footprint.clone();
        let curr_ledger_seq: u32 = host.get_ledger_sequence()?.into();
        for (key, access_type) in footprint.0.iter(host.budget_ref())? {
            let maybe_init_entry = prev_storage.get_from_map(key, host)?;
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
                metered_write_xdr(host.budget_ref(), init_entry.as_ref(), &mut buf)?;
                if is_disk_read {
                    invocation_resources.disk_read_bytes += buf.len() as u32;
                }
                init_entry_size_for_rent =
                    entry_size_for_rent(host.budget_ref(), &init_entry, buf.len() as u32)?;
            }
            let mut entry_size = 0;
            let mut new_entry_size_for_rent = 0;
            let mut entry_live_until_ledger = None;
            let maybe_entry = curr_storage.try_get_full(key, host, None)?;
            if let Some((entry, entry_live_until)) = maybe_entry {
                let mut buf = Vec::<u8>::new();
                metered_write_xdr(host.budget_ref(), entry.as_ref(), &mut buf)?;
                entry_size = buf.len() as u32;
                new_entry_size_for_rent =
                    entry_size_for_rent(host.budget_ref(), &entry, entry_size)?;
                entry_live_until_ledger = entry_live_until;
            }
            if is_disk_read {
                invocation_resources.disk_read_entries += 1;
            } else {
                invocation_resources.memory_read_entries += 1;
            }
            if matches!(access_type, AccessType::ReadWrite) {
                invocation_resources.write_entries += 1;
                invocation_resources.write_bytes += entry_size;
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
        let events = host.try_borrow_events()?.externalize(&host)?;
        let encoded_contract_events = encode_contract_events(host.budget_ref(), &events)?;
        for event in &encoded_contract_events {
            invocation_resources.contract_events_size_bytes += event.len() as u32;
        }
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
    ) -> Result<Option<InvocationMeterScope<'_>>, HostError> {
        // Note: we're using the standard `try_borrow_mut` instead of a helper
        // generated with `impl_checked_borrow_helpers` in order to not spam
        // the logs with failures. It is expected for metering_scope to be
        // borrowed.
        if let Ok(scope) = self.0.invocation_meter.try_borrow_mut() {
            InvocationMeter::start_invocation(scope, self)
        } else {
            Ok(None)
        }
    }

    /// Enables invocation metering (it's disabled by default).
    pub fn enable_invocation_metering(&self) {
        if let Ok(mut meter) = self.0.invocation_meter.try_borrow_mut() {
            meter.enabled = true;
        }
    }
}

fn compute_fee_per_increment(resource_value: i64, fee_rate: i64, increment: i64) -> i64 {
    num_integer::div_ceil(resource_value.saturating_mul(fee_rate), increment.max(1))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Symbol, TryFromVal, TryIntoVal};
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
}
