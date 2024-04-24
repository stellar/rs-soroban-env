use crate::SnapshotSourceWithArchive;
use anyhow::{anyhow, bail, Context, Result};
use soroban_env_host::budget::Budget;
use soroban_env_host::fees::{
    compute_write_fee_per_1kb, FeeConfiguration, RentFeeConfiguration, WriteFeeConfiguration,
};
use soroban_env_host::xdr::{
    ConfigSettingEntry, ConfigSettingId, ContractCostParams, LedgerEntry, LedgerEntryData,
    LedgerKey, LedgerKeyConfigSetting,
};
use soroban_env_host::LedgerInfo;
use std::rc::Rc;

/// Network configuration necessary for Soroban operation simulations.
///
/// This should normally be loaded from the ledger.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct NetworkConfig {
    pub fee_configuration: FeeConfiguration,
    pub rent_fee_configuration: RentFeeConfiguration,
    pub tx_max_instructions: i64,
    pub tx_memory_limit: u32,
    pub cpu_cost_params: ContractCostParams,
    pub memory_cost_params: ContractCostParams,
    // Configuration to use in `LedgerInfo`.
    pub min_temp_entry_ttl: u32,
    pub min_persistent_entry_ttl: u32,
    pub max_entry_ttl: u32,
}

fn load_configuration_setting(
    snapshot: &impl SnapshotSourceWithArchive,
    setting_id: ConfigSettingId,
) -> Result<ConfigSettingEntry> {
    let key = Rc::new(LedgerKey::ConfigSetting(LedgerKeyConfigSetting {
        config_setting_id: setting_id,
    }));
    let (entry, _) = snapshot
        .get_including_archived(&key)?
        .ok_or_else(|| anyhow!("setting {setting_id:?} is not present in the snapshot"))?;
    if let LedgerEntry {
        data: LedgerEntryData::ConfigSetting(cs),
        ..
    } = &*entry
    {
        Ok(cs.clone())
    } else {
        bail!("encountered unexpected config setting ledger entry {entry:#?}");
    }
}

macro_rules! load_setting {
    ($snapshot:ident, $enum_variant:ident) => {
        match load_configuration_setting($snapshot, ConfigSettingId::$enum_variant)? {
            ConfigSettingEntry::$enum_variant(setting) => setting,
            _ => bail!(
                "loaded unexpected config setting entry for {:?} key",
                stringify!($enum_variant)
            ),
        }
    };
}

impl NetworkConfig {
    /// Loads configuration from the ledger snapshot.
    ///
    /// This may only fail in case when provided snapshot doesn't contain
    /// all the necessary entries or when these entries are mis-configured.
    pub fn load_from_snapshot(
        snapshot: &impl SnapshotSourceWithArchive,
        bucket_list_size: u64,
    ) -> Result<Self> {
        let compute = load_setting!(snapshot, ContractComputeV0);
        let ledger_cost = load_setting!(snapshot, ContractLedgerCostV0);
        let historical_data = load_setting!(snapshot, ContractHistoricalDataV0);
        let events = load_setting!(snapshot, ContractEventsV0);
        let bandwidth = load_setting!(snapshot, ContractBandwidthV0);
        let state_archival = load_setting!(snapshot, StateArchival);
        let cpu_cost_params = load_setting!(snapshot, ContractCostParamsCpuInstructions);
        let memory_cost_params = load_setting!(snapshot, ContractCostParamsMemoryBytes);

        let write_fee_configuration = WriteFeeConfiguration {
            bucket_list_target_size_bytes: ledger_cost.bucket_list_target_size_bytes,
            write_fee_1kb_bucket_list_low: ledger_cost.write_fee1_kb_bucket_list_low,
            write_fee_1kb_bucket_list_high: ledger_cost.write_fee1_kb_bucket_list_high,
            bucket_list_write_fee_growth_factor: ledger_cost.bucket_list_write_fee_growth_factor,
        };
        let bucket_list_size: i64 = bucket_list_size
            .try_into()
            .context("bucket list size exceeds i64::MAX")?;
        let write_fee_per_1kb =
            compute_write_fee_per_1kb(bucket_list_size, &write_fee_configuration);

        let fee_configuration = FeeConfiguration {
            fee_per_instruction_increment: compute.fee_rate_per_instructions_increment,
            fee_per_read_entry: ledger_cost.fee_read_ledger_entry,
            fee_per_write_entry: ledger_cost.fee_write_ledger_entry,
            fee_per_read_1kb: ledger_cost.fee_read1_kb,
            fee_per_write_1kb: write_fee_per_1kb,
            fee_per_historical_1kb: historical_data.fee_historical1_kb,
            fee_per_contract_event_1kb: events.fee_contract_events1_kb,
            fee_per_transaction_size_1kb: bandwidth.fee_tx_size1_kb,
        };
        let rent_fee_configuration = RentFeeConfiguration {
            fee_per_write_1kb: write_fee_per_1kb,
            fee_per_write_entry: ledger_cost.fee_write_ledger_entry,
            persistent_rent_rate_denominator: state_archival.persistent_rent_rate_denominator,
            temporary_rent_rate_denominator: state_archival.temp_rent_rate_denominator,
        };

        Ok(Self {
            fee_configuration,
            rent_fee_configuration,
            cpu_cost_params,
            memory_cost_params,
            min_temp_entry_ttl: state_archival.min_temporary_ttl,
            min_persistent_entry_ttl: state_archival.min_persistent_ttl,
            tx_max_instructions: compute.tx_max_instructions,
            tx_memory_limit: compute.tx_memory_limit,
            max_entry_ttl: state_archival.max_entry_ttl,
        })
    }

    /// Fills the `ledger_info` fields that are loaded from the config.
    ///
    /// This should normally be used to populate TTL-related fields in
    /// `LedgerInfo`, so that config loading logic can be encapsulated
    /// just in `NetworkConfig`.
    pub fn fill_config_fields_in_ledger_info(&self, ledger_info: &mut LedgerInfo) {
        ledger_info.min_persistent_entry_ttl = self.min_persistent_entry_ttl;
        ledger_info.min_temp_entry_ttl = self.min_temp_entry_ttl;
        ledger_info.max_entry_ttl = self.max_entry_ttl;
    }

    pub(crate) fn create_budget(&self) -> Result<Budget> {
        Budget::try_from_configs(
            self.tx_max_instructions as u64,
            self.tx_memory_limit as u64,
            self.cpu_cost_params.clone(),
            self.memory_cost_params.clone(),
        )
        .context("cannot create budget from network configuration")
    }
}
