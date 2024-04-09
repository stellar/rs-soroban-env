use crate::network_config::NetworkConfig;
use soroban_env_host::{
    e2e_testutils::ledger_entry,
    fees::{FeeConfiguration, RentFeeConfiguration},
    xdr::{
        ConfigSettingContractBandwidthV0, ConfigSettingContractComputeV0,
        ConfigSettingContractEventsV0, ConfigSettingContractHistoricalDataV0,
        ConfigSettingContractLedgerCostV0, ConfigSettingEntry, ContractCostParamEntry,
        ContractCostParams, ExtensionPoint, LedgerEntry, LedgerEntryData, StateArchivalSettings,
    },
    LedgerInfo,
};

use crate::testutils::MockSnapshotSource;
use pretty_assertions::assert_eq;
use soroban_env_host::xdr::ContractCostType;

fn config_entry(entry: ConfigSettingEntry) -> (LedgerEntry, Option<u32>) {
    (ledger_entry(LedgerEntryData::ConfigSetting(entry)), None)
}

#[test]
fn test_load_config_from_snapshot() {
    let cpu_cost_params = ContractCostParams(
        vec![
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 35,
                linear_term: 36,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 37,
                linear_term: 38,
            },
        ]
        .try_into()
        .unwrap(),
    );
    let memory_cost_params = ContractCostParams(
        vec![
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 39,
                linear_term: 40,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 41,
                linear_term: 42,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 43,
                linear_term: 44,
            },
        ]
        .try_into()
        .unwrap(),
    );
    let snapshot_source = MockSnapshotSource::from_entries(
        vec![
            config_entry(ConfigSettingEntry::ContractComputeV0(
                ConfigSettingContractComputeV0 {
                    ledger_max_instructions: 1,
                    tx_max_instructions: 2,
                    fee_rate_per_instructions_increment: 3,
                    tx_memory_limit: 4,
                },
            )),
            config_entry(ConfigSettingEntry::ContractLedgerCostV0(
                ConfigSettingContractLedgerCostV0 {
                    ledger_max_read_ledger_entries: 5,
                    ledger_max_read_bytes: 6,
                    ledger_max_write_ledger_entries: 7,
                    ledger_max_write_bytes: 8,
                    tx_max_read_ledger_entries: 9,
                    tx_max_read_bytes: 10,
                    tx_max_write_ledger_entries: 11,
                    tx_max_write_bytes: 12,
                    fee_read_ledger_entry: 13,
                    fee_write_ledger_entry: 14,
                    fee_read1_kb: 15,
                    // From tests/resources `test_compute_write_fee`
                    bucket_list_target_size_bytes: 100_000_000_000_000,
                    write_fee1_kb_bucket_list_low: 1_000_000,
                    write_fee1_kb_bucket_list_high: 1_000_000_000,
                    bucket_list_write_fee_growth_factor: 50,
                },
            )),
            config_entry(ConfigSettingEntry::ContractHistoricalDataV0(
                ConfigSettingContractHistoricalDataV0 {
                    fee_historical1_kb: 20,
                },
            )),
            config_entry(ConfigSettingEntry::ContractEventsV0(
                ConfigSettingContractEventsV0 {
                    tx_max_contract_events_size_bytes: 21,
                    fee_contract_events1_kb: 22,
                },
            )),
            config_entry(ConfigSettingEntry::ContractBandwidthV0(
                ConfigSettingContractBandwidthV0 {
                    ledger_max_txs_size_bytes: 23,
                    tx_max_size_bytes: 24,
                    fee_tx_size1_kb: 25,
                },
            )),
            config_entry(ConfigSettingEntry::StateArchival(StateArchivalSettings {
                max_entry_ttl: 26,
                min_temporary_ttl: 27,
                min_persistent_ttl: 28,
                persistent_rent_rate_denominator: 29,
                temp_rent_rate_denominator: 30,
                max_entries_to_archive: 31,
                bucket_list_size_window_sample_size: 32,
                bucket_list_window_sample_period: 33,
                eviction_scan_size: 34,
                starting_eviction_scan_level: 35,
            })),
            config_entry(ConfigSettingEntry::ContractCostParamsCpuInstructions(
                cpu_cost_params.clone(),
            )),
            config_entry(ConfigSettingEntry::ContractCostParamsMemoryBytes(
                memory_cost_params.clone(),
            )),
        ],
        0,
    )
    .unwrap();

    let network_config =
        NetworkConfig::load_from_snapshot(&snapshot_source, 150_000_000_000_000).unwrap();
    // From tests/resources `test_compute_write_fee`
    let write_fee = 1_000_000_000 + 50 * (1_000_000_000_i64 - 1_000_000) / 2;
    assert_eq!(
        network_config,
        NetworkConfig {
            fee_configuration: FeeConfiguration {
                fee_per_instruction_increment: 3,
                fee_per_read_entry: 13,
                fee_per_write_entry: 14,
                fee_per_read_1kb: 15,
                fee_per_write_1kb: write_fee,
                fee_per_historical_1kb: 20,
                fee_per_contract_event_1kb: 22,
                fee_per_transaction_size_1kb: 25,
            },
            rent_fee_configuration: RentFeeConfiguration {
                fee_per_write_1kb: write_fee,
                fee_per_write_entry: 14,
                persistent_rent_rate_denominator: 29,
                temporary_rent_rate_denominator: 30,
            },
            tx_max_instructions: 2,
            tx_memory_limit: 4,
            cpu_cost_params,
            memory_cost_params,
            min_temp_entry_ttl: 27,
            min_persistent_entry_ttl: 28,
            max_entry_ttl: 26,
        }
    );
}

#[test]
fn test_create_budget() {
    let default_entry = ContractCostParamEntry {
        ext: ExtensionPoint::V0,
        const_term: 0,
        linear_term: 0,
    };
    let mut cpu_cost_params = vec![default_entry.clone(); ContractCostType::variants().len()];
    let mut mem_cost_params = vec![default_entry; ContractCostType::variants().len()];
    cpu_cost_params[ContractCostType::ComputeKeccak256Hash as usize] = ContractCostParamEntry {
        ext: ExtensionPoint::V0,
        const_term: 50,
        linear_term: 100 << 7,
    };
    mem_cost_params[ContractCostType::ComputeKeccak256Hash as usize] = ContractCostParamEntry {
        ext: ExtensionPoint::V0,
        const_term: 20,
        linear_term: 5 << 7,
    };
    let config = NetworkConfig {
        tx_max_instructions: 1000,
        tx_memory_limit: 500,
        cpu_cost_params: ContractCostParams(cpu_cost_params.try_into().unwrap()),
        memory_cost_params: ContractCostParams(mem_cost_params.try_into().unwrap()),
        ..Default::default()
    };
    let budget = config.create_budget().unwrap();
    assert_eq!(budget.get_cpu_insns_remaining().unwrap(), 1000);
    assert_eq!(budget.get_mem_bytes_remaining().unwrap(), 500);

    budget
        .charge(ContractCostType::ComputeKeccak256Hash, Some(5))
        .unwrap();
    assert_eq!(budget.get_cpu_insns_consumed().unwrap(), 50 + 5 * 100);
    assert_eq!(budget.get_mem_bytes_consumed().unwrap(), 20 + 5 * 5);
}

#[test]
fn test_fill_ledger_info() {
    let config = NetworkConfig {
        min_temp_entry_ttl: 111,
        min_persistent_entry_ttl: 222,
        max_entry_ttl: 333,
        ..Default::default()
    };
    let mut ledger_info = LedgerInfo::default();
    config.fill_config_fields_in_ledger_info(&mut ledger_info);
    assert_eq!(
        ledger_info,
        LedgerInfo {
            min_temp_entry_ttl: 111,
            min_persistent_entry_ttl: 222,
            max_entry_ttl: 333,
            ..Default::default()
        }
    )
}
