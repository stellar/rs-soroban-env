use crate::xdr::{ContractDataDurability, LedgerKey};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LedgerInfo {
    pub protocol_version: u32,
    pub sequence_number: u32,
    pub timestamp: u64,
    pub network_id: [u8; 32],
    pub base_reserve: u32,
    pub min_temp_entry_ttl: u32,
    pub min_persistent_entry_ttl: u32,
    pub max_entry_ttl: u32,
}

impl LedgerInfo {
    pub fn min_live_until_ledger_checked(&self, durability: ContractDataDurability) -> Option<u32> {
        let min_live_until = match durability {
            ContractDataDurability::Temporary => self.min_temp_entry_ttl,
            ContractDataDurability::Persistent => self.min_persistent_entry_ttl,
        };
        self.sequence_number
            .checked_add(min_live_until.saturating_sub(1))
    }

    pub fn max_live_until_ledger_checked(&self) -> Option<u32> {
        self.sequence_number
            // Entry can live for at most max_entry_live_until ledgers from
            // now, counting the current one.
            .checked_add(self.max_entry_ttl.saturating_sub(1))
    }
}

pub fn get_key_durability(key: &LedgerKey) -> Option<ContractDataDurability> {
    match &key {
        LedgerKey::ContractData(d) => Some(d.durability),
        LedgerKey::ContractCode(_) => Some(ContractDataDurability::Persistent),
        _ => None,
    }
}
