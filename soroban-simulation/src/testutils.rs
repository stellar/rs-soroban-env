use anyhow::{bail, Result};
use soroban_env_host::{
    e2e_testutils::ledger_entry,
    storage::{EntryWithLiveUntil, SnapshotSource},
    xdr::{
        ContractDataDurability, ContractDataEntry, ContractId, ExtensionPoint, Hash, LedgerEntry,
        LedgerEntryData, LedgerKey, LedgerKeyAccount, LedgerKeyConfigSetting,
        LedgerKeyContractCode, LedgerKeyContractData, LedgerKeyTrustLine, ScAddress, ScBytes,
        ScVal,
    },
    HostError,
};
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct MockSnapshotSource {
    map: BTreeMap<Rc<LedgerKey>, EntryWithLiveUntil>,
}

impl MockSnapshotSource {
    pub fn from_entries(entries: Vec<(LedgerEntry, Option<u32>)>) -> Result<Self> {
        let mut map = BTreeMap::<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>::new();
        for (e, maybe_ttl) in entries {
            let key = Rc::new(ledger_entry_to_ledger_key(&e)?);
            map.insert(key, (Rc::new(e), maybe_ttl));
        }
        Ok(Self { map })
    }
}

pub fn ledger_entry_to_ledger_key(entry: &LedgerEntry) -> Result<LedgerKey> {
    match &entry.data {
        LedgerEntryData::Account(a) => Ok(LedgerKey::Account(LedgerKeyAccount {
            account_id: a.account_id.clone(),
        })),
        LedgerEntryData::Trustline(tl) => Ok(LedgerKey::Trustline(LedgerKeyTrustLine {
            account_id: tl.account_id.clone(),
            asset: tl.asset.clone(),
        })),
        LedgerEntryData::ContractData(cd) => Ok(LedgerKey::ContractData(LedgerKeyContractData {
            contract: cd.contract.clone(),
            key: cd.key.clone(),
            durability: cd.durability,
        })),
        LedgerEntryData::ContractCode(code) => Ok(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: code.hash.clone(),
        })),
        LedgerEntryData::ConfigSetting(cs) => {
            Ok(LedgerKey::ConfigSetting(LedgerKeyConfigSetting {
                config_setting_id: cs.discriminant(),
            }))
        }
        _ => bail!("ledger entry type is not supported: {entry:#?}"),
    }
}

impl SnapshotSource for MockSnapshotSource {
    fn get(
        &self,
        key: &Rc<LedgerKey>,
    ) -> std::result::Result<Option<EntryWithLiveUntil>, HostError> {
        if let Some((entry, live_until)) = self.map.get(key) {
            Ok(Some((entry.clone(), *live_until)))
        } else {
            Ok(None)
        }
    }
}

pub fn temp_entry(key: &[u8]) -> LedgerEntry {
    ledger_entry(LedgerEntryData::ContractData(ContractDataEntry {
        ext: ExtensionPoint::V0,
        contract: ScAddress::Contract(ContractId(Hash([0; 32]))),
        key: ScVal::Bytes(ScBytes(key.try_into().unwrap())),
        durability: ContractDataDurability::Temporary,
        val: ScVal::Void,
    }))
}
