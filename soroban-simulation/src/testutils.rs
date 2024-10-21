use crate::snapshot_source::{
    LedgerEntryArchivalState, LedgerEntryWithArchivalState, SnapshotSourceWithArchive,
};
use anyhow::{bail, Result};
use soroban_env_host::{
    e2e_testutils::ledger_entry,
    ledger_info::get_key_durability,
    storage::{EntryWithLiveUntil, SnapshotSource},
    xdr::{
        ArchivalProof, ArchivalProofBody, ColdArchiveArchivedLeaf, ColdArchiveBucketEntry,
        ContractDataDurability, ContractDataEntry, ExistenceProofBody, ExtensionPoint, Hash,
        LedgerEntry, LedgerEntryData, LedgerKey, LedgerKeyAccount, LedgerKeyConfigSetting,
        LedgerKeyContractCode, LedgerKeyContractData, LedgerKeyTrustLine, NonexistenceProofBody,
        ScAddress, ScBytes, ScErrorCode, ScErrorType, ScVal,
    },
    HostError,
};
use std::collections::BTreeMap;
use std::rc::Rc;

pub struct MockSnapshotSource {
    map: BTreeMap<Rc<LedgerKey>, LedgerEntryWithArchivalState>,
}

impl MockSnapshotSource {
    pub fn from_entries(
        entries: Vec<(LedgerEntry, Option<u32>)>,
        current_ledger_seq: u32,
    ) -> Result<Self> {
        let mut map = BTreeMap::<Rc<LedgerKey>, LedgerEntryWithArchivalState>::new();
        for (e, live_until_ledger) in entries {
            let key = Rc::new(ledger_entry_to_ledger_key(&e)?);
            let state = if let Some(live_until_ledger) = &live_until_ledger {
                if *live_until_ledger < current_ledger_seq {
                    if matches!(
                        get_key_durability(&*key),
                        Some(ContractDataDurability::Persistent)
                    ) {
                        LedgerEntryArchivalState::Archived(false)
                    } else {
                        LedgerEntryArchivalState::New(false)
                    }
                } else {
                    LedgerEntryArchivalState::Live
                }
            } else {
                LedgerEntryArchivalState::Live
            };
            let adjusted_live_until_ledger = if matches!(state, LedgerEntryArchivalState::Live) {
                live_until_ledger
            } else {
                None
            };
            let adjusted_entry = if matches!(state, LedgerEntryArchivalState::New(_)) {
                None
            } else {
                Some(Rc::new(e))
            };
            map.insert(
                key,
                LedgerEntryWithArchivalState {
                    entry: adjusted_entry,
                    live_until_ledger: adjusted_live_until_ledger,
                    state,
                },
            );
        }
        Ok(Self { map })
    }

    pub fn from_entries_with_archival_state(
        entries: Vec<(
            Option<LedgerKey>,
            Option<LedgerEntry>,
            Option<u32>,
            LedgerEntryArchivalState,
        )>,
    ) -> Result<Self> {
        let mut map = BTreeMap::<Rc<LedgerKey>, LedgerEntryWithArchivalState>::new();
        for (maybe_key, maybe_entry, live_until_ledger, state) in entries {
            let key = if let Some(k) = maybe_key {
                k
            } else {
                ledger_entry_to_ledger_key(maybe_entry.as_ref().unwrap())?
            };
            let key = Rc::new(key);
            let entry = if let Some(e) = maybe_entry {
                Some(Rc::new(e))
            } else {
                None
            };
            map.insert(
                key,
                LedgerEntryWithArchivalState {
                    entry,
                    live_until_ledger,
                    state,
                },
            );
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

impl SnapshotSourceWithArchive for MockSnapshotSource {
    fn get_including_archived(
        &self,
        key: &Rc<LedgerKey>,
    ) -> std::result::Result<LedgerEntryWithArchivalState, HostError> {
        if let Some(state) = self.map.get(key) {
            Ok(state.clone())
        } else {
            Ok(LedgerEntryWithArchivalState {
                entry: None,
                live_until_ledger: None,
                state: LedgerEntryArchivalState::New(false),
            })
        }
    }

    fn generate_new_entries_proof(&self, keys: &[Rc<LedgerKey>]) -> Result<ArchivalProof> {
        Ok(ArchivalProof {
            epoch: 12345,
            body: ArchivalProofBody::Nonexistence(NonexistenceProofBody {
                keys_to_prove: keys
                    .iter()
                    .map(|k| (**k).clone())
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
                low_bound_entries: Default::default(),
                high_bound_entries: Default::default(),
                proof_levels: Default::default(),
            }),
        })
    }

    fn generate_restoration_proof(&self, keys: &[Rc<LedgerKey>]) -> Result<ArchivalProof> {
        let entries_to_prove = keys
            .iter()
            .map(|k| {
                ColdArchiveBucketEntry::ArchivedLeaf(ColdArchiveArchivedLeaf {
                    index: 111,
                    archived_entry: self
                        .map
                        .get(k)
                        .unwrap()
                        .entry
                        .as_ref()
                        .map(|e| (**e).clone())
                        .unwrap(),
                })
            })
            .collect::<Vec<_>>();
        Ok(ArchivalProof {
            epoch: 12345,
            body: ArchivalProofBody::Existence(ExistenceProofBody {
                entries_to_prove: entries_to_prove.try_into().unwrap(),
                proof_levels: Default::default(),
            }),
        })
    }
}

impl SnapshotSource for MockSnapshotSource {
    fn get(
        &self,
        key: &Rc<LedgerKey>,
    ) -> std::result::Result<Option<EntryWithLiveUntil>, HostError> {
        if let Some(entry_state) = self.map.get(key) {
            if let Some(entry) = &entry_state.entry {
                Ok(Some((entry.clone(), entry_state.live_until_ledger.clone())))
            } else {
                if matches!(entry_state.state, LedgerEntryArchivalState::Archived(_)) {
                    Err(HostError::from((
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                    )))
                } else {
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }
}

pub fn temp_entry(key: &[u8]) -> LedgerEntry {
    ledger_entry(LedgerEntryData::ContractData(ContractDataEntry {
        ext: ExtensionPoint::V0,
        contract: ScAddress::Contract(Hash([0; 32])),
        key: ScVal::Bytes(ScBytes(key.try_into().unwrap())),
        durability: ContractDataDurability::Temporary,
        val: ScVal::Void,
    }))
}

pub fn temp_entry_key(key: &[u8]) -> LedgerKey {
    let entry = temp_entry(key);
    ledger_entry_to_ledger_key(&entry).unwrap()
}
