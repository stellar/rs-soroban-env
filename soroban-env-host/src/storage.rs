use std::rc::Rc;

use crate::xdr::{LedgerEntry, LedgerKey, ScHostStorageErrorCode};
use crate::HostError;
use im_rc::OrdMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AccessType {
    ReadOnly,
    ReadWrite,
}

pub trait SnapshotSource {
    fn get(&self, key: &LedgerKey) -> Result<LedgerEntry, HostError>;
    fn has(&self, key: &LedgerKey) -> Result<bool, HostError>;
}

#[derive(Clone, Default)]
pub struct Footprint(pub OrdMap<LedgerKey, AccessType>);

impl Footprint {
    pub fn record_access(&mut self, key: &LedgerKey, ty: AccessType) {
        if let Some(existing) = self.0.get(key) {
            match (existing, ty.clone()) {
                (AccessType::ReadOnly, AccessType::ReadOnly) => (),
                (AccessType::ReadOnly, AccessType::ReadWrite) => {
                    // The only interesting case is an upgrade
                    // from previously-read-only to read-write.
                    self.0.insert(key.clone(), ty);
                }
                (AccessType::ReadWrite, AccessType::ReadOnly) => (),
                (AccessType::ReadWrite, AccessType::ReadWrite) => (),
            }
        } else {
            self.0.insert(key.clone(), ty);
        }
    }

    pub fn enforce_access(&mut self, key: &LedgerKey, ty: AccessType) -> Result<(), HostError> {
        if let Some(existing) = self.0.get(key) {
            match (existing, ty) {
                (AccessType::ReadOnly, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadOnly, AccessType::ReadWrite) => {
                    Err(ScHostStorageErrorCode::ReadwriteAccessToReadonlyEntry.into())
                }
                (AccessType::ReadWrite, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadWrite, AccessType::ReadWrite) => Ok(()),
            }
        } else {
            Err(ScHostStorageErrorCode::AccessToUnknownEntry.into())
        }
    }
}

#[derive(Clone)]
pub enum FootprintMode {
    Recording(Rc<dyn SnapshotSource>),
    Enforcing,
}

impl Default for FootprintMode {
    fn default() -> Self {
        FootprintMode::Enforcing
    }
}

#[derive(Clone, Default)]
pub struct Storage {
    pub footprint: Footprint,
    pub mode: FootprintMode,
    pub map: OrdMap<LedgerKey, Option<LedgerEntry>>,
}

impl Storage {
    pub fn with_enforcing_footprint_and_map(
        footprint: Footprint,
        map: OrdMap<LedgerKey, Option<LedgerEntry>>,
    ) -> Self {
        Self {
            mode: FootprintMode::Enforcing,
            footprint,
            map,
        }
    }

    pub fn with_recording_footprint(src: Rc<dyn SnapshotSource>) -> Self {
        Self {
            mode: FootprintMode::Recording(src),
            footprint: Footprint::default(),
            map: Default::default(),
        }
    }

    pub fn get(&mut self, key: &LedgerKey) -> Result<LedgerEntry, HostError> {
        let ty = AccessType::ReadOnly;
        match self.mode {
            FootprintMode::Recording(ref src) => {
                self.footprint.record_access(key, ty);
                // In recording mode we treat the map as a cache
                // that misses read-through to the underlying src.
                if !self.map.contains_key(key) {
                    self.map.insert(key.clone(), Some(src.get(key)?));
                }
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty)?;
            }
        };
        match self.map.get(key) {
            None => Err(ScHostStorageErrorCode::MissingKeyInGet.into()),
            Some(None) => Err(ScHostStorageErrorCode::GetOnDeletedKey.into()),
            Some(Some(val)) => Ok(val.clone()),
        }
    }

    fn put_opt(&mut self, key: &LedgerKey, val: Option<LedgerEntry>) -> Result<(), HostError> {
        let ty = AccessType::ReadWrite;
        match self.mode {
            FootprintMode::Recording(_) => {
                self.footprint.record_access(key, ty);
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty)?;
            }
        };
        self.map.insert(key.clone(), val);
        Ok(())
    }

    pub fn put(&mut self, key: &LedgerKey, val: &LedgerEntry) -> Result<(), HostError> {
        self.put_opt(key, Some(val.clone()))
    }

    pub fn del(&mut self, key: &LedgerKey) -> Result<(), HostError> {
        self.put_opt(key, None)
    }

    pub fn has(&mut self, key: &LedgerKey) -> Result<bool, HostError> {
        let ty = AccessType::ReadOnly;
        match self.mode {
            FootprintMode::Recording(ref src) => {
                self.footprint.record_access(key, ty);
                // We don't cache has() calls but we do
                // consult the cache before answering them.
                match self.map.get(key) {
                    Some(None) => Ok(false),
                    Some(Some(_)) => Ok(true),
                    None => src.has(key),
                }
            }
            FootprintMode::Enforcing => {
                self.footprint.enforce_access(key, ty)?;
                match self.map.get(key) {
                    Some(None) => Ok(false),
                    Some(Some(_)) => Ok(true),
                    None => Ok(false),
                }
            }
        }
    }
}

#[cfg(test)]
mod test_footprint {

    use super::*;
    use crate::xdr::{LedgerKeyContractData, ScVal};

    #[test]
    fn footprint_record_access() {
        let mut fp = Footprint::default();
        // record when key not exist
        let contract_id = [0; 32].into();
        let key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::I32(0),
        });
        fp.record_access(&key, AccessType::ReadOnly);
        assert_eq!(fp.0.contains_key(&key), true);
        assert_eq!(fp.0.get(&key), Some(&AccessType::ReadOnly));
        // record and change access
        fp.record_access(&key, AccessType::ReadWrite);
        assert_eq!(fp.0.get(&key), Some(&AccessType::ReadWrite));
        fp.record_access(&key, AccessType::ReadOnly);
        assert_eq!(fp.0.get(&key), Some(&AccessType::ReadWrite));
    }

    #[test]
    fn footprint_enforce_access() -> Result<(), HostError> {
        let contract_id = [0; 32].into();
        let key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::I32(0),
        });
        let om = OrdMap::unit(key.clone(), AccessType::ReadOnly);
        let mut fp = Footprint(om);
        fp.enforce_access(&key, AccessType::ReadOnly)?;
        fp.0.insert(key.clone(), AccessType::ReadWrite);
        fp.enforce_access(&key, AccessType::ReadOnly)?;
        fp.enforce_access(&key, AccessType::ReadWrite)?;
        Ok(())
    }

    #[test]
    fn footprint_enforce_access_not_exist() {
        let mut fp = Footprint::default();
        let contract_id = [0; 32].into();
        let key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::I32(0),
        });
        let res = fp.enforce_access(&key, AccessType::ReadOnly);
        assert!(HostError::result_matches_err_status(
            res,
            ScHostStorageErrorCode::AccessToUnknownEntry
        ));
    }

    #[test]
    fn footprint_attempt_to_write_readonly_entry() {
        let contract_id = [0; 32].into();
        let key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::I32(0),
        });
        let om = OrdMap::unit(key.clone(), AccessType::ReadOnly);
        let mut fp = Footprint(om);
        let res = fp.enforce_access(&key, AccessType::ReadWrite);
        assert!(HostError::result_matches_err_status(
            res,
            ScHostStorageErrorCode::ReadwriteAccessToReadonlyEntry
        ));
    }
}

#[cfg(test)]
mod test_storage {
    use soroban_env_common::xdr::ScUnknownErrorCode;

    use super::*;
    #[allow(dead_code)]
    struct MockSnapshotSource(OrdMap<LedgerKey, LedgerEntry>);
    #[allow(dead_code)]
    impl MockSnapshotSource {
        fn get(&self, key: &LedgerKey) -> Result<LedgerEntry, HostError> {
            if let Some(val) = self.0.get(key) {
                Ok(val.clone())
            } else {
                Err(ScUnknownErrorCode::General.into())
            }
        }

        fn has(&self, key: &LedgerKey) -> Result<bool, HostError> {
            Ok(self.0.contains_key(key))
        }
    }
}
