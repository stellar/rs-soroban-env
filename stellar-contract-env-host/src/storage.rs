use std::rc::Rc;

use crate::xdr::ScVal;
use crate::{ContractID, HostError};
use im_rc::OrdMap;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key {
    contract_id: ContractID,
    key: ScVal,
}

#[derive(Clone, Debug)]
pub enum AccessType {
    ReadOnly,
    ReadWrite,
}

pub trait SnapshotSource {
    fn get(&self, key: &Key) -> Result<ScVal, HostError>;
    fn has(&self, key: &Key) -> Result<bool, HostError>;
}

#[derive(Clone, Default)]
pub struct Footprint(OrdMap<Key, AccessType>);

impl Footprint {
    pub fn record_access(&mut self, key: &Key, ty: AccessType) {
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
    pub fn enforce_access(&mut self, key: &Key, ty: AccessType) -> Result<(), HostError> {
        if let Some(existing) = self.0.get(key) {
            match (existing, ty) {
                (AccessType::ReadOnly, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadOnly, AccessType::ReadWrite) => Err(HostError::General(
                    "read-write access to read-only footprint entry",
                )),
                (AccessType::ReadWrite, AccessType::ReadOnly) => Ok(()),
                (AccessType::ReadWrite, AccessType::ReadWrite) => Ok(()),
            }
        } else {
            Err(HostError::General("access to unknown footprint entry"))
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
    pub map: OrdMap<Key, Option<ScVal>>,
}

impl Storage {
    pub fn with_enforcing_footprint_and_map(
        footprint: Footprint,
        map: OrdMap<Key, Option<ScVal>>,
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

    pub fn get(&mut self, key: &Key) -> Result<ScVal, HostError> {
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
            None => Err(HostError::General("missing key in get")),
            Some(None) => Err(HostError::General("get on deleted key")),
            Some(Some(val)) => Ok(val.clone()),
        }
    }

    fn put_opt(&mut self, key: &Key, val: Option<ScVal>) -> Result<(), HostError> {
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

    pub fn put(&mut self, key: &Key, val: &ScVal) -> Result<(), HostError> {
        self.put_opt(key, Some(val.clone()))
    }

    pub fn del(&mut self, key: &Key) -> Result<(), HostError> {
        self.put_opt(key, None)
    }

    pub fn has(&mut self, key: &Key) -> Result<bool, HostError> {
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
