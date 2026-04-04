//! This module contains the [Storage] type and its supporting types, which
//! provide the [Host](crate::Host) with access to durable ledger entries.
//!
//! For more details, see the [Env](crate::Env) data access functions:
//!   - [Env::has_contract_data](crate::Env::has_contract_data)
//!   - [Env::get_contract_data](crate::Env::get_contract_data)
//!   - [Env::put_contract_data](crate::Env::put_contract_data)
//!   - [Env::del_contract_data](crate::Env::del_contract_data)

use std::cell::RefCell;
use std::rc::Rc;

use crate::host::metered_clone::{
    charge_shallow_copy, MeteredAlloc, MeteredClone, MeteredIterator,
};
use crate::host::metered_hash::MeteredHashMap;
use crate::host::metered_map::MeteredOrdMap;
use crate::{
    ledger_info::get_key_durability,
    xdr::{
        ContractDataDurability, ContractDataEntry, LedgerEntry, LedgerEntryData, LedgerEntryExt,
        LedgerKey, LedgerKeyContractData, ScContractInstance, ScErrorCode, ScErrorType, ScVal,
    },
    Env, Host, HostError, Val,
};

/// Common state computed during TTL extension setup.
/// Populated by `Storage::prepare_ttl_extension`.
struct TtlExtensionContext {
    old_live_until: u32,
    current_ttl: u32,
    max_live_until: u32,
    /// max_ttl expressed as ledgers-from-now (max_live_until - ledger_seq).
    max_ttl: u32,
    ledger_seq: u32,
}

impl TtlExtensionContext {
    /// Maximum network-allowed extension from the current live_until.
    fn max_network_extension(&self) -> u32 {
        self.max_live_until.saturating_sub(self.old_live_until)
    }
}

/// Returns an `InvalidAction` error for attempting to extend a temporary
/// entry past the maximum TTL allowed by the network.
fn temp_entry_exceeds_max_ttl_error(host: &Host, extend_to: u32, max_ttl: u32) -> HostError {
    host.err(
        ScErrorType::Storage,
        ScErrorCode::InvalidAction,
        "trying to extend temporary entry past max TTL allowed by network: {} > {}",
        &[extend_to.into(), max_ttl.into()],
    )
}

/// Entry with optional live_until ledger for snapshot source compatibility.
/// Used by the SnapshotSource trait at the external interface boundary.
pub type EntryWithLiveUntil = (Rc<LedgerEntry>, Option<u32>);
/// Map from LedgerKey to optional LedgerEntry with live_until.
/// Used to track initial and final ledger state for computing ledger changes.
pub(crate) type LedgerEntryMap = MeteredHashMap<Rc<LedgerKey>, Option<EntryWithLiveUntil>>;

/// A helper type used to designate which ways a given [LedgerKey] is accessed,
/// or is allowed to be accessed, in a given transaction.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum AccessType {
    /// When in recording mode, indicates that the [LedgerKey] is only read.
    /// When in enforcing mode, indicates that the [LedgerKey] is only _allowed_ to be read.
    ReadOnly,
    /// When in recording mode, indicates that the [LedgerKey] is written (and also possibly read)
    /// When in enforcing mode, indicates that the [LedgerKey] is _allowed_ to be written (and also allowed to be read).
    ReadWrite,
}

/// Optimized representation of a ledger entry.
#[derive(Clone)]
pub(crate) enum StorageLedgerEntryData {
    /// Contract data entry value (besides contract instance).
    ContractData(Val),
    /// Other entry types: ContractCode, Account, Trustline, contract instance.
    /// Uses Rc<RefCell<_>> for in-place mutation and fast Rc cloning.
    Entry(Rc<RefCell<LedgerEntry>>),
}

/// A single frame's TTL modification for a storage entry.
/// Stored in a separate stack from entry values to allow TTL-only updates
/// without affecting entry data.
#[derive(Clone, Hash)]
pub(crate) struct EntryTTLFrame {
    /// The frame depth at which this TTL was set (0 = ledger state).
    pub(crate) depth: u32,
    /// The live_until ledger for at this depth, or None if entry does not exist.
    pub(crate) live_until: Option<u32>,
}

/// A single frame's modification of a storage entry value.
#[derive(Clone, Hash)]
pub(crate) struct StorageEntryFrame {
    /// The frame depth at which this value was set (0 = ledger state).
    pub(crate) depth: u32,
    /// The value at this depth, or None if entry does not exist.
    pub(crate) value: Option<StorageLedgerEntryData>,
}

/// The entry data portion of a [StorageEntry], split by access type.
#[derive(Clone, Hash)]
pub(crate) enum StorageEntryData {
    /// ReadOnly: single flat value that can't be modified.
    ReadOnly(Option<StorageLedgerEntryData>),
    /// ReadWrite: non-empty sparse stack of per-frame values.
    /// entry_stack[0] is initial state at depth 0.
    /// entry_stack.last() is always the current value.
    ReadWrite(Vec<StorageEntryFrame>),
}

impl StorageEntryData {
    /// Returns the access type for this entry data.
    pub(crate) fn access_type(&self) -> AccessType {
        match self {
            StorageEntryData::ReadOnly(_) => AccessType::ReadOnly,
            StorageEntryData::ReadWrite(_) => AccessType::ReadWrite,
        }
    }
}

/// The storage map type.
pub(crate) type StorageMap = MeteredHashMap<Rc<LedgerKey>, StorageEntry>;

/// A storage entry containing mutable entry data (split by access type) and a
/// mutable TTL for entries that have TTL.
///
/// Mutation of the entry is only permitted for entries from ReadWrite
/// footprint. TTL mutation is allowed for all the entries that have TTL,
/// regardless of the access type.
///
/// All the entry mutations are performed via a sparse stack of per-frame
/// values, which allows us to efficiently commit/rollback the entry changes at
/// frame boundaries without unnecessary cloning.
///
/// TTL mutations are performed in the same fashion via a separate sparse stack
/// of TTL frames, which allows us to update TTLs independently from entry data.
#[derive(Clone, Hash)]
pub(crate) struct StorageEntry {
    /// The entry data differentiated by the access type.
    pub(crate) data: StorageEntryData,
    /// Sparse stack of per-frame TTLs. Empty for entries without TTL.
    /// ttl_stack[0] is initial TTL at depth 0, ttl_stack.last() is current TTL.
    pub(crate) ttl_stack: Vec<EntryTTLFrame>,
}

impl StorageEntry {
    /// Creates a new StorageEntry with initial value at depth 0.
    pub(crate) fn new(
        access_type: AccessType,
        has_ttl: bool,
        value: Option<(StorageLedgerEntryData, Option<u32>)>,
    ) -> Self {
        let (entry_value, live_until) = match value {
            Some((data, live_until)) => (Some(data), live_until),
            None => (None, None),
        };
        let ttl_stack = if has_ttl {
            vec![EntryTTLFrame {
                depth: 0,
                live_until,
            }]
        } else {
            vec![]
        };
        let data = match access_type {
            AccessType::ReadOnly => StorageEntryData::ReadOnly(entry_value),
            AccessType::ReadWrite => StorageEntryData::ReadWrite(vec![StorageEntryFrame {
                depth: 0,
                value: entry_value,
            }]),
        };
        Self { data, ttl_stack }
    }

    /// Returns the access type for this entry.
    pub(crate) fn access_type(&self) -> AccessType {
        self.data.access_type()
    }

    /// Returns the current value of this entry.
    pub(crate) fn current_value(
        &self,
        host: &Host,
    ) -> Result<Option<&StorageLedgerEntryData>, HostError> {
        match &self.data {
            StorageEntryData::ReadOnly(value) => Ok(value.as_ref()),
            StorageEntryData::ReadWrite(stack) => {
                let frame = stack
                    .last()
                    .ok_or_else(|| host.empty_storage_stack_error())?;
                Ok(frame.value.as_ref())
            }
        }
    }

    /// Sets the base frame (depth 0) value for an existing entry.
    /// This should only be used for initializing storage entries from ledger
    /// data, as that's a two step process: we create the storage map from the
    /// footprint first, then populate it with the provided entries.
    pub(crate) fn set_base_frame_value(
        &mut self,
        data: StorageLedgerEntryData,
        live_until: Option<u32>,
        host: &Host,
    ) -> Result<(), HostError> {
        match &mut self.data {
            StorageEntryData::ReadOnly(value) => {
                if value.is_some() {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "setting base frame value for non-None ReadOnly entry",
                        &[],
                    ));
                }
                *value = Some(data);
            }
            StorageEntryData::ReadWrite(stack) => {
                if stack.len() != 1 {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "stack doesn't have exactly one frame during set_base_frame_value",
                        &[],
                    ));
                }
                if let Some(frame) = stack.first_mut() {
                    frame.value = Some(data);
                }
            }
        }
        if let Some(lu) = live_until {
            if self.ttl_stack.len() != 1 {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "ttl_stack doesn't have exactly one frame during set_base_frame_value",
                    &[],
                ));
            }
            let ttl_frame = self.ttl_stack.first_mut().ok_or_else(|| {
                host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "cannot set live_until on non-TTL entry",
                    &[],
                )
            })?;
            ttl_frame.live_until = Some(lu);
        }
        Ok(())
    }

    /// Returns the current TTL value, or None if entry has no TTL or is deleted.
    pub(crate) fn current_ttl(&self) -> Option<u32> {
        self.ttl_stack.last().and_then(|f| f.live_until)
    }

    /// Updates the TTL value of this entry.
    ///
    /// If the current top of ttl_stack is at the same depth, modifies in-place.
    /// Otherwise, pushes a new frame.
    fn update_ttl(&mut self, live_until: Option<u32>, host: &Host) -> Result<(), HostError> {
        let depth = host.current_frame_depth()?;
        let curr = self.ttl_stack.last_mut().ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "ttl_stack unexpectedly empty during update_ttl",
                &[],
            )
        })?;
        // Same depth: modify in-place
        if curr.depth == depth {
            curr.live_until = live_until;
            return Ok(());
        }
        if depth <= curr.depth {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "ttl_stack depth invariant violated: new depth not greater than current",
                &[depth.into(), curr.depth.into()],
            ));
        }
        // Different depth: push new frame
        charge_shallow_copy::<EntryTTLFrame>(1, host)?;
        self.ttl_stack.push(EntryTTLFrame { depth, live_until });
        Ok(())
    }

    /// Adds a TTL value, returning an error if a TTL already exists.
    ///
    /// Similar to `update_ttl`, but strictly for creating new entries with TTL.
    fn create_ttl(&mut self, live_until: u32, host: &Host) -> Result<(), HostError> {
        if self.current_ttl().is_some() {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "TTL already exists in create_ttl",
                &[],
            ));
        }
        self.update_ttl(Some(live_until), host)
    }

    /// Helper for operating on the entry stack at the current frame depth.
    ///
    /// Ensures a frame exists at the current depth, then calls `f` on it.
    ///
    /// - If the top frame is already at the current depth, calls `f` directly.
    /// - Otherwise, enforces the depth invariant (new depth > current), pushes
    ///   a new frame, then calls `f` on the new frame.
    ///
    /// When `entry_is_replaced` is true the new frame is initialized with
    /// `None` to avoid unnecessary cloning. Otherwise, the new frame is
    /// initialized with a deep clone of the current value to preserve existing
    /// data for modification.
    ///
    /// Only valid for ReadWrite entries.
    fn with_entry_frame_at_current_depth<F, R>(
        &mut self,
        host: &Host,
        entry_is_replaced: bool,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(&mut StorageEntryFrame) -> Result<R, HostError>,
    {
        let stack = match &mut self.data {
            StorageEntryData::ReadOnly(_) => {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "cannot modify ReadOnly entry",
                    &[],
                ));
            }
            StorageEntryData::ReadWrite(stack) => stack,
        };
        let depth = host.current_frame_depth()?;

        // Determine whether we need to push a new frame.
        let new_value = {
            let entry = stack
                .last()
                .ok_or_else(|| host.empty_storage_stack_error())?;
            if entry.depth == depth {
                // Same depth: will modify in-place below.
                None
            } else {
                if depth <= entry.depth {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "entry stack depth invariant violated: new depth not greater than current",
                        &[depth.into(), entry.depth.into()],
                    ));
                }
                if entry_is_replaced {
                    Some(None)
                } else {
                    Some(match &entry.value {
                        Some(StorageLedgerEntryData::ContractData(val)) => {
                            Some(StorageLedgerEntryData::ContractData(*val))
                        }
                        Some(StorageLedgerEntryData::Entry(entry_rc)) => {
                            let cloned = entry_rc.borrow().metered_clone(host)?;
                            Some(StorageLedgerEntryData::Entry(Rc::metered_new(
                                RefCell::new(cloned),
                                host,
                            )?))
                        }
                        None => None,
                    })
                }
            }
        };

        if let Some(value) = new_value {
            // Different depth: push new frame
            charge_shallow_copy::<StorageEntryFrame>(1, host)?;
            stack.push(StorageEntryFrame { depth, value });
        }

        // Whether we pushed a new frame, or modify the existing one, the top
        // frame is now at current depth.
        let frame = stack
            .last_mut()
            .ok_or_else(|| host.empty_storage_stack_error())?;
        f(frame)
    }

    /// Updates the full value of this entry.
    ///
    /// This should be used for inserting new entries or deleting existing ones
    /// (by passing None). For modifying the existing entries, use modify_entry
    /// instead, which is more efficient for updating parts of the ledger
    /// entries without unnecessary clones.
    fn update_entry(
        &mut self,
        value: Option<StorageLedgerEntryData>,
        host: &Host,
    ) -> Result<(), HostError> {
        self.with_entry_frame_at_current_depth(host, true, |frame| {
            frame.value = value;
            Ok(())
        })
    }

    /// Creates a new entry.
    ///
    /// Similar to `update_entry`, but strictly for creating new entries.
    fn create_entry(
        &mut self,
        value: StorageLedgerEntryData,
        host: &Host,
    ) -> Result<(), HostError> {
        self.with_entry_frame_at_current_depth(host, true, |frame| {
            if frame.value.is_some() {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "entry already exists in create_entry",
                    &[],
                ));
            }
            frame.value = Some(value);
            Ok(())
        })
    }

    /// Modifies the current entry value at the current frame depth via a
    /// callback.
    fn modify_entry_at_current_depth<F, R>(&mut self, host: &Host, f: F) -> Result<R, HostError>
    where
        F: FnOnce(&mut StorageEntryFrame) -> Result<R, HostError>,
    {
        self.with_entry_frame_at_current_depth(host, false, f)
    }

    /// Sets contract data value for this entry.
    ///
    /// Important: this does not perform validation of the entry key, so only
    /// use this in code paths that ensure that this entry is indeed a contract
    /// data entry.
    fn set_contract_data(
        &mut self,
        val: Val,
        durability: ContractDataDurability,
        host: &Host,
    ) -> Result<(), HostError> {
        // Check if entry existed before in order to determine if TTL needs to
        // be created as well.
        let entry_existed = self.current_value(host)?.is_some();
        self.update_entry(Some(StorageLedgerEntryData::ContractData(val)), host)?;

        // Set TTL to default if entry has been created now.
        if !entry_existed {
            let default_live_until = host.get_min_live_until_ledger(durability)?;
            self.update_ttl(Some(default_live_until), host)?;
        }
        Ok(())
    }

    /// Modifies this LedgerEntry in-place via a callback.
    fn modify_entry<F, R>(&mut self, host: &Host, f: F) -> Result<R, HostError>
    where
        F: FnOnce(&mut LedgerEntry) -> Result<R, HostError>,
    {
        self.modify_entry_at_current_depth(host, |frame| match &mut frame.value {
            Some(StorageLedgerEntryData::Entry(entry_rc)) => {
                let mut borrowed = entry_rc.borrow_mut();
                f(&mut borrowed)
            }
            Some(StorageLedgerEntryData::ContractData(_)) => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "cannot modify ContractData as LedgerEntry",
                &[],
            )),
            None => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "cannot modify deleted entry",
                &[],
            )),
        })
    }

    /// Helper to commit or rollback a single stack at the frame boundary.
    /// Important: this should never return errors. This only returns Result to
    /// convey internal errors (i.e. invariant violations).
    fn commit_or_rollback_stack<T>(
        stack: &mut Vec<T>,
        get_depth: impl Fn(&T) -> u32,
        set_depth: impl Fn(&mut T, u32),
        rollback: bool,
        depth: u32,
        host: &Host,
        stack_name: &str,
    ) -> Result<(), HostError> {
        let curr_depth = get_depth(stack.last().ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                &format!("{} unexpectedly empty during commit/rollback", stack_name),
                &[],
            )
        })?);

        if curr_depth > depth {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                &format!("storage {} depth exceeds host frame depth", stack_name),
                &[curr_depth.into(), depth.into()],
            ));
        }

        if curr_depth != depth {
            // This stack wasn't modified at the current depth, nothing to do
            return Ok(());
        }

        // The top frame is at the current depth, so there must be a frame below it
        // (the base depth-0 frame at minimum). A single frame at the current depth
        // would mean depth==0 which is caught by the caller, or a missing base frame.
        if stack.len() < 2 {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                &format!("cannot commit/rollback {} with only base frame", stack_name),
                &[],
            ));
        }

        if rollback {
            // Rollback: pop the top frame to restore previous state.
            stack.pop();
        } else {
            // Commit: decrement the depth of the current frame.
            // depth > 0 is guaranteed by the caller (commit_or_rollback).
            let curr = stack.last_mut().ok_or_else(|| {
                host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    &format!("{} unexpectedly empty", stack_name),
                    &[],
                )
            })?;
            set_depth(curr, depth - 1);

            // After decrementing, check if we should merge with the frame below.
            let len = stack.len();
            let new_top_depth = get_depth(&stack[len - 1]);
            let below_depth = get_depth(&stack[len - 2]);
            if new_top_depth == below_depth {
                // Merge: the top value supersedes the one below.
                // Move the top frame down one slot, then truncate.
                // This avoids a push and cannot cause reallocation.
                stack.swap(len - 2, len - 1);
                stack.pop();
            }
        }
        Ok(())
    }

    /// Commits or rolls back changes at the current host frame depth.
    /// Important: this should never return errors. This only returns Result to
    /// convey internal errors (i.e. invariant violations).
    fn commit_or_rollback(
        &mut self,
        curr_depth: u32,
        rollback: bool,
        host: &Host,
    ) -> Result<(), HostError> {
        // Invariant: commit/rollback at depth 0 is meaningless -- we always
        // commit from a nested frame to its parent.
        if curr_depth == 0 {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "commit_or_rollback called at depth 0",
                &[],
            ));
        }

        // Commit/rollback entry stack only for ReadWrite entries.
        // ReadOnly entries have no entry stack to commit/rollback.
        if let StorageEntryData::ReadWrite(stack) = &mut self.data {
            Self::commit_or_rollback_stack(
                stack,
                |f| f.depth,
                |f, d| f.depth = d,
                rollback,
                curr_depth,
                host,
                "entry_stack",
            )?;
        }

        // Commit/rollback TTL stack only for TTL entries.
        // Non-TTL entries (Account, Trustline) have empty ttl_stack and are skipped.
        if !self.ttl_stack.is_empty() {
            Self::commit_or_rollback_stack(
                &mut self.ttl_stack,
                |f| f.depth,
                |f, d| f.depth = d,
                rollback,
                curr_depth,
                host,
                "ttl_stack",
            )?;
        }

        Ok(())
    }
}

#[cfg(any(test, feature = "recording_mode"))]
impl StorageEntry {
    /// Clears the base frame (depth 0) value
    /// This is only used for emulating temp entry expiration in recording mode.
    /// Sets entry value to None and resets the TTL base frame to None.
    ///
    /// Must only be called on entries with TTL.
    fn clear_base_frame(&mut self, host: &Host) -> Result<(), HostError> {
        match &mut self.data {
            StorageEntryData::ReadOnly(value) => {
                *value = None;
            }
            StorageEntryData::ReadWrite(stack) => {
                stack.truncate(1);
                if let Some(frame) = stack.first_mut() {
                    frame.value = None;
                }
            }
        }
        // Reset ttl_stack to single base frame with non-existent TTL.
        self.ttl_stack.truncate(1);
        let ttl_frame = self.ttl_stack.first_mut().ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "clear_base_frame called on non-TTL entry",
                &[],
            )
        })?;
        ttl_frame.live_until = None;
        Ok(())
    }

    /// Upgrades the entry from ReadOnly to ReadWrite. No-op for read-write entries.
    fn upgrade_to_read_write(&mut self) {
        if let StorageEntryData::ReadOnly(value) = &mut self.data {
            let taken = value.take();
            self.data = StorageEntryData::ReadWrite(vec![StorageEntryFrame {
                depth: 0,
                value: taken,
            }]);
        }
    }

    /// Updates the base frame (depth 0) TTL for auto-restore.
    ///
    /// Sets new TTL and upgrades access to ReadWrite.
    fn auto_restore_base_frame(
        &mut self,
        new_live_until: u32,
        host: &Host,
    ) -> Result<(), HostError> {
        if self.ttl_stack.len() != 1 {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                format!("ttl_stack doesn't have exactly one frame during auto_restore_base_frame: len={}", self.ttl_stack.len()).as_str(),
                &[],
            ));
        }
        self.upgrade_to_read_write();
        let ttl_frame = self.ttl_stack.first_mut().ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "auto_restore_base_frame called on non-TTL entry",
                &[],
            )
        })?;
        ttl_frame.live_until = Some(new_live_until);
        Ok(())
    }
}

#[cfg(any(test, feature = "testutils"))]
impl StorageEntry {
    /// Sets the access type of the entry, converting the internal variant.
    /// Used in fuzzing testutils for adjusting footprint access types in tests.
    /// TODO: This is only used for a single fuzzing code path, we should clean
    /// up the fuzzing code instead to re-create host instead of modifying the
    /// storage in-place.
    pub(crate) fn set_access_type(&mut self, access_type: AccessType) {
        if self.access_type() == access_type {
            return;
        }
        match access_type {
            AccessType::ReadOnly => {
                // Downgrade ReadWrite to ReadOnly: take current value from stack top.
                let value = match &self.data {
                    StorageEntryData::ReadWrite(stack) => {
                        stack.last().and_then(|f| f.value.clone())
                    }
                    StorageEntryData::ReadOnly(_) => return,
                };
                self.data = StorageEntryData::ReadOnly(value);
            }
            AccessType::ReadWrite => {
                // Upgrade ReadOnly to ReadWrite: wrap value into single-frame stack.
                let value = match &self.data {
                    StorageEntryData::ReadOnly(v) => v.clone(),
                    StorageEntryData::ReadWrite(_) => return,
                };
                self.data =
                    StorageEntryData::ReadWrite(vec![StorageEntryFrame { depth: 0, value }]);
            }
        }
    }

    /// Resets the entry to a specified value at depth 0.
    /// Used for nonce reset in fuzzing testutils.
    /// Clears stacks and sets initial state.
    /// TODO: This is only used for a single fuzzing code path, we should clean
    /// up the fuzzing code instead to re-create host instead of modifying the
    /// storage in-place.
    pub(crate) fn reset_to_none_at_depth_zero(&mut self) {
        match &mut self.data {
            StorageEntryData::ReadOnly(value) => {
                *value = None;
            }
            StorageEntryData::ReadWrite(stack) => {
                stack.clear();
                stack.push(StorageEntryFrame {
                    depth: 0,
                    value: None,
                });
            }
        }
        // Preserve TTL/non-TTL status: if entry had TTL, reset to base frame;
        // if entry had no TTL, keep empty.
        if !self.ttl_stack.is_empty() {
            self.ttl_stack.clear();
            self.ttl_stack.push(EntryTTLFrame {
                depth: 0,
                live_until: None,
            });
        }
    }
}

/// Unmetered hash of StorageLedgerEntryData is only used for observation tracing.
impl std::hash::Hash for StorageLedgerEntryData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            StorageLedgerEntryData::ContractData(val) => val.hash(state),
            StorageLedgerEntryData::Entry(entry) => entry.borrow().hash(state),
        }
    }
}

impl StorageLedgerEntryData {
    /// Creates a StorageLedgerEntryData from a LedgerEntry.
    pub(crate) fn from_ledger_entry(entry: &LedgerEntry, host: &Host) -> Result<Self, HostError> {
        match &entry.data {
            LedgerEntryData::ContractData(contract_data) => {
                // Check if this is contract instance data - it is not
                // Val-representable because LedgerKeyContractInstance cannot be
                // converted to Val.
                if matches!(contract_data.val, ScVal::ContractInstance(_)) {
                    // Store as Entry - don't try to convert to Val
                    return Ok(StorageLedgerEntryData::Entry(Rc::metered_new(
                        RefCell::new(entry.metered_clone(host)?),
                        host,
                    )?));
                }

                // The ContractData(Val) variant drops ext fields during
                // to_ledger_entry reconstruction, so we must ensure they are V0
                // to avoid silent data loss if non-V0 extensions are introduced.
                if !matches!(contract_data.ext, crate::xdr::ExtensionPoint::V0) {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "unexpected non-V0 ContractDataEntry ext",
                        &[],
                    ));
                }
                if !matches!(entry.ext, LedgerEntryExt::V0) {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "unexpected non-V0 LedgerEntry ext",
                        &[],
                    ));
                }

                // Convert ScVal to Val for efficient access
                let val = host.to_valid_host_val(&contract_data.val)?;
                Ok(StorageLedgerEntryData::ContractData(val))
            }
            _ => {
                // Other entry types are stored as Entry variant
                Ok(StorageLedgerEntryData::Entry(Rc::metered_new(
                    RefCell::new(entry.metered_clone(host)?),
                    host,
                )?))
            }
        }
    }

    /// Reconstructs a LedgerEntry from this StorageLedgerEntryData and its key.
    /// The key is necessary to convert the optimized ContractData entries.
    pub(crate) fn to_ledger_entry(
        &self,
        key: &LedgerKey,
        host: &Host,
    ) -> Result<Rc<LedgerEntry>, HostError> {
        match self {
            StorageLedgerEntryData::Entry(entry) => {
                let cloned = entry.borrow().metered_clone(host)?;
                Rc::metered_new(cloned, host)
            }
            StorageLedgerEntryData::ContractData(val) => {
                // Reconstruct the LedgerEntry from key and value
                if let LedgerKey::ContractData(LedgerKeyContractData {
                    contract,
                    key: data_key,
                    durability,
                }) = key
                {
                    let sc_val = host.from_host_val(*val)?;
                    let entry = LedgerEntry {
                        last_modified_ledger_seq: 0,
                        data: LedgerEntryData::ContractData(ContractDataEntry {
                            ext: crate::xdr::ExtensionPoint::V0,
                            contract: contract.metered_clone(host)?,
                            key: data_key.metered_clone(host)?,
                            durability: *durability,
                            val: sc_val,
                        }),
                        ext: LedgerEntryExt::V0,
                    };
                    Rc::metered_new(entry, host)
                } else {
                    Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "ContractData storage entry has non-ContractData key",
                        &[],
                    ))
                }
            }
        }
    }
}

/// The in-memory instance storage of the current running contract. Initially
/// contains entries from the `ScMap` of the corresponding `ScContractInstance`
/// contract data entry.
#[derive(Clone, Hash)]
pub(crate) struct InstanceStorageMap {
    pub(crate) map: MeteredOrdMap<Val, Val, Host>,
    pub(crate) is_modified: bool,
}

impl InstanceStorageMap {
    pub(crate) fn from_map(map: Vec<(Val, Val)>, host: &Host) -> Result<Self, HostError> {
        Ok(Self {
            map: MeteredOrdMap::from_map(map, host)?,
            is_modified: false,
        })
    }

    pub(crate) fn from_instance_xdr(
        instance: &ScContractInstance,
        host: &Host,
    ) -> Result<Self, HostError> {
        Self::from_map(
            instance.storage.as_ref().map_or_else(
                || Ok(vec![]),
                |m| {
                    m.iter()
                        .map(|i| {
                            Ok((
                                host.to_valid_host_val(&i.key)?,
                                host.to_valid_host_val(&i.val)?,
                            ))
                        })
                        .metered_collect::<Result<Vec<(Val, Val)>, HostError>>(host)?
                },
            )?,
            host,
        )
    }
}

/// A helper type used by [FootprintMode::Recording] to provide access
/// to a stable read-snapshot of a ledger.
/// The snapshot is expected to have access to all the persistent entries,
/// including the archived entries. It also is allowed (but doesn't have to) to
/// return expired temporary entries.
pub trait SnapshotSource {
    /// Returns the ledger entry for the key and its live_until ledger if entry
    /// exists, or `None` otherwise.
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError>;
}

#[derive(Clone, Default)]
pub(crate) enum FootprintMode {
    #[cfg(any(test, feature = "recording_mode"))]
    Recording(Rc<dyn SnapshotSource>),
    #[default]
    Enforcing,
}

/// A special-purpose map from [LedgerKey]s to [LedgerEntry]s. Represents a
/// transactional batch of contract IO from and to durable storage, while
/// partitioning that IO between concurrently executing groups of contracts
/// through the use of access types stored in each [StorageEntry].
///
/// Access to each [LedgerKey] is mediated by the [FootprintMode],
/// which may be in either [FootprintMode::Recording] or
/// [FootprintMode::Enforcing] mode.
///
/// [FootprintMode::Recording] mode is used to calculate access types during
/// the contract execution simulation. Once calculated, the recorded access types
/// can be provided to "real" execution, which always runs in
/// [FootprintMode::Enforcing] mode and enforces partitioned access.
///
/// Every storage operation has footprint access type - either read-only or
/// read-write, which maps to read-only/read-write footprint respectively.
///
/// The access type is recorded in [FootprintMode::Recording] mode
/// and enforced in [FootprintMode::Enforcing] mode. In
/// [FootprintMode::Recording] mode, read-only access type will be promoted to
/// read-write on any read-write operation (whether it succeeds or fails). In
/// the [FootprintMode::Enforcing] mode violation of the access type (i.e.
/// accessing an entry outside of the footprint, or writing read-only entry)
/// leads to an non-recoverable error and full rollback.
///
/// Modifications to the storage map are implemented via mutating the
/// [StorageEntry]s in [StorageMap]. The entries track the per-frame
/// modifications, which allows for efficient commit/rollback at frame
/// boundaries.
#[derive(Default)]
pub struct Storage {
    /// The mode of the storage footprint enforcement, either recording or enforcing.
    pub(crate) mode: FootprintMode,
    /// The storage map containing the entries with access-guarded
    /// modifications and TTLs.
    pub(crate) map: StorageMap,
}

impl Storage {
    /// Only a subset of Stellar's XDR ledger key or entry types are supported
    /// by Soroban: accounts, trustlines, contract code and data. The rest are
    /// never used by stellar-core when interacting with the Soroban host, nor
    /// does the Soroban host ever generate any. Therefore the storage system
    /// will reject them with [ScErrorCode::InternalError] if they ever occur.
    pub fn check_supported_ledger_entry_type(le: &LedgerEntry) -> Result<(), HostError> {
        use crate::xdr::LedgerEntryData::*;
        match le.data {
            Account(_) | Trustline(_) | ContractData(_) | ContractCode(_) => Ok(()),
            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
        }
    }

    /// Only a subset of Stellar's XDR ledger key or entry types are supported
    /// by Soroban: accounts, trustlines, contract code and data. The rest are
    /// never used by stellar-core when interacting with the Soroban host, nor
    /// does the Soroban host ever generate any. Therefore the storage system
    /// will reject them with [ScErrorCode::InternalError] if they ever occur.
    pub fn check_supported_ledger_key_type(lk: &LedgerKey) -> Result<(), HostError> {
        use LedgerKey::*;
        match lk {
            Account(_) | Trustline(_) | ContractData(_) | ContractCode(_) => Ok(()),
            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Storage, ScErrorCode::InternalError).into()),
        }
    }

    /// Constructs a new [Storage] in [FootprintMode::Enforcing] using a
    /// pre-populated storage map.
    pub(crate) fn with_enforcing_footprint_and_map(map: StorageMap) -> Self {
        Self {
            mode: FootprintMode::Enforcing,
            map,
        }
    }

    /// Consumes the [Storage] and converts it into a [LedgerEntryMap] containing
    /// the final state of all entries.
    ///
    /// This must only be called once after contract execution is complete.
    pub(crate) fn into_final_entry_map(self, host: &Host) -> Result<LedgerEntryMap, HostError> {
        let mut result = LedgerEntryMap::new();
        for (key, storage_entry) in self.map.into_iter_non_metered() {
            let final_value = match &storage_entry.data {
                StorageEntryData::ReadOnly(value) => value.as_ref(),
                StorageEntryData::ReadWrite(stack) => {
                    if stack.len() != 1 || stack[0].depth != 0 {
                        return Err(host.err(
                            ScErrorType::Storage,
                            ScErrorCode::InternalError,
                            "storage entry has leftover entry_stack frames at finalization",
                            &[],
                        ));
                    }
                    stack[0].value.as_ref()
                }
            };

            if !storage_entry.ttl_stack.is_empty()
                && (storage_entry.ttl_stack.len() != 1 || storage_entry.ttl_stack[0].depth != 0)
            {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "storage entry has leftover ttl_stack frames at finalization",
                    &[],
                ));
            }
            let entry_with_live_until = match final_value {
                Some(entry_data) => {
                    let ledger_entry = entry_data.to_ledger_entry(key.as_ref(), host)?;
                    let live_until = storage_entry.current_ttl();
                    Some((ledger_entry, live_until))
                }
                None => None,
            };
            result.insert(key, entry_with_live_until, host.budget_ref())?;
        }
        Ok(result)
    }

    /// Mode-agnostic entry access helper for read operations.
    ///
    /// Handles key validation, recording-mode lazy loading, and expiration.
    ///
    /// Calls the callback with `Some((&StorageLedgerEntryData, Option<u32>))` if
    /// the entry exists, or `None` for deleted/non-existent entries.
    ///
    /// This is the single `self.map.get` call site for reads (besides test-only
    /// `get_from_map`).
    ///
    /// Returns whatever the callback returns.
    ///
    /// Footprint access type: read-only.
    fn with_entry_for_read<F, R>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(Option<(&StorageLedgerEntryData, Option<u32>)>) -> Result<R, HostError>,
    {
        let _span = tracy_span!("storage get");
        Self::check_supported_ledger_key_type(key)?;
        // In recording mode, this loads from snapshot and handles expiration.
        #[cfg(any(test, feature = "recording_mode"))]
        self.maybe_prepare_recording_mode_access(key, host)?;
        // Single lookup path for both modes
        match self.map.get(key, host.budget_ref())? {
            None => {
                // Key not in map means it wasn't in the footprint
                Err(host.storage_error_exceeds_footprint(key, key_val))
            }
            Some(storage_entry) => match storage_entry.current_value(host)? {
                Some(data) => f(Some((data, storage_entry.current_ttl()))),
                None => f(None),
            },
        }
    }

    /// Executes a callback with the ContractData Val for the given key.
    ///
    /// This may only be called for the contract data keys that are expected to
    /// be Val-representable (i.e. not contract instance).
    ///
    /// Calls the callback with `Some(Val)` if the contract data entry exists,
    /// or `None` for deleted/non-existent entries.
    ///
    /// Returns whatever the callback returns.
    ///
    /// Footprint access type: read-only.
    pub(crate) fn with_contract_data_val<F, R>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(Option<Val>) -> Result<R, HostError>,
    {
        self.with_entry_for_read(key, host, key_val, |opt| match opt {
            Some((StorageLedgerEntryData::ContractData(val), _)) => f(Some(*val)),
            Some((StorageLedgerEntryData::Entry(_), _)) => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "expected ContractData, got Entry in with_contract_data_val",
                &[],
            )),
            None => f(None),
        })
    }

    /// Executes a callback with a reference to the LedgerEntry for the given
    /// key.
    ///
    /// This may only be called for the ledger keys that are expected to be
    /// stored as Entry variant (i.e. contract instance data or non-contract
    /// data entries).
    ///
    /// Calls the callback with `Some(&LedgerEntry)` if the ledger entry exists,
    /// or `None` for deleted/non-existent entries.
    ///
    /// Returns whatever the callback returns.
    ///
    /// Footprint access type: read-only.
    pub(crate) fn with_ledger_entry<F, R>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(Option<&LedgerEntry>) -> Result<R, HostError>,
    {
        self.with_entry_for_read(key, host, None, |opt| match opt {
            Some((StorageLedgerEntryData::Entry(entry_rc), _)) => f(Some(&entry_rc.borrow())),
            Some((StorageLedgerEntryData::ContractData(_), _)) => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "expected Entry, got ContractData",
                &[],
            )),
            None => f(None),
        })
    }

    /// Mode-agnostic entry access helper for write operations.
    ///
    /// Handles key validation, expired entry handling, mode dispatch, and access
    /// checking, then calls the callback with &mut [StorageEntry].
    ///
    /// Returns whatever the callback returns.
    ///
    /// Footprint access type: read-write.
    fn with_mut_entry<F, R>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(&mut StorageEntry) -> Result<R, HostError>,
    {
        Self::check_supported_ledger_key_type(key)?;

        // In recording mode, this loads from snapshot and handles expiration.
        #[cfg(any(test, feature = "recording_mode"))]
        self.maybe_prepare_recording_mode_access(key, host)?;

        // Single lookup for both modes
        let storage_entry = self
            .map
            .get_mut(key, host.budget_ref())?
            .ok_or_else(|| host.storage_error_exceeds_footprint(key, key_val))?;

        // In enforcing mode, check ReadWrite access.
        // In recording mode, upgrade to ReadWrite.
        match &self.mode {
            #[cfg(any(test, feature = "recording_mode"))]
            FootprintMode::Recording(_) => {
                storage_entry.upgrade_to_read_write();
            }
            FootprintMode::Enforcing => {
                if storage_entry.access_type() != AccessType::ReadWrite {
                    return Err(host.storage_error_readonly(key, key_val));
                }
            }
        }
        f(storage_entry)
    }

    /// Puts a ContractData [Val] into storage.
    ///
    /// Footprint access type: read-write.
    pub(crate) fn put_contract_data(
        &mut self,
        key: &Rc<LedgerKey>,
        val: Val,
        durability: ContractDataDurability,
        host: &Host,
        key_val: Val,
    ) -> Result<(), HostError> {
        self.with_mut_entry(key, host, Some(key_val), |storage_entry| {
            storage_entry.set_contract_data(val, durability, host)
        })
    }

    /// Executes a callback with a mutable reference to the [LedgerEntry] for
    /// the given key.
    ///
    /// This may only be called for the ledger keys that are expected to be
    /// stored as Entry variant (i.e. contract instance data or non-contract
    /// data entries).
    ///
    /// Calls the callback with `Some(&mut LedgerEntry)` if the ledger entry
    /// exists, or `None` for deleted/non-existent entries.
    ///
    /// Returns whatever the callback returns.
    ///
    /// Footprint access type: read-write.
    pub(crate) fn modify_ledger_entry<F, R>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(Option<&mut LedgerEntry>) -> Result<R, HostError>,
    {
        self.with_mut_entry(key, host, None, |storage_entry| {
            if storage_entry.current_value(host)?.is_none() {
                return f(None);
            }
            storage_entry.modify_entry(host, |entry| f(Some(entry)))
        })
    }

    /// Creates a new ledger entry in storage with an optional TTL.
    ///
    /// Entry must not exist before. To update an existing entry,
    /// use [Storage::modify_entry].
    ///
    /// Footprint access type: read-write.
    pub(crate) fn create_entry(
        &mut self,
        key: &Rc<LedgerKey>,
        entry: &Rc<LedgerEntry>,
        live_until: Option<u32>,
        host: &Host,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("storage create_entry");
        let data = StorageLedgerEntryData::from_ledger_entry(entry, host)?;
        self.with_mut_entry(key, host, None, |storage_entry| {
            storage_entry.create_entry(data, host)?;
            if let Some(lu) = live_until {
                storage_entry.create_ttl(lu, host)?;
            }
            Ok(())
        })
    }

    /// Modifies an existing [LedgerEntry] in-place, or creates it if missing.
    ///
    /// The callback is invoked with the `None` if the entry is missing, or
    /// `Some(&mut LedgerEntry)` if the entry exists. The callback must return
    /// the new entry when entry is missing, and `None`` if entry exists.
    ///
    /// Footprint access type: read-write.
    pub(crate) fn modify_or_create_entry<F>(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        modify_or_create: F,
    ) -> Result<(), HostError>
    where
        F: FnOnce(
            Option<&mut LedgerEntry>,
        ) -> Result<Option<(Rc<LedgerEntry>, Option<u32>)>, HostError>,
    {
        let _span = tracy_span!("storage modify_or_create_entry");
        self.with_mut_entry(key, host, None, |storage_entry| {
            match storage_entry.current_value(host)? {
                None => {
                    let (entry, live_until) = modify_or_create(None)?.ok_or_else(|| {
                        host.err(
                            ScErrorType::Storage,
                            ScErrorCode::InternalError,
                            "expected create result for missing entry",
                            &[],
                        )
                    })?;
                    let data_if_missing = StorageLedgerEntryData::from_ledger_entry(&entry, host)?;
                    storage_entry.create_entry(data_if_missing, host)?;
                    if let Some(lu) = live_until {
                        storage_entry.create_ttl(lu, host)?;
                    }
                    Ok(())
                }
                Some(StorageLedgerEntryData::Entry(_)) => {
                    storage_entry.modify_entry(host, |entry| {
                        if modify_or_create(Some(entry))?.is_some() {
                            return Err(host.err(
                                ScErrorType::Storage,
                                ScErrorCode::InternalError,
                                "unexpected create result for existing entry",
                                &[],
                            ));
                        }
                        Ok(())
                    })
                }
                Some(StorageLedgerEntryData::ContractData(_)) => {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "expected Entry, got ContractData",
                        &[],
                    ));
                }
            }
        })
    }

    /// Deletes a storage entry with the given key.
    ///
    /// Footprint access type: read-write.
    pub(crate) fn del(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("storage del");
        self.with_mut_entry(key, host, key_val, |storage_entry| {
            storage_entry.update_entry(None, host)?;
            if !storage_entry.ttl_stack.is_empty() {
                storage_entry.update_ttl(None, host)?;
            }
            Ok(())
        })
    }

    /// Determines the presence of a [LedgerEntry] associated with a
    /// given [LedgerKey] in the [Storage], returning `Ok(true)` if an entry
    /// with the key exists and `Ok(false)` if it does not.
    ///
    /// Footprint access type: read-only.
    pub(crate) fn has(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
    ) -> Result<bool, HostError> {
        let _span = tracy_span!("storage has");
        self.with_entry_for_read(key, host, key_val, |opt| Ok(opt.is_some()))
    }

    /// Common setup for TTL extension operations.
    ///
    /// Validates the key, loads it in recording mode, looks up the entry,
    /// checks it exists and is live, and returns a mutable reference to the
    /// storage entry along with the computed TTL context.
    fn prepare_ttl_extension<'a>(
        &'a mut self,
        host: &Host,
        key: &Rc<LedgerKey>,
        key_val: Option<Val>,
    ) -> Result<(&'a mut StorageEntry, TtlExtensionContext), HostError> {
        Self::check_supported_ledger_key_type(key)?;

        #[cfg(any(test, feature = "recording_mode"))]
        self.maybe_prepare_recording_mode_access(key, host)?;

        let budget = host.budget_ref();
        let max_live_until = host.max_live_until_ledger()?;

        let storage_entry = self
            .map
            .get_mut(key, budget)?
            .ok_or_else(|| host.storage_error_exceeds_footprint(key, key_val))?;

        if storage_entry.current_value(host)?.is_none() {
            return Err(host.storage_error_missing_value(key, key_val));
        }
        let old_live_until = storage_entry.current_ttl().ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trying to extend invalid entry",
                &[],
            )
        })?;

        let ledger_seq: u32 = host.get_ledger_sequence()?.into();

        if old_live_until < ledger_seq {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "accessing no-longer-live entry",
                &[old_live_until.into(), ledger_seq.into()],
            ));
        }

        let ctx = TtlExtensionContext {
            old_live_until,
            current_ttl: old_live_until.saturating_sub(ledger_seq),
            max_live_until,
            max_ttl: max_live_until.saturating_sub(ledger_seq),
            ledger_seq,
        };

        Ok((storage_entry, ctx))
    }

    /// Extends `key` to live `extend_to` ledgers from now (not counting the
    /// current ledger) if the current `live_until_ledger_seq` for the entry is
    /// `threshold` ledgers or less away from the current ledger.
    ///
    /// If attempting to extend an entry past `Host::max_live_until_ledger()`
    /// - if the entry is `Persistent`, the entry's new
    ///   `live_until_ledger_seq` is clamped to it.
    /// - if the entry is `Temporary`, returns error.
    ///
    /// This operation only modifies the TTL of the ledger entry, which is
    /// stored separately from the entry itself.
    ///
    /// Footprint access type: read-only.
    pub(crate) fn extend_ttl(
        &mut self,
        host: &Host,
        key: Rc<LedgerKey>,
        threshold: u32,
        extend_to: u32,
        key_val: Option<Val>,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("extend key");

        if threshold > extend_to {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "threshold must be <= extend_to",
                &[threshold.into(), extend_to.into()],
            ));
        }

        let (storage_entry, ctx) = self.prepare_ttl_extension(host, &key, key_val)?;

        // For temporary entries, extending past max TTL is an error.
        // For persistent entries, clamp to max.
        if extend_to > ctx.max_ttl {
            if let Some(durability) = get_key_durability(&key) {
                if matches!(durability, ContractDataDurability::Temporary) {
                    return Err(temp_entry_exceeds_max_ttl_error(
                        host,
                        extend_to,
                        ctx.max_ttl,
                    ));
                }
            }
        }

        let new_live_until = ctx
            .ledger_seq
            .checked_add(extend_to)
            .ok_or_else(|| {
                host.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "overflow computing new live_until in extend_ttl",
                    &[],
                )
            })?
            .min(ctx.max_live_until);

        if new_live_until > ctx.old_live_until && ctx.current_ttl <= threshold {
            storage_entry.update_ttl(Some(new_live_until), host)?;
        }
        Ok(())
    }

    /// Extends TTL of a ledger entry with min/max extension controls.
    ///
    /// The function extends the TTL to be up to `extend_to` ledgers from
    /// current ledger. The TTL extension only actually happens if the
    /// amount of extension ledgers is at least `min_extension`; otherwise this
    /// function is a no-op. The amount of extension ledgers will not exceed
    /// `max_extension` ledgers.
    ///
    /// If attempting to extend an entry past `Host::max_live_until_ledger()`
    /// - if the entry is `Persistent`, the entry's new
    ///   `live_until_ledger_seq` is clamped to it.
    /// - if the entry is `Temporary`, returns error.
    ///
    /// This operation only modifies the TTL of the ledger entry, which is
    /// stored separately from the entry itself.
    ///
    /// Footprint access type: read-only.
    pub(crate) fn extend_ttl_v2(
        &mut self,
        host: &Host,
        key: Rc<LedgerKey>,
        extend_to: u32,
        min_extension: u32,
        max_extension: u32,
        key_val: Option<Val>,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("extend key v2");

        if max_extension < min_extension {
            return Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "max_extension must be >= min_extension",
                &[max_extension.into(), min_extension.into()],
            ));
        }

        let (storage_entry, ctx) = self.prepare_ttl_extension(host, &key, key_val)?;

        // For temporary entries, extending past max TTL is an error.
        if let Some(durability) = get_key_durability(&key) {
            if matches!(durability, ContractDataDurability::Temporary) && extend_to > ctx.max_ttl {
                return Err(temp_entry_exceeds_max_ttl_error(
                    host,
                    extend_to,
                    ctx.max_ttl,
                ));
            }
        }

        // Compute the initial TTL extension and clamp to not exceed
        // network/argument limits.
        let ttl_ext_init = extend_to.saturating_sub(ctx.current_ttl);
        let ttl_ext_final = ttl_ext_init
            .min(max_extension)
            .min(ctx.max_network_extension());

        // If final extension < min_extension, return without changes.
        if ttl_ext_final < min_extension {
            return Ok(());
        }

        let new_live_until = ctx
            .old_live_until
            .checked_add(ttl_ext_final)
            .ok_or_else(|| {
                // Overflowing here means a misconfiguration of the network (the
                // TTL is too large), in which case we immediately flag it as an
                // unrecoverable `InternalError`, even though the source is
                // external to the host.
                host.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "overflow computing new live_until in extend_ttl_v2",
                    &[],
                )
            })?;

        if new_live_until > ctx.old_live_until {
            storage_entry.update_ttl(Some(new_live_until), host)?;
        }
        Ok(())
    }

    /// Commits or rolls back all storage entries modified at the current depth.    
    pub(crate) fn commit_or_rollback_frame(
        &mut self,
        rollback: bool,
        host: &Host,
    ) -> Result<(), HostError> {
        // We intentionally don't meter `commit_or_rollback` in any way. It is
        // generally cheap, bounded by the size of the storage (which is
        // limited by the network config) times the number of frames (and
        // creating a frame is expensive and metered).
        // Metering rollbacks would be problematic because we're already on the
        // error path, so failure here would leave storage in an inconsistent
        // state. Metering commits is possible, but the cost is inconsequential(
        // a couple of pointer copies/moves per entry).
        let depth = host.current_frame_depth()?;
        // Iterate over all entries and commit/rollback those at current depth.
        // The iteration order here is non-deterministic, but that should be
        // fine given that commit_or_rollback may only fail due to an invariant
        // failure (internal error), at which point it doesn't matter which
        // entry we hit first.
        for (_, entry) in self.map.iter_mut_unmetered() {
            let res = entry.commit_or_rollback(depth, rollback, host);
            // As an additional check, if commit_or_rollback returns an error,
            // we verify that it's an internal error, and promote it to internal
            // error if not. This way we can be sure that if this ever fails
            // with a different error due to e.g. a bug in the commit_or_rollback
            // logic, we won't accidentally treat it as a recoverable error that
            // could somehow be observed and make non-deterministic iteration
            // order matter.
            if let Err(e) = res {
                if !e.error.is_code(ScErrorCode::InternalError) {
                    return Err(host.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "non-internal error in commit_or_rollback",
                        &[],
                    ));
                }
                return Err(e);
            }
        }
        Ok(())
    }
}

#[cfg(any(test, feature = "recording_mode"))]
impl Storage {
    /// Constructs a new [Storage] in [FootprintMode::Recording] using a
    /// given [SnapshotSource].
    pub(crate) fn with_recording_footprint(src: Rc<dyn SnapshotSource>) -> Self {
        Self {
            mode: FootprintMode::Recording(src),
            map: Default::default(),
        }
    }

    /// Returns the recorded footprint as a sorted vector of
    /// (key, access_type) pairs: read_write keys first, then read_only.
    pub(crate) fn sorted_footprint_entries(&self) -> Vec<(Rc<LedgerKey>, AccessType)> {
        let mut rw_entries: Vec<(Rc<LedgerKey>, AccessType)> = Vec::new();
        let mut ro_entries: Vec<(Rc<LedgerKey>, AccessType)> = Vec::new();
        for (key, entry) in self.map.iter_non_metered() {
            let at = entry.access_type();
            match at {
                AccessType::ReadWrite => rw_entries.push((Rc::clone(key), at)),
                AccessType::ReadOnly => ro_entries.push((Rc::clone(key), at)),
            }
        }
        rw_entries.sort_by(|a, b| a.0.cmp(&b.0));
        ro_entries.sort_by(|a, b| a.0.cmp(&b.0));
        rw_entries.extend(ro_entries);
        rw_entries
    }

    /// Returns `true` if the key exists in the snapshot and is live w.r.t
    /// the current ledger sequence.
    pub(crate) fn is_key_live_in_snapshot(
        &self,
        host: &Host,
        key: &Rc<LedgerKey>,
    ) -> Result<bool, HostError> {
        match &self.mode {
            FootprintMode::Recording(snapshot) => {
                let snapshot_value = snapshot.get(key)?;
                if let Some((_, live_until_ledger)) = snapshot_value {
                    if let Some(live_until_ledger) = live_until_ledger {
                        let current_ledger_sequence =
                            host.with_ledger_info(|li| Ok(li.sequence_number))?;
                        Ok(live_until_ledger >= current_ledger_sequence)
                    } else {
                        // Non-Soroban entries are always live.
                        Ok(true)
                    }
                } else {
                    // Key is not in the snapshot.
                    Ok(false)
                }
            }
            FootprintMode::Enforcing => Err(host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "trying to get snapshot value in enforcing mode",
                &[],
            )),
        }
    }

    /// In recording mode: ensures the entry is in the storage map, handling
    /// snapshot loading and expiration logic.
    ///
    /// After this call, the entry will be in the map with its final state:
    /// - Expired temporary entries: None value, no TTL
    /// - Expired persistent entries: original value, auto-restored TTL, ReadWrite access
    /// - Live entries: original value and TTL
    /// - Non-existent entries: None value, no TTL
    fn maybe_prepare_recording_mode_access(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
    ) -> Result<(), HostError> {
        let src = match &self.mode {
            FootprintMode::Recording(src) => Rc::clone(src),
            FootprintMode::Enforcing => return Ok(()),
        };

        // Handle expiration for the existing entries.
        if let Some(storage_entry) = self.map.get_mut(key, host.budget_ref())? {
            return Self::handle_expiration(storage_entry, key, host);
        }

        // Load missing entries from snapshot, then handle expiration.
        let storage_entry = self.load_entry_from_snapshot(key, &src, host)?;
        Self::handle_expiration(storage_entry, key, host)
    }

    /// Handles expiration logic for an entry in the storage map.
    ///
    /// Temporary entries are treated as deleted upon expiration, while
    /// persistent entries are auto-restored with a new TTL.
    fn handle_expiration(
        storage_entry: &mut StorageEntry,
        key: &Rc<LedgerKey>,
        host: &Host,
    ) -> Result<(), HostError> {
        let Some(durability) = get_key_durability(key.as_ref()) else {
            // Entry without durability is never expired, so nothing to do
            return Ok(());
        };

        let Some(live_until) = storage_entry.current_ttl() else {
            // Entry does not exist, so nothing to do
            return Ok(());
        };

        // Capture the entry's current depth before the closure
        let entry_depth = match &storage_entry.data {
            StorageEntryData::ReadOnly(e) => e.as_ref().map(|_| 0),
            StorageEntryData::ReadWrite(stack) => stack.last().map(|f| f.depth),
        }
        .ok_or_else(|| {
            host.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "storage entry does not exist but has TTL",
                &[],
            )
        })?;

        host.with_ledger_info(|li| {
            if live_until >= li.sequence_number {
                // Entry is still live, nothing to do
                return Ok(());
            }

            // Entry has expired - verify entry is at its base state (only
            // depth-0 frames).
            // If the entry has nested frames, something is wrong - the ledger
            // sequence shouldn't advance while an entry has uncommitted
            // modifications.
            if entry_depth != 0 {
                return Err(host.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "entry expiration cannot occur with uncommitted modifications",
                    &[],
                ));
            }

            match durability {
                ContractDataDurability::Temporary => {
                    // Treat temp entries as deleted - clear the base frame
                    storage_entry.clear_base_frame(host)?;
                }
                ContractDataDurability::Persistent => {
                    // Auto-restore persistent entries: update base frame with
                    // new TTL and promote to RW.
                    let new_live_until = li
                        .min_live_until_ledger_checked(ContractDataDurability::Persistent)
                        .ok_or_else(|| {
                            host.err(
                                ScErrorType::Storage,
                                ScErrorCode::InternalError,
                                "persistent entry TTL overflow, ledger is mis-configured",
                                &[],
                            )
                        })?;
                    storage_entry.auto_restore_base_frame(new_live_until, host)?;
                }
            }
            Ok(())
        })
    }

    /// Loads an entry from snapshot and inserts it into the storage map.
    ///
    /// The entry is loaded as-is from the snapshot without expiration handling;
    /// expiration is handled separately by `handle_expiration`.
    fn load_entry_from_snapshot(
        &mut self,
        key: &Rc<LedgerKey>,
        src: &Rc<dyn SnapshotSource>,
        host: &Host,
    ) -> Result<&mut StorageEntry, HostError> {
        let snapshot_value = src.get(key)?;
        let durability = get_key_durability(key.as_ref());
        let has_ttl = durability.is_some();

        let entry_value = if let Some((ledger_entry, live_until_opt)) = snapshot_value {
            let data = StorageLedgerEntryData::from_ledger_entry(&ledger_entry, host)?;
            Some((data, live_until_opt))
        } else {
            None
        };

        let storage_entry = StorageEntry::new(AccessType::ReadOnly, has_ttl, entry_value);
        self.map
            .insert_and_get_mut(Rc::clone(key), storage_entry, host.budget_ref())
    }
}

#[cfg(any(test, feature = "testutils"))]
impl Storage {
    /// Returns the live_until ledger for an entry, or None if the entry doesn't exist.
    /// Returns an error for out-of-footprint access or other storage errors.
    /// This is a test-only helper for TTL verification.
    pub(crate) fn get_live_until(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
    ) -> Result<Option<u32>, HostError> {
        self.with_entry_for_read(key, host, key_val, |opt| {
            Ok(opt.and_then(|(_, live_until)| live_until))
        })
    }

    /// Gets a ledger entry with its live_until for external interface use.
    ///
    /// This materializes ContractData entries back to LedgerEntry format.
    /// Use typed helpers (with_contract_data_val, with_ledger_entry) for internal code.
    pub(crate) fn try_get_full(
        &mut self,
        key: &Rc<LedgerKey>,
        host: &Host,
        key_val: Option<Val>,
    ) -> Result<Option<EntryWithLiveUntil>, HostError> {
        self.with_entry_for_read(key, host, key_val, |opt| match opt {
            Some((data, live_until)) => {
                let entry = data.to_ledger_entry(key, host)?;
                Ok(Some((entry, live_until)))
            }
            None => Ok(None),
        })
    }

    // Test-only helper for getting the storage entry directly from the storage map,
    // without the footprint management and autorestoration.
    // Returns (entry, live_until) tuple.
    pub(crate) fn get_from_map(
        &self,
        key: &Rc<LedgerKey>,
        host: &Host,
    ) -> Result<Option<(StorageLedgerEntryData, Option<u32>)>, HostError> {
        match self.map.get(key, host.budget_ref())? {
            Some(storage_entry) => match storage_entry.current_value(host)? {
                Some(entry) => Ok(Some((entry.clone(), storage_entry.current_ttl()))),
                None => Ok(None),
            },
            None => Ok(None),
        }
    }
}

/// Returns true if the ledger key represents a persistent entry
/// (persistent contract data or contract code).
#[cfg(any(test, feature = "recording_mode"))]
pub(crate) fn is_persistent_key(key: &LedgerKey) -> bool {
    match key {
        LedgerKey::ContractData(k) => {
            matches!(k.durability, ContractDataDurability::Persistent)
        }
        LedgerKey::ContractCode(_) => true,
        _ => false,
    }
}

/// Returns a human-readable string describing the type of ledger key for error messages.
fn get_key_type_string_for_error(lk: &LedgerKey) -> &'static str {
    match lk {
        LedgerKey::ContractData(cd) => match cd.key {
            ScVal::LedgerKeyContractInstance => "contract instance",
            ScVal::LedgerKeyNonce(_) => "nonce",
            _ => "contract data key",
        },
        LedgerKey::ContractCode(_) => "contract code",
        LedgerKey::Account(_) => "account",
        LedgerKey::Trustline(_) => "account trustline",
        // This shouldn't normally trigger, but it's safer to just return
        // a safe default instead of an error in case if new key types are
        // accessed.
        _ => "ledger key",
    }
}

// Helpers for creating various storage-related errors.
impl Host {
    fn empty_storage_stack_error(&self) -> HostError {
        self.err(
            ScErrorType::Storage,
            ScErrorCode::InternalError,
            "storage entry_stack is unexpectedly empty",
            &[],
        )
    }

    /// Creates an error for accessing a key outside of the footprint.
    fn storage_error_exceeds_footprint(&self, lk: &LedgerKey, key_val: Option<Val>) -> HostError {
        let args = if let Some(v) = key_val {
            vec![v]
        } else {
            vec![]
        };
        self.err(
            ScErrorType::Storage,
            ScErrorCode::ExceededLimit,
            &format!(
                "trying to access {} outside of the footprint",
                get_key_type_string_for_error(lk)
            ),
            args.as_slice(),
        )
    }

    /// Creates an error for trying to get a non-existing value.
    pub(crate) fn storage_error_missing_value(
        &self,
        lk: &LedgerKey,
        key_val: Option<Val>,
    ) -> HostError {
        let args = if let Some(v) = key_val {
            vec![v]
        } else {
            vec![]
        };
        self.err(
            ScErrorType::Storage,
            ScErrorCode::MissingValue,
            &format!(
                "trying to get non-existing value for {}",
                get_key_type_string_for_error(lk)
            ),
            args.as_slice(),
        )
    }

    /// Creates an error for trying to write to a read-only entry.
    fn storage_error_readonly(&self, lk: &LedgerKey, key_val: Option<Val>) -> HostError {
        let mut err = self.err(
            ScErrorType::Storage,
            ScErrorCode::ExceededLimit,
            "cannot write to read-only ledger key",
            &[],
        );
        // ExceededLimit is non-recoverable, so we can safely create new objects.
        // This runs in shadow budget mode.
        self.with_debug_mode_allowing_new_objects(
            || {
                let key_type_str = get_key_type_string_for_error(lk);
                let args = self
                    .get_args_for_storage_error(lk, key_val, true)
                    .unwrap_or_else(|_| vec![]);
                err = self.err(
                    ScErrorType::Storage,
                    ScErrorCode::ExceededLimit,
                    &format!("cannot write to read-only {}", key_type_str),
                    args.as_slice(),
                );
                Ok(())
            },
            true,
        );
        err
    }

    /// Helper to get error arguments based on ledger key type.
    fn get_args_for_storage_error(
        &self,
        lk: &LedgerKey,
        key_val: Option<Val>,
        can_create_new_objects: bool,
    ) -> Result<Vec<Val>, HostError> {
        let mut res = vec![];
        match lk {
            LedgerKey::ContractData(cd) => {
                if can_create_new_objects {
                    let address_val = self
                        .add_host_object(cd.contract.metered_clone(self.budget_ref())?)?
                        .into();
                    res.push(address_val);
                }
                match &cd.key {
                    ScVal::LedgerKeyContractInstance => (),
                    ScVal::LedgerKeyNonce(n) => {
                        if can_create_new_objects {
                            res.push(self.add_host_object(n.nonce)?.into());
                        }
                    }
                    _ => {
                        if let Some(key) = key_val {
                            res.push(key);
                        }
                    }
                }
            }
            LedgerKey::ContractCode(c) => {
                if can_create_new_objects {
                    res.push(
                        self.add_host_object(self.scbytes_from_hash(&c.hash)?)?
                            .into(),
                    );
                }
            }
            LedgerKey::Account(_) | LedgerKey::Trustline(_) => {
                if can_create_new_objects {
                    res.push(self.account_address_from_key(lk)?)
                }
            }
            // This shouldn't normally trigger, but it's safer to just return
            // a safe default instead of an error in case if new key types are
            // accessed.
            _ => (),
        };
        Ok(res)
    }
}
