#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use std::rc::Rc;

use sha2::{Digest, Sha256};
use soroban_env_common::{
    xdr::{
        AccountId, Asset, ContractCodeEntry, ContractDataEntry, ContractEvent, ContractEventBody,
        ContractEventType, ContractEventV0, ContractId, CreateContractArgs, ExtensionPoint, Hash,
        HashIdPreimage, HostFunction, HostFunctionType, InstallContractCodeArgs, Int128Parts,
        LedgerEntryData, LedgerKey, LedgerKeyContractCode, ScContractCode, ScHostContextErrorCode,
        ScHostFnErrorCode, ScHostObjErrorCode, ScHostStorageErrorCode, ScHostValErrorCode, ScMap,
        ScMapEntry, ScObject, ScStatusType, ScVal, ScVec, ThresholdIndexes,
    },
    Convert, InvokerType, Status, TryFromVal, TryIntoVal, VmCaller, VmCallerCheckedEnv,
};

use crate::budget::{AsBudget, Budget, CostType};
use crate::events::{DebugError, DebugEvent, Events};
use crate::storage::{Storage, StorageMap};

use crate::host_object::{HostMap, HostObject, HostObjectType, HostVec};
#[cfg(feature = "vm")]
use crate::SymbolStr;
#[cfg(feature = "vm")]
use crate::Vm;
use crate::{EnvBase, Object, RawVal, RawValConvertible, Symbol};

pub(crate) mod comparison;
mod conversion;
mod data_helper;
mod err_helper;
mod error;
pub(crate) mod metered_clone;
pub(crate) mod metered_map;
#[cfg(feature = "vm")]
mod metered_utils;
pub(crate) mod metered_vector;
pub(crate) mod metered_xdr;
mod validity;
pub use error::HostError;

use self::metered_clone::MeteredClone;
use self::metered_vector::MeteredVector;
use crate::Compare;

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
// Notes on metering: `RollbackPoint` are metered under Frame operations
#[derive(Clone)]
pub(crate) struct RollbackPoint {
    storage: StorageMap,
    objects: usize,
}

#[cfg(any(test, feature = "testutils"))]
pub trait ContractFunctionSet {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Option<RawVal>;
}

#[cfg(any(test, feature = "testutils"))]
#[derive(Debug, Clone)]
pub struct TestContractFrame {
    pub id: Hash,
    pub func: Symbol,
    panic: Rc<RefCell<Option<Status>>>,
}

#[cfg(any(test, feature = "testutils"))]
impl TestContractFrame {
    pub fn new(id: Hash, func: Symbol) -> Self {
        Self {
            id,
            func,
            panic: Rc::new(RefCell::new(None)),
        }
    }
}

/// Holds contextual information about a single invocation, either
/// a reference to a contract [`Vm`] or an enclosing [`HostFunction`]
/// invocation.
///
/// Frames are arranged into a stack in [`HostImpl::context`], and are pushed
/// with [`Host::push_frame`], which returns a [`FrameGuard`] that will
/// pop the frame on scope-exit.
///
/// Frames are also the units of (sub-)transactions: each frame captures
/// the host state when it is pushed, and the [`FrameGuard`] will either
/// commit or roll back that state when it pops the stack.
#[derive(Clone)]
pub(crate) enum Frame {
    #[cfg(feature = "vm")]
    ContractVM(Rc<Vm>, Symbol),
    HostFunction(HostFunctionType),
    Token(Hash, Symbol),
    #[cfg(any(test, feature = "testutils"))]
    TestContract(TestContractFrame),
}

/// Temporary helper for denoting a slice of guest memory, as formed by
/// various bytes operations.
#[cfg(feature = "vm")]
struct VmSlice {
    vm: Rc<Vm>,
    pos: u32,
    len: u32,
}

#[derive(Debug, Clone)]
pub struct LedgerInfo {
    pub protocol_version: u32,
    pub sequence_number: u32,
    pub timestamp: u64,
    pub network_passphrase: Vec<u8>,
    pub base_reserve: u32,
}

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    source_account: RefCell<Option<AccountId>>,
    ledger: RefCell<Option<LedgerInfo>>,
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    pub(crate) context: RefCell<Vec<Frame>>,
    // Note: budget is refcounted and is _not_ deep-cloned when you call HostImpl::deep_clone,
    // mainly because it's not really possible to achieve (the same budget is connected to many
    // metered sub-objects) but also because it's plausible that the person calling deep_clone
    // actually wants their clones to be metered by "the same" total budget
    pub(crate) budget: Budget,
    events: RefCell<Events>,
    // Note: we're not going to charge metering for testutils because it's out of the scope
    // of what users will be charged for in production -- it's scaffolding for testing a contract,
    // but shouldn't be charged to the contract itself (and will never be compiled-in to
    // production hosts)
    #[cfg(any(test, feature = "testutils"))]
    contracts: RefCell<std::collections::HashMap<Hash, Rc<dyn ContractFunctionSet>>>,
}
// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Default, Clone)]
pub struct Host(pub(crate) Rc<HostImpl>);

impl Debug for HostImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HostImpl(...)")
    }
}

impl Debug for Host {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Host({:x})", Rc::<HostImpl>::as_ptr(&self.0) as usize)
    }
}

impl Convert<&Object, ScObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: &Object) -> Result<ScObject, Self::Error> {
        self.from_host_obj(*ob)
    }
}

impl Convert<Object, ScObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: Object) -> Result<ScObject, Self::Error> {
        self.from_host_obj(ob)
    }
}

impl Convert<&ScObject, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: &ScObject) -> Result<Object, Self::Error> {
        self.to_host_obj(ob)
    }
}

impl Convert<ScObject, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: ScObject) -> Result<Object, Self::Error> {
        self.to_host_obj(&ob)
    }
}

impl Host {
    /// Constructs a new [`Host`] that will use the provided [`Storage`] for
    /// contract-data access functions such as
    /// [`CheckedEnv::get_contract_data`].
    pub fn with_storage_and_budget(storage: Storage, budget: Budget) -> Self {
        Self(Rc::new(HostImpl {
            source_account: RefCell::new(None),
            ledger: RefCell::new(None),
            objects: Default::default(),
            storage: RefCell::new(storage),
            context: Default::default(),
            budget,
            events: Default::default(),
            #[cfg(any(test, feature = "testutils"))]
            contracts: Default::default(),
        }))
    }

    pub fn set_source_account(&self, source_account: AccountId) {
        *self.0.source_account.borrow_mut() = Some(source_account);
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn remove_source_account(&self) {
        *self.0.source_account.borrow_mut() = None;
    }

    pub fn source_account(&self) -> Result<AccountId, HostError> {
        if let Some(account_id) = self.0.source_account.borrow().as_ref() {
            Ok(account_id.clone())
        } else {
            Err(self.err_general("invoker account is not configured"))
        }
    }

    pub fn set_ledger_info(&self, info: LedgerInfo) {
        *self.0.ledger.borrow_mut() = Some(info)
    }

    pub fn with_ledger_info<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&LedgerInfo) -> Result<T, HostError>,
    {
        match self.0.ledger.borrow().as_ref() {
            None => Err(self.err_general("missing ledger info")),
            Some(li) => f(li),
        }
    }

    pub fn with_mut_ledger_info<F>(&self, mut f: F) -> Result<(), HostError>
    where
        F: FnMut(&mut LedgerInfo),
    {
        match self.0.ledger.borrow_mut().as_mut() {
            None => Err(self.err_general("missing ledger info")),
            Some(li) => Ok(f(li)),
        }
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    pub fn with_budget<T, F>(&self, f: F) -> T
    where
        F: FnOnce(Budget) -> T,
    {
        f(self.0.budget.clone())
    }

    pub(crate) fn budget_ref(&self) -> &Budget {
        &self.0.budget
    }

    pub fn budget_cloned(&self) -> Budget {
        self.0.budget.clone()
    }

    pub fn charge_budget(&self, ty: CostType, input: u64) -> Result<(), HostError> {
        self.0.budget.clone().charge(ty, input)
    }

    pub(crate) fn get_events_mut<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Events) -> Result<U, HostError>,
    {
        f(&mut *self.0.events.borrow_mut())
    }

    /// Records a debug event. This in itself is not necessarily an error; it
    /// might just be some contextual event we want to put in a debug log for
    /// diagnostic purpopses. The return value from this is therefore () when
    /// the event is recorded successfully, even if the event itself
    /// _represented_ some other error. This function only returns Err(...) when
    /// there was a failure to record the event, such as when budget is
    /// exceeded.
    pub fn record_debug_event<T>(&self, src: T) -> Result<(), HostError>
    where
        DebugEvent: From<T>,
    {
        // We want to record an event _before_ we charge the budget, to maximize
        // the chance we return "what the contract was doing when it ran out of
        // gas" in cases it does. This does mean in that one case we'll exceed
        // the gas limit a tiny amount (one event-worth) but it's not something
        // users can harm us with nor does it observably effect the order the
        // contract runs out of gas in; this is an atomic action from the
        // contract's perspective.
        let event: DebugEvent = src.into();
        let len = self.get_events_mut(|events| Ok(events.record_debug_event(event)))?;
        self.charge_budget(CostType::HostEventDebug, len)
    }

    // Records a contract event.
    pub fn record_contract_event(
        &self,
        type_: ContractEventType,
        topics: ScVec,
        data: ScVal,
    ) -> Result<(), HostError> {
        let ce = ContractEvent {
            ext: ExtensionPoint::V0,
            contract_id: self.get_current_contract_id().ok(),
            type_,
            body: ContractEventBody::V0(ContractEventV0 { topics, data }),
        };
        self.get_events_mut(|events| Ok(events.record_contract_event(ce)))?;
        // Notes on metering: the length of topics and the complexity of data
        // have been covered by various `ValXdrConv` charges. Here we charge 1
        // unit just for recording this event.
        self.charge_budget(CostType::HostEventContract, 1)
    }

    pub fn with_mut_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Storage) -> Result<U, HostError>,
    {
        f(&mut *self.0.storage.borrow_mut())
    }

    /// Accept a _unique_ (refcount = 1) host reference and destroy the
    /// underlying [`HostImpl`], returning its constituent components to the
    /// caller as a tuple wrapped in `Ok(...)`. If the provided host reference
    /// is not unique, returns `Err(self)`.
    pub fn try_finish(self) -> Result<(Storage, Budget, Events), Self> {
        Rc::try_unwrap(self.0)
            .map(|host_impl| {
                let storage = host_impl.storage.into_inner();
                let budget = host_impl.budget;
                let events = host_impl.events.into_inner();
                (storage, budget, events)
            })
            .map_err(Host)
    }

    /// Helper function for [`Host::with_frame`] below. Pushes a new [`Frame`]
    /// on the context stack, returning a [`RollbackPoint`] such that if
    /// operation fails, it can be used to roll the [`Host`] back to the state
    /// it had before its associated [`Frame`] was pushed.
    fn push_frame(&self, frame: Frame) -> Result<RollbackPoint, HostError> {
        self.0.context.borrow_mut().push(frame);
        Ok(RollbackPoint {
            objects: self.0.objects.borrow().len(),
            storage: self.0.storage.borrow().map.clone(),
        })
    }

    /// Helper function for [`Host::with_frame`] below. Pops a [`Frame`] off
    /// the current context and optionally rolls back the [`Host`]'s objects
    /// and storage map to the state in the provided [`RollbackPoint`].
    fn pop_frame(&self, orp: Option<RollbackPoint>) -> Result<(), HostError> {
        self.0
            .context
            .borrow_mut()
            .pop()
            .expect("unmatched host frame push/pop");
        if let Some(rp) = orp {
            self.0.objects.borrow_mut().truncate(rp.objects);
            self.0.storage.borrow_mut().map = rp.storage;
        }
        Ok(())
    }

    /// Applies a function to the top [`Frame`] of the context stack. Returns
    /// [`HostError`] if the context stack is empty, otherwise returns result of
    /// function call.
    // Notes on metering: aquiring the current frame is cheap and not charged.
    /// Metering happens in the passed-in closure where actual work is being done.
    fn with_current_frame<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&Frame) -> Result<U, HostError>,
    {
        f(self
            .0
            .context
            .borrow()
            .last()
            .ok_or_else(|| self.err(DebugError::new(ScHostContextErrorCode::NoContractRunning)))?)
    }

    /// Same as [`Self::with_current_frame`] but passes `None` when there is no current
    /// frame, rather than logging an error.
    fn with_current_frame_opt<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(Option<&Frame>) -> Result<U, HostError>,
    {
        f(self.0.context.borrow().last())
    }

    /// Pushes a [`Frame`], runs a closure, and then pops the frame, rolling back
    /// if the closure returned an error. Returns the result that the closure
    /// returned (or any error caused during the frame push/pop).
    // Notes on metering: `GuardFrame` charges on the work done on protecting the `context`.
    // It does not cover the cost of the actual closure call. The closure needs to be
    // metered separately.
    pub(crate) fn with_frame<F>(&self, frame: Frame, f: F) -> Result<RawVal, HostError>
    where
        F: FnOnce() -> Result<RawVal, HostError>,
    {
        self.charge_budget(CostType::GuardFrame, 1)?;
        let start_depth = self.0.context.borrow().len();
        let rp = self.push_frame(frame)?;
        let res = f();
        let res = match res {
            Ok(v) if v.is::<Status>() => {
                let st: Status = v.try_into()?;
                if st.is_ok() {
                    // Transform Status::OK => Ok(())
                    Ok(RawVal::from_void())
                } else {
                    // All other Status::N => Err(N)
                    Err(self.err_status(st))
                }
            }
            res => res,
        };
        if res.is_err() {
            // Pop and rollback on error.
            self.pop_frame(Some(rp))?;
        } else {
            // Just pop on success.
            self.pop_frame(None)?;
        }
        // Every push and pop should be matched; if not there is a bug.
        let end_depth = self.0.context.borrow().len();
        assert_eq!(start_depth, end_depth);
        res
    }

    /// Pushes a test contract [`Frame`], runs a closure, and then pops the
    /// frame, rolling back if the closure returned an error. Returns the result
    /// that the closure returned (or any error caused during the frame
    /// push/pop). Used for testing.
    #[cfg(any(test, feature = "testutils"))]
    pub fn with_test_contract_frame<F>(
        &self,
        id: Hash,
        func: Symbol,
        f: F,
    ) -> Result<RawVal, HostError>
    where
        F: FnOnce() -> Result<RawVal, HostError>,
    {
        self.with_frame(Frame::TestContract(TestContractFrame::new(id, func)), f)
    }

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    fn get_current_contract_id(&self) -> Result<Hash, HostError> {
        self.with_current_frame(|frame| match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm, _) => vm.contract_id.metered_clone(&self.0.budget),
            Frame::HostFunction(_) => {
                Err(self.err_general("Host function context has no contract ID"))
            }
            Frame::Token(id, _) => id.metered_clone(&self.0.budget),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => Ok(tc.id.clone()),
        })
    }

    // Notes on metering: closure call needs to be metered separatedly. `VisitObject` only covers
    /// the cost of visiting an object.
    pub(crate) unsafe fn unchecked_visit_val_obj<F, U>(
        &self,
        val: RawVal,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(Option<&HostObject>) -> Result<U, HostError>,
    {
        self.charge_budget(CostType::VisitObject, 1)?;
        let r = self.0.objects.borrow();
        let index = <Object as RawValConvertible>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }

    // Notes on metering: object visiting part is covered by unchecked_visit_val_obj. Closure function
    /// needs to be metered separately.
    pub(crate) fn visit_obj<HOT: HostObjectType, F, U>(
        &self,
        obj: Object,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(&HOT) -> Result<U, HostError>,
    {
        unsafe {
            self.unchecked_visit_val_obj(obj.into(), |hopt| match hopt {
                None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(self.err_status(ScHostObjErrorCode::UnexpectedType)),
                    Some(hot) => f(hot),
                },
            })
        }
    }

    // Testing interface to create values directly for later use via Env functions.
    // Notes on metering: covered by `to_host_val`
    pub fn inject_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.to_host_val(v).map(Into::into)
    }

    pub fn get_events(&self) -> Result<Events, HostError> {
        self.0.events.borrow().metered_clone(&self.0.budget)
    }

    // Notes on metering: free
    #[cfg(feature = "vm")]
    fn decode_vmslice(&self, pos: RawVal, len: RawVal) -> Result<VmSlice, HostError> {
        let pos: u32 = self.u32_from_rawval_input("pos", pos)?;
        let len: u32 = self.u32_from_rawval_input("len", len)?;
        self.with_current_frame(|frame| match frame {
            Frame::ContractVM(vm, _) => {
                let vm = vm.clone();
                Ok(VmSlice { vm, pos, len })
            }
            _ => Err(self.err_general("attempt to access guest bytes in non-VM frame")),
        })
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        // Charges a single unit to for the RawVal -> ScVal conversion.
        // The actual conversion logic occurs in the `common` crate, which
        // translates a u64 into another form defined by the xdr.
        // For an `Object`, the actual structural conversion (such as byte
        // cloning) occurs in `from_host_obj` and is metered there.
        self.charge_budget(CostType::ValXdrConv, 1)?;
        ScVal::try_from_val(self, val)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.charge_budget(CostType::ValXdrConv, 1)?;
        v.try_into_val(self)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn from_host_obj(&self, ob: Object) -> Result<ScObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| {
                // This accounts for conversion of "primitive" objects (e.g U64)
                // and the "shell" of a complex object (ScMap). Any non-trivial
                // work such as byte cloning, has to be accounted for and
                // metered in indivial match arms.
                self.charge_budget(CostType::ValXdrConv, 1)?;
                match ob {
                    None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            // Here covers the cost of space allocating and maneuvering needed to go
                            // from one structure to the other. The actual conversion work (heavy lifting)
                            // is covered by `from_host_val`, which is recursive.
                            self.charge_budget(CostType::ScVecFromHostVec, vv.len() as u64)?;
                            let sv = vv.iter().map(|e| self.from_host_val(*e)).collect::<Result<
                                Vec<ScVal>,
                                HostError,
                            >>(
                            )?;
                            Ok(ScObject::Vec(ScVec(self.map_err(sv.try_into())?)))
                        }
                        HostObject::Map(mm) => {
                            // Here covers the cost of space allocating and maneuvering needed to go
                            // from one structure to the other. The actual conversion work (heavy lifting)
                            // is covered by `from_host_val`, which is recursive.
                            self.charge_budget(CostType::ScMapFromHostMap, mm.len() as u64)?;
                            let mut mv = Vec::new();
                            for (k, v) in mm.iter(self)? {
                                let key = self.from_host_val(*k)?;
                                let val = self.from_host_val(*v)?;
                                mv.push(ScMapEntry { key, val });
                            }
                            Ok(ScObject::Map(ScMap(self.map_err(mv.try_into())?)))
                        }
                        HostObject::U64(u) => Ok(ScObject::U64(*u)),
                        HostObject::I64(i) => Ok(ScObject::I64(*i)),
                        HostObject::U128(u) => Ok(ScObject::U128(Int128Parts {
                            lo: *u as u64,
                            hi: (*u >> 64) as u64,
                        })),
                        HostObject::I128(u) => {
                            let u = *u as u128;
                            Ok(ScObject::I128(Int128Parts {
                                lo: u as u64,
                                hi: (u >> 64) as u64,
                            }))
                        }
                        HostObject::Bytes(b) => Ok(ScObject::Bytes(
                            self.map_err(b.metered_clone(&self.0.budget)?.try_into())?,
                        )),
                        HostObject::ContractCode(cc) => {
                            Ok(ScObject::ContractCode(cc.metered_clone(&self.0.budget)?))
                        }
                        HostObject::AccountId(aid) => {
                            Ok(ScObject::AccountId(aid.metered_clone(&self.0.budget)?))
                        }
                    },
                }
            })
        }
    }

    pub(crate) fn to_host_obj(&self, ob: &ScObject) -> Result<Object, HostError> {
        self.charge_budget(CostType::ValXdrConv, 1)?;
        match ob {
            ScObject::Vec(v) => {
                self.charge_budget(CostType::ScVecToHostVec, v.len() as u64)?;
                let mut vv = Vec::with_capacity(v.len());
                for e in v.iter() {
                    vv.push(self.to_host_val(e)?)
                }
                self.add_host_object(HostVec::from_vec(vv)?)
            }
            ScObject::Map(m) => {
                self.charge_budget(CostType::ScMapToHostMap, m.len() as u64)?;
                let mut mm = Vec::with_capacity(m.len());
                for pair in m.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.push((k, v))
                }
                self.add_host_object(HostMap::from_map(mm, self)?)
            }
            ScObject::U64(u) => self.add_host_object(*u),
            ScObject::I64(i) => self.add_host_object(*i),
            ScObject::U128(u) => self.add_host_object(u.lo as u128 | ((u.hi as u128) << 64)),
            ScObject::I128(i) => {
                self.add_host_object((i.lo as u128 | ((i.hi as u128) << 64)) as i128)
            }
            ScObject::Bytes(b) => {
                self.add_host_object::<Vec<u8>>(b.as_vec().metered_clone(&self.0.budget)?.into())
            }
            ScObject::ContractCode(cc) => self.add_host_object(cc.metered_clone(&self.0.budget)?),
            ScObject::AccountId(account_id) => {
                self.add_host_object(account_id.metered_clone(&self.0.budget)?)
            }
        }
    }

    pub(crate) fn charge_for_new_host_object(
        &self,
        prev_len: usize,
        ho: HostObject,
    ) -> Result<HostObject, HostError> {
        self.charge_budget(CostType::HostObjAllocSlot, prev_len as u64)?;
        Ok(ho)
    }

    /// Moves a value of some type implementing [`HostObjectType`] into the host's
    /// object array, returning a [`HostObj`] containing the new object's array
    /// index, tagged with the [`xdr::ScObjectType`].
    // Notes on metering: new object is charged by `charge_for_new_host_object`. The
    // rest is free.
    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<Object, HostError> {
        let prev_len = self.0.objects.borrow().len();
        if prev_len > u32::MAX as usize {
            return Err(self.err_status(ScHostObjErrorCode::ObjectCountExceedsU32Max));
        }
        self.0
            .objects
            .borrow_mut()
            .push(self.charge_for_new_host_object(prev_len, HOT::inject(hot))?);
        let handle = prev_len as u32;
        Ok(Object::from_type_and_handle(HOT::get_type(), handle))
    }

    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: Object,
        contract_source: ScContractCode,
    ) -> Result<(), HostError> {
        let new_contract_id = self.hash_from_obj_input("id_obj", contract_id)?;
        let storage_key =
            self.contract_source_ledger_key(new_contract_id.metered_clone(&self.0.budget)?);
        if self
            .0
            .storage
            .borrow_mut()
            .has(&storage_key, self.as_budget())?
        {
            return Err(self.err_general("Contract already exists"));
        }
        // Make sure the contract code exists. With immutable contracts and
        // without this check it would be possible to accidentally create a
        // contract that never may be invoked (just by providing a bad hash).
        if let ScContractCode::WasmRef(wasm_hash) = &contract_source {
            let wasm_storage_key =
                self.contract_code_ledger_key(wasm_hash.metered_clone(&self.0.budget)?);
            if !self
                .0
                .storage
                .borrow_mut()
                .has(&wasm_storage_key, self.as_budget())?
            {
                return Err(self.err_general("Contract code was not installed"));
            }
        }
        self.store_contract_source(contract_source, new_contract_id, &storage_key)?;
        Ok(())
    }

    fn maybe_initialize_asset_token(
        &self,
        contract_id: Object,
        id_preimage: HashIdPreimage,
    ) -> Result<(), HostError> {
        if let HashIdPreimage::ContractIdFromAsset(asset_preimage) = id_preimage {
            let mut asset_bytes: Vec<u8> = Default::default();
            self.metered_write_xdr(&asset_preimage.asset, &mut asset_bytes)?;
            self.call_n(
                contract_id,
                Symbol::from_str("init_asset"),
                &[self.add_host_object(asset_bytes)?.into()],
                false,
            )?;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn create_contract_with_id_preimage(
        &self,
        contract_source: ScContractCode,
        id_preimage: HashIdPreimage,
    ) -> Result<Object, HostError> {
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        let id_obj = self.add_host_object(id_arr.to_vec())?;
        self.create_contract_with_id(id_obj, contract_source.metered_clone(self.budget_ref())?)?;
        self.maybe_initialize_asset_token(id_obj, id_preimage)?;
        Ok(id_obj)
    }

    pub fn get_contract_id_from_asset(&self, asset: Asset) -> Result<Object, HostError> {
        let id_preimage = self.id_preimage_from_asset(asset)?;
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        Ok(self.add_host_object(id_arr.to_vec())?)
    }

    // Notes on metering: this is covered by the called components.
    fn call_contract_fn(
        &self,
        id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        // Create key for storage
        let storage_key = self.contract_source_ledger_key(id.metered_clone(&self.0.budget)?);
        match self.retrieve_contract_source_from_storage(&storage_key)? {
            #[cfg(feature = "vm")]
            ScContractCode::WasmRef(wasm_hash) => {
                let code_entry = self.retrieve_contract_code_from_storage(wasm_hash)?;
                let vm = Vm::new(
                    self,
                    id.metered_clone(&self.0.budget)?,
                    code_entry.code.as_slice(),
                )?;
                vm.invoke_function_raw(self, SymbolStr::from(func).as_ref(), args)
            }
            #[cfg(not(feature = "vm"))]
            ScContractCode::WasmRef(_) => Err(self.err_general("could not dispatch")),
            ScContractCode::Token => {
                self.with_frame(Frame::Token(id.clone(), func.clone()), || {
                    use crate::native_contract::{NativeContract, Token};
                    Token.call(func, self, args)
                })
            }
        }
    }

    // Notes on metering: this is covered by the called components.
    fn call_n(
        &self,
        contract: Object,
        func: Symbol,
        args: &[RawVal],
        allow_reentry: bool,
    ) -> Result<RawVal, HostError> {
        // Get contract ID
        let id = self.hash_from_obj_input("contract", contract)?;

        if !allow_reentry {
            for f in self.0.context.borrow().iter() {
                let exist_id = match f {
                    #[cfg(feature = "vm")]
                    Frame::ContractVM(vm, _) => &vm.contract_id,
                    Frame::Token(id, _) => id,
                    #[cfg(any(test, feature = "testutils"))]
                    Frame::TestContract(tc) => &tc.id,
                    Frame::HostFunction(_) => continue,
                };
                if id == *exist_id {
                    return Err(self.err_status_msg(
                        // TODO: proper error code
                        ScHostContextErrorCode::UnknownError,
                        "Contract re-entry is not allowed",
                    ));
                }
            }
        }

        // "testutils" is not covered by budget metering.
        #[cfg(any(test, feature = "testutils"))]
        {
            // This looks a little un-idiomatic, but this avoids maintaining a borrow of
            // self.0.contracts. Implementing it as
            //
            //     if let Some(cfs) = self.0.contracts.borrow().get(&id).cloned() { ... }
            //
            // maintains a borrow of self.0.contracts, which can cause borrow errors.
            let cfs_option = self.0.contracts.borrow().get(&id).cloned();
            if let Some(cfs) = cfs_option {
                let frame = TestContractFrame::new(id.clone(), func.clone());
                let panic = frame.panic.clone();
                return self.with_frame(Frame::TestContract(frame), || {
                    use std::any::Any;
                    use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
                    use crate::xdr::ScUnknownErrorCode;
                    type PanicVal = Box<dyn Any + Send>;

                    // We're directly invoking a native rust contract here,
                    // which we allow only in local testing scenarios, and we
                    // want it to behave as close to the way it would behave if
                    // the contract were actually compiled to WASM and running
                    // in a VM.
                    //
                    // In particular: if the contract function panics, if it
                    // were WASM it would cause the VM to trap, so we do
                    // something "as similar as we can" in the native case here,
                    // catch the native panic and attempt to continue by
                    // translating the panic back to an error, so that
                    // `with_frame` will rollback the host to its pre-call state
                    // (as best it can) and propagate the error to its caller
                    // (which might be another contract doing try_call).
                    //
                    // This is somewhat best-effort, but it's compiled-out when
                    // building a host for production use, so we're willing to
                    // be a bit forgiving.
                    let closure = AssertUnwindSafe(move || cfs.call(&func, self, args));
                    let existing_panic_hook = take_hook();
                    set_hook(Box::new(|_| ()));
                    let res: Result<Option<RawVal>, PanicVal> = catch_unwind(closure);
                    set_hook(existing_panic_hook);
                    match res {
                        Ok(Some(rawval)) => Ok(rawval),
                        Ok(None) => Err(self.err(
                            DebugError::general()
                                .msg("error '{}': calling unknown contract function '{}'")
                                .arg::<RawVal>(func.into()),
                        )),
                        Err(panic_payload) => {
                            // Return an error indicating the contract function
                            // panicked. If if was a panic generated by a
                            // CheckedEnv-upgraded HostError, it had its status
                            // captured by VmCallerCheckedEnv::escalate_error_to_panic:
                            // fish the Status stored in the frame back out and
                            // propagate it.
                            let func: RawVal = func.into();
                            let mut status: Status = ScUnknownErrorCode::General.into();
                            let mut event = DebugEvent::new().msg("caught panic from contract function '{}'").arg(func);

                            if let Some(st) = *panic.borrow() {
                                status = st;
                                event = DebugEvent::new().msg("caught panic from contract function '{}', propagating escalated error '{}'").arg(func).arg(st.to_raw());
                            }
                            // If we're allowed to record dynamic strings (which happens in
                            // native test configurations), also log the panic payload into
                            // the .
                            else if cfg!(feature = "hostfn_log_fmt_values") {
                                if let Some(str) = panic_payload.downcast_ref::<&str>() {
                                    let msg: String = format!("caught panic '{}' from contract function '{:?}'", str, func);
                                    event = DebugEvent::new().msg(msg);
                                } else if let Some(str) = panic_payload.downcast_ref::<String>() {
                                    let msg: String = format!("caught panic '{}' from contract function '{:?}'", str, func);
                                    event = DebugEvent::new().msg(msg);
                                }
                            }
                            Err(self.err(DebugError{event, status}))
                        }
                    }
                });
            }
        }

        return self.call_contract_fn(&id, &func, args);
    }

    // Notes on metering: covered by the called components.
    fn invoke_function_raw(&self, hf: HostFunction) -> Result<RawVal, HostError> {
        let hf_type = hf.discriminant();
        //TODO: should the create_* methods below return a RawVal instead of Object to avoid this conversion?
        match hf {
            HostFunction::InvokeContract(args) => {
                if let [ScVal::Object(Some(scobj)), ScVal::Symbol(scsym), rest @ ..] =
                    args.as_slice()
                {
                    self.with_frame(Frame::HostFunction(hf_type), || {
                        // Metering: conversions to host objects are covered. Cost of collecting
                        // RawVals into Vec is ignored. Since 1. RawVals are cheap to clone 2. the
                        // max number of args is fairly limited.
                        let object = self.to_host_obj(scobj)?;
                        let symbol = <Symbol>::try_from(scsym)?;
                        let args = self.scvals_to_rawvals(rest)?;
                        // since the `HostFunction` frame must be the bottom of the call stack,
                        // reentry is irrelevant, we always pass in `allow_reentry = false`.
                        self.call_n(object, symbol, &args[..], false)
                    })
                } else {
                    Err(self.err_status_msg(
                        ScHostFnErrorCode::InputArgsWrongLength,
                        "unexpected arguments to 'call' host function",
                    ))
                }
            }
            HostFunction::CreateContract(args) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.create_contract(args).map(|obj| <RawVal>::from(obj))
                }),
            HostFunction::InstallContractCode(args) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.install_contract(args).map(|obj| <RawVal>::from(obj))
                }),
        }
    }

    // Notes on metering: covered by the called components.
    pub fn invoke_function(&self, hf: HostFunction) -> Result<ScVal, HostError> {
        let rv = self.invoke_function_raw(hf)?;
        self.from_host_val(rv)
    }

    // "testutils" is not covered by budget metering.
    #[cfg(any(test, feature = "testutils"))]
    pub fn register_test_contract(
        &self,
        contract_id: Object,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        let hash = self.hash_from_obj_input("contract_id", contract_id)?;
        let mut contracts = self.0.contracts.borrow_mut();
        if !contracts.contains_key(&hash) {
            contracts.insert(hash, contract_fns);
            Ok(())
        } else {
            Err(self.err_general("vtable already exists"))
        }
    }

    // Writes an arbitrary ledger entry to storage.
    // "testutils" is not covered by budget metering.
    #[cfg(any(test, feature = "testutils"))]
    pub fn add_ledger_entry(
        &self,
        key: LedgerKey,
        val: soroban_env_common::xdr::LedgerEntry,
    ) -> Result<(), HostError> {
        self.with_mut_storage(|storage| storage.put(&key, &val, self.as_budget()))
    }

    /// Records a `System` contract event. `topics` is expected to be a `SCVec`
    /// length <= 4 that cannot contain `Vec`, `Map`, or `Bytes` with length > 32
    /// On success, returns an `SCStatus::Ok`.
    pub fn system_event(&self, topics: Object, data: RawVal) -> Result<RawVal, HostError> {
        let topics = self.event_topics_from_host_obj(topics)?;
        let data = self.from_host_val(data)?;
        self.record_contract_event(ContractEventType::System, topics, data)?;
        Ok(Status::OK.into())
    }

    fn create_contract(&self, args: CreateContractArgs) -> Result<Object, HostError> {
        let id_preimage = match args.contract_id {
            ContractId::Asset(asset) => self.id_preimage_from_asset(asset)?,
            ContractId::SourceAccount(salt) => self.id_preimage_from_source_account(salt)?,
            ContractId::Ed25519PublicKey(key_with_signature) => {
                let signature_payload_preimage = self.create_contract_args_hash_preimage(
                    args.source.metered_clone(&self.budget_ref())?,
                    key_with_signature.salt.metered_clone(self.budget_ref())?,
                )?;
                let signature_payload = self.metered_hash_xdr(&signature_payload_preimage)?;
                self.verify_sig_ed25519_internal(
                    &signature_payload,
                    &self.ed25519_pub_key_from_bytes(&key_with_signature.key.0)?,
                    &self.signature_from_bytes(
                        "create_contract_sig",
                        &key_with_signature.signature.0,
                    )?,
                )?;
                self.id_preimage_from_ed25519(key_with_signature.key, key_with_signature.salt)?
            }
        };
        self.create_contract_with_id_preimage(args.source, id_preimage)
    }

    fn install_contract(&self, args: InstallContractCodeArgs) -> Result<Object, HostError> {
        let hash_bytes = self.metered_hash_xdr(&args)?;
        let hash_obj = self.add_host_object(hash_bytes.to_vec())?;
        let code_key = LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: Hash(hash_bytes.metered_clone(self.budget_ref())?),
        });
        if !self
            .0
            .storage
            .borrow_mut()
            .has(&code_key, self.as_budget())?
        {
            self.with_mut_storage(|storage| {
                let data = LedgerEntryData::ContractCode(ContractCodeEntry {
                    hash: Hash(hash_bytes),
                    code: args.code,
                    ext: ExtensionPoint::V0,
                });
                storage.put(
                    &code_key,
                    &Host::ledger_entry_from_data(data),
                    self.as_budget(),
                )
            })?;
        }
        Ok(hash_obj)
    }

    pub(crate) fn verify_sig_ed25519_internal(
        &self,
        payload: &[u8],
        public_key: &ed25519_dalek::PublicKey,
        sig: &ed25519_dalek::Signature,
    ) -> Result<(), HostError> {
        use ed25519_dalek::Verifier;
        self.charge_budget(CostType::VerifyEd25519Sig, payload.len() as u64)?;
        public_key
            .verify(payload, &sig)
            .map_err(|_| self.err_general("Failed ED25519 verification"))
    }
}

// Notes on metering: these are called from the guest and thus charged on the VM instructions.
impl EnvBase for Host {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }

    fn deep_clone(&self) -> Self {
        Host(Rc::new((*self.0).clone()))
    }

    fn bytes_copy_from_slice(
        &self,
        b: Object,
        b_pos: RawVal,
        mem: &[u8],
    ) -> Result<Object, Status> {
        // This is only called from native contracts, either when testing or
        // when the contract is otherwise linked into the same address space as
        // us. We therefore access the memory we were passed directly.
        let b_pos = u32::try_from(b_pos).map_err(|_| ScHostValErrorCode::UnexpectedValType)?;
        let len = u32::try_from(mem.len()).map_err(|_| ScHostValErrorCode::U32OutOfRange)?;
        let mut vnew = self
            .visit_obj(b, |hv: &Vec<u8>| Ok(hv.clone()))
            .map_err(|he| he.status)?;
        let end_idx = b_pos
            .checked_add(len)
            .ok_or(ScHostValErrorCode::U32OutOfRange)? as usize;
        // TODO: we currently grow the destination vec if it's not big enough,
        // make sure this is desirable behaviour.
        if end_idx > vnew.len() {
            vnew.resize(end_idx, 0);
        }
        self.validate_index_lt_bound(b_pos, vnew.len())
            .map_err(|he| he.status)?;
        let write_slice = &mut vnew[b_pos as usize..end_idx];
        write_slice.copy_from_slice(mem);
        self.add_host_object(vnew)
            .map(|ev| ev.into())
            .map_err(|he| he.status)
    }

    fn bytes_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) -> Result<(), Status> {
        let b_pos = u32::try_from(b_pos).map_err(|_| ScHostValErrorCode::UnexpectedValType)?;
        let len = u32::try_from(mem.len()).map_err(|_| ScHostValErrorCode::U32OutOfRange)?;
        self.visit_obj(b, move |hv: &Vec<u8>| {
            let end_idx = b_pos
                .checked_add(len)
                .ok_or(ScHostValErrorCode::U32OutOfRange)?;
            self.validate_index_lt_bound(b_pos, mem.len())?;
            self.validate_index_le_bound(end_idx, mem.len())?;
            mem.copy_from_slice(&hv.as_slice()[b_pos as usize..end_idx as usize]);
            Ok(())
        })
        .map_err(|he| he.status)
    }

    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<Object, Status> {
        self.add_host_object::<Vec<u8>>(mem.into())
            .map_err(|he| he.status)
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), Status> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(v))
            .map_err(|he| he.status)
    }

    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) -> Result<(), Status> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(s))
            .map_err(|he| he.status)
    }

    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), Status> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(v).arg(s))
            .map_err(|he| he.status)
    }

    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), Status> {
        let mut evt = DebugEvent::new().msg(fmt);
        for v in vals {
            evt = evt.arg(*v)
        }
        for s in strs {
            evt = evt.arg(*s)
        }
        self.record_debug_event(evt).map_err(|he| he.status)
    }
}

impl VmCallerCheckedEnv for Host {
    type VmUserState = Host;
    type Error = HostError;

    // This function is somewhat subtle.
    //
    // It exists to allow the client of the (VmCaller)CheckedEnv interface(s) to
    // essentially _reject_ an error returned by one of the Result-returning
    // methods on the trait, choosing to panic instead. But doing so in some way
    // that the trait defines, rather than calling panic in the client.
    //
    // The only "client" we expect to _do_ this is the `impl Env for CheckedEnv`
    // definition, in checked_env.rs, which itself is only used for testing
    // native contracts, adapting `CheckedEnv` to the non-error-returning `Env`
    // interface that contracts expect.
    //
    // When such a "rejected error" occurs, we do panic, but only after checking
    // to see if we're in a `TestContract` invocation, and if so storing the
    // error's Status value in that frame, such that `Host::call_n` above can
    // recover the Status when it _catches_ the panic and converts it back to an
    // error.
    //
    // It might seem like we ought to `std::panic::panic_any(e)` here, making
    // the panic carry a `HostError` or `Status` and catching it by dynamic type
    // inspection in the `call_n` catch logic. The reason we don't do so is that
    // `panic_any` will not provide a nice printable value to the `PanicInfo`,
    // it constructs, so when/if the panic makes it to a top-level printout it
    // will display a relatively ugly message like "thread panicked at Box<dyn
    // Any>" to stderr, when it is much more useful to the user if we have it
    // print the result of HostError::Debug, with its glorious status code,
    // site-of-origin backtrace and debug log.
    //
    // To get it to do that, we have to call `panic!()`, not `panic_any`.
    // Personally I think this is a glaring weakness of `panic_any` but we are
    // not in a position to improve it.
    fn escalate_error_to_panic(&self, e: Self::Error) -> ! {
        #[cfg(any(test, feature = "testutils"))]
        let _ = self.with_current_frame_opt(|f| {
            if let Some(Frame::TestContract(frame)) = f {
                *frame.panic.borrow_mut() = Some(e.status);
            }
            Ok(())
        });
        let escalation = self.err_status_msg(e.status, "escalating error '{}' to panic");
        panic!("{:?}", escalation)
    }

    // Notes on metering: covered by the components
    fn log_value(&self, _vmcaller: &mut VmCaller<Host>, v: RawVal) -> Result<RawVal, HostError> {
        self.record_debug_event(DebugEvent::new().arg(v))?;
        Ok(RawVal::from_void())
    }

    fn log_fmt_values(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        fmt: Object,
        args: Object,
    ) -> Result<RawVal, HostError> {
        if cfg!(feature = "hostfn_log_fmt_values") {
            let fmt: String = self
                .visit_obj(fmt, move |hv: &Vec<u8>| Ok(String::from_utf8(hv.clone())))?
                .map_err(|_| {
                    self.err_general("log_fmt_values fmt string contains is invalid utf8")
                })?;
            let args: HostVec = self.visit_obj(args, move |hv: &HostVec| Ok(hv.clone()))?;
            self.record_debug_event(DebugEvent::new().msg(fmt).args(args.iter().cloned()))?;
        }
        Ok(RawVal::from_void())
    }

    // Metering: mostly free or already covered by components (e.g. err_general)
    fn get_invoker_type(&self, _vmcaller: &mut VmCaller<Host>) -> Result<u64, HostError> {
        let frames = self.0.context.borrow();
        // If the previous frame exists and is a contract, return its ID, otherwise return
        // the account invoking.
        let st = match frames.as_slice() {
            // There are always two frames when WASM is executed in the VM.
            [.., f2, _] => match f2 {
                #[cfg(feature = "vm")]
                Frame::ContractVM(_, _) => Ok(InvokerType::Contract),
                Frame::HostFunction(_) => Ok(InvokerType::Account),
                Frame::Token(id, _) => Ok(InvokerType::Contract),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(_) => Ok(InvokerType::Contract),
            },
            // In tests contracts are executed with a single frame.
            // TODO: Investigate this discrepancy: https://github.com/stellar/rs-soroban-env/issues/485.
            [f1] => Ok(InvokerType::Account),
            _ => Err(self.err_general("no frames to derive the invoker from")),
        }?;
        Ok(st as u64)
    }

    // Notes on metering: covered by the components
    fn get_invoking_account(&self, vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        if self.get_invoker_type(vmcaller)? != InvokerType::Account as u64 {
            return Err(self.err_general("invoker is not an account"));
        }
        self.source_account().map(|aid| self.add_host_object(aid))?
    }

    // Notes on metering: covered by the components
    fn get_invoking_contract(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        let frames = self.0.context.borrow();
        // the previous frame must exist and must be a contract
        let hash = match frames.as_slice() {
            [.., f2, _] => match f2 {
                #[cfg(feature = "vm")]
                Frame::ContractVM(vm, _) => Ok(vm.contract_id.metered_clone(&self.0.budget)?),
                Frame::HostFunction(_) => Err(self.err_general("invoker is not a contract")),
                Frame::Token(id, _) => Ok(id.clone()),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => Ok(tc.id.clone()), // no metering
            },
            _ => Err(self.err_general("no frames to derive the invoker from")),
        }?;
        self.add_host_object(<Vec<u8>>::from(hash.0))
    }

    // Metered: covered by `visit` and `metered_cmp`.
    fn obj_cmp(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: RawVal,
        b: RawVal,
    ) -> Result<i64, HostError> {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| {
                self.unchecked_visit_val_obj(b, |bo| Ok(self.compare(&ao, &bo)?))
            })?
        };
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
    }

    fn contract_event(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        topics: Object,
        data: RawVal,
    ) -> Result<RawVal, HostError> {
        let topics = self.event_topics_from_host_obj(topics)?;
        let data = self.from_host_val(data)?;
        self.record_contract_event(ContractEventType::Contract, topics, data)?;
        Ok(Status::OK.into())
    }

    // Notes on metering: covered by the components.
    fn get_current_contract(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        let hash: Hash = self.get_current_contract_id()?;
        self.add_host_object(<Vec<u8>>::from(hash.0))
    }

    // Notes on metering: covered by `add_host_object`.
    fn obj_from_u64(&self, _vmcaller: &mut VmCaller<Host>, u: u64) -> Result<Object, HostError> {
        self.add_host_object(u)
    }

    // Notes on metering: covered by `visit_obj`.
    fn obj_to_u64(&self, _vmcaller: &mut VmCaller<Host>, obj: Object) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &u64| Ok(*u))
    }

    // Notes on metering: covered by `add_host_object`.
    fn obj_from_i64(&self, _vmcaller: &mut VmCaller<Host>, i: i64) -> Result<Object, HostError> {
        self.add_host_object(i)
    }

    // Notes on metering: covered by `visit_obj`.
    fn obj_to_i64(&self, _vmcaller: &mut VmCaller<Host>, obj: Object) -> Result<i64, HostError> {
        self.visit_obj(obj, |i: &i64| Ok(*i))
    }

    fn obj_from_u128_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        lo: u64,
        hi: u64,
    ) -> Result<Object, Self::Error> {
        let u: u128 = ((hi as u128) << 64) | lo as u128;
        self.add_host_object(u)
    }

    fn obj_to_u128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok(*u as u64))
    }

    fn obj_to_u128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok((*u >> 64) as u64))
    }

    fn obj_from_i128_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        lo: u64,
        hi: u64,
    ) -> Result<Object, Self::Error> {
        // NB: always do assembly/disassembly as unsigned, to avoid sign extension.
        let u: u128 = ((hi as u128) << 64) | lo as u128;
        let i: i128 = u as i128;
        self.add_host_object(i)
    }

    fn obj_to_i128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &i128| Ok((*u as u128) as u64))
    }

    fn obj_to_i128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &i128| Ok(((*u as u128) >> 64) as u64))
    }

    fn map_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        self.add_host_object(HostMap::new(self)?)
    }

    fn map_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
        v: RawVal,
    ) -> Result<Object, HostError> {
        let mnew = self.visit_obj(m, |hm: &HostMap| hm.insert(k, v, self))?;
        self.add_host_object(mnew)
    }

    fn map_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, move |hm: &HostMap| {
            hm.get(&k, self)?
                .map(|v| *v)
                .ok_or_else(|| self.err_general("map key not found")) // FIXME: need error code
        })
    }

    fn map_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
    ) -> Result<Object, HostError> {
        match self.visit_obj(m, |hm: &HostMap| hm.remove(&k, self))? {
            Some((mnew, _)) => Ok(self.add_host_object(mnew)?.into()),
            None => Err(self.err_general("map key not found")),
        }
    }

    fn map_len(&self, _vmcaller: &mut VmCaller<Host>, m: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(m, |hm: &HostMap| Ok(hm.len()))?;
        self.usize_to_rawval_u32(len)
    }

    fn map_has(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, move |hm: &HostMap| Ok(hm.contains_key(&k, self)?.into()))
    }

    fn map_prev_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_prev(&k, self)? {
                Ok(*pk)
            } else {
                Ok(Status::UNKNOWN_ERROR.to_raw())
            }
        })
    }

    fn map_next_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: Object,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_next(&k, self)? {
                Ok(*pk)
            } else {
                Ok(Status::UNKNOWN_ERROR.to_raw())
            }
        })
    }

    fn map_min_key(&self, _vmcaller: &mut VmCaller<Host>, m: Object) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            match hm.get_min(self)? {
                Some((pk, pv)) => Ok(*pk),
                None => Ok(Status::UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
            }
        })
    }

    fn map_max_key(&self, _vmcaller: &mut VmCaller<Host>, m: Object) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            match hm.get_max(self)? {
                Some((pk, pv)) => Ok(*pk),
                None => Ok(Status::UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
            }
        })
    }

    fn map_keys(&self, _vmcaller: &mut VmCaller<Host>, m: Object) -> Result<Object, HostError> {
        let vec = self.visit_obj(m, |hm: &HostMap| {
            HostVec::from_exact_iter(hm.keys(self)?.cloned(), self.budget_ref())
        })?;
        self.add_host_object(vec)
    }

    fn map_values(&self, _vmcaller: &mut VmCaller<Host>, m: Object) -> Result<Object, HostError> {
        let vec = self.visit_obj(m, |hm: &HostMap| {
            HostVec::from_exact_iter(hm.values(self)?.cloned(), self.budget_ref())
        })?;
        self.add_host_object(vec)
    }

    fn vec_new(&self, _vmcaller: &mut VmCaller<Host>, c: RawVal) -> Result<Object, HostError> {
        let capacity: usize = if c.is_void() {
            0
        } else {
            self.usize_from_rawval_u32_input("c", c)?
        };
        // TODO: optimize the vector based on capacity
        self.add_host_object(HostVec::new(self.budget_ref())?)
    }

    fn vec_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        i: RawVal,
        x: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.u32_from_rawval_input("i", i)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_lt_bound(i, hv.len())?;
            hv.set(i as usize, x, self.as_budget())
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        i: RawVal,
    ) -> Result<RawVal, HostError> {
        let i: usize = self.usize_from_rawval_u32_input("i", i)?;
        self.visit_obj(v, move |hv: &HostVec| {
            hv.get(i, self.as_budget()).map(|r| *r)
        })
    }

    fn vec_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        i: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.u32_from_rawval_input("i", i)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_lt_bound(i, hv.len())?;
            hv.remove(i as usize, self.as_budget())
        })?;
        self.add_host_object(vnew)
    }

    fn vec_len(&self, _vmcaller: &mut VmCaller<Host>, v: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        self.usize_to_rawval_u32(len)
    }

    fn vec_push_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        x: RawVal,
    ) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.push_front(x, self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
    ) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_front(self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_push_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        x: RawVal,
    ) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.push_back(x, self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop_back(&self, _vmcaller: &mut VmCaller<Host>, v: Object) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_back(self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_front(&self, _vmcaller: &mut VmCaller<Host>, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.front(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_back(&self, _vmcaller: &mut VmCaller<Host>, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.back(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_insert(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        i: RawVal,
        x: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.u32_from_rawval_input("i", i)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_le_bound(i, hv.len())?;
            hv.insert(i as usize, x, self.as_budget())
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_append(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v1: Object,
        v2: Object,
    ) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v1, |hv1: &HostVec| {
            self.visit_obj(v2, |hv2: &HostVec| {
                if hv1.len() > u32::MAX as usize - hv2.len() {
                    Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"))
                } else {
                    hv1.append(hv2, self.as_budget())
                }
            })
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_slice(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        start: RawVal,
        end: RawVal,
    ) -> Result<Object, HostError> {
        let start = self.u32_from_rawval_input("start", start)?;
        let end = self.u32_from_rawval_input("end", end)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let range = self.valid_range_from_start_end_bound(start, end, hv.len())?;
            hv.slice(range, self.as_budget())
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_first_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        x: RawVal,
    ) -> Result<RawVal, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.first_index_of(|other| self.compare(&x, other), &self.as_budget())? {
                    Some(u) => self.usize_to_rawval_u32(u)?,
                    None => RawVal::from_void(),
                },
            )
        })
    }

    fn vec_last_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        x: RawVal,
    ) -> Result<RawVal, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.last_index_of(|other| self.compare(&x, other), self.as_budget())? {
                    Some(u) => self.usize_to_rawval_u32(u)?,
                    None => RawVal::from_void(),
                },
            )
        })
    }

    fn vec_binary_search(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Object,
        x: RawVal,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            let res = hv.binary_search_by(|probe| self.compare(probe, &x), self.as_budget())?;
            self.u64_from_binary_search_result(res)
        })
    }

    // Notes on metering: covered by components
    fn put_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
        v: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.contract_data_key_from_rawval(k)?;
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: self.get_current_contract_id()?,
            key: self.from_host_val(k)?,
            val: self.from_host_val(v)?,
        });
        self.0.storage.borrow_mut().put(
            &key,
            &Host::ledger_entry_from_data(data),
            self.as_budget(),
        )?;
        Ok(().into())
    }

    // Notes on metering: covered by components
    fn has_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.storage_key_from_rawval(k)?;
        let res = self.0.storage.borrow_mut().has(&key, self.as_budget())?;
        Ok(RawVal::from_bool(res))
    }

    // Notes on metering: covered by components
    fn get_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.storage_key_from_rawval(k)?;
        match self
            .0
            .storage
            .borrow_mut()
            .get(&key, self.as_budget())?
            .data
        {
            LedgerEntryData::ContractData(ContractDataEntry {
                contract_id,
                key,
                val,
            }) => Ok(self.to_host_val(&val)?.into()),
            _ => Err(self.err_status_msg(
                ScHostStorageErrorCode::ExpectContractData,
                "expected contract data",
            )),
        }
    }

    // Notes on metering: covered by components
    fn del_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.contract_data_key_from_rawval(k)?;
        self.0.storage.borrow_mut().del(&key, self.as_budget())?;
        Ok(().into())
    }

    // Notes on metering: covered by the components.
    fn create_contract_from_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        wasm_hash: Object,
        salt: Object,
    ) -> Result<Object, HostError> {
        let contract_id = self.get_current_contract_id()?;
        let salt = self.uint256_from_obj_input("salt", salt)?;

        let code = ScContractCode::WasmRef(self.hash_from_obj_input("wasm_hash", wasm_hash)?);
        let id_preimage = self.id_preimage_from_contract(contract_id, salt)?;
        self.create_contract_with_id_preimage(code, id_preimage)
    }

    // Notes on metering: here covers the args unpacking. The actual VM work is changed at lower layers.
    fn call(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        contract: Object,
        func: Symbol,
        args: Object,
    ) -> Result<RawVal, HostError> {
        let args = self.call_args_from_obj(args)?;
        // this is the recommanded path of calling a contract, with `reentry` always set `false`
        let res = self.call_n(contract, func, args.as_slice(), false);
        if let Err(e) = &res {
            let evt = DebugEvent::new()
                .msg("contract call invocation resulted in error {}")
                .arg::<RawVal>(e.status.into());
            self.record_debug_event(evt)?;
        }
        res
    }

    // Notes on metering: covered by the components.
    fn try_call(
        &self,
        vmcaller: &mut VmCaller<Host>,
        contract: Object,
        func: Symbol,
        args: Object,
    ) -> Result<RawVal, HostError> {
        let args = self.call_args_from_obj(args)?;
        // this is the "loosened" path of calling a contract.
        // TODO: A `reentry` flag will be passed from `try_call` into here.
        // For now, we are passing in `false` to disable reentry.
        let res = self.call_n(contract, func, args.as_slice(), false);
        match res {
            Ok(rv) => Ok(rv),
            Err(e) => {
                let status: RawVal = e.status.into();
                let evt = DebugEvent::new()
                    .msg("contract call invocation resulted in error {}")
                    .arg(status);
                self.record_debug_event(evt)?;
                Ok(status)
            }
        }
    }

    // Notes on metering: covered by components
    fn serialize_to_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: RawVal,
    ) -> Result<Object, HostError> {
        let scv = self.from_host_val(v)?;
        let mut buf = Vec::<u8>::new();
        self.metered_write_xdr(&scv, &mut buf)?;
        Ok(self.add_host_object(buf)?.into())
    }

    // Notes on metering: covered by components
    fn deserialize_from_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
    ) -> Result<RawVal, HostError> {
        let scv = self.visit_obj(b, |hv: &Vec<u8>| {
            self.metered_from_xdr::<ScVal>(hv.as_slice())
        })?;
        Ok(self.to_host_val(&scv)?.into())
    }

    fn bytes_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: Object,
        b_pos: RawVal,
        lm_pos: RawVal,
        len: RawVal,
    ) -> Result<RawVal, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            let b_pos = u32::try_from(b_pos)?;
            self.visit_obj(b, move |hv: &Vec<u8>| {
                let range = self.valid_range_from_start_span_bound(b_pos, len, hv.len())?;
                self.metered_vm_mem_write(vmcaller, vm, pos, range, hv)?;
                Ok(().into())
            })
        }
    }

    fn bytes_copy_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: Object,
        b_pos: RawVal,
        lm_pos: RawVal,
        len: RawVal,
    ) -> Result<Object, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            let b_pos = u32::try_from(b_pos)?;
            let mut vnew =
                self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.metered_clone(&self.0.budget)?))?;
            let end_idx = b_pos.checked_add(len).ok_or_else(|| {
                self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
            })? as usize;
            // TODO: we currently grow the destination vec if it's not big enough,
            // make sure this is desirable behaviour.
            if end_idx > vnew.len() {
                vnew.resize(end_idx, 0);
            }
            self.metered_vm_mem_read(vmcaller, vm, pos, b_pos as usize..end_idx, &mut vnew)?;
            Ok(self.add_host_object(vnew)?.into())
        }
    }

    fn bytes_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: RawVal,
        len: RawVal,
    ) -> Result<Object, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            let mut vnew: Vec<u8> = vec![0; len as usize];
            self.metered_vm_mem_read(vmcaller, vm, pos, 0..len as usize, &mut vnew)?;
            Ok(self.add_host_object(vnew)?.into())
        }
    }

    // Notes on metering: covered by `add_host_object`
    fn bytes_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        Ok(self.add_host_object(Vec::<u8>::new())?.into())
    }

    // Notes on metering: `get_mut` is free
    fn bytes_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        i: RawVal,
        u: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.metered_clone(&self.0.budget)?;
            match vnew.get_mut(i) {
                None => Err(self.err_status(ScHostObjErrorCode::VecIndexOutOfBound)),
                Some(v) => {
                    *v = u;
                    Ok(vnew)
                }
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    // Notes on metering: `get` is free
    fn bytes_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        i: RawVal,
    ) -> Result<RawVal, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        self.visit_obj(b, |hv: &Vec<u8>| {
            hv.get(i)
                .map(|u| Into::<RawVal>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
        })
    }

    fn bytes_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        i: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.u32_from_rawval_input("i", i)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            self.validate_index_lt_bound(i, hv.len())?;
            let mut vnew = hv.metered_clone(&self.0.budget)?;
            self.charge_budget(CostType::BytesDel, hv.len() as u64)?; // O(n) worst case
            vnew.remove(i as usize);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    // Notes on metering: `len` is free
    fn bytes_len(&self, _vmcaller: &mut VmCaller<Host>, b: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.len()))?;
        self.usize_to_rawval_u32(len)
    }

    // Notes on metering: `push` is free
    fn bytes_push(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        u: RawVal,
    ) -> Result<Object, HostError> {
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.metered_clone(&self.0.budget)?;
            // Passing `len()` since worse case can cause reallocation.
            self.charge_budget(CostType::BytesPush, hv.len() as u64)?;
            vnew.push(u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    // Notes on metering: `pop` is free
    fn bytes_pop(&self, _vmcaller: &mut VmCaller<Host>, b: Object) -> Result<Object, HostError> {
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.metered_clone(&self.0.budget)?;
            // Passing `len()` since worse case can cause reallocation.
            self.charge_budget(CostType::BytesPop, hv.len() as u64)?;
            vnew.pop()
                .map(|_| vnew)
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    // Notes on metering: `first` is free
    fn bytes_front(&self, _vmcaller: &mut VmCaller<Host>, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            hv.first()
                .map(|u| Into::<RawVal>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
        })
    }

    // Notes on metering: `last` is free
    fn bytes_back(&self, _vmcaller: &mut VmCaller<Host>, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            hv.last()
                .map(|u| Into::<RawVal>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
        })
    }

    fn bytes_insert(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        i: RawVal,
        u: RawVal,
    ) -> Result<Object, HostError> {
        let i = self.u32_from_rawval_input("i", i)?;
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            self.validate_index_le_bound(i, hv.len())?;
            let mut vnew = hv.metered_clone(&self.0.budget)?;
            self.charge_budget(CostType::BytesInsert, hv.len() as u64)?; // insert is O(n) worst case
            vnew.insert(i as usize, u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn bytes_append(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b1: Object,
        b2: Object,
    ) -> Result<Object, HostError> {
        let mut vnew = self.visit_obj(b1, |hv: &Vec<u8>| Ok(hv.metered_clone(&self.0.budget)?))?;
        let mut b2 = self.visit_obj(b2, |hv: &Vec<u8>| Ok(hv.metered_clone(&self.0.budget)?))?;
        if b2.len() > u32::MAX as usize - vnew.len() {
            return Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"));
        }
        self.charge_budget(CostType::BytesAppend, (vnew.len() + b2.len()) as u64)?; // worst case can cause rellocation
        vnew.append(&mut b2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn bytes_slice(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: Object,
        start: RawVal,
        end: RawVal,
    ) -> Result<Object, HostError> {
        let start = self.u32_from_rawval_input("start", start)?;
        let end = self.u32_from_rawval_input("end", end)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let range = self.valid_range_from_start_end_bound(start, end, hv.len())?;
            self.charge_budget(CostType::BytesClone, range.len() as u64)?;
            Ok(hv.as_slice()[range].to_vec())
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn hash_from_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
    ) -> Result<Object, HostError> {
        todo!()
    }

    fn hash_to_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
    ) -> Result<Object, HostError> {
        todo!()
    }

    fn public_key_from_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
    ) -> Result<Object, HostError> {
        todo!()
    }

    fn public_key_to_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
    ) -> Result<Object, HostError> {
        todo!()
    }

    // Notes on metering: covered by components.
    fn compute_hash_sha256(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
    ) -> Result<Object, HostError> {
        let hash = self.sha256_hash_from_bytes_input(x)?;
        Ok(self.add_host_object(hash)?.into())
    }

    // Notes on metering: covered by components.
    fn verify_sig_ed25519(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: Object,
        k: Object,
        s: Object,
    ) -> Result<RawVal, HostError> {
        let public_key = self.ed25519_pub_key_from_obj_input(k)?;
        let sig = self.signature_from_obj_input("sig", s)?;
        let res = self.visit_obj(x, |payload: &Vec<u8>| {
            self.verify_sig_ed25519_internal(payload, &public_key, &sig)
        });
        Ok(res?.into())
    }

    // Notes on metering: covered by components.
    fn account_get_low_threshold(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: Object,
    ) -> Result<RawVal, Self::Error> {
        let threshold =
            self.load_account(self.to_account_id(a)?)?.thresholds.0[ThresholdIndexes::Low as usize];
        let threshold = Into::<u32>::into(threshold);
        Ok(threshold.into())
    }

    // Notes on metering: covered by components.
    fn account_get_medium_threshold(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: Object,
    ) -> Result<RawVal, Self::Error> {
        let threshold =
            self.load_account(self.to_account_id(a)?)?.thresholds.0[ThresholdIndexes::Med as usize];
        let threshold = Into::<u32>::into(threshold);
        Ok(threshold.into())
    }

    // Notes on metering: covered by components.
    fn account_get_high_threshold(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: Object,
    ) -> Result<RawVal, Self::Error> {
        let threshold = self.load_account(self.to_account_id(a)?)?.thresholds.0
            [ThresholdIndexes::High as usize];
        let threshold = Into::<u32>::into(threshold);
        Ok(threshold.into())
    }

    // Notes on metering: covered by components.
    fn account_exists(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: Object,
    ) -> Result<RawVal, Self::Error> {
        Ok(self.has_account(self.to_account_id(a)?)?.into())
    }

    // Notes on metering: some covered. The for loop and comparisons are free (for now).
    fn account_get_signer_weight(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: Object,
        s: Object,
    ) -> Result<RawVal, Self::Error> {
        let target_signer = self.to_u256(s)?;

        let ae = self.load_account(self.to_account_id(a)?)?;
        let weight = self.get_signer_weight_from_account(target_signer, &ae)?;
        Ok((weight as u32).into())
    }

    fn get_ledger_version(&self, _vmcaller: &mut VmCaller<Host>) -> Result<RawVal, Self::Error> {
        self.with_ledger_info(|li| Ok(li.protocol_version.into()))
    }

    fn get_ledger_sequence(&self, _vmcaller: &mut VmCaller<Host>) -> Result<RawVal, Self::Error> {
        self.with_ledger_info(|li| Ok(li.sequence_number.into()))
    }

    fn get_ledger_timestamp(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, Self::Error> {
        self.with_ledger_info(|li| Ok(self.add_host_object(li.timestamp)?.into()))
    }

    fn get_ledger_network_passphrase(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<Object, Self::Error> {
        Ok(self
            .with_ledger_info(|li| self.add_host_object(li.network_passphrase.clone()))?
            .into())
    }

    fn get_ledger_network_id(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, Self::Error> {
        Ok(self
            .with_ledger_info(|li| {
                let hash = Sha256::digest(li.network_passphrase.clone())
                    .as_slice()
                    .to_vec();
                if hash.len() != 32 {
                    return Err(self.err_general("incorrect hash size"));
                }
                self.add_host_object(hash)
            })?
            .into())
    }

    fn get_current_call_stack(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        let frames = self.0.context.borrow();

        let get_host_val_tuple = |id: &Hash, function: &Symbol| -> Result<[RawVal; 2], HostError> {
            let id_val = self.add_host_object(<Vec<u8>>::from(id.0))?.into();
            let function_val = function.clone().into();
            Ok([id_val, function_val])
        };

        let mut outer = Vec::with_capacity(frames.len());
        for frame in frames.iter() {
            let vals = match frame {
                #[cfg(feature = "vm")]
                Frame::ContractVM(vm, function) => get_host_val_tuple(&vm.contract_id, &function)?,
                Frame::HostFunction(_) => continue,
                Frame::Token(id, function) => get_host_val_tuple(&id, &function)?,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => get_host_val_tuple(&tc.id, &tc.func)?,
            };
            let inner = MeteredVector::from_array(vals, self.as_budget())?;
            outer.push(self.add_host_object(inner)?.into());
        }
        Ok(self.add_host_object(HostVec::from_vec(outer)?)?.into())
    }

    fn fail_with_status(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        status: Status,
    ) -> Result<RawVal, Self::Error> {
        if status.is_type(ScStatusType::ContractError) {
            Err(self.err_status_msg(status, "failing with contract error status code '{}'"))
        } else {
            Err(self.err_status_msg(
                ScHostValErrorCode::UnexpectedValType,
                "contract attempted to fail with non-ContractError status code",
            ))
        }
    }

    fn dummy0(&self, vmcaller: &mut VmCaller<Self::VmUserState>) -> Result<RawVal, Self::Error> {
        Ok(().into())
    }
}
