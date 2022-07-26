#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use im_rc::{OrdMap, Vector};
use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::cast::ToPrimitive;
use num_traits::{Pow, Signed, Zero};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use stellar_contract_env_common::{TryConvert, TryFromVal, TryIntoVal};

use stellar_contract_env_common::xdr::{
    AccountEntry, AccountId, Hash, PublicKey, ReadXdr, ThresholdIndexes, WriteXdr,
};

use crate::budget::{Budget, CostType};
use crate::events::{DebugError, DebugEvent, Events};
use crate::storage::Storage;
use crate::weak_host::WeakHost;

use crate::xdr;
use crate::xdr::{
    ContractDataEntry, HostFunction, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyContractData, ScBigInt, ScHostContextErrorCode, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScMap, ScMapEntry, ScObject, ScStatic, ScVal,
    ScVec,
};
use std::rc::Rc;

use crate::host_object::{HostMap, HostObj, HostObject, HostObjectType, HostVal, HostVec};
use crate::CheckedEnv;
#[cfg(feature = "vm")]
use crate::SymbolStr;
#[cfg(feature = "vm")]
use crate::Vm;
use crate::{
    ConversionError, EnvBase, IntoEnvVal, Object, RawVal, RawValConvertible, Status, Symbol, Val,
    UNKNOWN_ERROR,
};

mod conversion;
mod error;
pub use error::HostError;

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
#[derive(Clone)]
struct RollbackPoint {
    storage: OrdMap<LedgerKey, Option<LedgerEntry>>,
    objects: usize,
}

#[cfg(feature = "testutils")]
pub trait ContractFunctionSet {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Option<RawVal>;
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
    ContractVM(Rc<Vm>),
    HostFunction(HostFunction),
    #[cfg(feature = "testutils")]
    TestContract(Hash),
}

/// Temporary helper for denoting a slice of guest memory, as formed by
/// various binary operations.
#[cfg(feature = "vm")]
struct VmSlice {
    vm: Rc<Vm>,
    pos: u32,
    len: u32,
}

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    context: RefCell<Vec<Frame>>,
    budget: RefCell<Budget>,
    events: RefCell<Events>,
    #[cfg(feature = "testutils")]
    contracts: RefCell<std::collections::HashMap<Hash, Rc<dyn ContractFunctionSet>>>,
}

/// A guard struct that exists to call [`Host::pop_frame`] when it is dropped,
/// providing whatever rollback it is holding as an argument. By default, this
/// means that when a [`FrameGuard`] drops, it will roll the [`Host`] back to
/// the state it had before its associated [`Frame`] was pushed.
///
/// Users may call [`FrameGuard::commit`] to cause the rollback point to be set
/// to `None`, which will cause the [`FrameGuard`] to commit changes to the host
/// that occurred during its lifetime, rather than rollling them back.
pub struct FrameGuard {
    rollback: Option<RollbackPoint>,
    host: Host,
}

impl FrameGuard {
    pub(crate) fn commit(&mut self) {
        if self.rollback.is_none() {
            panic!("committing on already-committed FrameGuard")
        }
        self.rollback = None;
    }
}

impl Drop for FrameGuard {
    fn drop(&mut self) {
        let rollback = self.rollback.take();
        self.host.pop_frame(rollback);
    }
}

// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Default, Clone)]
pub struct Host(pub(crate) Rc<HostImpl>);

impl Debug for Host {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Host({:x})", Rc::<HostImpl>::as_ptr(&self.0) as usize)
    }
}

impl TryConvert<Object, ScObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: Object) -> Result<ScObject, Self::Error> {
        self.from_host_obj(ob)
    }
}

impl TryConvert<&ScObject, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: &ScObject) -> Result<Object, Self::Error> {
        self.to_host_obj(ob).map(|ob| ob.val)
    }
}

impl TryConvert<ScObject, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: ScObject) -> Result<Object, Self::Error> {
        self.to_host_obj(&ob).map(|ob| ob.val)
    }
}

impl Host {
    /// Constructs a new [`Host`] that will use the provided [`Storage`] for
    /// contract-data access functions such as
    /// [`CheckedEnv::get_contract_data`].
    pub fn with_storage(storage: Storage) -> Self {
        Self(Rc::new(HostImpl {
            objects: Default::default(),
            storage: RefCell::new(storage),
            context: Default::default(),
            budget: Default::default(),
            events: Default::default(),
            #[cfg(feature = "testutils")]
            contracts: Default::default(),
        }))
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    pub fn get_budget_mut<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&mut Budget) -> T,
    {
        f(&mut *self.0.budget.borrow_mut())
    }

    pub fn get_budget<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&Budget) -> T,
    {
        f(&*self.0.budget.borrow())
    }

    pub fn charge_budget(&self, ty: CostType, input: u64) -> Result<(), HostError> {
        self.get_budget_mut(|budget| budget.charge(ty, input))
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
    pub(crate) fn debug_event<T>(&self, src: T) -> Result<(), HostError>
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
        let len = event.args.len() as u64;
        self.get_events_mut(|events| Ok(events.record_debug_event(event)))?;
        self.charge_budget(CostType::HostEventDebug, len)
    }

    /// Records a debug-event from its input in as much detail as possible, then
    /// converts its input to a (often coarser-granularity) [Status] code, and then
    /// forms a [HostError] with it (which also captures a [backtrace::Backtrace]).
    /// This is the method you want to call any time there's a finer-granularity error
    /// type that you want to log the details of and then downgrade fail with.
    pub(crate) fn err<T>(&self, src: T) -> HostError
    where
        DebugError: From<T>,
    {
        let ds: DebugError = src.into();
        if let Err(e) = self.debug_event(ds.event) {
            e
        } else {
            let mut he: HostError = ds.status.into();
            he.events = Some(self.0.events.borrow().clone());
            he
        }
    }

    /// Helper for the simplest status-only error path.
    pub(crate) fn err_status<T>(&self, status: T) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status))
    }

    /// Helper for the simplest string + general-error path.
    pub(crate) fn err_general(&self, msg: &'static str) -> HostError {
        self.err(DebugError::general().msg(msg))
    }

    pub(crate) fn err_conversion_raw_val_to<T>(&self, rv: RawVal) -> HostError {
        self.err(
            DebugError::new(ConversionError)
                .msg("error converting {} into {}")
                .arg(rv)
                .arg(std::any::type_name::<T>()),
        )
    }

    /// Helper for the next-simplest status-and-extended-debug-message error path.
    pub(crate) fn err_status_msg<T>(&self, status: T, msg: &'static str) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status).msg(msg))
    }

    pub(crate) fn err_convert<T>(&self, v: RawVal) -> HostError {
        if let Err(e) = self.debug_event(
            DebugEvent::new()
                .msg("can't convert {} to {}")
                .arg(v)
                .arg(core::any::type_name::<T>()),
        ) {
            return e;
        }
        ConversionError {}.into()
    }

    pub(crate) fn err_convert_general(&self, msg: &'static str) -> HostError {
        if let Err(e) = self.debug_event(DebugEvent::new().msg(msg)) {
            return e;
        }
        ConversionError {}.into()
    }

    /// Given a result carrying some error type that can be converted to a
    /// DebugStatus, calls self.err with it when there's an error. Returns a
    /// result over HostError.
    ///
    /// If you have an error type T you want to record as a detailed debug event
    /// and a less-detailed Status code embedded in a HostError, add an `impl
    /// From<T> for DebugStatus` over in the [events] module and call this where
    /// the error is generated.
    ///
    /// Note: we do _not_ want to `impl From<T> for HostError` for such types,
    /// as doing so will avoid routing them through the host in order to record
    /// their extended diagnostic information into the debug buffer. This means
    /// you will wind up writing `host.map_err(...)?` a bunch in code that you used
    /// to be able to get away with just writing `...?`, there's no way around
    /// this if we want to record the diagnostic information.
    pub(crate) fn map_err<T, E>(&self, res: Result<T, E>) -> Result<T, HostError>
    where
        DebugError: From<E>,
    {
        res.map_err(|e| self.err(e.into()))
    }

    pub(crate) fn visit_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Storage) -> Result<U, HostError>,
    {
        f(&mut *self.0.storage.borrow_mut())
    }

    pub fn recover_storage(self) -> Result<Storage, Self> {
        Rc::try_unwrap(self.0)
            .map(|host_impl| host_impl.storage.into_inner())
            .map_err(Host)
    }

    fn capture_rollback_point(&self) -> RollbackPoint {
        RollbackPoint {
            objects: self.0.objects.borrow().len(),
            storage: self.0.storage.borrow().map.clone(),
        }
    }

    fn pop_frame(&self, rollback: Option<RollbackPoint>) {
        self.0
            .context
            .borrow_mut()
            .pop()
            .expect("unmatched host frame push/pop");
        match rollback {
            None => (),
            Some(rp) => {
                self.0.objects.borrow_mut().truncate(rp.objects);
                self.0.storage.borrow_mut().map = rp.storage;
            }
        }
    }

    /// Pushes a new VM-related [`Frame`] on the context stack, returning a [`FrameGuard`]
    /// that will pop the stack when it is dropped. This should be called at
    /// least any time a new contract is invoked.
    #[cfg(feature = "vm")]
    pub(crate) fn push_vm_frame(&self, vm: Rc<Vm>) -> FrameGuard {
        self.0.context.borrow_mut().push(Frame::ContractVM(vm));
        FrameGuard {
            rollback: Some(self.capture_rollback_point()),
            host: self.clone(),
        }
    }

    pub(crate) fn push_host_function_frame(&self, func: HostFunction) -> FrameGuard {
        self.0.context.borrow_mut().push(Frame::HostFunction(func));
        FrameGuard {
            rollback: Some(self.capture_rollback_point()),
            host: self.clone(),
        }
    }

    #[cfg(feature = "testutils")]
    pub(crate) fn push_test_frame(&self, contract_id: Hash) -> FrameGuard {
        self.0
            .context
            .borrow_mut()
            .push(Frame::TestContract(contract_id));
        FrameGuard {
            rollback: Some(self.capture_rollback_point()),
            host: self.clone(),
        }
    }

    /// Applies a function to the top [`Frame`] of the context stack. Returns
    /// [`HostError`] if the context stack is empty, otherwise returns result of
    /// function call.
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

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    fn get_current_contract_id(&self) -> Result<Hash, HostError> {
        self.with_current_frame(|frame| match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm) => Ok(vm.contract_id.clone()),
            Frame::HostFunction(_) => {
                Err(self.err_general("Host function context has no contract ID"))
            }
            #[cfg(feature = "testutils")]
            Frame::TestContract(id) => Ok(id.clone()),
        })
    }

    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: RawVal, f: F) -> Result<U, HostError>
    where
        F: FnOnce(Option<&HostObject>) -> Result<U, HostError>,
    {
        self.charge_budget(CostType::VisitObject, 1)?;
        let r = self.0.objects.borrow();
        let index = <Object as RawValConvertible>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }

    fn visit_obj<HOT: HostObjectType, F, U>(&self, obj: Object, f: F) -> Result<U, HostError>
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

    fn reassociate_val(hv: &mut HostVal, weak: WeakHost) {
        hv.env = weak;
    }

    pub(crate) fn get_weak(&self) -> WeakHost {
        WeakHost(Rc::downgrade(&self.0))
    }

    pub(crate) fn associate_raw_val(&self, val: RawVal) -> HostVal {
        let env = self.get_weak();
        HostVal { env, val }
    }

    pub(crate) fn associate_env_val_type<V: Val, CVT: IntoEnvVal<WeakHost, RawVal>>(
        &self,
        v: CVT,
    ) -> HostVal {
        let env = self.get_weak();
        v.into_env_val(&env)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        ScVal::try_from_val(self, val)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    // Testing interface to create values directly for later use via Env functions.
    pub fn inject_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.to_host_val(v).map(Into::into)
    }

    pub fn get_events(&self) -> Events {
        self.0.events.borrow().clone()
    }

    #[cfg(feature = "vm")]
    fn decode_vmslice(&self, pos: RawVal, len: RawVal) -> Result<VmSlice, HostError> {
        let pos: u32 = self.u32_from_rawval_input("pos", pos)?;
        let len: u32 = self.u32_from_rawval_input("len", len)?;
        self.with_current_frame(|frame| match frame {
            Frame::ContractVM(vm) => {
                let vm = vm.clone();
                Ok(VmSlice { vm, pos, len })
            }
            _ => Err(self.err_general("attempt to access guest binary in non-VM frame")),
        })
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<HostVal, HostError> {
        let rv = v
            .try_into_val(self)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))?;
        Ok(self.associate_raw_val(rv))
    }

    pub(crate) fn from_host_obj(&self, ob: Object) -> Result<ScObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| match ob {
                None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                Some(ho) => match ho {
                    HostObject::Vec(vv) => {
                        let mut sv = Vec::new();
                        for e in vv.iter() {
                            sv.push(self.from_host_val(e.val)?);
                        }
                        Ok(ScObject::Vec(ScVec(self.map_err(sv.try_into())?)))
                    }
                    HostObject::Map(mm) => {
                        let mut mv = Vec::new();
                        for (k, v) in mm.iter() {
                            let key = self.from_host_val(k.val)?;
                            let val = self.from_host_val(v.val)?;
                            mv.push(ScMapEntry { key, val });
                        }
                        Ok(ScObject::Map(ScMap(self.map_err(mv.try_into())?)))
                    }
                    HostObject::U64(u) => Ok(ScObject::U64(*u)),
                    HostObject::I64(i) => Ok(ScObject::I64(*i)),
                    HostObject::Bin(b) => Ok(ScObject::Binary(self.map_err(b.clone().try_into())?)),
                    HostObject::BigInt(bi) => {
                        let (sign, data) = bi.to_bytes_be();
                        match sign {
                            Sign::Minus => Ok(ScObject::BigInt(ScBigInt::Negative(
                                self.map_err(data.try_into())?,
                            ))),
                            Sign::NoSign => Ok(ScObject::BigInt(ScBigInt::Zero)),
                            Sign::Plus => Ok(ScObject::BigInt(ScBigInt::Positive(
                                self.map_err(data.try_into())?,
                            ))),
                        }
                    }
                    HostObject::Hash(_) => todo!(),
                    HostObject::PublicKey(_) => todo!(),
                },
            })
        }
    }

    pub(crate) fn to_host_obj(&self, ob: &ScObject) -> Result<HostObj, HostError> {
        match ob {
            ScObject::Vec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    vv.push_back(self.to_host_val(e)?);
                }
                self.add_host_object(vv)
            }
            ScObject::Map(m) => {
                let mut mm = OrdMap::new();
                for pair in m.0.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.insert(k, v);
                }
                self.add_host_object(mm)
            }
            ScObject::U64(u) => self.add_host_object(*u),
            ScObject::I64(i) => self.add_host_object(*i),
            ScObject::Binary(b) => self.add_host_object::<Vec<u8>>(b.clone().into()),
            ScObject::BigInt(sbi) => {
                let bi = match sbi {
                    ScBigInt::Zero => BigInt::default(),
                    ScBigInt::Positive(bytes) => BigInt::from_bytes_be(Sign::Plus, bytes.as_ref()),
                    ScBigInt::Negative(bytes) => BigInt::from_bytes_be(Sign::Minus, bytes.as_ref()),
                };
                self.add_host_object(bi)
            }
            ScObject::Hash(_) => todo!(),
            ScObject::PublicKey(_) => todo!(),
        }
    }

    pub(crate) fn charge_for_new_host_object(
        &self,
        ho: HostObject,
    ) -> Result<HostObject, HostError> {
        match &ho {
            HostObject::Vec(v) => {
                self.charge_budget(CostType::HostVecAllocVec, 1)?;
                self.charge_budget(CostType::HostVecAllocCell, v.len() as u64)?;
            }
            HostObject::Map(m) => {
                self.charge_budget(CostType::HostMapAllocMap, 1)?;
                self.charge_budget(CostType::HostMapAllocCell, m.len() as u64)?;
            }
            HostObject::U64(_) => {}
            HostObject::I64(_) => {}
            HostObject::Bin(_) => {}
            HostObject::BigInt(_) => {}
            HostObject::Hash(_) => {}
            HostObject::PublicKey(_) => {}
        }
        Ok(ho)
    }

    /// Moves a value of some type implementing [`HostObjectType`] into the host's
    /// object array, returning a [`HostObj`] containing the new object's array
    /// index, tagged with the [`xdr::ScObjectType`] and associated with the current
    /// host via a weak reference.
    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<HostObj, HostError> {
        let handle = self.0.objects.borrow().len();
        if handle > u32::MAX as usize {
            return Err(self.err_status(ScHostObjErrorCode::ObjectCountExceedsU32Max));
        }
        self.0
            .objects
            .borrow_mut()
            .push(self.charge_for_new_host_object(HOT::inject(hot))?);
        let env = WeakHost(Rc::downgrade(&self.0));
        let v = Object::from_type_and_handle(HOT::get_type(), handle as u32);
        Ok(v.into_env_val(&env))
    }

    /// Converts a [`RawVal`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    fn to_storage_key(&self, k: RawVal) -> Result<LedgerKey, HostError> {
        Ok(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: self.get_current_contract_id()?,
            key: self.from_host_val(k)?,
        }))
    }

    fn create_contract_helper(
        &self,
        contract: Object,
        id_preimage: Vec<u8>,
    ) -> Result<Object, HostError> {
        let id_obj = self.compute_hash_sha256(self.add_host_object(id_preimage)?.into())?;
        let new_contract_id = self.hash_from_rawval_input("id_obj", id_obj)?;

        let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: new_contract_id.clone(),
            key: ScVal::Static(ScStatic::LedgerKeyContractCodeWasm),
        });

        if self.0.storage.borrow_mut().has(&storage_key)? {
            return Err(self.err_general("Contract already exists"));
        }

        let val = self.from_host_obj(contract)?;

        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: new_contract_id,
            key: ScVal::Static(ScStatic::LedgerKeyContractCodeWasm),
            val: ScVal::Object(Some(val)),
        });
        let val = LedgerEntry {
            last_modified_ledger_seq: 0,
            data,
            ext: LedgerEntryExt::V0,
        };
        self.0.storage.borrow_mut().put(&storage_key, &val)?;

        Ok(id_obj)
    }

    #[cfg(feature = "vm")]
    fn call_wasm_fn(&self, id: &Hash, func: &Symbol, args: &[RawVal]) -> Result<RawVal, HostError> {
        // Create key for storage
        let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
        let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: id.clone(),
            key,
        });
        // Retrieve the contract code and create vm
        let scval = match self.0.storage.borrow_mut().get(&storage_key)?.data {
            LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
            _ => Err(self.err_status(ScHostStorageErrorCode::ExpectContractData)),
        }?;
        let code = match scval {
            ScVal::Object(Some(ScObject::Binary(b))) => b,
            _ => {
                return Err(self.err_status_msg(
                    ScHostValErrorCode::UnexpectedValType,
                    "ledger entry for contract code is not a binary object",
                ))
            }
        };
        let vm = Vm::new(self, id.clone(), code.as_slice())?;
        // Resolve the function symbol and invoke contract call
        vm.invoke_function_raw(self, SymbolStr::from(func).as_ref(), args)
    }

    fn call_n(&self, contract: Object, func: Symbol, args: &[RawVal]) -> Result<RawVal, HostError> {
        // Get contract ID
        let id = self.hash_from_rawval_input("contract", contract)?;

        #[cfg(feature = "testutils")]
        {
            // This looks a little un-idiomatic, but this avoids maintaining a borrow of
            // self.0.contracts. Implementing it as
            //
            //     if let Some(cfs) = self.0.contracts.borrow().get(&id).cloned() { ... }
            //
            // maintains a borrow of self.0.contracts, which can cause borrow errors.
            let cfs_option = self.0.contracts.borrow().get(&id).cloned();
            if let Some(cfs) = cfs_option {
                let mut fg = self.push_test_frame(id.clone());
                let res = cfs
                    .call(&func, self, args)
                    .ok_or_else(|| self.err_general("function not found"))?;
                fg.commit();
                return Ok(res);
            }
        }

        #[cfg(feature = "vm")]
        return self.call_wasm_fn(&id, &func, args);
        #[cfg(not(feature = "vm"))]
        return Err(self.err_general("could not dispatch"));
    }

    pub fn invoke_function(&mut self, hf: HostFunction, args: ScVec) -> Result<ScVal, HostError> {
        match hf {
            HostFunction::Call => {
                if let [ScVal::Object(Some(scobj)), ScVal::Symbol(scsym), rest @ ..] =
                    args.as_slice()
                {
                    let mut frame_guard = self.push_host_function_frame(hf);

                    let object: Object = self.to_host_obj(scobj)?.to_object();
                    let symbol: Symbol = scsym.try_into()?;
                    let mut raw_args: Vec<RawVal> = Vec::new();
                    for scv in rest.iter() {
                        raw_args.push(self.to_host_val(scv)?.val);
                    }
                    let res = self.call_n(object, symbol, &raw_args[..])?;
                    frame_guard.commit();
                    Ok(self.from_host_val(res)?)
                } else {
                    Err(self.err_status_msg(
                        ScHostFnErrorCode::InputArgsWrongLength,
                        "unexpected arguments to 'call' host function",
                    ))
                }
            }
            HostFunction::CreateContract => {
                if let [ScVal::Object(Some(c_obj)), ScVal::Object(Some(s_obj)), ScVal::Object(Some(k_obj)), ScVal::Object(Some(sig_obj))] =
                    args.as_slice()
                {
                    let mut frame_guard = self.push_host_function_frame(hf);

                    let contract: Object = self.to_host_obj(c_obj)?.to_object();
                    let salt: Object = self.to_host_obj(s_obj)?.to_object();
                    let key: Object = self.to_host_obj(k_obj)?.to_object();
                    let signature: Object = self.to_host_obj(sig_obj)?.to_object();

                    //TODO: should create_contract_from_ed25519 return a RawVal instead of Object to avoid this conversion?
                    let res = self.create_contract_from_ed25519(contract, salt, key, signature)?;
                    let sc_obj = self.from_host_obj(res)?;
                    frame_guard.commit();
                    Ok(ScVal::Object(Some(sc_obj)))
                } else {
                    Err(self.err_status_msg(
                        ScHostFnErrorCode::InputArgsWrongLength,
                        "unexpected arguments to 'CreateContract' host function",
                    ))
                }
            }
        }
    }

    fn load_account(&self, a: Object) -> Result<AccountEntry, HostError> {
        use xdr::LedgerKeyAccount;
        let acc = xdr::LedgerKey::Account(LedgerKeyAccount {
            account_id: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(a)?)),
        });
        self.visit_storage(|storage| match storage.get(&acc)?.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(self.err_general("not account")),
        })
    }

    #[cfg(feature = "testutils")]
    pub fn register_test_contract(
        &self,
        contract_id: Object,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        self.visit_obj(contract_id, |bin: &Vec<u8>| {
            let mut contracts = self.0.contracts.borrow_mut();
            let hash = Hash(
                bin.clone()
                    .try_into()
                    .map_err(|_| self.err_general("bad contract id"))?,
            );
            if !contracts.contains_key(&hash) {
                contracts.insert(hash, contract_fns);
                Ok(())
            } else {
                Err(self.err_general("vtable already exists"))
            }
        })
    }
}

impl EnvBase for Host {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }

    fn deep_clone(&self) -> Self {
        // Step 1: naive deep-clone the HostImpl. At this point some of the
        // objects in new_host may have WeakHost refs to the old host.
        let new_host = Host(Rc::new((*self.0).clone()));

        // Step 2: adjust all the objects that have internal WeakHost refs
        // to point to a weakhost associated with the new host. There are
        // only a few of these.
        let new_weak = new_host.get_weak();
        for hobj in new_host.0.objects.borrow_mut().iter_mut() {
            match hobj {
                HostObject::Vec(vs) => {
                    vs.iter_mut().for_each(|v| v.env = new_weak.clone());
                }
                HostObject::Map(m) => {
                    *m = m
                        .clone()
                        .into_iter()
                        .map(|(mut k, mut v)| {
                            k.env = new_weak.clone();
                            v.env = new_weak.clone();
                            (k, v)
                        })
                        .collect();
                }
                _ => (),
            }
        }
        new_host
    }

    fn binary_copy_from_slice(&self, b: Object, b_pos: RawVal, mem: &[u8]) -> Object {
        // This is only called from native contracts, either when testing or
        // when the contract is otherwise linked into the same address space as
        // us. We therefore access the memory we were passed directly.
        //
        // This is also why we _panic_ on errors in here, rather than attempting
        // to return a recoverable error code: native contracts that call this
        // function do so through APIs that _should_ never pass bad data.
        //
        // TODO: we may revisit this choice of panicing in the future, depending
        // on how we choose to try to contain panics-caused-by-native-contracts.
        let b_pos = u32::try_from(b_pos).expect("pos input is not u32");
        let len = u32::try_from(mem.len()).expect("slice len exceeds u32");
        let mut vnew = self
            .visit_obj(b, |hv: &Vec<u8>| Ok(hv.clone()))
            .expect("access to unknown host binary object");
        let end_idx = b_pos.checked_add(len).expect("u32 overflow") as usize;
        // TODO: we currently grow the destination vec if it's not big enough,
        // make sure this is desirable behaviour.
        if end_idx > vnew.len() {
            vnew.resize(end_idx, 0);
        }
        let write_slice = &mut vnew[b_pos as usize..end_idx];
        write_slice.copy_from_slice(mem);
        self.add_host_object(vnew)
            .expect("unable to add host object")
            .into()
    }

    fn binary_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) {
        let b_pos = u32::try_from(b_pos).expect("pos input is not u32");
        let len = u32::try_from(mem.len()).expect("slice len exceeds u32");
        self.visit_obj(b, move |hv: &Vec<u8>| {
            let end_idx = b_pos.checked_add(len).expect("u32 overflow") as usize;
            if end_idx > hv.len() {
                panic!("index out of bounds");
            }
            mem.copy_from_slice(&hv.as_slice()[b_pos as usize..end_idx]);
            Ok(())
        })
        .expect("access to unknown host object");
    }

    fn binary_new_from_slice(&self, mem: &[u8]) -> Object {
        self.add_host_object::<Vec<u8>>(mem.into())
            .expect("unable to add host binary object")
            .into()
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) {
        self.debug_event(DebugEvent::new().msg(fmt).arg(v))
            .expect("unable to record debug event")
    }

    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) {
        self.debug_event(DebugEvent::new().msg(fmt).arg(s))
            .expect("unable to record debug event")
    }

    fn log_static_fmt_val_static_str(&self, fmt: &'static str, v: RawVal, s: &'static str) {
        self.debug_event(DebugEvent::new().msg(fmt).arg(v).arg(s))
            .expect("unable to record debug event")
    }

    fn log_static_fmt_general(&self, fmt: &'static str, vals: &[RawVal], strs: &[&'static str]) {
        let mut evt = DebugEvent::new().msg(fmt);
        for v in vals {
            evt = evt.arg(*v)
        }
        for s in strs {
            evt = evt.arg(*s)
        }
        self.debug_event(evt).expect("unable to record debug event")
    }
}

impl CheckedEnv for Host {
    type Error = HostError;

    fn log_value(&self, v: RawVal) -> Result<RawVal, HostError> {
        self.debug_event(DebugEvent::new().msg("log").arg(v))?;
        Ok(RawVal::from_void())
    }

    fn get_invoking_contract(&self) -> Result<Object, HostError> {
        let frames = self.0.context.borrow();
        // the previous frame must exist and must be a contract
        let hash: Hash = if frames.len() >= 2 {
            match &frames[frames.len() - 2] {
                #[cfg(feature = "vm")]
                Frame::ContractVM(vm) => Ok(vm.contract_id.clone()),
                Frame::HostFunction(_) => {
                    Err(self.err_general("Host function context has no contract ID"))
                }
                #[cfg(feature = "testutils")]
                Frame::TestContract(id) => Ok(id.clone()),
            }
        } else {
            Err(self.err_general("no invoking contract"))
        }?;
        let bin: Vec<u8> = hash
            .0
            .try_into()
            .map_err(|_| self.err_convert_general("Hash has wrong size"))?;
        Ok(self.add_host_object(bin)?.into())
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> Result<i64, HostError> {
        self.charge_budget(CostType::HostFunction, 2)?;
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| {
                self.unchecked_visit_val_obj(b, |bo| Ok(ao.cmp(&bo)))
            })?
        };
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
    }

    fn contract_event(&self, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn system_event(&self, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_current_contract(&self) -> Result<Object, HostError> {
        let hash: Hash = self.get_current_contract_id()?;
        let bin: Vec<u8> = hash
            .0
            .try_into()
            .map_err(|_| self.err_convert_general("Hash has wrong size"))?;
        Ok(self.add_host_object(bin)?.into())
    }

    fn obj_from_u64(&self, u: u64) -> Result<Object, HostError> {
        Ok(self.add_host_object(u)?.into())
    }

    fn obj_to_u64(&self, obj: Object) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &u64| Ok(*u))
    }

    fn obj_from_i64(&self, i: i64) -> Result<Object, HostError> {
        Ok(self.add_host_object(i)?.into())
    }

    fn obj_to_i64(&self, obj: Object) -> Result<i64, HostError> {
        self.visit_obj(obj, |i: &i64| Ok(*i))
    }

    fn map_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(HostMap::new())?.into())
    }

    fn map_put(&self, m: Object, k: RawVal, v: RawVal) -> Result<Object, HostError> {
        let k = self.associate_raw_val(k);
        let v = self.associate_raw_val(v);
        let mnew = self.visit_obj(m, move |hm: &HostMap| {
            let mut mnew = hm.clone();
            mnew.insert(k, v);
            Ok(mnew)
        })?;
        Ok(self.add_host_object(mnew)?.into())
    }

    fn map_get(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, move |hm: &HostMap| match hm.get(&k) {
            Some(v) => Ok(v.to_raw()),
            None => Err(self.err_general("map key not found")),
        })
    }

    fn map_del(&self, m: Object, k: RawVal) -> Result<Object, HostError> {
        let k = self.associate_raw_val(k);
        let mnew = self.visit_obj(m, |hm: &HostMap| {
            let mut mnew = hm.clone();
            match mnew.remove(&k) {
                Some(v) => Ok(mnew),
                None => Err(self.err_general("map key not found")),
            }
        })?;
        Ok(self.add_host_object(mnew)?.into())
    }

    fn map_len(&self, m: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(m, |hm: &HostMap| Ok(hm.len()))?;
        self.usize_to_rawval_u32(len)
    }

    fn map_has(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, move |hm: &HostMap| Ok(hm.contains_key(&k).into()))
    }

    fn map_prev_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, |hm: &HostMap| {
            // OrdMap's `get_prev`/`get_next` return the previous/next key if the input key is not found.
            // Otherwise it returns the input key. Therefore, if `get_prev`/`get_next` returns the same
            // key, we will clone the map, delete the input key, and call `get_prev`/`get_next` again.
            // Note on performance: OrdMap does "lazy cloning", which only happens if data is modified,
            // and we are only modifying one entry. The cloned object will be thrown away in the end
            // so there is no cost associated with host object creation and allocation.
            if let Some((pk, pv)) = hm.get_prev(&k) {
                if *pk != k {
                    Ok(pk.to_raw())
                } else {
                    if let Some((pk2, pv2)) = hm
                        .clone()
                        .extract(pk) // removes (pk, pv) and returns an Option<(pv, updated_map)>
                        .ok_or_else(|| self.err_general("key not exist"))?
                        .1
                        .get_prev(pk)
                    {
                        Ok(pk2.to_raw())
                    } else {
                        Ok(UNKNOWN_ERROR.to_raw()) //FIXME: replace with the actual status code
                    }
                }
            } else {
                Ok(UNKNOWN_ERROR.to_raw()) //FIXME: replace with the actual status code
            }
        })
    }

    fn map_next_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, pv)) = hm.get_next(&k) {
                if *pk != k {
                    Ok(pk.to_raw())
                } else {
                    if let Some((pk2, pv2)) = hm
                        .clone()
                        .extract(pk) // removes (pk, pv) and returns an Option<(pv, updated_map)>
                        .ok_or_else(|| self.err_general("key not exist"))?
                        .1
                        .get_next(pk)
                    {
                        Ok(pk2.to_raw())
                    } else {
                        Ok(UNKNOWN_ERROR.to_raw()) //FIXME: replace with the actual status code
                    }
                }
            } else {
                Ok(UNKNOWN_ERROR.to_raw()) //FIXME: replace with the actual status code
            }
        })
    }

    fn map_min_key(&self, m: Object) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_min() {
            Some((pk, pv)) => Ok(pk.to_raw()),
            None => Ok(UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
        })
    }

    fn map_max_key(&self, m: Object) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_max() {
            Some((pk, pv)) => Ok(pk.to_raw()),
            None => Ok(UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
        })
    }

    fn map_keys(&self, m: Object) -> Result<Object, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            let cap: u32 = hm
                .len()
                .try_into()
                .map_err(|_| self.err_general("host map too large"))?;
            let mut vec = self.vec_new(cap.into())?;
            for k in hm.keys() {
                vec = self.vec_push(vec, k.to_raw())?;
            }
            Ok(vec)
        })
    }

    fn map_values(&self, m: Object) -> Result<Object, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            let cap: u32 = hm
                .len()
                .try_into()
                .map_err(|_| self.err_general("host map too large"))?;
            let mut vec = self.vec_new(cap.into())?;
            for k in hm.values() {
                vec = self.vec_push(vec, k.to_raw())?;
            }
            Ok(vec)
        })
    }

    fn vec_new(&self, c: RawVal) -> Result<Object, HostError> {
        let capacity: usize = if c.is_void() {
            0
        } else {
            self.usize_from_rawval_u32_input("c", c)?
        };
        // TODO: optimize the vector based on capacity
        Ok(self.add_host_object(HostVec::new())?.into())
    }

    fn vec_put(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i: usize = self.usize_from_rawval_u32_input("i", i)?;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i >= hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.set(i, x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_get(&self, v: Object, i: RawVal) -> Result<RawVal, HostError> {
        let i: usize = self.usize_from_rawval_u32_input("i", i)?;
        self.visit_obj(v, move |hv: &HostVec| match hv.get(i) {
            None => {
                Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"))
            }
            Some(hval) => Ok(hval.to_raw()),
        })
    }

    fn vec_del(&self, v: Object, i: RawVal) -> Result<Object, HostError> {
        let i: usize = self.usize_from_rawval_u32_input("i", i)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i >= hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.remove(i);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_len(&self, v: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        self.usize_to_rawval_u32(len)
    }

    fn vec_push(&self, v: Object, x: RawVal) -> Result<Object, HostError> {
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let mut vnew = hv.clone();
            vnew.push_back(x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop(&self, v: Object) -> Result<Object, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let mut vnew = hv.clone();
            match vnew.pop_back() {
                None => Err(self.err_status_msg(
                    ScHostObjErrorCode::VecIndexOutOfBound,
                    "value does not exist",
                )),
                Some(_) => Ok(vnew),
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_front(&self, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| match hv.front() {
            None => Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "value does not exist",
            )),
            Some(front) => Ok(front.to_raw()),
        })
    }

    fn vec_back(&self, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| match hv.back() {
            None => Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "value does not exist",
            )),
            Some(back) => Ok(back.to_raw()),
        })
    }

    fn vec_insert(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i > hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.insert(i, x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_append(&self, v1: Object, v2: Object) -> Result<Object, HostError> {
        let mut vnew = self.visit_obj(v1, |hv: &HostVec| Ok(hv.clone()))?;
        let v2 = self.visit_obj(v2, |hv: &HostVec| Ok(hv.clone()))?;
        if v2.len() > u32::MAX as usize - vnew.len() {
            return Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"));
        }
        vnew.append(v2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_slice(&self, v: Object, start: RawVal, end: RawVal) -> Result<Object, HostError> {
        let start = self.usize_from_rawval_u32_input("start", start)?;
        let end = self.usize_from_rawval_u32_input("end", end)?;
        if start > end {
            return Err(self.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "start greater than end",
            ));
        }
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if start > hv.len() {
                return Err(self.err_status_msg(
                    ScHostObjErrorCode::VecIndexOutOfBound,
                    "start out of bounds",
                ));
            }
            if end > hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "end out of bounds"));
            }
            Ok(hv.clone().slice(start..end))
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn put_contract_data(&self, k: RawVal, v: RawVal) -> Result<RawVal, HostError> {
        let data_key = self.from_host_val(k)?;
        if data_key == ScVal::Static(ScStatic::LedgerKeyContractCodeWasm) {
            return Err(self.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "cannot update contract code",
            ));
        }

        let key = self.to_storage_key(k)?;
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: self.get_current_contract_id()?,
            key: self.from_host_val(k)?,
            val: self.from_host_val(v)?,
        });
        let val = LedgerEntry {
            last_modified_ledger_seq: 0,
            data,
            ext: LedgerEntryExt::V0,
        };
        self.0.storage.borrow_mut().put(&key, &val)?;
        Ok(().into())
    }

    fn has_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        let key = self.to_storage_key(k)?;
        let res = self.0.storage.borrow_mut().has(&key)?;
        Ok(RawVal::from_bool(res))
    }

    fn get_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        let key = self.to_storage_key(k)?;
        match self.0.storage.borrow_mut().get(&key)?.data {
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

    fn del_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        if self.from_host_val(k)? == ScVal::Static(ScStatic::LedgerKeyContractCodeWasm) {
            return Err(self.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "cannot delete contract code",
            ));
        }

        let key = self.to_storage_key(k)?;
        self.0.storage.borrow_mut().del(&key)?;
        Ok(().into())
    }

    fn create_contract_from_ed25519(
        &self,
        v: Object,
        salt: Object,
        key: Object,
        sig: Object,
    ) -> Result<Object, HostError> {
        let salt_val = self.uint256_from_rawval_input("salt", salt)?;
        let key_val = self.uint256_from_rawval_input("key", key)?;

        // Verify parameters
        let params = self.visit_obj(v, |bin: &Vec<u8>| {
            let separator = "create_contract_from_ed25519(contract: Vec<u8>, salt: u256, key: u256, sig: Vec<u8>)";
            let params = [separator.as_bytes(), salt_val.as_ref(), bin].concat();
            Ok(params)
        })?;
        let hash = self.compute_hash_sha256(self.add_host_object(params)?.into())?;
        self.verify_sig_ed25519(hash, key, sig)?;

        //Create contract and contractID
        let pre_image =
            xdr::HashIdPreimage::ContractIdFromEd25519(xdr::HashIdPreimageEd25519ContractId {
                ed25519: key_val,
                salt: salt_val,
            });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| self.err_general("invalid hash"))?;
        self.create_contract_helper(v, buf)
    }

    fn create_contract_from_contract(&self, v: Object, salt: Object) -> Result<Object, HostError> {
        let contract_id = self.get_current_contract_id()?;
        let salt_val = self.uint256_from_rawval_input("salt", salt)?;

        let pre_image =
            xdr::HashIdPreimage::ContractIdFromContract(xdr::HashIdPreimageContractId {
                contract_id,
                salt: salt_val,
            });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| self.err_general("invalid hash"))?;

        self.create_contract_helper(v, buf)
    }

    fn call(&self, contract: Object, func: Symbol, args: Object) -> Result<RawVal, HostError> {
        let args: Vec<RawVal> = self.visit_obj(args, |hv: &HostVec| {
            Ok(hv.iter().map(|a| a.to_raw()).collect())
        })?;
        self.call_n(contract, func, args.as_slice())
    }

    fn try_call(&self, contract: Object, func: Symbol, args: Object) -> Result<RawVal, HostError> {
        match self.call(contract, func, args) {
            Ok(rv) => Ok(rv),
            Err(e) => {
                let evt = DebugEvent::new()
                    .msg("try_call got error from callee contract")
                    .arg::<RawVal>(e.status.clone().into());
                self.debug_event(evt)?;
                Ok(e.status.into())
            }
        }
    }

    fn bigint_from_u64(&self, x: u64) -> Result<Object, HostError> {
        Ok(self.add_host_object(Into::<BigInt>::into(x))?.into())
    }

    fn bigint_to_u64(&self, x: Object) -> Result<u64, HostError> {
        self.visit_obj(x, |bi: &BigInt| {
            bi.to_u64().ok_or_else(|| self.err_convert::<u64>(x.into()))
        })
    }

    fn bigint_from_i64(&self, x: i64) -> Result<Object, HostError> {
        Ok(self.add_host_object(Into::<BigInt>::into(x))?.into())
    }

    fn bigint_to_i64(&self, x: Object) -> Result<i64, HostError> {
        self.visit_obj(x, |bi: &BigInt| {
            bi.to_i64().ok_or_else(|| self.err_convert::<i64>(x.into()))
        })
    }

    fn bigint_add(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| self.visit_obj(y, |b: &BigInt| Ok(a.add(b))))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_sub(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| self.visit_obj(y, |b: &BigInt| Ok(a.sub(b))))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_mul(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| self.visit_obj(y, |b: &BigInt| Ok(a.mul(b))))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_div(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| {
                if b.is_zero() {
                    return Err(self.err_general("bigint division by zero"));
                }
                Ok(a.div(b))
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_rem(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| {
                if b.is_zero() {
                    return Err(self.err_general("bigint division by zero"));
                }
                Ok(a.rem(b))
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_and(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| Ok(a.bitand(b)))
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_or(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| Ok(a.bitor(b)))
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_xor(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| Ok(a.bitxor(b)))
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_shl(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| {
                if b.is_negative() {
                    return Err(self.err_general("attempt to shift left with negative"));
                }
                if let Some(u) = b.to_usize() {
                    Ok(a.shl(u))
                } else {
                    Err(self.err_general("left-shift overflow"))
                }
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_shr(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| {
                if b.is_negative() {
                    return Err(self.err_general("attempt to shift right with negative"));
                }
                if let Some(u) = b.to_usize() {
                    Ok(a.shr(u))
                } else {
                    Err(self.err_general("right-shift overflow"))
                }
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_cmp(&self, x: Object, y: Object) -> Result<RawVal, HostError> {
        self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| Ok((a.cmp(b) as i32).into()))
        })
    }

    fn bigint_is_zero(&self, x: Object) -> Result<RawVal, HostError> {
        self.visit_obj(x, |a: &BigInt| Ok(a.is_zero().into()))
    }

    fn bigint_neg(&self, x: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| Ok(a.neg()))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_not(&self, x: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| Ok(a.not()))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_gcd(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| self.visit_obj(y, |b: &BigInt| Ok(a.gcd(b))))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_lcm(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| self.visit_obj(y, |b: &BigInt| Ok(a.lcm(b))))?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_pow(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |e: &BigInt| {
                if e.is_negative() {
                    return Err(self.err_general("negative exponentiation not supported"));
                }
                if let Some(u) = e.to_usize() {
                    Ok(Pow::pow(a, u))
                } else {
                    Err(self.err_general("pow overflow"))
                }
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_pow_mod(&self, p: Object, q: Object, m: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(p, |a: &BigInt| {
            self.visit_obj(q, |exponent: &BigInt| {
                if exponent.is_negative() {
                    return Err(self.err_general("negative exponentiation not supported"));
                }
                self.visit_obj(m, |modulus: &BigInt| {
                    if modulus.is_zero() {
                        return Err(self.err_general("zero modulus not supported"));
                    }
                    Ok(a.modpow(exponent, modulus))
                })
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_sqrt(&self, x: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            if a.is_negative() {
                return Err(self.err_general("sqrt is imaginary"));
            }
            Ok(a.sqrt())
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_bits(&self, x: Object) -> Result<u64, HostError> {
        self.visit_obj(x, |a: &BigInt| Ok(a.bits()))
    }

    fn bigint_to_bytes_be(&self, x: Object) -> Result<Object, Self::Error> {
        let bytes: Vec<u8> = self.visit_obj(x, |a: &BigInt| Ok(a.to_bytes_be().1))?;
        Ok(self.add_host_object(bytes)?.into())
    }

    fn bigint_to_radix_be(&self, x: Object, radix: RawVal) -> Result<Object, Self::Error> {
        let r: u32 = self.u32_from_rawval_input("radix", radix)?;
        let bytes: Vec<u8> = self.visit_obj(x, |a: &BigInt| Ok(a.to_radix_be(r).1))?;
        Ok(self.add_host_object(bytes)?.into())
    }

    fn serialize_to_binary(&self, v: RawVal) -> Result<Object, HostError> {
        let scv = self.from_host_val(v)?;
        let mut buf = Vec::<u8>::new();
        scv.write_xdr(&mut buf)
            .map_err(|_| self.err_general("failed to serialize ScVal"))?;
        Ok(self.add_host_object(buf)?.into())
    }

    fn deserialize_from_binary(&self, b: Object) -> Result<RawVal, HostError> {
        let scv = self.visit_obj(b, |hv: &Vec<u8>| {
            ScVal::read_xdr(&mut hv.as_slice())
                .map_err(|_| self.err_general("failed to de-serialize ScVal"))
        })?;
        Ok(self.to_host_val(&scv)?.into())
    }

    fn binary_copy_to_linear_memory(
        &self,
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
                let end_idx = b_pos.checked_add(len).ok_or_else(|| {
                    self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
                })? as usize;
                if end_idx > hv.len() {
                    return Err(self.err_status_msg(
                        ScHostObjErrorCode::VecIndexOutOfBound,
                        "index out of bound",
                    ));
                }
                vm.with_memory_access(self, |mem| {
                    self.map_err(mem.set(pos, &hv.as_slice()[b_pos as usize..end_idx]))
                })?;
                Ok(().into())
            })
        }
    }

    fn binary_copy_from_linear_memory(
        &self,
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
            let mut vnew = self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.clone()))?;
            let end_idx = b_pos.checked_add(len).ok_or_else(|| {
                self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
            })? as usize;
            // TODO: we currently grow the destination vec if it's not big enough,
            // make sure this is desirable behaviour.
            if end_idx > vnew.len() {
                vnew.resize(end_idx, 0);
            }
            vm.with_memory_access(self, |mem| {
                Ok(self.map_err(
                    mem.get_into(pos, &mut vnew.as_mut_slice()[b_pos as usize..end_idx]),
                )?)
            })?;
            Ok(self.add_host_object(vnew)?.into())
        }
    }

    fn binary_new_from_linear_memory(
        &self,
        lm_pos: RawVal,
        len: RawVal,
    ) -> Result<Object, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            let mut vnew: Vec<u8> = vec![0; len as usize];
            vm.with_memory_access(self, |mem| {
                self.map_err(mem.get_into(pos, vnew.as_mut_slice()))
            })?;
            Ok(self.add_host_object(vnew)?.into())
        }
    }

    fn binary_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(Vec::<u8>::new())?.into())
    }

    fn binary_put(&self, b: Object, i: RawVal, u: RawVal) -> Result<Object, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.clone();
            if i >= hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let _old_val = std::mem::replace(&mut vnew[i], u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_get(&self, b: Object, i: RawVal) -> Result<RawVal, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        self.visit_obj(b, move |hv: &Vec<u8>| match hv.get(i) {
            None => {
                Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"))
            }
            Some(u) => Ok((*u).into()),
        })
    }

    fn binary_del(&self, b: Object, i: RawVal) -> Result<Object, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if i >= hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.remove(i);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_len(&self, b: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.len()))?;
        self.usize_to_rawval_u32(len)
    }

    fn binary_push(&self, b: Object, u: RawVal) -> Result<Object, HostError> {
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.clone();
            vnew.push(u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_pop(&self, b: Object) -> Result<Object, HostError> {
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.clone();
            match vnew.pop() {
                None => {
                    Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"))
                }
                Some(_) => Ok(vnew),
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_front(&self, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            if hv.is_empty() {
                return Err(
                    self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
                );
            }
            Ok(hv[0].into())
        })
    }

    fn binary_back(&self, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            if hv.is_empty() {
                return Err(
                    self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
                );
            }
            Ok(hv[hv.len() - 1].into())
        })
    }

    fn binary_insert(&self, b: Object, i: RawVal, u: RawVal) -> Result<Object, HostError> {
        let i = self.usize_from_rawval_u32_input("i", i)?;
        let u = self.u8_from_rawval_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if i > hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.insert(i, u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_append(&self, b1: Object, b2: Object) -> Result<Object, HostError> {
        let mut vnew = self.visit_obj(b1, |hv: &Vec<u8>| Ok(hv.clone()))?;
        let mut b2 = self.visit_obj(b2, |hv: &Vec<u8>| Ok(hv.clone()))?;
        if b2.len() > u32::MAX as usize - vnew.len() {
            return Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"));
        }
        vnew.append(&mut b2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_slice(&self, b: Object, start: RawVal, end: RawVal) -> Result<Object, HostError> {
        let start = self.usize_from_rawval_u32_input("start", start)?;
        let end = self.usize_from_rawval_u32_input("end", end)?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if start > hv.len() {
                return Err(self.err_status_msg(
                    ScHostObjErrorCode::VecIndexOutOfBound,
                    "start out of bounds",
                ));
            }
            if end > hv.len() {
                return Err(self
                    .err_status_msg(ScHostObjErrorCode::VecIndexOutOfBound, "end out of bounds"));
            }
            Ok(hv.as_slice()[start..end].to_vec())
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn hash_from_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn hash_to_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn public_key_from_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn public_key_to_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn compute_hash_sha256(&self, x: Object) -> Result<Object, HostError> {
        use sha2::{Digest, Sha256};
        let hash = self.visit_obj(x, |bin: &Vec<u8>| {
            Ok(Sha256::digest(bin).as_slice().to_vec())
        })?;

        if hash.len() != 32 {
            return Err(self.err_general("incorrect hash size"));
        }

        Ok(self.add_host_object(hash)?.into())
    }

    fn verify_sig_ed25519(&self, x: Object, k: Object, s: Object) -> Result<RawVal, HostError> {
        use ed25519_dalek::{PublicKey, Signature, Verifier};

        let public_key: PublicKey = self.visit_obj(k, |bin: &Vec<u8>| {
            PublicKey::from_bytes(bin).map_err(|_| {
                self.err_status_msg(ScHostObjErrorCode::UnexpectedType, "invalid public key")
            })
        })?;

        let sig: Signature = self.visit_obj(s, |bin: &Vec<u8>| {
            Signature::from_bytes(bin).map_err(|_| {
                self.err_status_msg(
                    ScHostObjErrorCode::UnexpectedType,
                    "unable to decode signature",
                )
            })
        })?;

        let res = self.visit_obj(x, |bin: &Vec<u8>| {
            public_key
                .verify(bin, &sig)
                .map_err(|_| self.err_general("Failed ED25519 verification"))
        });
        Ok(res?.into())
    }

    fn account_get_low_threshold(&self, a: Object) -> Result<RawVal, Self::Error> {
        Ok(self.load_account(a)?.thresholds.0[ThresholdIndexes::Low as usize].into())
    }

    fn account_get_medium_threshold(&self, a: Object) -> Result<RawVal, Self::Error> {
        Ok(self.load_account(a)?.thresholds.0[ThresholdIndexes::Med as usize].into())
    }

    fn account_get_high_threshold(&self, a: Object) -> Result<RawVal, Self::Error> {
        Ok(self.load_account(a)?.thresholds.0[ThresholdIndexes::High as usize].into())
    }

    fn account_get_signer_weight(&self, a: Object, s: Object) -> Result<RawVal, Self::Error> {
        use xdr::{Signer, SignerKey};

        let target_signer = self.to_u256(s)?;

        let ae = self.load_account(a)?;
        if ae.account_id == AccountId(PublicKey::PublicKeyTypeEd25519(target_signer.clone())) {
            // Target signer is the master key, so return the master weight
            Ok(ae.thresholds.0[ThresholdIndexes::MasterWeight as usize].into())
        } else {
            // Target signer is not the master key, so search the account signers
            let signers: &Vec<Signer> = ae.signers.as_ref();
            for signer in signers {
                if let SignerKey::Ed25519(ref this_signer) = signer.key {
                    if &target_signer == this_signer {
                        // We've found the target signer in the account signers, so return the weight
                        return Ok(signer.weight.into());
                    }
                }
            }
            // We didn't find the target signer, so it must have no weight
            Ok(0u32.into())
        }
    }
}
