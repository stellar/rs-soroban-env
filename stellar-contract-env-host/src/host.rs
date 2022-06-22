#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use im_rc::{OrdMap, Vector};
use std::num::TryFromIntError;

use crate::budget::Budget;
use crate::storage::Storage;
use crate::weak_host::WeakHost;

use crate::xdr;
use crate::xdr::{
    ContractDataEntry, Hash, HostFunction, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyContractData, ScMap, ScMapEntry, ScObject, ScStatic, ScStatus, ScStatusType, ScVal,
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
    BitSet, BitSetError, EnvBase, IntoEnvVal, Object, RawVal, RawValConvertible, Status, Symbol,
    SymbolError, Tag, Val,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum HostError {
    #[error("general host error: {0}")]
    General(&'static str),
    #[error("XDR error")]
    XDRError(#[from] xdr::Error),
    #[cfg(feature = "vm")]
    #[error("WASMI error")]
    WASMIError(#[from] wasmi::Error),
    #[cfg(feature = "vm")]
    #[error("ParityWasmElements error")]
    ParityWasmElementsError(#[from] parity_wasm::elements::Error),
}

impl From<TryFromIntError> for HostError {
    fn from(_: TryFromIntError) -> Self {
        HostError::General("number out of range of u32")
    }
}

impl From<SymbolError> for HostError {
    fn from(_: SymbolError) -> Self {
        HostError::General("symbol error")
    }
}

impl From<BitSetError> for HostError {
    fn from(_: BitSetError) -> Self {
        HostError::General("bitset error")
    }
}

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
#[derive(Clone)]
struct RollbackPoint {
    storage: OrdMap<LedgerKey, Option<LedgerEntry>>,
    objects: usize,
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
}

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    context: RefCell<Vec<Frame>>,
    budget: RefCell<Budget>,
}

/// A guard struct that exists to call [`Host::pop_frame`] when it is dropped,
/// providing whatever rollback it is holding as an argument. By default, this
/// means that when a [`FrameGuard`] drops, it will roll the [`Host`] back to
/// the state it had before its associated [`Frame`] was pushed.
///
/// Users may call [`FrameGuard::commit`] to cause the rollback point to be set
/// to `None`, which will cause the [`FrameGuard`] to commit changes to the host
/// that occurred during its lifetime, rather than rollling them back.
pub(crate) struct FrameGuard {
    rollback: Option<RollbackPoint>,
    host: Host,
}

impl FrameGuard {
    pub(crate) fn commit(&mut self) {
        if self.rollback.is_none() {
            panic!("committing on already-committed FrameGuard")
        }
        self.rollback = None
    }
}

impl Drop for FrameGuard {
    fn drop(&mut self) {
        let rollback = self.rollback.take();
        self.host.pop_frame(rollback)
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
        }))
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    pub fn modify_budget<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&mut Budget) -> T,
    {
        f(&mut *self.0.budget.borrow_mut())
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
            .map_err(|rc_host_impl| Host(rc_host_impl))
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
            .ok_or(HostError::General("no contract currently running"))?)
    }

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    fn get_current_contract_id(&self) -> Result<Hash, HostError> {
        self.with_current_frame(|frame| match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm) => Ok(vm.contract_id.clone()),
            Frame::HostFunction(_) => Err(HostError::General(
                "Host function context has no contract ID",
            )),
        })
    }

    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: RawVal, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
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
                None => Err(HostError::General("unknown object reference")),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(HostError::General("unexpected host object type")),
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
        if val.is_u63() {
            Ok(ScVal::U63(unsafe { val.unchecked_as_u63() }))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe {
                    <u32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::I32(unsafe {
                    <i32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    if let Some(b) = <bool as RawValConvertible>::try_convert(val) {
                        if b {
                            Ok(ScVal::Static(ScStatic::True))
                        } else {
                            Ok(ScVal::Static(ScStatic::False))
                        }
                    } else if <() as RawValConvertible>::is_val_type(val) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else {
                        Err(HostError::General("unknown Tag::Static case"))
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    let scob = self.from_host_obj(ob)?;
                    Ok(ScVal::Object(Some(scob)))
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::Symbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    if status.is_ok() {
                        Ok(ScVal::Status(ScStatus::Ok))
                    } else if status.is_type(ScStatusType::UnknownError) {
                        Ok(ScVal::Status(ScStatus::UnknownError(status.get_code())))
                    } else {
                        Err(HostError::General("unknown Tag::Status case"))
                    }
                }
                Tag::Reserved => Err(HostError::General("Tag::Reserved value")),
            }
        }
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<HostVal, HostError> {
        let ok = match v {
            ScVal::U63(i) => {
                if *i >= 0 {
                    unsafe { RawVal::unchecked_from_u63(*i) }
                } else {
                    return Err(HostError::General("ScvU63 > i64::MAX"));
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Static(other) => RawVal::from_other_static(*other),
            ScVal::Object(None) => return Err(HostError::General("missing expected ScvObject")),
            ScVal::Object(Some(ob)) => return Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(HostError::General("non-UTF-8 in symbol")),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::Status(st) => {
                let status = match st {
                    ScStatus::Ok => Status::from_type_and_code(ScStatusType::Ok, 0),
                    ScStatus::UnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::UnknownError, *e)
                    }
                };
                status.into()
            }
        };
        Ok(self.associate_raw_val(ok))
    }

    pub(crate) fn from_host_obj(&self, ob: Object) -> Result<ScObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| match ob {
                None => Err(HostError::General("object not found")),
                Some(ho) => match ho {
                    HostObject::Vec(vv) => {
                        let mut sv = Vec::new();
                        for e in vv.iter() {
                            sv.push(self.from_host_val(e.val)?);
                        }
                        Ok(ScObject::Vec(ScVec(sv.try_into()?)))
                    }
                    HostObject::Map(mm) => {
                        let mut mv = Vec::new();
                        for (k, v) in mm.iter() {
                            let key = self.from_host_val(k.val)?;
                            let val = self.from_host_val(v.val)?;
                            mv.push(ScMapEntry { key, val });
                        }
                        Ok(ScObject::Map(ScMap(mv.try_into()?)))
                    }
                    HostObject::U64(u) => Ok(ScObject::U64(*u)),
                    HostObject::I64(i) => Ok(ScObject::I64(*i)),
                    HostObject::Bin(b) => Ok(ScObject::Binary(b.clone().try_into()?)),
                },
            })
        }
    }

    pub(crate) fn to_host_obj(&self, ob: &ScObject) -> Result<HostObj, HostError> {
        match ob {
            ScObject::Vec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    vv.push_back(self.to_host_val(e)?)
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
        }
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
            return Err(HostError::General("object handle exceeds u32::MAX"));
        }
        self.0.objects.borrow_mut().push(HOT::inject(hot));
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

    #[cfg(feature = "vm")]
    fn call_n(&self, contract: Object, func: Symbol, args: &[RawVal]) -> Result<RawVal, HostError> {
        // Create key for storage
        let id = self.visit_obj(contract, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin
                .as_slice()
                .try_into()
                .map_err(|_| HostError::General("invalid contract hash"))?;
            Ok(xdr::Hash(arr))
        })?;
        let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
        let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: id.clone(),
            key,
        });
        // Retrieve the contract code and create vm
        let scval = match self.0.storage.borrow_mut().get(&storage_key)?.data {
            LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
            _ => Err(HostError::General("expected contract data")),
        }?;
        let scobj = match scval {
            ScVal::Object(Some(ob)) => ob,
            _ => return Err(HostError::General("not an object")),
        };
        let code = match scobj {
            ScObject::Binary(b) => b,
            _ => return Err(HostError::General("not a binary object")),
        };
        let vm = Vm::new(&self, id, code.as_slice())?;
        // Resolve the function symbol and invoke contract call
        vm.invoke_function_raw(self, SymbolStr::from(func).as_ref(), args)
    }

    pub fn invoke_function(&mut self, hf: HostFunction, args: ScVec) -> Result<ScVal, HostError> {
        if let [ScVal::Object(Some(scobj)), ScVal::Symbol(scsym), rest @ ..] = args.as_slice() {
            let object: Object = self.to_host_obj(&scobj)?.to_object();
            let symbol: Symbol = scsym.as_slice().try_into()?;
            let mut raw_args: Vec<RawVal> = Vec::new();
            for scv in rest.iter() {
                raw_args.push(self.to_host_val(&scv)?.val);
            }
            let mut frame_guard = self.push_host_function_frame(hf);
            let raw_res = match hf {
                HostFunction::Call => {
                    #[cfg(not(feature = "vm"))]
                    unimplemented!();
                    #[cfg(feature = "vm")]
                    self.call_n(object, symbol, &raw_args[..])
                }
                _ => Err(HostError::General("bad host function")),
            }?;
            frame_guard.commit();
            Ok(self.from_host_val(raw_res)?)
        } else {
            return Err(HostError::General("unexpected args"));
        }
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
                    *m = HostMap::from_iter(m.clone().into_iter().map(|(mut k, mut v)| {
                        k.env = new_weak.clone();
                        v.env = new_weak.clone();
                        (k, v)
                    }))
                }
                _ => (),
            }
        }
        new_host
    }
}

impl CheckedEnv for Host {
    type Error = HostError;

    fn log_value(&self, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_last_operation_result(&self) -> Result<RawVal, HostError> {
        todo!()
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> Result<i64, HostError> {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| self.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
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
        todo!()
    }

    fn map_get(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_del(&self, m: Object, k: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn map_len(&self, m: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_has(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_prev_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_next_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_min_key(&self, m: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_max_key(&self, m: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_keys(&self, m: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn map_values(&self, m: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(HostVec::new())?.into())
    }

    fn vec_put(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i: u32 = i
            .try_into()
            .map_err(|_| HostError::General("i must be u32"))?;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let mut vnew = hv.clone();
            vnew.set(i as usize, x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_get(&self, v: Object, i: RawVal) -> Result<RawVal, HostError> {
        let i: u32 = i
            .try_into()
            .map_err(|_| HostError::General("i must be u32"))?;
        let res = self.visit_obj(v, move |hv: &HostVec| match hv.get(i as usize) {
            None => Err(HostError::General("index out of bound")),
            Some(hval) => Ok(hval.to_raw()),
        });
        res
    }

    fn vec_del(&self, v: Object, i: RawVal) -> Result<Object, HostError> {
        let i: u32 = i
            .try_into()
            .map_err(|_| HostError::General("i must be u32"))?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i as usize >= hv.len() {
                return Err(HostError::General("index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.remove(i as usize);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_len(&self, v: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        Ok(u32::try_from(len)?.into())
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
                None => Err(HostError::General("value does not exist")),
                Some(_) => Ok(vnew),
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_front(&self, v: Object) -> Result<RawVal, HostError> {
        let front = self.visit_obj(v, |hv: &HostVec| match hv.front() {
            None => Err(HostError::General("value does not exist")),
            Some(front) => Ok(front.to_raw()),
        });
        front
    }

    fn vec_back(&self, v: Object) -> Result<RawVal, HostError> {
        let back = self.visit_obj(v, |hv: &HostVec| match hv.back() {
            None => Err(HostError::General("value does not exist")),
            Some(back) => Ok(back.to_raw()),
        });
        back
    }

    fn vec_insert(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i: u32 = i
            .try_into()
            .map_err(|_| HostError::General("i must be u32"))?;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i as usize > hv.len() {
                return Err(HostError::General("index out of bound"));
            }
            let mut vnew = hv.clone();
            vnew.insert(i as usize, x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_append(&self, v1: Object, v2: Object) -> Result<Object, HostError> {
        let mut vnew = self.visit_obj(v1, |hv: &HostVec| Ok(hv.clone()))?;
        let v2 = self.visit_obj(v2, |hv: &HostVec| Ok(hv.clone()))?;
        vnew.append(v2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_slice(&self, v: Object, i: RawVal, l: RawVal) -> Result<Object, HostError> {
        let i: u32 = i
            .try_into()
            .map_err(|_| HostError::General("i must be u32"))?;
        let l: u32 = l
            .try_into()
            .map_err(|_| HostError::General("l must be u32"))?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i > u32::MAX - l {
                return Err(HostError::General("u32 overflow"));
            }
            if (i + l) as usize > hv.len() {
                return Err(HostError::General("index out of bound"));
            }
            Ok(hv.clone().slice(i as usize..(i + l) as usize))
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn put_contract_data(&self, k: RawVal, v: RawVal) -> Result<RawVal, HostError> {
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
            _ => Err(HostError::General("expected contract data")),
        }
    }

    fn del_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        let key = self.to_storage_key(k)?;
        self.0.storage.borrow_mut().del(&key)?;
        Ok(().into())
    }

    fn call(&self, contract: Object, func: Symbol, args: Object) -> Result<RawVal, HostError> {
        #[cfg(not(feature = "vm"))]
        todo!();
        #[cfg(feature = "vm")]
        {
            let args: Vec<RawVal> = self.visit_obj(args, |hv: &HostVec| {
                Ok(hv.iter().map(|a| a.to_raw()).collect())
            })?;
            self.call_n(contract, func, args.as_slice())
        }
    }

    fn bigint_from_u64(&self, x: u64) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_to_u64(&self, x: Object) -> Result<u64, HostError> {
        todo!()
    }

    fn bigint_from_i64(&self, x: i64) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_to_i64(&self, x: Object) -> Result<i64, HostError> {
        todo!()
    }

    fn bigint_add(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_sub(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_mul(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_div(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_rem(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_and(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_or(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_xor(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_shl(&self, x: Object, y: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_shr(&self, x: Object, y: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_cmp(&self, x: Object, y: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_is_zero(&self, x: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_neg(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_not(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_gcd(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_lcm(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_pow(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_pow_mod(&self, p: Object, q: Object, m: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_sqrt(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_bits(&self, x: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn serialize_to_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn deserialize_from_binary(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_copy_to_guest_mem(
        &self,
        x: Object,
        i: RawVal,
        j: RawVal,
        l: RawVal,
    ) -> Result<RawVal, HostError> {
        todo!()
    }

    fn binary_copy_from_guest_mem(
        &self,
        x: Object,
        i: RawVal,
        j: RawVal,
        l: RawVal,
    ) -> Result<RawVal, HostError> {
        todo!()
    }

    fn binary_new(&self) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_put(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_get(&self, x: Object, i: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_del(&self, v: Object, i: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_len(&self, x: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn binary_push(&self, x: Object, v: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_pop(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_front(&self, v: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn binary_back(&self, v: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn binary_insert(&self, x: Object, i: RawVal, v: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_append(&self, v1: Object, v2: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn binary_slice(&self, v: Object, i: RawVal, l: RawVal) -> Result<Object, HostError> {
        todo!()
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
            return Err(HostError::General("incorrect hash size"));
        }

        Ok(self.add_host_object(hash)?.into())
    }

    fn verify_sig_ed25519(&self, x: Object, k: Object, s: Object) -> Result<RawVal, HostError> {
        use ed25519_dalek::{PublicKey, Signature, Verifier};

        let public_key: PublicKey = self.visit_obj(k, |bin: &Vec<u8>| {
            Ok(PublicKey::from_bytes(bin).map_err(|_| HostError::General("Invalid PublicKey"))?)
        })?;

        let sig: Signature = self.visit_obj(s, |bin: &Vec<u8>| {
            Ok(Signature::from_bytes(bin).map_err(|_| HostError::General("Invalid Signature"))?)
        })?;

        let res = self.visit_obj(x, |bin: &Vec<u8>| {
            Ok(public_key
                .verify(bin, &sig)
                .map_err(|_| HostError::General("Failed ED25519 verification"))?)
        });
        Ok(res?.into())
    }
}
