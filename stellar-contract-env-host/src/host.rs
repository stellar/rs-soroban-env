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
use std::num::TryFromIntError;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
#[cfg(feature = "vm")]
use stellar_contract_env_common::xdr::ScVmErrorCode;
use stellar_contract_env_common::xdr::{
    AccountEntry, AccountId, Hash, PublicKey, ReadXdr, ThresholdIndexes, Uint256, WriteXdr,
};

use crate::budget::{Budget, CostType};
use crate::storage::Storage;
use crate::weak_host::WeakHost;

use crate::xdr;
use crate::xdr::{
    ContractDataEntry, HostFunction, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyContractData, ScBigInt, ScHostContextErrorCode, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScMap, ScMapEntry, ScObject, ScStatic, ScStatus,
    ScStatusType, ScUnknownErrorCode, ScVal, ScVec,
};
use std::rc::Rc;

use crate::host_object::{HostMap, HostObj, HostObject, HostObjectType, HostVal, HostVec};
use crate::CheckedEnv;
#[cfg(feature = "vm")]
use crate::SymbolStr;
#[cfg(feature = "vm")]
use crate::Vm;
use crate::{
    BitSet, BitSetError, ConversionError, EnvBase, IntoEnvVal, Object, RawVal, RawValConvertible,
    Static, Status, Symbol, SymbolError, Tag, Val, UNKNOWN_ERROR,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum HostError {
    /// An error with specific status code.
    #[error("host error: {0}, status: {:?}")]
    WithStatus(String, ScStatus),
    /// XDR error.
    #[error("XDR error: {0}")]
    XDR(#[from] xdr::Error),
    /// WASMI error.
    #[cfg(feature = "vm")]
    #[error("WASMI error: {0}")]
    WASMI(#[from] wasmi::Error),
    /// ParityWasmElements error.
    #[cfg(feature = "vm")]
    #[error("ParityWasmElements error: {0}")]
    ParityWasmElements(#[from] parity_wasm::elements::Error),
    /// General error.
    #[error("general host error: {0}")]
    General(&'static str),
}

impl From<TryFromIntError> for HostError {
    fn from(_: TryFromIntError) -> Self {
        HostError::WithStatus(
            String::from("number out of range of u32"),
            ScStatus::HostValueError(ScHostValErrorCode::U32OutOfRange),
        )
    }
}

impl From<SymbolError> for HostError {
    fn from(err: SymbolError) -> Self {
        HostError::WithStatus(
            String::from("symbol error"),
            ScStatus::HostValueError(match err {
                SymbolError::TooLong(_) => ScHostValErrorCode::SymbolTooLong,
                SymbolError::BadChar(_) => ScHostValErrorCode::SymbolBadChar,
            }),
        )
    }
}

impl From<BitSetError> for HostError {
    fn from(err: BitSetError) -> Self {
        HostError::WithStatus(
            String::from("bitset error"),
            ScStatus::HostValueError(match err {
                BitSetError::TooManyBits(_) => ScHostValErrorCode::BitsetTooManyBits,
            }),
        )
    }
}

impl From<&HostError> for ScStatus {
    fn from(err: &HostError) -> Self {
        match err {
            HostError::General(_) => ScStatus::UnknownError(ScUnknownErrorCode::General),
            HostError::WithStatus(_, status) => status.to_owned(),
            HostError::XDR(_) => ScStatus::UnknownError(ScUnknownErrorCode::Xdr),
            #[cfg(feature = "vm")]
            HostError::WASMI(err) => match err {
                wasmi::Error::Trap(trap) => match trap {
                    wasmi::Trap::Code(code) => match code {
                        wasmi::TrapCode::Unreachable => {
                            ScStatus::VmError(ScVmErrorCode::TrapUnreachable)
                        }
                        wasmi::TrapCode::MemoryAccessOutOfBounds => {
                            ScStatus::VmError(ScVmErrorCode::TrapMemoryAccessOutOfBounds)
                        }
                        wasmi::TrapCode::TableAccessOutOfBounds => {
                            ScStatus::VmError(ScVmErrorCode::TrapTableAccessOutOfBounds)
                        }
                        wasmi::TrapCode::ElemUninitialized => {
                            ScStatus::VmError(ScVmErrorCode::TrapElemUninitialized)
                        }
                        wasmi::TrapCode::DivisionByZero => {
                            ScStatus::VmError(ScVmErrorCode::TrapDivisionByZero)
                        }
                        wasmi::TrapCode::IntegerOverflow => {
                            ScStatus::VmError(ScVmErrorCode::TrapIntegerOverflow)
                        }
                        wasmi::TrapCode::InvalidConversionToInt => {
                            ScStatus::VmError(ScVmErrorCode::TrapInvalidConversionToInt)
                        }
                        wasmi::TrapCode::StackOverflow => {
                            ScStatus::VmError(ScVmErrorCode::TrapStackOverflow)
                        }
                        wasmi::TrapCode::UnexpectedSignature => {
                            ScStatus::VmError(ScVmErrorCode::TrapUnexpectedSignature)
                        }
                        wasmi::TrapCode::MemLimitExceeded => {
                            ScStatus::VmError(ScVmErrorCode::TrapMemLimitExceeded)
                        }
                        wasmi::TrapCode::CpuLimitExceeded => {
                            ScStatus::VmError(ScVmErrorCode::TrapCpuLimitExceeded)
                        }
                    },
                    wasmi::Trap::Host(err) => match err.downcast_ref::<HostError>() {
                        Some(e) => e.into(),
                        None => ScStatus::VmError(ScVmErrorCode::Unknown),
                    },
                },
                wasmi::Error::Host(err) => match err.downcast_ref::<HostError>() {
                    Some(e) => e.into(),
                    None => ScStatus::VmError(ScVmErrorCode::Unknown),
                },
                wasmi::Error::Validation(_) => ScStatus::VmError(ScVmErrorCode::Validation),
                wasmi::Error::Instantiation(_) => ScStatus::VmError(ScVmErrorCode::Instantiation),
                wasmi::Error::Function(_) => ScStatus::VmError(ScVmErrorCode::Function),
                wasmi::Error::Table(_) => ScStatus::VmError(ScVmErrorCode::Table),
                wasmi::Error::Memory(_) => ScStatus::VmError(ScVmErrorCode::Memory),
                wasmi::Error::Global(_) => ScStatus::VmError(ScVmErrorCode::Global),
                wasmi::Error::Value(_) => ScStatus::VmError(ScVmErrorCode::Value),
            },
            #[cfg(feature = "vm")]
            HostError::ParityWasmElements(_) => ScStatus::VmError(ScVmErrorCode::Unknown),
        }
    }
}

impl From<ConversionError> for HostError {
    fn from(_: ConversionError) -> Self {
        HostError::General("conversion error")
    }
}

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
#[derive(Clone)]
struct RollbackPoint {
    storage: OrdMap<LedgerKey, Option<LedgerEntry>>,
    objects: usize,
}

#[cfg(feature = "testutils")]
pub trait ContractVTableFnTrait {
    fn call(&self, args: &[RawVal]) -> RawVal;

    fn duplicate(&self) -> ContractVTableFn;
}

#[cfg(feature = "testutils")]
pub struct ContractVTableFn(pub Box<dyn ContractVTableFnTrait>);

#[cfg(feature = "testutils")]
impl Clone for ContractVTableFn {
    fn clone(&self) -> Self {
        self.0.duplicate()
    }
}

#[cfg(feature = "testutils")]
#[derive(Clone)]
pub struct ContractVTable(pub std::collections::HashMap<Symbol, ContractVTableFn>);

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

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    context: RefCell<Vec<Frame>>,
    budget: RefCell<Budget>,
    #[cfg(feature = "testutils")]
    vtables: RefCell<std::collections::HashMap<Hash, ContractVTable>>,
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
            #[cfg(feature = "testutils")]
            vtables: Default::default(),
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
        f(self.0.context.borrow().last().ok_or_else(|| {
            HostError::WithStatus(
                String::from("no contract currently running"),
                ScStatus::HostContextError(ScHostContextErrorCode::NoContractRunning),
            )
        })?)
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
            #[cfg(feature = "testutils")]
            Frame::TestContract(id) => Ok(id.clone()),
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
                None => Err(HostError::WithStatus(
                    String::from("unknown object reference"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnknownReference),
                )),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(HostError::WithStatus(
                        String::from("unexpected host object type"),
                        ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                    )),
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
                    let tag_static =
                        unsafe { <Static as RawValConvertible>::unchecked_from_val(val) };
                    if tag_static.is_type(ScStatic::True) {
                        Ok(ScVal::Static(ScStatic::True))
                    } else if tag_static.is_type(ScStatic::False) {
                        Ok(ScVal::Static(ScStatic::False))
                    } else if tag_static.is_type(ScStatic::Void) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else if tag_static.is_type(ScStatic::LedgerKeyContractCodeWasm) {
                        Ok(ScVal::Static(ScStatic::LedgerKeyContractCodeWasm))
                    } else {
                        Err(HostError::WithStatus(
                            String::from("unknown Tag::Static case"),
                            ScStatus::HostValueError(ScHostValErrorCode::StaticUnknown),
                        ))
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
                        Ok(ScVal::Status(ScStatus::UnknownError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::HostValueError) {
                        Ok(ScVal::Status(ScStatus::HostValueError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::HostObjectError) {
                        Ok(ScVal::Status(ScStatus::HostObjectError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::HostFunctionError) {
                        Ok(ScVal::Status(ScStatus::HostFunctionError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::HostStorageError) {
                        Ok(ScVal::Status(ScStatus::HostStorageError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::HostContextError) {
                        Ok(ScVal::Status(ScStatus::HostContextError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else if status.is_type(ScStatusType::VmError) {
                        Ok(ScVal::Status(ScStatus::VmError(
                            (status.get_code() as i32).try_into()?,
                        )))
                    } else {
                        Err(HostError::WithStatus(
                            String::from("unknown Tag::Status case"),
                            ScStatus::HostValueError(ScHostValErrorCode::StatusUnknown),
                        ))
                    }
                }
                Tag::Reserved => Err(HostError::WithStatus(
                    String::from("Tag::Reserved value"),
                    ScStatus::HostValueError(ScHostValErrorCode::ReservedTagValue),
                )),
            }
        }
    }

    // Testing interface to create values directly for later use via Env functions.
    pub fn intern(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.to_host_val(v).map(|hv| hv.into())
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<HostVal, HostError> {
        let ok = match v {
            ScVal::U63(i) => {
                if *i >= 0 {
                    unsafe { RawVal::unchecked_from_u63(*i) }
                } else {
                    return Err(HostError::WithStatus(
                        String::from("ScvU63 > i64::MAX"),
                        ScStatus::HostValueError(ScHostValErrorCode::U63OutOfRange),
                    ));
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Static(other) => RawVal::from_other_static(*other),
            ScVal::Object(None) => {
                return Err(HostError::WithStatus(
                    String::from("missing expected ScvObject"),
                    ScStatus::HostValueError(ScHostValErrorCode::MissingObject),
                ))
            }
            ScVal::Object(Some(ob)) => return Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => {
                        return Err(HostError::WithStatus(
                            String::from("non-UTF-8 in symbol"),
                            ScStatus::HostValueError(ScHostValErrorCode::SymbolContainsNonUtf8),
                        ))
                    }
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::Status(st) => {
                let status = match st {
                    ScStatus::Ok => Status::from_type_and_code(ScStatusType::Ok, 0),
                    ScStatus::UnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::UnknownError, *e as u32)
                    }
                    ScStatus::HostValueError(e) => {
                        Status::from_type_and_code(ScStatusType::HostValueError, *e as u32)
                    }
                    ScStatus::HostObjectError(e) => {
                        Status::from_type_and_code(ScStatusType::HostObjectError, *e as u32)
                    }
                    ScStatus::HostFunctionError(e) => {
                        Status::from_type_and_code(ScStatusType::HostFunctionError, *e as u32)
                    }
                    ScStatus::HostStorageError(e) => {
                        Status::from_type_and_code(ScStatusType::HostStorageError, *e as u32)
                    }
                    ScStatus::HostContextError(e) => {
                        Status::from_type_and_code(ScStatusType::HostContextError, *e as u32)
                    }
                    ScStatus::VmError(e) => {
                        Status::from_type_and_code(ScStatusType::VmError, *e as u32)
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
                None => Err(HostError::WithStatus(
                    String::from("unknown object reference"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnknownReference),
                )),
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
                    HostObject::BigInt(bi) => {
                        let (sign, data) = bi.to_bytes_be();
                        match sign {
                            Sign::Minus => {
                                Ok(ScObject::BigInt(ScBigInt::Negative(data.try_into()?)))
                            }
                            Sign::NoSign => Ok(ScObject::BigInt(ScBigInt::Zero)),
                            Sign::Plus => {
                                Ok(ScObject::BigInt(ScBigInt::Positive(data.try_into()?)))
                            }
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
                self.charge_budget(CostType::HostVecAlloc, v.0.len() as u64)?;
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
            return Err(HostError::WithStatus(
                String::from("object count exceeds u32::MAX"),
                ScStatus::HostObjectError(ScHostObjErrorCode::ObjectCountExceedsU32Max),
            ));
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

    fn create_contract_helper(
        &self,
        contract: Object,
        id_preimage: Vec<u8>,
    ) -> Result<Object, HostError> {
        let id_obj = self.compute_hash_sha256(self.add_host_object(id_preimage)?.into())?;

        let new_contract_id = self.visit_obj(id_obj, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin.as_slice().try_into().map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid contract hash"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::ContractHashWrongLength),
                )
            })?;
            Ok(xdr::Hash(arr))
        })?;

        let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: new_contract_id.clone(),
            key: ScVal::Static(ScStatic::LedgerKeyContractCodeWasm),
        });

        if self.0.storage.borrow_mut().has(&storage_key)? {
            return Err(HostError::General("Contract already exists"));
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

    fn try_call_vtable(
        &self,
        id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Option<Result<RawVal, HostError>> {
        #[cfg(feature = "testutils")]
        if let Some(vtable) = self.0.vtables.borrow().get(&id) {
            if let Some(f) = vtable.0.get(&func) {
                let mut frame_guard = self.push_test_frame(id.clone());
                let res = Some(Ok(f.0.call(args)));
                frame_guard.commit();
                res
            } else {
                Some(Err(HostError::General("function not in vtable")))
            }
        } else {
            None
        }

        #[cfg(not(feature = "testutils"))]
        None
    }

    #[cfg(feature = "vm")]
    fn call_vm(&self, id: &Hash, func: &Symbol, args: &[RawVal]) -> Result<RawVal, HostError> {
        // Create key for storage
        let key = ScVal::Static(ScStatic::LedgerKeyContractCodeWasm);
        let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: id.clone(),
            key,
        });
        // Retrieve the contract code and create vm
        let scval = match self.0.storage.borrow_mut().get(&storage_key)?.data {
            LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
            _ => Err(HostError::WithStatus(
                String::from("expected contract data"),
                ScStatus::HostStorageError(ScHostStorageErrorCode::ExpectContractData),
            )),
        }?;
        let scobj = match scval {
            ScVal::Object(Some(ob)) => ob,
            _ => {
                return Err(HostError::WithStatus(
                    String::from("not an object"),
                    ScStatus::HostValueError(ScHostValErrorCode::UnexpectedValType),
                ))
            }
        };
        let code = match scobj {
            ScObject::Binary(b) => b,
            _ => {
                return Err(HostError::WithStatus(
                    String::from("not a binary object"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                ))
            }
        };
        let vm = Vm::new(self, id.clone(), code.as_slice())?;
        // Resolve the function symbol and invoke contract call
        vm.invoke_function_raw(self, SymbolStr::from(func).as_ref(), args)
    }

    fn try_call_vm(
        &self,
        id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Option<Result<RawVal, HostError>> {
        #[cfg(feature = "vm")]
        return Some(self.call_vm(id, func, args));
        #[cfg(not(feature = "vm"))]
        return None;
    }

    fn call_n(&self, contract: Object, func: Symbol, args: &[RawVal]) -> Result<RawVal, HostError> {
        // Get contract ID
        let id = self.visit_obj(contract, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin.as_slice().try_into().map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid contract hash"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::ContractHashWrongLength),
                )
            })?;
            Ok(xdr::Hash(arr))
        })?;

        self.try_call_vtable(&id, &func, args)
            .or_else(|| self.try_call_vm(&id, &func, args))
            .unwrap_or_else(|| unimplemented!())
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
                    Err(HostError::WithStatus(
                        String::from("unexpected Call args"),
                        ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongLength),
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
                    Err(HostError::WithStatus(
                        String::from("unexpected CreateContract args"),
                        ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongLength),
                    ))
                }
            }
        }
    }

    fn to_u256(&self, a: Object) -> Result<Uint256, HostError> {
        self.visit_obj(a, |bin: &Vec<u8>| {
            bin.try_into()
                .map_err(|_| HostError::General("bad u256 length"))
        })
    }

    fn load_account(&self, a: Object) -> Result<AccountEntry, HostError> {
        use xdr::LedgerKeyAccount;
        let acc = xdr::LedgerKey::Account(LedgerKeyAccount {
            account_id: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(a)?)),
        });
        self.visit_storage(|storage| match storage.get(&acc)?.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(HostError::General("not account")),
        })
    }

    #[cfg(feature = "testutils")]
    pub fn register_test_contract(
        &self,
        contract_id: Object,
        vtable: ContractVTable,
    ) -> Result<(), HostError> {
        self.visit_obj(contract_id, |bin: &Vec<u8>| {
            let mut vtables = self.0.vtables.borrow_mut();
            let hash = Hash(
                bin.clone()
                    .try_into()
                    .map_err(|_| HostError::General("bad contract id"))?,
            );
            if !vtables.contains_key(&hash) {
                vtables.insert(hash, vtable);
                Ok(())
            } else {
                Err(HostError::General("vtable already exists"))
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
}

impl CheckedEnv for Host {
    type Error = HostError;

    fn log_value(&self, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_invoking_contract(&self) -> Result<Object, HostError> {
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
            None => Err(HostError::General("map key not found")),
        })
    }

    fn map_del(&self, m: Object, k: RawVal) -> Result<Object, HostError> {
        let k = self.associate_raw_val(k);
        let mnew = self.visit_obj(m, |hm: &HostMap| {
            let mut mnew = hm.clone();
            match mnew.remove(&k) {
                Some(v) => Ok(mnew),
                None => Err(HostError::General("map key not found")),
            }
        })?;
        Ok(self.add_host_object(mnew)?.into())
    }

    fn map_len(&self, m: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(m, |hm: &HostMap| Ok(hm.len()))?;
        Ok(u32::try_from(len)?.into())
    }

    fn map_has(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, move |hm: &HostMap| Ok(hm.contains_key(&k).into()))
    }

    fn map_prev_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, |hm: &HostMap| match hm.get_prev(&k) {
            Some((pk, pv)) => Ok(pk.to_raw()),
            None => Ok(UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
        })
    }

    fn map_next_key(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        let k = self.associate_raw_val(k);
        self.visit_obj(m, |hm: &HostMap| match hm.get_next(&k) {
            Some((pk, pv)) => Ok(pk.to_raw()),
            None => Ok(UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
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
            let mut vec = self.vec_new()?;
            for k in hm.keys() {
                vec = self.vec_push(vec, k.to_raw())?;
            }
            Ok(vec)
        })
    }

    fn map_values(&self, m: Object) -> Result<Object, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            let mut vec = self.vec_new()?;
            for k in hm.values() {
                vec = self.vec_push(vec, k.to_raw())?;
            }
            Ok(vec)
        })
    }

    fn vec_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(HostVec::new())?.into())
    }

    fn vec_put(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i >= hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            let mut vnew = hv.clone();
            vnew.set(i, x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_get(&self, v: Object, i: RawVal) -> Result<RawVal, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        self.visit_obj(v, move |hv: &HostVec| match hv.get(i) {
            None => Err(HostError::WithStatus(
                String::from("index out of bound"),
                ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
            )),
            Some(hval) => Ok(hval.to_raw()),
        })
    }

    fn vec_del(&self, v: Object, i: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i >= hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            let mut vnew = hv.clone();
            vnew.remove(i);
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
                None => Err(HostError::WithStatus(
                    String::from("value does not exist"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                )),
                Some(_) => Ok(vnew),
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_front(&self, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| match hv.front() {
            None => Err(HostError::WithStatus(
                String::from("value does not exist"),
                ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
            )),
            Some(front) => Ok(front.to_raw()),
        })
    }

    fn vec_back(&self, v: Object) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| match hv.back() {
            None => Err(HostError::WithStatus(
                String::from("value does not exist"),
                ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
            )),
            Some(back) => Ok(back.to_raw()),
        })
    }

    fn vec_insert(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if i > hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
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
            return Err(HostError::WithStatus(
                String::from("u32 overflow"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
            ));
        }
        vnew.append(v2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_slice(&self, v: Object, start: RawVal, end: RawVal) -> Result<Object, HostError> {
        let start: usize = u32::try_from(start).map_err(|_| {
            HostError::WithStatus(
                String::from("start must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let end: usize = u32::try_from(end).map_err(|_| {
            HostError::WithStatus(
                String::from("end must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        if start > end {
            return Err(HostError::WithStatus(
                String::from("start greater than end"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
            ));
        }
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            if start > hv.len() {
                return Err(HostError::WithStatus(
                    String::from("start out of bounds"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            if end > hv.len() {
                return Err(HostError::WithStatus(
                    String::from("end out of bounds"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            Ok(hv.clone().slice(start..end))
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn put_contract_data(&self, k: RawVal, v: RawVal) -> Result<RawVal, HostError> {
        let data_key = self.from_host_val(k)?;
        if data_key == ScVal::Static(ScStatic::LedgerKeyContractCodeWasm) {
            return Err(HostError::WithStatus(
                String::from("cannot update contract code"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
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
            _ => Err(HostError::WithStatus(
                String::from("expected contract data"),
                ScStatus::HostStorageError(ScHostStorageErrorCode::ExpectContractData),
            )),
        }
    }

    fn del_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        if self.from_host_val(k)? == ScVal::Static(ScStatic::LedgerKeyContractCodeWasm) {
            return Err(HostError::WithStatus(
                String::from("cannot delete contract code"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
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
        let salt_val = self.visit_obj(salt, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin.as_slice().try_into().map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid salt"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                )
            })?;
            Ok(Uint256(arr))
        })?;

        let key_val = self.visit_obj(key, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin.as_slice().try_into().map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid key"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                )
            })?;
            Ok(Uint256(arr))
        })?;

        // Verify parameters
        let params = self.visit_obj(v, |bin: &Vec<u8>| {
            let separator = "create_contract_from_ed25519(nonce: u256, contract: Vec<u8>, salt: u256, key: u256, sig: Vec<u8>)";
            let params = [separator.as_bytes(), salt_val.as_ref(), bin].concat();
            Ok(params)
        })?;
        let hash = self.compute_hash_sha256(self.add_host_object(params)?.into())?;
        self.verify_sig_ed25519(hash, key, sig)?;

        //Create contract and contractID
        let pre_image = xdr::HashIdPreimage::ContractIdFromEd25519(xdr::HashIdPreimageContractId {
            ed25519: key_val,
            salt: salt_val,
        });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| HostError::General("invalid hash"))?;
        self.create_contract_helper(v, buf)
    }

    fn create_contract_from_contract(&self, v: Object, salt: Object) -> Result<Object, HostError> {
        let contract_id = self.get_current_contract_id()?;

        let salt_val = self.visit_obj(salt, |bin: &Vec<u8>| {
            let arr: [u8; 32] = bin.as_slice().try_into().map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid salt"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                )
            })?;
            Ok(Uint256(arr))
        })?;

        let pre_image =
            xdr::HashIdPreimage::ContractIdFromContract(xdr::HashIdPreimageChildContractId {
                contract_id,
                salt: salt_val,
            });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| HostError::General("invalid hash"))?;

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
                let st: ScStatus = (&e).into();
                Ok(st.into())
            }
        }
    }

    fn bigint_from_u64(&self, x: u64) -> Result<Object, HostError> {
        Ok(self.add_host_object(Into::<BigInt>::into(x))?.into())
    }

    fn bigint_to_u64(&self, x: Object) -> Result<u64, HostError> {
        self.visit_obj(x, |bi: &BigInt| {
            bi.to_u64().ok_or_else(|| ConversionError.into())
        })
    }

    fn bigint_from_i64(&self, x: i64) -> Result<Object, HostError> {
        Ok(self.add_host_object(Into::<BigInt>::into(x))?.into())
    }

    fn bigint_to_i64(&self, x: Object) -> Result<i64, HostError> {
        self.visit_obj(x, |bi: &BigInt| {
            bi.to_i64().ok_or_else(|| ConversionError.into())
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
                    return Err(HostError::General("bigint division by zero"));
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
                    return Err(HostError::General("bigint division by zero"));
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
                    return Err(HostError::General("attempt to shift left with negative"));
                }
                if let Some(u) = b.to_usize() {
                    Ok(a.shl(u))
                } else {
                    Err(HostError::General("left-shift overflow"))
                }
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_shr(&self, x: Object, y: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(x, |a: &BigInt| {
            self.visit_obj(y, |b: &BigInt| {
                if b.is_negative() {
                    return Err(HostError::General("attempt to shift right with negative"));
                }
                if let Some(u) = b.to_usize() {
                    Ok(a.shr(u))
                } else {
                    Err(HostError::General("right-shift overflow"))
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
                    return Err(HostError::General("negative exponentiation not supported"));
                }
                if let Some(u) = e.to_usize() {
                    Ok(Pow::pow(a, u))
                } else {
                    Err(HostError::General("pow overflow"))
                }
            })
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_pow_mod(&self, p: Object, q: Object, m: Object) -> Result<Object, HostError> {
        let res = self.visit_obj(p, |a: &BigInt| {
            self.visit_obj(q, |exponent: &BigInt| {
                if exponent.is_negative() {
                    return Err(HostError::General("negative exponentiation not supported"));
                }
                self.visit_obj(m, |modulus: &BigInt| {
                    if modulus.is_zero() {
                        return Err(HostError::General("zero modulus not supported"));
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
                return Err(HostError::General("sqrt is imaginary"));
            }
            Ok(a.sqrt())
        })?;
        Ok(self.add_host_object(res)?.into())
    }

    fn bigint_bits(&self, x: Object) -> Result<u64, HostError> {
        self.visit_obj(x, |a: &BigInt| Ok(a.bits()))
    }

    fn serialize_to_binary(&self, v: RawVal) -> Result<Object, HostError> {
        let scv = self.from_host_val(v)?;
        let mut buf = Vec::<u8>::new();
        scv.write_xdr(&mut buf)
            .map_err(|_| HostError::General("failed to serialize ScVal"))?;
        Ok(self.add_host_object(buf)?.into())
    }

    fn deserialize_from_binary(&self, b: Object) -> Result<RawVal, HostError> {
        let scv = self.visit_obj(b, |hv: &Vec<u8>| {
            ScVal::read_xdr(&mut hv.as_slice())
                .map_err(|_| HostError::General("failed to de-serialize ScVal"))
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
        if let [b_pos, lm_pos, len] = [b_pos, lm_pos, len]
            .iter()
            .map(|v| u32::try_from(*v))
            .collect::<Result<Vec<u32>, ConversionError>>()?
            .as_slice()
        {
            self.visit_obj(b, move |hv: &Vec<u8>| {
                let end_idx = b_pos.checked_add(*len).ok_or_else(|| {
                    HostError::WithStatus(
                        String::from("u32 overflow"),
                        ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                    )
                })? as usize;
                if end_idx > hv.len() {
                    return Err(HostError::WithStatus(
                        String::from("index out of bound"),
                        ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                    ));
                }
                self.with_current_frame(|frame| match frame {
                    Frame::ContractVM(vm) => vm.with_memory_access(|mem| {
                        Ok(mem.set(*lm_pos, &hv.as_slice()[*b_pos as usize..end_idx])?)
                    }),
                    Frame::HostFunction(_) => {
                        Err(HostError::General("linear memory not supported"))
                    }
                    #[cfg(feature = "testutils")]
                    Frame::TestContract(id) => {
                        Err(HostError::General("linear memory not supported"))
                    }
                })
            })?;
            Ok(().into())
        } else {
            return Err(HostError::WithStatus(
                String::from("failed conversion to u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            ));
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
        if let [b_pos, lm_pos, len] = [b_pos, lm_pos, len]
            .iter()
            .map(|v| u32::try_from(*v))
            .collect::<Result<Vec<u32>, ConversionError>>()?
            .as_slice()
        {
            let mut vnew = self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.clone()))?;
            let end_idx = b_pos.checked_add(*len).ok_or_else(|| {
                HostError::WithStatus(
                    String::from("u32 overflow"),
                    ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                )
            })? as usize;
            self.with_current_frame(|frame| match frame {
                Frame::ContractVM(vm) => vm.with_memory_access(|mem| {
                    // we would potentially let the vec grow
                    if end_idx > vnew.len() {
                        vnew.resize(end_idx, 0);
                    }
                    Ok(mem.get_into(*lm_pos, &mut vnew.as_mut_slice()[*b_pos as usize..end_idx])?)
                }),
                Frame::HostFunction(_) => Err(HostError::General("linear memory not supported")),
                #[cfg(feature = "testutils")]
                Frame::TestContract(id) => Err(HostError::General("linear memory not supported")),
            })?;
            Ok(self.add_host_object(vnew)?.into())
        } else {
            return Err(HostError::WithStatus(
                String::from("failed conversion to u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            ));
        }
    }

    fn binary_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(Vec::<u8>::new())?.into())
    }

    fn binary_new_from_linear_memory(
        &self,
        lm_pos: RawVal,
        len: RawVal,
    ) -> Result<Object, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        if let [lm_pos, len] = [lm_pos, len]
            .iter()
            .map(|v| u32::try_from(*v))
            .collect::<Result<Vec<u32>, ConversionError>>()?
            .as_slice()
        {
            return self.with_current_frame(|frame| match frame {
                Frame::ContractVM(vm) => vm.with_memory_access(|mem| {
                    let mut vnew: Vec<u8> = vec![0; *len as usize];
                    mem.get_into(*lm_pos, vnew.as_mut_slice())?;
                    Ok(self.add_host_object(vnew)?.into())
                }),
                Frame::HostFunction(_) => Err(HostError::General("linear memory not supported")),
                #[cfg(feature = "testutils")]
                Frame::TestContract(id) => Err(HostError::General("linear memory not supported")),
            });
        } else {
            return Err(HostError::WithStatus(
                String::from("failed conversion to u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            ));
        }
    }

    fn binary_put(&self, b: Object, i: RawVal, u: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let u: u8 = u.try_into().map_err(|_| {
            HostError::WithStatus(
                String::from("u must be u8"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            let mut vnew = hv.clone();
            if i >= hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            let _old_val = std::mem::replace(&mut vnew[i], u);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_get(&self, b: Object, i: RawVal) -> Result<RawVal, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        self.visit_obj(b, move |hv: &Vec<u8>| match hv.get(i) {
            None => Err(HostError::WithStatus(
                String::from("index out of bound"),
                ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
            )),
            Some(u) => Ok((*u).into()),
        })
    }

    fn binary_del(&self, b: Object, i: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if i >= hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            let mut vnew = hv.clone();
            vnew.remove(i);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_len(&self, b: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(b, |hv: &Vec<u8>| Ok(hv.len()))?;
        Ok(u32::try_from(len)?.into())
    }

    fn binary_push(&self, b: Object, u: RawVal) -> Result<Object, HostError> {
        let u: u8 = u.try_into().map_err(|_| {
            HostError::WithStatus(
                String::from("x must be u8"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })?;
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
                None => Err(HostError::WithStatus(
                    String::from("u32 overflow"),
                    ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                )),
                Some(_) => Ok(vnew),
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_front(&self, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            if hv.is_empty() {
                return Err(HostError::WithStatus(
                    String::from("u32 overflow"),
                    ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                ));
            }
            Ok(hv[0].into())
        })
    }

    fn binary_back(&self, b: Object) -> Result<RawVal, HostError> {
        self.visit_obj(b, |hv: &Vec<u8>| {
            if hv.is_empty() {
                return Err(HostError::WithStatus(
                    String::from("u32 overflow"),
                    ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                ));
            }
            Ok(hv[hv.len() - 1].into())
        })
    }

    fn binary_insert(&self, b: Object, i: RawVal, u: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let u: u8 = u.try_into().map_err(|_| {
            HostError::WithStatus(
                String::from("x must be u8"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })?;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if i > hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
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
            return Err(HostError::WithStatus(
                String::from("u32 overflow"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
            ));
        }
        vnew.append(&mut b2);
        Ok(self.add_host_object(vnew)?.into())
    }

    fn binary_slice(&self, b: Object, i: RawVal, l: RawVal) -> Result<Object, HostError> {
        let i: usize = u32::try_from(i).map_err(|_| {
            HostError::WithStatus(
                String::from("i must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let l: usize = u32::try_from(l).map_err(|_| {
            HostError::WithStatus(
                String::from("l must be u32"),
                ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsWrongType),
            )
        })? as usize;
        let vnew = self.visit_obj(b, move |hv: &Vec<u8>| {
            if i > u32::MAX as usize - l {
                return Err(HostError::WithStatus(
                    String::from("u32 overflow"),
                    ScStatus::HostFunctionError(ScHostFnErrorCode::InputArgsInvalid),
                ));
            }
            if (i + l) > hv.len() {
                return Err(HostError::WithStatus(
                    String::from("index out of bound"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::VecIndexOutOfBound),
                ));
            }
            Ok(hv.as_slice()[i..(i + l)].to_vec())
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
            return Err(HostError::General("incorrect hash size"));
        }

        Ok(self.add_host_object(hash)?.into())
    }

    fn verify_sig_ed25519(&self, x: Object, k: Object, s: Object) -> Result<RawVal, HostError> {
        use ed25519_dalek::{PublicKey, Signature, Verifier};

        let public_key: PublicKey = self.visit_obj(k, |bin: &Vec<u8>| {
            PublicKey::from_bytes(bin).map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid public key"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                )
            })
        })?;

        let sig: Signature = self.visit_obj(s, |bin: &Vec<u8>| {
            Signature::from_bytes(bin).map_err(|_| {
                HostError::WithStatus(
                    String::from("invalid signature"),
                    ScStatus::HostObjectError(ScHostObjErrorCode::UnexpectedType),
                )
            })
        })?;

        let res = self.visit_obj(x, |bin: &Vec<u8>| {
            public_key
                .verify(bin, &sig)
                .map_err(|_| HostError::General("Failed ED25519 verification"))
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
