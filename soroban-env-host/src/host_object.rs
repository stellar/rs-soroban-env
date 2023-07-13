use soroban_env_common::{
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Compare, DurationSmall, I128Small, I256Small, I64Small, SymbolSmall, SymbolStr, Tag,
    TimepointSmall, U128Small, U256Small, U64Small,
};

use crate::{
    budget::{AsBudget, Budget},
    host::metered_clone::{self, MeteredClone},
    HostError,
};

use super::{
    host::metered_map::MeteredOrdMap,
    host::metered_vector::MeteredVector,
    num::{I256, U256},
    xdr, AddressObject, BytesObject, DurationObject, Host, I128Object, I256Object, I64Object,
    MapObject, Object, StringObject, SymbolObject, TimepointObject, U128Object, U256Object,
    U64Object, Val, VecObject,
};

pub(crate) type HostMap = MeteredOrdMap<Val, Val, Host>;
pub(crate) type HostVec = MeteredVector<Val>;

#[derive(Clone)]
pub enum HostObject {
    Vec(HostVec),
    Map(HostMap),
    U64(u64),
    I64(i64),
    TimePoint(xdr::TimePoint),
    Duration(xdr::Duration),
    U128(u128),
    I128(i128),
    U256(U256),
    I256(I256),
    Bytes(xdr::ScBytes),
    String(xdr::ScString),
    Symbol(xdr::ScSymbol),
    Address(xdr::ScAddress),
}

impl core::fmt::Debug for HostObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vec(_) => f.debug_tuple("Vec(...)").finish(),
            Self::Map(_) => f.debug_tuple("Map(...)").finish(),
            Self::U64(arg0) => f.debug_tuple("U64").field(arg0).finish(),
            Self::I64(arg0) => f.debug_tuple("I64").field(arg0).finish(),
            Self::TimePoint(arg0) => f.debug_tuple("TimePoint").field(arg0).finish(),
            Self::Duration(arg0) => f.debug_tuple("Duration").field(arg0).finish(),
            Self::U128(arg0) => f.debug_tuple("U128").field(arg0).finish(),
            Self::I128(arg0) => f.debug_tuple("I128").field(arg0).finish(),
            Self::U256(arg0) => f.debug_tuple("U256").field(arg0).finish(),
            Self::I256(arg0) => f.debug_tuple("I256").field(arg0).finish(),
            Self::Bytes(arg0) => f.debug_tuple("Bytes").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::Address(arg0) => f.debug_tuple("Address").field(arg0).finish(),
        }
    }
}

impl HostObject {
    // Temporarily performs a shallow comparison against a Val of the
    // associated small value type, returning None if the Val is of
    // the wrong type.
    pub(crate) fn try_compare_to_small(
        &self,
        budget: &Budget,
        rv: Val,
    ) -> Result<Option<core::cmp::Ordering>, HostError> {
        let res = match self {
            HostObject::U64(u) => {
                let Ok(small) = U64Small::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObject::I64(i) => {
                let Ok(small) = I64Small::try_from(rv) else { return Ok(None) };
                let small: i64 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObject::TimePoint(tp) => {
                let Ok(small) = TimepointSmall::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(&tp.0, &small)?)
            }
            HostObject::Duration(d) => {
                let Ok(small) = DurationSmall::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(&d.0, &small)?)
            }
            HostObject::U128(u) => {
                let Ok(small) = U128Small::try_from(rv) else { return Ok(None) };
                let small: u128 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObject::I128(i) => {
                let Ok(small) = I128Small::try_from(rv) else { return Ok(None) };
                let small: i128 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObject::U256(u) => {
                let Ok(small) = U256Small::try_from(rv) else { return Ok(None) };
                let small: U256 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObject::I256(i) => {
                let Ok(small) = I256Small::try_from(rv) else { return Ok(None) };
                let small: I256 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObject::Symbol(s) => {
                let Ok(small) = SymbolSmall::try_from(rv) else { return Ok(None) };
                let small: SymbolStr = small.into();
                let rhs: &[u8] = small.as_ref();
                Some(budget.compare(&s.as_vec().as_slice(), &rhs)?)
            }

            HostObject::Vec(_)
            | HostObject::Map(_)
            | HostObject::Bytes(_)
            | HostObject::String(_)
            | HostObject::Address(_) => None,
        };
        Ok(res)
    }
}

pub trait HostObjectType: MeteredClone {
    type Wrapper: Into<Object> + Clone;
    fn new_from_handle(handle: u32) -> Self::Wrapper;
    fn inject(self) -> HostObject;
    fn try_extract(obj: &HostObject) -> Option<&Self>;
}

// Some host objects are "a slab of memory" which we want
// to treat fairly uniformly in memory-related host functions.
pub(crate) trait MemHostObjectType:
    HostObjectType + TryFrom<Vec<u8>, Error = xdr::Error> + Into<Vec<u8>>
{
    fn as_byte_slice(&self) -> &[u8];
}

macro_rules! declare_host_object_type {
    ($TY:ty, $TAG:ident, $CASE:ident) => {
        impl HostObjectType for $TY {
            type Wrapper = $TAG;
            fn new_from_handle(handle: u32) -> Self::Wrapper {
                unsafe { $TAG::from_handle(handle) }
            }
            fn inject(self) -> HostObject {
                HostObject::$CASE(self)
            }

            fn try_extract(obj: &HostObject) -> Option<&Self> {
                match obj {
                    HostObject::$CASE(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! declare_mem_host_object_type {
    ($TY:ty, $TAG:ident, $CASE:ident) => {
        declare_host_object_type!($TY, $TAG, $CASE);
        impl MemHostObjectType for $TY {
            fn as_byte_slice(&self) -> &[u8] {
                self.as_slice()
            }
        }
    };
}

// ${type of contained data}, ${object-wrapper common type}, ${case in HostObject}
declare_host_object_type!(HostMap, MapObject, Map);
declare_host_object_type!(HostVec, VecObject, Vec);
declare_host_object_type!(u64, U64Object, U64);
declare_host_object_type!(i64, I64Object, I64);
declare_host_object_type!(xdr::TimePoint, TimepointObject, TimePoint);
declare_host_object_type!(xdr::Duration, DurationObject, Duration);
declare_host_object_type!(u128, U128Object, U128);
declare_host_object_type!(i128, I128Object, I128);
declare_host_object_type!(U256, U256Object, U256);
declare_host_object_type!(I256, I256Object, I256);
declare_mem_host_object_type!(xdr::ScBytes, BytesObject, Bytes);
declare_mem_host_object_type!(xdr::ScString, StringObject, String);
declare_mem_host_object_type!(xdr::ScSymbol, SymbolObject, Symbol);
declare_host_object_type!(xdr::ScAddress, AddressObject, Address);

// We store system objects in a separate Vec from user objects, and
// differentiate handles to them by the low bit of the handle: 1 means system
// objects, 0 means user objects. This way user code is literally unaffected by
// system-object allocation -- won't even change the handles users get -- and we
// can also check that system objects are never leaking out of the host.
pub fn is_system_object_handle(handle: u32) -> bool {
    handle & 1 == 1
}

pub fn is_system_object(obj: Object) -> bool {
    is_system_object_handle(obj.get_handle())
}

pub fn is_system_object_value(val: Val) -> bool {
    if let Ok(obj) = Object::try_from(val) {
        is_system_object(obj)
    } else {
        false
    }
}

pub fn handle_to_index(handle: u32) -> usize {
    (handle as usize) >> 1
}

pub fn index_to_handle(host: &Host, index: usize, system_mode: bool) -> Result<u32, HostError> {
    if let Ok(smaller) = u32::try_from(index) {
        if let Some(shifted) = smaller.checked_shl(1) {
            if system_mode {
                return Ok(shifted | 1);
            } else {
                return Ok(shifted | 0);
            }
        }
    }
    Err(host.err_arith_overflow())
}

impl Host {
    pub fn check_value_is_non_system_object(&self, val: Val) -> Result<(), HostError> {
        if is_system_object_value(val) {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "unexpected system value",
                &[val],
            ))
        } else {
            Ok(())
        }
    }

    // Execute `f` in system mode, restoring the state of the flag on exit (and allowing re-entry of system-mode).
    pub(crate) fn with_system_mode<T>(
        &self,
        f: impl FnOnce() -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        self.with_system_mode_flag(true, f)
    }

    pub(crate) fn with_user_mode<T>(
        &self,
        f: impl FnOnce() -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        self.with_system_mode_flag(false, f)
    }

    pub(crate) fn with_system_mode_flag<T>(
        &self,
        new_flag: bool,
        f: impl FnOnce() -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        let saved_flag = {
            let mut flag = self.try_borrow_system_mode_mut()?;
            let saved_flag = *flag;
            *flag = new_flag;
            saved_flag
        };
        let res = f();
        let mut flag = self.try_borrow_system_mode_mut()?;
        *flag = saved_flag;
        res
    }

    fn in_system_mode(&self) -> Result<bool, HostError> {
        Ok(*self.try_borrow_system_mode()?)
    }

    pub(crate) fn system_to_user(&self, val: Val) -> Result<Val, HostError> {
        let scval = self.with_system_mode(|| self.from_host_val(val))?;
        self.with_user_mode(|| self.to_host_val(&scval))
    }

    fn obj_allowed(&self, obj: Object) -> Result<bool, HostError> {
        if self.in_system_mode()? {
            // System mode can read all objects.
            return Ok(true);
        }
        self.with_current_context_opt(|ctx| {
            Ok(if let Some(ctx) = ctx {
                // If we're in a specific context, there is an ACL
                // restricting which objects can be seen.
                if is_system_object(obj) {
                    // ACLs never allow access to system objects.
                    false
                } else {
                    ctx.acl.contains(handle_to_index(obj.get_handle()))
                }
            } else {
                // When there's no context, it means we're accessing the
                // host from outside of a contract or anything (eg. in the
                // embedding program, or a test) so all objects are allowed.
                true
            })
        })
    }

    fn allow_obj(&self, obj: Object) -> Result<(), HostError> {
        if is_system_object(obj) {
            if !self.in_system_mode()? {
                return Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "allow_obj on system object in non-system mode",
                    &[obj.to_val()],
                ));
            }
            // Allowing a system object in system mode is a no-op,
            // they're always allowed.
            return Ok(());
        }
        self.with_current_context_mut_opt(|ctx| {
            if let Some(ctx) = ctx {
                ctx.acl.insert(handle_to_index(obj.get_handle()));
            }
            Ok(())
        })
    }

    pub(crate) fn allow_val(&self, val: Val) -> Result<(), HostError> {
        if let Ok(obj) = Object::try_from(val) {
            self.allow_obj(obj)
        } else {
            Ok(())
        }
    }

    /// Moves a value of some type implementing [`HostObjectType`] into the host's
    /// object array, returning a [`HostObj`] containing the new object's array
    /// index, tagged with the [`xdr::ScObjectType`].
    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<HOT::Wrapper, HostError> {
        let system_mode = self.in_system_mode()?;
        let mut objects = if system_mode {
            self.try_borrow_system_objects_mut()?
        } else {
            self.try_borrow_objects_mut()?
        };
        let index = objects.len();
        // charge for the new host object, which is just the amortized cost of a single
        // `HostObject` allocation
        metered_clone::charge_heap_alloc::<HostObject>(1, self.as_budget())?;
        objects.push(HOT::inject(hot));
        let handle = index_to_handle(self, index, system_mode)?;
        let wrapper = HOT::new_from_handle(handle);
        // Any object we _create_ we're implicitly allowed to access.
        self.allow_obj(wrapper.clone().into())?;
        Ok(wrapper)
    }

    // Notes on metering: closure call needs to be metered separatedly. `VisitObject` only covers
    // the cost of visiting an object.
    pub(crate) unsafe fn unchecked_visit_val_obj<F, U>(
        &self,
        obj: impl Into<Object>,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(Option<&HostObject>) -> Result<U, HostError>,
    {
        self.charge_budget(ContractCostType::VisitObject, None)?;
        let obj: Object = obj.into();
        let handle: u32 = obj.get_handle();
        let index = handle_to_index(handle);
        let objects = if is_system_object_handle(handle) {
            self.try_borrow_system_objects()?
        } else {
            self.try_borrow_objects()?
        };
        if self.obj_allowed(obj)? {
            f(objects.get(index))
        } else {
            self.log_diagnostics("denied access to object", &[obj.to_val()])?;
            f(None)
        }
    }

    pub(crate) fn check_val_integrity(&self, val: Val) -> Result<(), HostError> {
        if let Ok(obj) = Object::try_from(val) {
            self.check_obj_integrity(obj)?;
        }
        Ok(())
    }

    pub(crate) fn check_obj_integrity(&self, obj: Object) -> Result<(), HostError> {
        unsafe {
            self.unchecked_visit_val_obj(obj, |hopt| match hopt {
                None => Err(self.err(
                    xdr::ScErrorType::Object,
                    xdr::ScErrorCode::MissingValue,
                    "unknown object reference",
                    &[],
                )),
                Some(hobj) => match (hobj, obj.to_val().get_tag()) {
                    (HostObject::Vec(_), Tag::VecObject)
                    | (HostObject::Map(_), Tag::MapObject)
                    | (HostObject::U64(_), Tag::U64Object)
                    | (HostObject::I64(_), Tag::I64Object)
                    | (HostObject::TimePoint(_), Tag::TimepointObject)
                    | (HostObject::Duration(_), Tag::DurationObject)
                    | (HostObject::U128(_), Tag::U128Object)
                    | (HostObject::I128(_), Tag::I128Object)
                    | (HostObject::U256(_), Tag::U256Object)
                    | (HostObject::I256(_), Tag::I256Object)
                    | (HostObject::Bytes(_), Tag::BytesObject)
                    | (HostObject::String(_), Tag::StringObject)
                    | (HostObject::Symbol(_), Tag::SymbolObject)
                    | (HostObject::Address(_), Tag::AddressObject) => Ok(()),
                    _ => Err(self.err(
                        xdr::ScErrorType::Object,
                        xdr::ScErrorCode::UnexpectedType,
                        "mis-tagged object reference",
                        &[],
                    )),
                },
            })
        }
    }

    // Notes on metering: object visiting part is covered by unchecked_visit_val_obj. Closure function
    // needs to be metered separately.
    pub(crate) fn visit_obj<HOT: HostObjectType, F, U>(
        &self,
        obj: HOT::Wrapper,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(&HOT) -> Result<U, HostError>,
    {
        unsafe {
            self.unchecked_visit_val_obj(obj, |hopt| match hopt {
                None => Err(self.err(
                    xdr::ScErrorType::Object,
                    xdr::ScErrorCode::MissingValue,
                    "unknown object reference",
                    &[],
                )),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(self.err(
                        xdr::ScErrorType::Object,
                        xdr::ScErrorCode::UnexpectedType,
                        "object reference type does not match tag",
                        &[],
                    )),
                    Some(hot) => f(hot),
                },
            })
        }
    }
}
