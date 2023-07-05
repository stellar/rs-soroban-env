use bs::Bs;
use soroban_env_common::{
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Compare, DurationSmall, Error, I128Small, I256Small, I64Small, SymbolSmall, SymbolStr, Tag,
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

/// A ticket is a number that represents permission to read a set of host
/// objects: all those created with the ticket, or readable transitively through
/// other objects readable with the ticket.
///
/// Every object holds a compact (typically 1-word) [`Tickets`] bitset
/// containing all the tickets that are allowed to read it. These are updated
/// lazily on access: whenever an object reference X is read out of an object Y,
/// the ticket T(Y) of Y is unioned into the ticket of X, propagating the fact
/// that "since Y is reachable from T(Y), X is also reachable from T(Y)".
///
/// The [`Host`] maintains a "current" ticket, which is incremented every time a
/// new frame is pushed, and is used for the initial ticket-set of new objects,
/// as well as the ticket checked for read access to objects.
///
/// By default this means that an object created in a frame is inaccessible from
/// other frames. The exception is when a user explicitly grants access to an
/// object by _passing_ a reference to it from one frame to another. This sets
/// the ticket of the object to include the callee frame's ticket, which in turn
/// makes all objects transitively reachable through that object readable by the
/// callee.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Ticket(usize);
impl Ticket {
    // Increments the ticket and returns its new value.
    pub(crate) fn bump(&mut self) -> Result<Ticket, HostError> {
        match self.0.checked_add(1) {
            Some(t) => {
                self.0 = t;
                Ok(Ticket(t))
            }
            None => {
                Err(Error::from_type_and_code(ScErrorType::Value, ScErrorCode::ArithDomain).into())
            }
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq)]
pub(crate) struct Tickets(Bs);
impl Tickets {
    fn from_ticket(ticket: Ticket) -> Tickets {
        let mut tickets = Self::default();
        tickets.allow_ticket(ticket);
        tickets
    }
    fn allow_ticket(&mut self, ticket: Ticket) {
        self.0.insert(ticket.0);
    }
    fn allow_tickets(&mut self, tickets: &Tickets) {
        self.0.union_with(&tickets.0)
    }
    fn is_allowed(&self, ticket: Ticket) -> bool {
        self.0.contains(ticket.0)
    }
}
impl core::fmt::Debug for Tickets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elts: Vec<usize> = (&self.0).into();
        f.debug_tuple("Tickets").field(&elts).finish()
    }
}

pub(crate) type HostMap = MeteredOrdMap<Val, Val, Host>;
pub(crate) type HostVec = MeteredVector<Val>;

#[derive(Clone)]
pub(crate) struct HostObject {
    pub(crate) tickets: Tickets,
    pub(crate) body: HostObjectBody,
}

#[derive(Clone)]
pub enum HostObjectBody {
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

impl HostObjectBody {
    // Temporarily performs a shallow comparison against a Val of the
    // associated small value type, returning None if the Val is of
    // the wrong type.
    pub(crate) fn try_compare_to_small(
        &self,
        budget: &Budget,
        rv: Val,
    ) -> Result<Option<core::cmp::Ordering>, HostError> {
        let res = match self {
            HostObjectBody::U64(u) => {
                let Ok(small) = U64Small::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObjectBody::I64(i) => {
                let Ok(small) = I64Small::try_from(rv) else { return Ok(None) };
                let small: i64 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObjectBody::TimePoint(tp) => {
                let Ok(small) = TimepointSmall::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(&tp.0, &small)?)
            }
            HostObjectBody::Duration(d) => {
                let Ok(small) = DurationSmall::try_from(rv) else { return Ok(None) };
                let small: u64 = small.into();
                Some(budget.compare(&d.0, &small)?)
            }
            HostObjectBody::U128(u) => {
                let Ok(small) = U128Small::try_from(rv) else { return Ok(None) };
                let small: u128 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObjectBody::I128(i) => {
                let Ok(small) = I128Small::try_from(rv) else { return Ok(None) };
                let small: i128 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObjectBody::U256(u) => {
                let Ok(small) = U256Small::try_from(rv) else { return Ok(None) };
                let small: U256 = small.into();
                Some(budget.compare(u, &small)?)
            }
            HostObjectBody::I256(i) => {
                let Ok(small) = I256Small::try_from(rv) else { return Ok(None) };
                let small: I256 = small.into();
                Some(budget.compare(i, &small)?)
            }
            HostObjectBody::Symbol(s) => {
                let Ok(small) = SymbolSmall::try_from(rv) else { return Ok(None) };
                let small: SymbolStr = small.into();
                let rhs: &[u8] = small.as_ref();
                Some(budget.compare(&s.as_vec().as_slice(), &rhs)?)
            }

            HostObjectBody::Vec(_)
            | HostObjectBody::Map(_)
            | HostObjectBody::Bytes(_)
            | HostObjectBody::String(_)
            | HostObjectBody::Address(_) => None,
        };
        Ok(res)
    }
}

pub trait HostObjectType: MeteredClone {
    type Wrapper: Into<Object>;
    fn new_from_handle(handle: u32) -> Self::Wrapper;
    fn inject(self) -> HostObjectBody;
    fn try_extract(obj: &HostObjectBody) -> Option<&Self>;
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
            fn inject(self) -> HostObjectBody {
                HostObjectBody::$CASE(self)
            }

            fn try_extract(obj: &HostObjectBody) -> Option<&Self> {
                match obj {
                    HostObjectBody::$CASE(v) => Some(v),
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

impl Host {
    /// Moves a value of some type implementing [`HostObjectType`] into the host's
    /// object array, returning a [`HostObj`] containing the new object's array
    /// index, tagged with the [`xdr::ScObjectType`].
    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<HOT::Wrapper, HostError> {
        let prev_len = self.try_borrow_objects()?.len();
        if prev_len > u32::MAX as usize {
            return Err(self.err_arith_overflow());
        }
        // charge for the new host object, which is just the amortized cost of a single
        // `HostObject` allocation
        metered_clone::charge_heap_alloc::<HostObject>(1, self.as_budget())?;
        let tickets = Tickets::from_ticket(self.get_current_ticket()?);
        // eprintln!("adding host object with tickets {:?}", tickets);
        let body = HOT::inject(hot);
        self.try_borrow_objects_mut()?
            .push(HostObject { tickets, body });
        let handle = prev_len as u32;
        Ok(HOT::new_from_handle(handle))
    }

    // Notes on metering: closure call needs to be metered separatedly. `VisitObject` only covers
    // the cost of visiting an object.
    pub(crate) unsafe fn unchecked_visit_val_obj<F, U>(
        &self,
        obj: impl Into<Object>,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(Option<&HostObjectBody>) -> Result<U, HostError>,
    {
        self.charge_budget(ContractCostType::VisitObject, None)?;
        let r = self.try_borrow_objects()?;
        let obj: Object = obj.into();
        let handle: u32 = obj.get_handle();
        let tkt = self.get_current_ticket()?;
        match r.get(handle as usize) {
            Some(hobj) if hobj.tickets.is_allowed(tkt) => f(Some(&hobj.body)),
            // We treat "missing object" and "bad ticket" identically to
            // eliminate risk of even leaking existence-of-an-object.
            _ => f(None),
        }
    }

    /// If `val` is an [`Object`] that is readable by `caller_ticket`, adds
    /// `callee_ticket` to the object's set of [`Ticket`]s. This should
    /// be called any time `val` is passed from a caller contract to a callee.
    ///
    /// # Errors
    ///
    /// If `val` is an object that is not readable by `caller_ticket` or is
    /// a handle to a nonexistent object, an error is returned.
    pub(crate) fn propagate_ticket(
        &self,
        val: Val,
        curr_ticket: Ticket,
        new_ticket: Ticket,
    ) -> Result<(), HostError> {
        //eprintln!("checking if {:?} is object", val);
        let Ok(obj) = Object::try_from(val) else { return Ok(()) };
        self.charge_budget(ContractCostType::VisitObject, None)?;
        let mut r = self.try_borrow_objects_mut()?;
        let handle: u32 = obj.get_handle();
        //eprintln!("propagating tickets {:?} -> {:?} on object {}", curr_ticket, new_ticket, handle);
        match r.get_mut(handle as usize) {
            Some(hobj) if hobj.tickets.is_allowed(curr_ticket) => {
                hobj.tickets.allow_ticket(new_ticket);
                // eprintln!("updated ticket on obj {:?} to {:?}", obj, hobj.tickets);
                Ok(())
            }
            _ => Err(self.err(
                xdr::ScErrorType::Object,
                xdr::ScErrorCode::MissingValue,
                "unknown object reference in propagate_ticket",
                &[],
            )),
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
                    (HostObjectBody::Vec(_), Tag::VecObject)
                    | (HostObjectBody::Map(_), Tag::MapObject)
                    | (HostObjectBody::U64(_), Tag::U64Object)
                    | (HostObjectBody::I64(_), Tag::I64Object)
                    | (HostObjectBody::TimePoint(_), Tag::TimepointObject)
                    | (HostObjectBody::Duration(_), Tag::DurationObject)
                    | (HostObjectBody::U128(_), Tag::U128Object)
                    | (HostObjectBody::I128(_), Tag::I128Object)
                    | (HostObjectBody::U256(_), Tag::U256Object)
                    | (HostObjectBody::I256(_), Tag::I256Object)
                    | (HostObjectBody::Bytes(_), Tag::BytesObject)
                    | (HostObjectBody::String(_), Tag::StringObject)
                    | (HostObjectBody::Symbol(_), Tag::SymbolObject)
                    | (HostObjectBody::Address(_), Tag::AddressObject) => Ok(()),
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
                    "unknown object reference in visit_obj",
                    &[],
                )),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(self.err(
                        xdr::ScErrorType::Object,
                        xdr::ScErrorCode::UnexpectedType,
                        "object reference type does not match tag in visit_obj",
                        &[],
                    )),
                    Some(hot) => f(hot),
                },
            })
        }
    }

    /// Similar to [`visit_obj`] but assumes that `f` is a function that
    /// extracts a [`Val`] from `outer_obj` and, if the extracted `Val` is an
    /// [`Object`], propagates the ticket-set from `outer_obj` to that object.
    /// This lazily propagates the reachability relationship from containers to
    /// any objects contained within them. It should be used in preference to
    /// `visit_obj` for any host function extracting a value from a container.
    pub(crate) fn visit_obj_propagating_tickets<HOT: HostObjectType, F>(
        &self,
        outer_obj: impl Into<Object>,
        f: F,
    ) -> Result<Val, HostError>
    where
        F: FnOnce(&HOT) -> Result<Val, HostError>,
    {
        self.charge_budget(ContractCostType::VisitObject, None)?;
        let mut r = self.try_borrow_objects_mut()?;
        let outer_obj: Object = outer_obj.into();
        let outer_handle: u32 = outer_obj.get_handle();
        let tkt = self.get_current_ticket()?;
        let (outer_tickets, val) = match r.get(outer_handle as usize) {
            Some(hobj) if hobj.tickets.is_allowed(tkt) => match HOT::try_extract(&hobj.body) {
                None => {
                    return Err(self.err(
                        xdr::ScErrorType::Object,
                        xdr::ScErrorCode::UnexpectedType,
                        "object reference type does not match tag in visit_obj_propagating_tag",
                        &[],
                    ))
                }
                Some(hot) => (hobj.tickets.clone(), f(hot)?),
            },
            // We treat "missing object" and "bad ticket" identically to
            // eliminate risk of even leaking existence-of-an-object.
            _ => {
                return Err(self.err(
                    xdr::ScErrorType::Object,
                    xdr::ScErrorCode::MissingValue,
                    "unknown object reference in visit_obj_propagating_tag",
                    &[],
                ))
            }
        };

        // If `f` returned an object reference, we assume -- the point of
        // calling this function is to imply -- that `f` extracted that
        // return-value reference from the outer object reference and so we
        // propagate the ticket from the outer to inner.
        if let Ok(inner_obj) = Object::try_from(val) {
            self.charge_budget(ContractCostType::VisitObject, None)?;
            let inner_handle = inner_obj.get_handle();
            match r.get_mut(inner_handle as usize) {
                Some(hobj) => hobj.tickets.allow_tickets(&outer_tickets),
                None => (),
            }
        }
        Ok(val)
    }
}
