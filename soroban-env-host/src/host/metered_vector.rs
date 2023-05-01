use soroban_env_common::{xdr::ScHostFnErrorCode, Compare};

use super::{declared_size::DeclaredSizeForMetering, MeteredClone};
use crate::{
    budget::{AsBudget, Budget},
    xdr::{ContractCostType, ScHostObjErrorCode},
    Host, HostError,
};
use std::{cmp::Ordering, ops::Range};

#[derive(Clone, Default)]
pub struct MeteredVector<A> {
    vec: Vec<A>,
}

impl<A> MeteredVector<A>
where
    A: DeclaredSizeForMetering,
{
    fn charge_access(&self, count: usize, budget: &Budget) -> Result<(), HostError> {
        budget.batched_charge(ContractCostType::VecEntry, count as u64, None)
    }

    fn charge_scan(&self, budget: &Budget) -> Result<(), HostError> {
        budget.batched_charge(ContractCostType::VecEntry, self.vec.len() as u64, None)
    }

    fn charge_binsearch(&self, budget: &Budget) -> Result<(), HostError> {
        let mag = 64 - (self.vec.len() as u64).leading_zeros();
        budget.batched_charge(ContractCostType::VecEntry, 1 + mag as u64, None)
    }
}

impl<A> MeteredVector<A>
where
    A: MeteredClone,
{
    pub fn new() -> Result<Self, HostError> {
        Self::from_vec(Vec::new())
    }

    // Constructs a new, empty `MeteredVector` with at least the specified capacity.
    // This is purely used for the cost calibration of allocating host memory.
    // Do *not* use it for construction, since `MeteredVector` is immutable,
    // the allocation will be wasted.
    #[cfg(any(test, feature = "testutils"))]
    pub fn with_capacity(capacity: usize, budget: &Budget) -> Result<Self, HostError> {
        super::metered_clone::charge_heap_alloc::<A>(capacity as u64, budget)?;
        Self::from_vec(Vec::with_capacity(capacity))
    }

    pub fn from_array(buf: &[A], budget: &Budget) -> Result<Self, HostError> {
        // we may temporarily go over budget here.
        let vec: Vec<A> = buf.into();
        vec.charge_deep_clone(budget)?;
        Self::from_vec(vec)
    }

    // No meter charge, assuming allocation cost has been covered by the caller from the outside.
    pub fn from_vec(vec: Vec<A>) -> Result<Self, HostError> {
        Ok(Self { vec })
    }

    pub fn as_slice(&self) -> &[A] {
        self.vec.as_slice()
    }

    // This doesn't take ExactSizeIterator since that is not implemented for Chain
    // (see https://github.com/rust-lang/rust/issues/34433) but it only works
    // with iterators that report an exact size_hint, and it constructs a new
    // Vec from that iterator with a single allocation-and-copy.
    pub fn from_exact_iter<I: Iterator<Item = A>>(
        iter: I,
        budget: &Budget,
    ) -> Result<Self, HostError> {
        if let (_, Some(sz)) = iter.size_hint() {
            // It's possible we temporarily go over-budget here before charging, but
            // only by the cost of temporarily allocating twice the size of our largest
            // possible object. In exchange we get to batch all charges associated with
            // the clone into one (when A::IS_SHALLOW==true).
            let vec: Vec<A> = iter.collect();
            vec.charge_deep_clone(budget)?;
            Self::from_vec(vec)
        } else {
            // TODO use a better error code for "unbounded input iterators"
            Err(ScHostFnErrorCode::UnknownError.into())
        }
    }

    pub fn set(&self, index: usize, value: A, budget: &Budget) -> Result<Self, HostError> {
        let mut new = self.metered_clone(budget)?;
        new.charge_access(1, budget)?;
        let cell: Result<&mut A, HostError> = new
            .vec
            .get_mut(index)
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into());
        *(cell?) = value;
        Ok(new)
    }

    pub fn get(&self, index: usize, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec
            .get(index)
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn push_front(&self, value: A, budget: &Budget) -> Result<Self, HostError> {
        let iter = [value].into_iter().chain(self.vec.iter().cloned());
        Self::from_exact_iter(iter, budget)
    }

    pub fn pop_front(&self, budget: &Budget) -> Result<Self, HostError> {
        if self.vec.is_empty() {
            Err(ScHostObjErrorCode::VecIndexOutOfBound.into())
        } else {
            let iter = self.vec.iter().skip(1).cloned();
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn push_back(&self, value: A, budget: &Budget) -> Result<Self, HostError> {
        let iter = self.vec.iter().cloned().chain([value].into_iter());
        Self::from_exact_iter(iter, budget)
    }

    fn err_overflow() -> HostError {
        // TODO: need a better overflow code.
        ScHostFnErrorCode::UnknownError.into()
    }

    fn add_or_overflow(x: usize, y: usize) -> Result<usize, HostError> {
        x.checked_add(y).ok_or_else(|| Self::err_overflow())
    }

    fn sub_or_overflow(x: usize, y: usize) -> Result<usize, HostError> {
        x.checked_sub(y).ok_or_else(|| Self::err_overflow())
    }

    pub fn pop_back(&self, budget: &Budget) -> Result<Self, HostError> {
        if self.vec.is_empty() {
            Err(ScHostObjErrorCode::VecIndexOutOfBound.into())
        } else {
            let count = Self::sub_or_overflow(self.vec.len(), 1)?;
            let iter = self.vec.iter().take(count).cloned();
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn remove(&self, idx: usize, budget: &Budget) -> Result<Self, HostError> {
        if idx >= self.vec.len() || idx == usize::MAX - 1 {
            Err(ScHostObjErrorCode::VecIndexOutOfBound.into())
        } else {
            // [0, 1, 2]
            // del 1 => take(1) + skip(2)
            let skip = Self::add_or_overflow(idx, 1)?;
            let init = self.vec.iter().take(idx).cloned();
            let fini = self.vec.iter().skip(skip).cloned();
            Self::from_exact_iter(init.chain(fini), budget)
        }
    }

    pub fn front(&self, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec
            .first()
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    pub fn back(&self, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec
            .last()
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    pub fn insert(&self, index: usize, value: A, budget: &Budget) -> Result<Self, HostError> {
        let len = self.vec.len();
        if index > len {
            Err(ScHostObjErrorCode::VecIndexOutOfBound.into())
        } else if index == len {
            self.push_back(value, budget)
        } else if index == 0 {
            self.push_front(value, budget)
        } else {
            let init = self.vec.iter().take(index).cloned();
            let fini = self.vec.iter().skip(index).cloned();
            let iter = init.chain([value].into_iter()).chain(fini);
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn append(&self, other: &Self, budget: &Budget) -> Result<Self, HostError> {
        let iter = self.vec.iter().cloned().chain(other.vec.iter().cloned());
        Self::from_exact_iter(iter, budget)
    }

    pub fn slice(&self, range: Range<usize>, budget: &Budget) -> Result<Self, HostError> {
        match self.vec.get(range) {
            Some(slice) => Self::from_exact_iter(slice.iter().cloned(), budget),
            None => Err(ScHostObjErrorCode::VecIndexOutOfBound.into()),
        }
    }

    pub fn first_index_of<F>(&self, f: F, budget: &Budget) -> Result<Option<usize>, HostError>
    where
        F: Fn(&A) -> Result<Ordering, HostError>,
    {
        self.charge_scan(budget)?;
        let mut i = 0;
        let iter = self.vec.iter();
        // this is similar logic to `iter.position(f)` but is fallible
        for val in iter {
            if f(val)? == Ordering::Equal {
                return Ok(Some(i));
            }
            i += 1;
        }
        Ok(None)
    }

    pub fn last_index_of<F>(&self, f: F, budget: &Budget) -> Result<Option<usize>, HostError>
    where
        F: Fn(&A) -> Result<Ordering, HostError>,
    {
        self.charge_scan(budget)?;
        let mut i = self.vec.len();
        let mut iter = self.vec.iter();
        // this is similar logic to `iter.rposition(f)` but is fallible
        while let Some(val) = iter.next_back() {
            i -= 1;
            if f(val)? == Ordering::Equal {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn binary_search_by<F>(
        &self,
        mut cmp: F,
        budget: &Budget,
    ) -> Result<Result<usize, usize>, HostError>
    where
        F: FnMut(&A) -> Result<Ordering, HostError>,
    {
        self.charge_binsearch(budget)?;
        let mut err: Option<HostError> = None;
        let res = self.vec.binary_search_by(|probe| {
            // We've already hit an error, return Ordering::Equal
            // to terminate search asap.
            if err.is_some() {
                return Ordering::Equal;
            }
            match cmp(probe) {
                Ok(ord) => ord,
                Err(he) => {
                    err = Some(he);
                    Ordering::Equal
                }
            }
        });
        match err {
            Some(he) => Err(he),
            None => Ok(res),
        }
    }

    pub fn retain_mut<F>(&mut self, mut f: F, budget: &Budget) -> Result<Self, HostError>
    where
        F: FnMut(usize, &mut A) -> Result<bool, HostError>,
    {
        // The closure evaluation is not metered here, it is assumed to be taken care of outside.
        // Here just covers the cost of cloning a Vec.
        let mut vec = Vec::with_capacity(self.len());
        for (i, v) in self.vec.iter_mut().enumerate() {
            if f(i, v)? {
                vec.push(v.clone());
            }
        }
        vec.charge_deep_clone(budget)?;
        Self::from_vec(vec)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, A> {
        self.vec.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, A> {
        self.vec.iter_mut()
    }
}

impl<A> DeclaredSizeForMetering for MeteredVector<A>
where
    A: DeclaredSizeForMetering,
{
    const DECLARED_SIZE: u64 = <Vec<A> as DeclaredSizeForMetering>::DECLARED_SIZE;
}

impl<A> MeteredClone for MeteredVector<A>
where
    A: MeteredClone,
{
    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.vec.charge_for_substructure(budget)
    }
}

impl<Elt: MeteredClone> Compare<MeteredVector<Elt>> for Budget
where
    Budget: Compare<Elt, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredVector<Elt>,
        b: &MeteredVector<Elt>,
    ) -> Result<Ordering, Self::Error> {
        self.as_budget().batched_charge(
            ContractCostType::VecEntry,
            a.vec.len().min(b.vec.len()) as u64,
            None,
        )?;
        <Self as Compare<Vec<Elt>>>::compare(self, &a.vec, &b.vec)
    }
}

impl<Elt: MeteredClone> Compare<MeteredVector<Elt>> for Host
where
    Host: Compare<Elt, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredVector<Elt>,
        b: &MeteredVector<Elt>,
    ) -> Result<Ordering, Self::Error> {
        self.as_budget().batched_charge(
            ContractCostType::VecEntry,
            a.vec.len().min(b.vec.len()) as u64,
            None,
        )?;
        <Self as Compare<Vec<Elt>>>::compare(self, &a.vec, &b.vec)
    }
}
