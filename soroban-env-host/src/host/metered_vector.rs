use soroban_env_common::{xdr::ScHostFnErrorCode, Compare};

use super::MeteredClone;
use crate::{
    budget::{AsBudget, Budget, CostType},
    xdr::ScHostObjErrorCode,
    Host, HostError,
};
use std::{cmp::Ordering, ops::Range};

#[derive(Clone)]
pub struct MeteredVector<A>
where
    A: MeteredClone,
{
    vec: Vec<A>,
}

impl<A> MeteredVector<A>
where
    A: MeteredClone,
{
    fn charge_new(size: usize, budget: &Budget) -> Result<(), HostError> {
        budget.charge(CostType::VecNew, size as u64)
    }

    fn charge_access(&self, count: usize, budget: &Budget) -> Result<(), HostError> {
        budget.charge(CostType::VecEntry, count as u64)
    }

    fn charge_scan(&self, budget: &Budget) -> Result<(), HostError> {
        budget.charge(CostType::VecEntry, self.len() as u64)
    }

    fn charge_binsearch(&self, budget: &Budget) -> Result<(), HostError> {
        let mag = 64 - (self.len() as u64).leading_zeros();
        budget.charge(CostType::VecEntry, 1 + mag as u64)
    }
}

impl<A> MeteredVector<A>
where
    A: MeteredClone,
{
    pub fn new(budget: &Budget) -> Result<Self, HostError> {
        Self::charge_new(0, budget)?;
        Self::from_vec(Vec::new())
    }

    pub fn from_array<const N: usize>(buf: [A; N], budget: &Budget) -> Result<Self, HostError> {
        Self::charge_new(N, budget)?;
        Self::from_vec(buf.into())
    }

    pub fn from_vec(vec: Vec<A>) -> Result<Self, HostError> {
        // No charge here: vector already allocated, charge happened in caller.
        Ok(Self { vec })
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
            Self::charge_new(sz, budget)?;
            // It's possible we temporarily go over-budget here before charging, but
            // only by the cost of temporarily allocating twice the size of our largest
            // possible object. In exchange we get to batch all charges associated with
            // the clone into one (when A::IS_SHALLOW==true).
            let vec: Vec<A> = iter.collect();
            A::charge_for_clones(vec.as_slice(), budget)?;
            Ok(Self { vec })
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
        if self.vec.len() == 0 {
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
        if self.vec.len() == 0 {
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
        let mut iter = self.vec.iter();
        // this is similar logic to `iter.position(f)` but is fallible
        while let Some(val) = iter.next() {
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

    pub fn iter(&self) -> std::slice::Iter<'_, A> {
        self.vec.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, A> {
        self.vec.iter_mut()
    }
}

impl<A> MeteredClone for MeteredVector<A>
where
    A: MeteredClone,
{
    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        Self::charge_new(self.len(), budget)
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
        self.as_budget()
            .charge(CostType::VecEntry, a.vec.len().min(b.vec.len()) as u64)?;
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
        self.as_budget()
            .charge(CostType::VecEntry, a.vec.len().min(b.vec.len()) as u64)?;
        <Self as Compare<Vec<Elt>>>::compare(self, &a.vec, &b.vec)
    }
}
