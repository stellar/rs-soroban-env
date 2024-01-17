use crate::{
    budget::{AsBudget, Budget},
    host::{declared_size::DeclaredSizeForMetering, MeteredClone},
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Compare, Error, Host, HostError,
};

use std::{cmp::Ordering, ops::Range};

const VEC_OOB: Error = Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);

#[derive(Clone)]
pub struct MeteredVector<A> {
    pub(crate) vec: Vec<A>,
}

impl<A> Default for MeteredVector<A> {
    fn default() -> Self {
        Self {
            vec: Default::default(),
        }
    }
}

impl<T> std::hash::Hash for MeteredVector<T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.vec.hash(state);
    }
}

impl<A> MeteredVector<A>
where
    A: DeclaredSizeForMetering,
{
    fn charge_access(&self, count: usize, budget: &Budget) -> Result<(), HostError> {
        budget.charge(
            ContractCostType::MemCpy,
            Some(A::DECLARED_SIZE.saturating_mul(count as u64)),
        )
    }

    fn charge_scan(&self, budget: &Budget) -> Result<(), HostError> {
        budget.charge(
            ContractCostType::MemCpy,
            Some(A::DECLARED_SIZE.saturating_mul(self.vec.len() as u64)),
        )
    }

    // Charge binary search includes accessing number of entries expected for
    // finding an entry. Cost of comparison is charged separately and not covered here.
    fn charge_binsearch(&self, budget: &Budget) -> Result<(), HostError> {
        let mag = 64u32.saturating_sub((self.vec.len() as u64).leading_zeros());
        budget.charge(
            ContractCostType::MemCpy,
            Some(A::DECLARED_SIZE.saturating_mul((1 + mag) as u64)),
        )
    }
}

impl<A> MeteredVector<A>
where
    A: MeteredClone,
{
    // Constructs a empty new `MeteredVector`.
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    // Constructs a new, empty `MeteredVector` with at least the specified capacity.
    // This is purely used for the cost calibration of allocating host memory.
    // Do *not* use it for construction, since `MeteredVector` is immutable,
    // the allocation will be wasted.
    #[cfg(feature = "bench")]
    pub fn with_capacity(capacity: usize, budget: &Budget) -> Result<Self, HostError> {
        super::metered_clone::charge_heap_alloc::<A>(capacity as u64, budget)?;
        Self::from_vec(Vec::with_capacity(capacity))
    }

    // No meter charge, assuming allocation cost has been covered by the caller from the outside.
    pub fn from_vec(vec: Vec<A>) -> Result<Self, HostError> {
        if u32::try_from(vec.len()).is_err() {
            Err(VEC_OOB.into())
        } else {
            Ok(Self { vec })
        }
    }

    pub fn as_slice(&self) -> &[A] {
        self.vec.as_slice()
    }

    pub fn as_mut_slice(&mut self) -> &mut [A] {
        self.vec.as_mut_slice()
    }

    // This doesn't take ExactSizeIterator since that is not implemented for Chain
    // (see https://github.com/rust-lang/rust/issues/34433) but it only works
    // with iterators that report an exact size_hint, and it constructs a new
    // Vec from that iterator with a single allocation-and-copy.
    pub fn from_exact_iter<I: Iterator<Item = A>>(
        iter: I,
        budget: &Budget,
    ) -> Result<Self, HostError> {
        let _span = tracy_span!("new vec");
        if let (_, Some(sz)) = iter.size_hint() {
            if u32::try_from(sz).is_err() {
                Err(VEC_OOB.into())
            } else {
                // It's possible we temporarily go over-budget here before charging, but
                // only by the cost of temporarily allocating twice the size of our largest
                // possible object. In exchange we get to batch all charges associated with
                // the clone into one (when A::IS_SHALLOW==true).
                let vec: Vec<A> = iter.collect();
                vec.charge_deep_clone(budget)?;
                Self::from_vec(vec)
            }
        } else {
            // This is a logic error, we should never get here.
            Err((ScErrorType::Object, ScErrorCode::InternalError).into())
        }
    }

    pub fn set(&self, index: usize, value: A, budget: &Budget) -> Result<Self, HostError> {
        let mut new = self.metered_clone(budget)?;
        new.charge_access(1, budget)?;
        let cell: Result<&mut A, HostError> = new.vec.get_mut(index).ok_or_else(|| VEC_OOB.into());
        *(cell?) = value;
        Ok(new)
    }

    pub fn get(&self, index: usize, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec.get(index).ok_or_else(|| VEC_OOB.into())
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn push_front(&self, value: A, budget: &Budget) -> Result<Self, HostError> {
        if self.len() == u32::MAX as usize {
            Err(VEC_OOB.into())
        } else {
            let iter = [value].into_iter().chain(self.vec.iter().cloned());
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn pop_front(&self, budget: &Budget) -> Result<Self, HostError> {
        if self.vec.is_empty() {
            Err(VEC_OOB.into())
        } else {
            let iter = self.vec.iter().skip(1).cloned();
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn push_back(&self, value: A, budget: &Budget) -> Result<Self, HostError> {
        if self.len() == u32::MAX as usize {
            Err(VEC_OOB.into())
        } else {
            let iter = self.vec.iter().cloned().chain([value]);
            Self::from_exact_iter(iter, budget)
        }
    }

    fn err_oob() -> HostError {
        VEC_OOB.into()
    }

    fn add_or_err(x: usize, y: usize) -> Result<usize, HostError> {
        let sz = x.checked_add(y).ok_or_else(|| Self::err_oob())?;
        if u32::try_from(sz).is_err() {
            Err(Self::err_oob())
        } else {
            Ok(sz)
        }
    }

    fn sub_or_err(x: usize, y: usize) -> Result<usize, HostError> {
        x.checked_sub(y).ok_or_else(|| Self::err_oob())
    }

    pub fn pop_back(&self, budget: &Budget) -> Result<Self, HostError> {
        if self.vec.is_empty() {
            Err(VEC_OOB.into())
        } else {
            let count = Self::sub_or_err(self.vec.len(), 1)?;
            let iter = self.vec.iter().take(count).cloned();
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn remove(&self, idx: usize, budget: &Budget) -> Result<Self, HostError> {
        if idx >= self.vec.len() || idx == usize::MAX - 1 {
            Err(VEC_OOB.into())
        } else {
            // [0, 1, 2]
            // del 1 => take(1) + skip(2)
            let skip = Self::add_or_err(idx, 1)?;
            let init = self.vec.iter().take(idx).cloned();
            let fini = self.vec.iter().skip(skip).cloned();
            Self::from_exact_iter(init.chain(fini), budget)
        }
    }

    pub fn front(&self, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec.first().ok_or_else(|| VEC_OOB.into())
    }

    pub fn back(&self, budget: &Budget) -> Result<&A, HostError> {
        self.charge_access(1, budget)?;
        self.vec.last().ok_or_else(|| VEC_OOB.into())
    }

    pub fn insert(&self, index: usize, value: A, budget: &Budget) -> Result<Self, HostError> {
        if self.vec.len() == u32::MAX as usize {
            return Err(VEC_OOB.into());
        }
        let len = self.vec.len();
        if index > len {
            Err(VEC_OOB.into())
        } else if index == len {
            self.push_back(value, budget)
        } else if index == 0 {
            self.push_front(value, budget)
        } else {
            let init = self.vec.iter().take(index).cloned();
            let fini = self.vec.iter().skip(index).cloned();
            let iter = init.chain([value]).chain(fini);
            Self::from_exact_iter(iter, budget)
        }
    }

    pub fn append(&self, other: &Self, budget: &Budget) -> Result<Self, HostError> {
        Self::add_or_err(self.len(), other.len())?;
        let iter = self.vec.iter().cloned().chain(other.vec.iter().cloned());
        Self::from_exact_iter(iter, budget)
    }

    pub fn slice(&self, range: Range<usize>, budget: &Budget) -> Result<Self, HostError> {
        match self.vec.get(range) {
            Some(slice) => Self::from_exact_iter(slice.iter().cloned(), budget),
            None => Err(VEC_OOB.into()),
        }
    }

    pub fn first_index_of<F>(&self, f: F, budget: &Budget) -> Result<Option<usize>, HostError>
    where
        F: Fn(&A) -> Result<Ordering, HostError>,
    {
        self.charge_scan(budget)?;
        // this is similar logic to `iter.position(f)` but is fallible
        for (i, val) in self.vec.iter().enumerate() {
            if f(val)? == Ordering::Equal {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    pub fn last_index_of<F>(&self, f: F, budget: &Budget) -> Result<Option<usize>, HostError>
    where
        F: Fn(&A) -> Result<Ordering, HostError>,
    {
        self.charge_scan(budget)?;
        // this is similar logic to `iter.rposition(f)` but is fallible
        for (i, val) in self.vec.iter().enumerate().rev() {
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

    pub fn to_vec(&self, budget: &Budget) -> Result<Vec<A>, HostError> {
        self.vec.metered_clone(budget)
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
    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
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
        // Here covers the cost of accessing number of map entries. The cost of
        // comparing entries is covered by the `compare` call below.
        self.as_budget().charge(
            ContractCostType::MemCpy,
            Some(Elt::DECLARED_SIZE.saturating_mul(a.vec.len().min(b.vec.len()) as u64)),
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
        // Here covers the cost of accessing number of map entries. The cost of
        // comparing entries is covered by the `compare` call below.
        self.as_budget().charge(
            ContractCostType::MemCpy,
            Some(Elt::DECLARED_SIZE.saturating_mul(a.vec.len().min(b.vec.len()) as u64)),
        )?;
        <Self as Compare<Vec<Elt>>>::compare(self, &a.vec, &b.vec)
    }
}
