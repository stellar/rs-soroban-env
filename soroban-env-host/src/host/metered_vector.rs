use super::MeteredClone;
use crate::xdr::ScHostObjErrorCode;
use crate::{
    budget::{Budget, CostType},
    HostError,
};
use im_rc::{vector::Iter, vector::IterMut, Vector};
use std::ops::RangeBounds;
use std::rc::Rc;

pub(crate) struct MeteredVector<A> {
    budget: Budget,
    vec: Vector<A>,
}

impl<A> MeteredVector<A> {
    fn charge_new(&self) -> Result<(), HostError> {
        self.budget.charge(CostType::ImVecNew, 1)
    }

    fn charge_mut_access(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::ImVecMutEntry, x)
    }

    fn charge_immut_access(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::ImVecImmutEntry, x)
    }
}

// TODO: this whole section can probably be done with macro
impl<A: Clone> MeteredVector<A> {
    pub fn new(budget: Budget) -> Result<Self, HostError> {
        budget.charge(CostType::ImVecNew, 1)?;
        Ok(MeteredVector {
            budget,
            vec: Vector::new(),
        })
    }

    pub fn from_vec(budget: Budget, vec: Vector<A>) -> Result<Self, HostError> {
        budget.charge(CostType::ImVecNew, 1)?;
        Ok(MeteredVector { budget, vec })
    }

    // Time: O(log n)
    pub fn set(&mut self, index: usize, value: A) -> Result<A, HostError> {
        self.charge_mut_access(self.len() as u64)?;
        Ok(self.vec.set(index, value))
    }

    // Time: O(log n)
    pub fn get(&self, index: usize) -> Result<&A, HostError> {
        self.charge_immut_access(self.len() as u64)?;
        self.vec
            .get(index)
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    // Time: O(log n)
    pub fn remove(&mut self, index: usize) -> Result<A, HostError> {
        self.charge_mut_access(self.len() as u64)?;
        Ok(self.vec.remove(index))
    }

    // Time: O(1). Free of charge.
    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    // Time: O(log n) worst case, O(1) ammortized
    pub fn push_back(&mut self, value: A) -> Result<(), HostError> {
        self.charge_immut_access(self.len() as u64)?;
        Ok(self.vec.push_back(value))
    }

    // Time: O(1)
    pub fn pop_back(&mut self) -> Result<A, HostError> {
        self.charge_immut_access(self.len() as u64)?;
        self.vec
            .pop_back()
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    // Time: O(log n)
    pub fn front(&self) -> Result<&A, HostError> {
        self.charge_immut_access(self.len() as u64)?;
        self.vec
            .front()
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    // Time: O(log n)
    pub fn back(&self) -> Result<&A, HostError> {
        self.charge_immut_access(self.len() as u64)?;
        self.vec
            .back()
            .ok_or_else(|| ScHostObjErrorCode::VecIndexOutOfBound.into())
    }

    // Time: O(log n)
    pub fn insert(&mut self, index: usize, value: A) -> Result<(), HostError> {
        self.charge_mut_access(self.len() as u64)?;
        Ok(self.vec.insert(index, value))
    }

    // Time: O(log n)
    pub fn append(&mut self, other: Self) -> Result<(), HostError> {
        self.charge_mut_access((self.len() + other.len()) as u64)?;
        Ok(self.vec.append(other.vec))
    }

    // Time: O(log n)
    pub fn slice<R>(&mut self, range: R) -> Result<Self, HostError>
    where
        R: RangeBounds<usize>,
    {
        self.charge_mut_access(self.len() as u64)?;
        Ok(MeteredVector {
            budget: self.budget.clone(),
            vec: self.vec.slice(range),
        })
    }

    /// Time: O(1)
    #[inline]
    pub fn iter(&self) -> Iter<'_, A> {
        self.vec.iter()
    }

    /// Time: O(1)
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, A> {
        self.vec.iter_mut()
    }
}

impl<A: Clone> Clone for MeteredVector<A> {
    fn clone(&self) -> Self {
        Self {
            budget: self.budget.clone(),
            vec: self.vec.clone(),
        }
    }
}

impl<A: Clone> MeteredClone for MeteredVector<A> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        assert!(Rc::ptr_eq(&self.budget.0, &budget.0));
        self.charge_new()?;
        Ok(self.clone())
    }
}

impl<A: Clone + PartialEq> PartialEq for MeteredVector<A> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.budget.0, &other.budget.0) && self.vec == other.vec
    }
}

impl<A: Clone + Eq> Eq for MeteredVector<A> {}

impl<A: Clone + PartialOrd> PartialOrd for MeteredVector<A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.vec.partial_cmp(&other.vec)
    }
}

impl<A: Clone + Ord> Ord for MeteredVector<A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.vec.cmp(&other.vec)
    }
}
