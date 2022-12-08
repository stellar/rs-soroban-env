use std::rc::Rc;

use soroban_env_common::{
    xdr::{BytesM, LedgerEntry, LedgerKey, ScMap, ScObject, ScVal},
    RawVal,
};

use crate::{
    budget::{Budget, CostType},
    host::Events,
    storage::AccessType,
    xdr::{AccountId, Hash, ScContractCode, ScVec, Uint256},
    HostError,
};

// Charge for an N-element "shallow copy" of some type, not cloning any
// substructure. In a better world we would multiply this by size_of<Self> but
// that's not guaranteed to be stable. Also this has a fairly serious level of
// inaccuracy since we call the same charge for any shallow type, whether it's a
// simple byte-copy or a sequence of Rc<>.clone calls (each of which bumps a
// refcount -- shallow but not as cheap as memcpy).
//
// TODO: this is correct for CPU-cost but not memory-cost, since there's no heap
// allocation implied by a shallow clone. We should (maybe) split BytesClone
// into ByteCopy (the CPU byte-copying part) and HeapAlloc (the heap-allocating
// part). See https://github.com/stellar/rs-soroban-env/issues/604.
fn charge_shallow_copy(n_elts: usize, budget: &Budget) -> Result<(), HostError> {
    budget.charge(CostType::BytesClone, n_elts as u64)
}

pub trait MeteredClone: Clone {
    // By default every MeteredClone type just charges as though it's shallow;
    // if a type is non-shallow (has variable-sized substructure to consider) it
    // should override both `IS_SHALLOW` (setting it to `false`) and
    // `charge_for_clone` (correctly charging for cloning the substructure).
    const IS_SHALLOW: bool = true;

    // Called to clone a single element of type Self, a default implementation
    // that charges for a 1-unit memcpy and is _only_ appropriate when Self is
    // a shallow type (with no substructure). If you override Self::IS_SHALLOW
    // and set it to false, you should override this method also (it will actually
    // panic if you don't).
    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        assert!(Self::IS_SHALLOW);
        charge_shallow_copy(1, budget)
    }

    // Called when cloning a slice of elements of type Self. Can be more
    // efficient than cloning element-by-element when Self::IS_SHALLOW, because
    // it's acceptable to charge once for the whole slice -- a single charge for
    // the cost of a memcpy -- followed by copying the slice via memcpy (or
    // something similar to a memcpy, eg. cloning a bunch of Rc<>s). If
    // Self::IS_SHALLOW is false, this method will do element-wise charging,
    // assuming it is paired with element-wise cloning.
    fn charge_for_clones(clones: &[Self], budget: &Budget) -> Result<(), HostError> {
        if Self::IS_SHALLOW {
            // If we're shallow, we're allowed to batch our charges.
            charge_shallow_copy(clones.len(), budget)
        } else {
            for elt in clones {
                elt.charge_for_clone(budget)?;
            }
            Ok(())
        }
    }
    // Composite helper that just does a charge_for_clone followed by a clone. Some types
    // might choose to do these two steps separately (eg. if they have a separate nontrivial and non-metered clone).
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        self.charge_for_clone(budget)?;
        Ok(self.clone())
    }
}

impl<T> MeteredClone for Rc<T> {}
impl MeteredClone for Hash {}
impl MeteredClone for RawVal {}
impl MeteredClone for AccessType {}
impl MeteredClone for AccountId {}
impl MeteredClone for ScContractCode {}
impl MeteredClone for Uint256 {}
impl<const N: usize> MeteredClone for [u8; N] {}

// TODO: this isn't correct: these two have substructure to account for;
// probably they should never be cloned in the middle of a contract at all (the
// storage maps are Rc<> now) but changing that means changing the storage
// interface. See https://github.com/stellar/rs-soroban-env/issues/603
impl MeteredClone for LedgerKey {}
impl MeteredClone for LedgerEntry {}

impl MeteredClone for ScVal {
    const IS_SHALLOW: bool = false;

    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        charge_shallow_copy(1, budget)?;
        match self {
            ScVal::Object(Some(obj)) => {
                match obj {
                    ScObject::Vec(v) => ScVec::charge_for_clone(v, budget),
                    ScObject::Map(m) => ScMap::charge_for_clone(m, budget),
                    ScObject::Bytes(b) => BytesM::charge_for_clone(b, budget),
                    // Everything else was handled by the memcpy above.
                    ScObject::U64(_)
                    | ScObject::I64(_)
                    | ScObject::U128(_)
                    | ScObject::I128(_)
                    | ScObject::ContractCode(_)
                    | ScObject::AccountId(_) => Ok(()),
                }
            }
            ScVal::Object(None)
            | ScVal::U63(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::Static(_)
            | ScVal::Symbol(_)
            | ScVal::Bitset(_)
            | ScVal::Status(_) => Ok(()),
        }
    }
}

impl MeteredClone for ScVec {
    const IS_SHALLOW: bool = false;
    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        for elt in self.0.iter() {
            elt.charge_for_clone(budget)?
        }
        Ok(())
    }
}

impl MeteredClone for ScMap {
    const IS_SHALLOW: bool = false;
    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        for elt in self.0.iter() {
            elt.key.charge_for_clone(budget)?;
            elt.val.charge_for_clone(budget)?;
        }
        Ok(())
    }
}

impl<const C: u32> MeteredClone for BytesM<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        charge_shallow_copy(1, budget)?;
        budget.charge(CostType::BytesClone, self.len() as u64)
    }
}

impl MeteredClone for Vec<u8> {
    const IS_SHALLOW: bool = false;

    fn charge_for_clone(&self, budget: &Budget) -> Result<(), HostError> {
        charge_shallow_copy(1, budget)?;
        budget.charge(CostType::BytesClone, self.len() as u64)
    }
}

impl<C: MeteredClone> MeteredClone for Vec<C> {
    const IS_SHALLOW: bool = false;
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        if C::IS_SHALLOW {
            // Shallow types we can batch-up our charging for.
            budget.charge(CostType::BytesClone, self.len() as u64)?;
            Ok(self.clone())
        } else {
            self.iter().map(|elt| elt.metered_clone(budget)).collect()
        }
    }
}

impl<C: MeteredClone> MeteredClone for Box<C> {
    const IS_SHALLOW: bool = false;

    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        let inner: &C = &**self;
        Ok(Box::new(inner.metered_clone(budget)?))
    }
}

impl<C: MeteredClone> MeteredClone for Option<C> {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        match self {
            Some(elt) => Ok(Some(elt.metered_clone(budget)?)),
            None => Ok(None),
        }
    }
}

impl MeteredClone for Events {
    const IS_SHALLOW: bool = false;
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneEvents, self.0.len() as u64)?;
        Ok(self.clone())
    }
}
