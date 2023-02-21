use std::{mem, rc::Rc};

use soroban_env_common::{
    xdr::{
        BytesM, ContractEvent, ContractEventBody, LedgerEntry, LedgerKey, ScAddress, ScMap,
        ScMapEntry, ScObject, ScVal,
    },
    RawVal,
};

use crate::{
    budget::{Budget, CostType},
    events::{DebugArg, DebugEvent, HostEvent, InternalContractEvent, InternalEvent},
    host::Events,
    storage::AccessType,
    xdr::{AccountId, Hash, ScContractCode, ScVec, Uint256},
    HostError,
};

// Charge for an N-element "shallow copy" of some type, not cloning any substructure. The charge
// unit is number of elements `n_elts` multiplied by a declared size of each element. In a better
// world we would multiply by `size_of<Self>` instead but that's not guaranteed to be stable, which
// might cause metering to differ across compilations, causing problems in concensus and replay.
fn charge_shallow_copy<T: MeteredClone>(n_elts: u64, budget: &Budget) -> Result<(), HostError> {
    // Ideally we would want a static assertion. However, it does not work due to rust restrictions
    // (e.g. see https://github.com/rust-lang/rust/issues/57775). Here we make a runtime assertion
    // that the type's size is below its promised element size for budget charging. This assertion
    // only happens in debug build. In optimized build, not satisfying the asserted condition just
    // means an underestimation of the cost.
    debug_assert!(mem::size_of::<T>() as u64 <= T::ELT_SIZE);
    budget.charge(CostType::HostMemCpy, n_elts.saturating_mul(T::ELT_SIZE))
}

// Let it be a free function instead of a trait because charge_heap_alloc maybe called elsewhere,
// not just metered clone, e.g. Box::<T>::new().
fn charge_heap_alloc<T: MeteredClone>(n_elts: u64, budget: &Budget) -> Result<(), HostError> {
    // Here we make a runtime assertion that the type's size is below its promised element size for
    // budget charging.
    debug_assert!(mem::size_of::<T>() as u64 <= T::ELT_SIZE);
    budget.charge(CostType::HostMemAlloc, n_elts.saturating_mul(T::ELT_SIZE))
}

pub trait MeteredClone: Clone {
    // By default every MeteredClone type just charges as though it's shallow;
    // if a type is non-shallow (has variable-sized substructure to consider) it
    // should override both `IS_SHALLOW` (setting it to `false`) and
    // `charge_for_clone` (correctly charging for cloning the substructure).
    const IS_SHALLOW: bool = true;

    // Size (bytes) of a single element. This value determines the input for budget charging.
    // It should be the upperbound (across various compilations and platforms) of the actual type's
    // size. Implementer of the trait needs to decide this value based on Rust's guideline on type
    // layout: https://doc.rust-lang.org/reference/type-layout.html
    const ELT_SIZE: u64;

    // Called to clone the substructures of Self. The default implementation is a no-op and is
    // _only_ appropriate when Self is a shallow type (with no substructure). If you override
    // Self::IS_SHALLOW and set it to false, you should override this method also (it will actually
    // panic if you don't). This charge does not include shallow copying of `Self` because that
    // should be taken care of by the caller beforehand, e.g. in `metered_clone`.
    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        assert!(Self::IS_SHALLOW);
        Ok(())
    }

    // Called when cloning the substructures of a slice of Self. When Self::IS_SHALLOW, this is a
    // no-op. If Self::IS_SHALLOW is false, this method will do element-wise charging for
    // substructure, assuming it is paired with element-wise cloning.
    fn bulk_charge_for_substructure(clones: &[Self], budget: &Budget) -> Result<(), HostError> {
        if Self::IS_SHALLOW {
            // If we're shallow, just return.
            Ok(())
        } else {
            for elt in clones {
                elt.charge_for_substructure(budget)?;
            }
            Ok(())
        }
    }

    // Convenience method for charging for a deep clone knowing the type is not shallow.
    fn charge_deep_clone(&self, budget: &Budget) -> Result<(), HostError> {
        debug_assert!(!Self::IS_SHALLOW);
        charge_shallow_copy::<Self>(1, budget)?;
        self.charge_for_substructure(budget)
    }

    // Composite helper handles metering before clone. It first charges for the shallow footprint
    // of the type, followed by charging the substructure clone. The reason
    // `charge_for_substructure` does not include shallow copying of `Self` is because it is taken
    // care of here. Typically overriding `IS_SHALLOW` and `charge_for_substructure` should
    // propertly take care of custom non-trivial cloning. Thus this method should not need to be
    // overriden.
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        charge_shallow_copy::<Self>(1, budget)?;
        self.charge_for_substructure(budget)?;
        Ok(self.clone())
    }
}

macro_rules! impl_metered_clone_for_shallow_types {
    ($name:ident, $size:literal) => {
        impl MeteredClone for $name {
            const ELT_SIZE: u64 = $size;
        }
    };
}

impl_metered_clone_for_shallow_types!(u8, 1);
impl_metered_clone_for_shallow_types!(u32, 4);
impl_metered_clone_for_shallow_types!(i32, 4);
impl_metered_clone_for_shallow_types!(u64, 8);
impl_metered_clone_for_shallow_types!(i64, 8);
impl_metered_clone_for_shallow_types!(Hash, 32);
impl_metered_clone_for_shallow_types!(RawVal, 8);
impl_metered_clone_for_shallow_types!(AccessType, 1);
impl_metered_clone_for_shallow_types!(AccountId, 32);
impl_metered_clone_for_shallow_types!(ScContractCode, 33);
impl_metered_clone_for_shallow_types!(Uint256, 32);
impl_metered_clone_for_shallow_types!(ScAddress, 33);
impl_metered_clone_for_shallow_types!(DebugArg, 16);
impl_metered_clone_for_shallow_types!(InternalContractEvent, 40);

// Rc is an exception, nothing is being cloned. We approximate ref counter bump with the cost of
// cloning 16 bytes. Also it can't be below 16 since that's the size of an `Rc` structure.
impl<T> MeteredClone for Rc<T> {
    const ELT_SIZE: u64 = 16;
}

// Cloning a slice only clones the reference without deep cloning its contents.
impl<T> MeteredClone for &[T] {
    const ELT_SIZE: u64 = 16;
}

impl<K, V> MeteredClone for (K, V)
where
    K: MeteredClone,
    V: MeteredClone,
{
    const ELT_SIZE: u64 = <K as MeteredClone>::ELT_SIZE + <V as MeteredClone>::ELT_SIZE;
}

// TODO: this isn't correct: these two have substructure to account for;
// probably they should never be cloned in the middle of a contract at all (the
// storage maps are Rc<> now) but changing that means changing the storage
// interface. See https://github.com/stellar/rs-soroban-env/issues/603
impl MeteredClone for LedgerKey {
    const ELT_SIZE: u64 = 80;
}
impl MeteredClone for LedgerEntry {
    const ELT_SIZE: u64 = 264;
}

impl<C: MeteredClone, const N: usize> MeteredClone for [C; N] {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    const ELT_SIZE: u64 = C::ELT_SIZE.saturating_mul(N as u64);

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        C::bulk_charge_for_substructure(self.as_slice(), budget)
    }
}

impl MeteredClone for ScVal {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 40;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            ScVal::Object(Some(obj)) => {
                match obj {
                    ScObject::Vec(v) => ScVec::charge_for_substructure(v, budget),
                    ScObject::Map(m) => ScMap::charge_for_substructure(m, budget),
                    ScObject::Bytes(b) => BytesM::charge_for_substructure(b, budget),
                    // Everything else was handled by the memcpy above.
                    ScObject::U64(_)
                    | ScObject::I64(_)
                    | ScObject::U128(_)
                    | ScObject::I128(_)
                    | ScObject::ContractCode(_)
                    | ScObject::Address(_)
                    | ScObject::NonceKey(_) => Ok(()),
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

impl MeteredClone for ScMapEntry {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 80;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.key.charge_for_substructure(budget)?;
        self.val.charge_for_substructure(budget)
    }
}

impl MeteredClone for ScVec {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 24;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl MeteredClone for ScMap {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 24;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl<const C: u32> MeteredClone for BytesM<C> {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 24;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl<C: MeteredClone> MeteredClone for Vec<C> {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 24;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        // first take care of Vec clone, which involves allocating memory and shallowly cloning the
        // data type into it, then deep clone any substructure.
        charge_heap_alloc::<C>(self.len() as u64, budget)?;
        charge_shallow_copy::<C>(self.len() as u64, budget)?;
        C::bulk_charge_for_substructure(self.as_slice(), budget)?;
        Ok(())
    }
}

impl<C: MeteredClone> MeteredClone for Box<C> {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 8;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        // first take care of the Box clone: allocating memory and shallow cloning of data type.
        charge_heap_alloc::<C>(1, budget)?;
        charge_shallow_copy::<C>(1, budget)?;
        // then take care of any substructure clone (recursively)
        let inner: &C = &**self;
        inner.charge_for_substructure(budget)
    }
}

impl<C: MeteredClone> MeteredClone for Option<C> {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    // Size of C plus an 8 byte alignment overhead. If we need to handle types with larger
    // alignment size, we need to increase the overhead.
    const ELT_SIZE: u64 = C::ELT_SIZE + 8;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            Some(elt) => elt.charge_for_substructure(budget),
            None => Ok(()),
        }
    }
}

impl MeteredClone for DebugEvent {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 80;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        if self.args.is_heap() {
            // equivalent to charging for a `Vec` clone except we already know there is no
            // substructure. `TinyVec` doesn't let us retrieve the `Vec` directly.
            charge_heap_alloc::<Vec<DebugArg>>(self.args.len() as u64, budget)?;
            charge_shallow_copy::<Vec<DebugArg>>(self.args.len() as u64, budget)
        } else {
            // equivalent to charging for a slice clone.
            // Here the size is limited to the `tiny_vec`'s limit we defined.
            DebugArg::bulk_charge_for_substructure(self.args.as_slice(), budget)
        }
    }
}

impl MeteredClone for ContractEvent {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 104;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        let ContractEventBody::V0(event) = &self.body;
        event.topics.charge_for_substructure(budget)?;
        event.data.charge_for_substructure(budget)
    }
}

impl MeteredClone for HostEvent {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 112;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            HostEvent::Contract(c) => c.charge_for_substructure(budget),
            HostEvent::Debug(d) => d.charge_for_substructure(budget),
        }
    }
}

impl MeteredClone for Events {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 24;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget)
    }
}

impl MeteredClone for InternalEvent {
    const IS_SHALLOW: bool = false;

    const ELT_SIZE: u64 = 80;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            InternalEvent::Contract(c) => c.charge_for_substructure(budget),
            InternalEvent::Debug(d) => d.charge_for_substructure(budget),
            InternalEvent::None => Ok(()),
        }
    }
}

mod test {
    #[allow(unused)]
    use super::*;

    #[test]
    fn test_elt_size() {
        use expect_test::expect;
        // This section is for outputting the actual size of types. They are for informational use.
        // They might become outdated due to Rust type changes. Run `UPDATE_EXPECT=true cargo test`
        // to update this.
        expect!["32"].assert_eq(std::mem::size_of::<Hash>().to_string().as_str());
        expect!["8"].assert_eq(std::mem::size_of::<RawVal>().to_string().as_str());
        expect!["1"].assert_eq(std::mem::size_of::<AccessType>().to_string().as_str());
        expect!["32"].assert_eq(std::mem::size_of::<AccountId>().to_string().as_str());
        expect!["33"].assert_eq(std::mem::size_of::<ScContractCode>().to_string().as_str());
        expect!["32"].assert_eq(std::mem::size_of::<Uint256>().to_string().as_str());
        expect!["33"].assert_eq(std::mem::size_of::<ScAddress>().to_string().as_str());
        expect!["80"].assert_eq(std::mem::size_of::<LedgerKey>().to_string().as_str());
        expect!["264"].assert_eq(std::mem::size_of::<LedgerEntry>().to_string().as_str());
        expect!["40"].assert_eq(std::mem::size_of::<ScVal>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<ScVec>().to_string().as_str());
        expect!["80"].assert_eq(std::mem::size_of::<ScMapEntry>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<ScMap>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<Vec<u8>>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<Vec<ScVal>>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<BytesM<10000>>().to_string().as_str());
        expect!["8"].assert_eq(std::mem::size_of::<Box<u8>>().to_string().as_str());
        expect!["8"].assert_eq(std::mem::size_of::<Box<ScVal>>().to_string().as_str());
        expect!["80"].assert_eq(std::mem::size_of::<DebugEvent>().to_string().as_str());
        expect!["104"].assert_eq(std::mem::size_of::<ContractEvent>().to_string().as_str());
        expect!["112"].assert_eq(std::mem::size_of::<HostEvent>().to_string().as_str());
        expect!["24"].assert_eq(std::mem::size_of::<Events>().to_string().as_str());
        expect!["80"].assert_eq(std::mem::size_of::<InternalEvent>().to_string().as_str());
        expect!["40"].assert_eq(
            std::mem::size_of::<InternalContractEvent>()
                .to_string()
                .as_str(),
        );

        // These are the actual tests. We use a tighter condition (==) than the actual code (<=).
        // For any new `MeteredClone` type, a new test case needs to be added below.
        macro_rules! assert_mem_size_equals_elt_size {
            ($name:ident) => {
                assert_eq!(
                    std::mem::size_of::<$name>() as u64,
                    <$name as MeteredClone>::ELT_SIZE
                );
            };
        }
        assert_mem_size_equals_elt_size!(Hash);
        assert_mem_size_equals_elt_size!(RawVal);
        assert_mem_size_equals_elt_size!(AccessType);
        assert_mem_size_equals_elt_size!(AccountId);
        assert_mem_size_equals_elt_size!(ScContractCode);
        assert_mem_size_equals_elt_size!(Uint256);
        assert_mem_size_equals_elt_size!(ScAddress);
        assert_mem_size_equals_elt_size!(LedgerKey);
        assert_mem_size_equals_elt_size!(LedgerEntry);
        assert_mem_size_equals_elt_size!(ScVal);
        assert_mem_size_equals_elt_size!(ScVec);
        assert_mem_size_equals_elt_size!(ScMapEntry);
        assert_mem_size_equals_elt_size!(ScMap);
        assert_mem_size_equals_elt_size!(DebugEvent);
        assert_mem_size_equals_elt_size!(ContractEvent);
        assert_mem_size_equals_elt_size!(HostEvent);
        assert_mem_size_equals_elt_size!(Events);
        assert_mem_size_equals_elt_size!(InternalEvent);
        assert_eq!(
            std::mem::size_of::<Vec<u8>>() as u64,
            <Vec<u8> as MeteredClone>::ELT_SIZE
        );
        assert_eq!(
            std::mem::size_of::<Vec<ScVal>>() as u64,
            <Vec<ScVal> as MeteredClone>::ELT_SIZE
        );
        assert_eq!(
            std::mem::size_of::<BytesM<10000>>() as u64,
            <BytesM<10000> as MeteredClone>::ELT_SIZE
        );
        assert_eq!(
            std::mem::size_of::<Box<u8>>() as u64,
            <Box<u8> as MeteredClone>::ELT_SIZE
        );
        assert_eq!(
            std::mem::size_of::<Box<ScVal>>() as u64,
            <Box<ScVal> as MeteredClone>::ELT_SIZE
        );
    }
}
