//! Functions to support measurement of the cost of cloning (and allocating)
//! objects that may contain pointers into the heap.
//!
//! Note that in this file the term "substructure" refers to the presence of
//! _pointers to owned data_ in a type, such that "merely counting the size" of
//! the type fails to capture the cost of cloning it. For example a struct with
//! 10 `Option<i32>` fields has "no substructure", because counting the size of
//! the struct gives you the right cost of cloning it; whereas a struct with
//! even one `Vec<u8>` is considered to "have substructure" because the struct's
//! size is just the fixed size of the vector header, and fails to account to
//! the owned heap allocation that the `Vec` _points to_, and that will be
//! cloned anytime we clone the struct.
//!
//! Consequently in methods where we "charge for the substructure" we are only
//! concerned with visiting any fields that (transitively) contain owning
//! pointers. The rest of each struct is considered its "shallow" part and
//! charged based on its declared size (see [`DeclaredSizeForMetering`]).

use std::{cell::RefCell, mem, rc::Rc};

use crate::{
    budget::{AsBudget, DepthLimiter},
    builtin_contracts::base_types::Address,
    storage::AccessType,
    xdr::{
        AccountEntry, AccountId, Asset, BytesM, ContractCodeCostInputs, ContractCodeEntry,
        ContractCodeEntryExt, ContractCodeEntryV1, ContractCostType, ContractExecutable,
        ContractIdPreimage, CreateContractArgs, Duration, Hash, InvokeContractArgs, LedgerEntry,
        LedgerEntryData, LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyContractCode,
        LedgerKeyTrustLine, PublicKey, ScAddress, ScBytes, ScContractInstance, ScErrorCode,
        ScErrorType, ScMap, ScMapEntry, ScNonceKey, ScString, ScSymbol, ScVal, ScVec, Signer,
        SorobanAuthorizationEntry, SorobanAuthorizedFunction, SorobanAuthorizedInvocation, StringM,
        TimePoint, TrustLineAsset, TrustLineEntry, Uint256,
    },
    AddressObject, Bool, BytesObject, DurationObject, DurationSmall, DurationVal, Error, HostError,
    I128Object, I128Small, I128Val, I256Object, I256Small, I256Val, I32Val, I64Object, I64Small,
    I64Val, MapObject, Object, ScValObject, StringObject, Symbol, SymbolObject, SymbolSmall,
    SymbolSmallIter, SymbolStr, TimepointObject, TimepointSmall, TimepointVal, U128Object,
    U128Small, U128Val, U256Object, U256Small, U256Val, U32Val, U64Object, U64Small, U64Val, Val,
    VecObject, Void, I256, U256,
};

use super::declared_size::DeclaredSizeForMetering;

/// Charge for an N-element "shallow copy" of some type, not cloning any substructure. The charge
/// unit is number of elements `n_elts` multiplied by a declared size of each element. In a better
/// world we would multiply by `size_of<Self>` instead but that's not guaranteed to be stable, which
/// might cause metering to differ across compilations, causing problems in concensus and replay.
pub(crate) fn charge_shallow_copy<T: DeclaredSizeForMetering>(
    n_elts: u64,
    budget: impl AsBudget,
) -> Result<(), HostError> {
    // Ideally we would want a static assertion. However, it does not work due to rust restrictions
    // (e.g. see https://github.com/rust-lang/rust/issues/57775). Here we make a runtime assertion
    // that the type's size is below its promised element size for budget charging. This assertion
    // only happens in debug build. In optimized build, not satisfying the asserted condition just
    // means an underestimation of the cost.
    debug_assert!(
        mem::size_of::<T>() as u64 <= T::DECLARED_SIZE,
        "mem size: {}, declared: {}",
        std::mem::size_of::<T>(),
        T::DECLARED_SIZE
    );
    budget.as_budget().charge(
        ContractCostType::MemCpy,
        Some(n_elts.saturating_mul(T::DECLARED_SIZE)),
    )
}

// Let it be a free function instead of a trait because charge_heap_alloc maybe called elsewhere,
// not just metered clone, e.g. Box::<T>::new().
pub(crate) fn charge_heap_alloc<T: DeclaredSizeForMetering>(
    n_elts: u64,
    budget: impl AsBudget,
) -> Result<(), HostError> {
    // Here we make a runtime assertion that the type's size is below its promised element size for
    // budget charging.
    debug_assert!(
        mem::size_of::<T>() as u64 <= T::DECLARED_SIZE,
        "mem size: {}, declared: {}",
        std::mem::size_of::<T>(),
        T::DECLARED_SIZE
    );
    budget.as_budget().charge(
        ContractCostType::MemAlloc,
        Some(n_elts.saturating_mul(T::DECLARED_SIZE)),
    )
}

pub trait MeteredAlloc<T: DeclaredSizeForMetering>: Sized {
    fn metered_new(value: T, budget: impl AsBudget) -> Result<Self, HostError>;
}

impl<T: DeclaredSizeForMetering> MeteredAlloc<T> for Rc<T> {
    fn metered_new(value: T, budget: impl AsBudget) -> Result<Self, HostError> {
        charge_heap_alloc::<T>(1, budget)?;
        Ok(Rc::new(value))
    }
}

/// Represents a collection type which can be created from an iterator, and provides
/// a method for bulk charging its creation cost. This does not clone elements from
/// the iterator, just moves them into the container, so only charges shallow copies.
pub trait MeteredContainer: FromIterator<Self::Item> + DeclaredSizeForMetering
where
    Self: Sized,
{
    type Item: DeclaredSizeForMetering;
    /// Charges the cost for bulk initializing the container and copying `len` elements
    /// into it.
    fn charge_bulk_init_cpy(len: u64, budget: impl AsBudget) -> Result<(), HostError> {
        charge_shallow_copy::<Self>(1, budget.clone())?;
        charge_heap_alloc::<Self::Item>(len, budget.clone())?;
        charge_shallow_copy::<Self::Item>(len, budget)
    }

    /// Metered version of the container's `with_capacity` method.
    fn with_metered_capacity(capacity: usize, budget: impl AsBudget) -> Result<Self, HostError>;
}

impl<T: DeclaredSizeForMetering> MeteredContainer for Vec<T> {
    type Item = T;

    fn with_metered_capacity(capacity: usize, budget: impl AsBudget) -> Result<Self, HostError> {
        Self::charge_bulk_init_cpy(capacity as u64, budget)?;
        Ok(Vec::with_capacity(capacity))
    }
}

/// This is implemented in order `metered_collect` a `Vec<Result<T, E>>` into a
/// `Result<Vec<T>, E>`. It is only supposed to exist for charging purpose.
impl<T: DeclaredSizeForMetering, E: DeclaredSizeForMetering> MeteredContainer
    for Result<Vec<T>, E>
{
    type Item = Result<T, E>;

    /// This function will return an error. It is not expected to initialize a
    /// `Result<Vec<T, E>>` with capacity.
    fn with_metered_capacity(_capacity: usize, _budget: impl AsBudget) -> Result<Self, HostError> {
        Err(Error::from_type_and_code(ScErrorType::Object, ScErrorCode::InvalidAction).into())
    }
}

/// Represents an iterator which can collect its elements into a `MeteredContainer`
/// and automatically account for the metering cost.
///
/// Returns:
///   Err - if either the budget was exceeded or the iterator lacks the size hint
///   Ok - otherwise
pub trait MeteredIterator: Iterator
where
    Self::Item: DeclaredSizeForMetering,
    Self: Sized,
{
    fn metered_collect<B: MeteredContainer<Item = Self::Item>>(
        self,
        budget: impl AsBudget,
    ) -> Result<B, HostError> {
        if let (_, Some(ub)) = self.size_hint() {
            B::charge_bulk_init_cpy(ub as u64, budget)?;
            Ok(self.collect())
        } else {
            Err((ScErrorType::Object, ScErrorCode::InternalError).into())
        }
    }
}

impl<T: DeclaredSizeForMetering, I: Iterator, F> MeteredIterator for std::iter::Map<I, F> where
    F: FnMut(I::Item) -> T
{
}

impl<A, B> MeteredIterator for std::iter::Chain<A, B>
where
    A: Iterator,
    B: Iterator<Item = A::Item>,
    A::Item: DeclaredSizeForMetering,
{
}

/// A type that can be cloned, and charges for the cost of doing so, possibly
/// failing if the clone runs out of budget. The cost of cloning may include
/// both "shallow" and "substructure" components.
pub trait MeteredClone: Clone + DeclaredSizeForMetering {
    /// By default every MeteredClone type just charges as though it's shallow;
    /// if a type is non-shallow (owns any substructure in the heap) it should
    /// override both `IS_SHALLOW` (setting it to `false`) and
    /// `charge_for_substructure` (correctly charging for cloning the
    /// substructure).
    const IS_SHALLOW: bool = true;

    /// Called to clone the substructures of Self. The default implementation is
    /// a no-op and is _only_ appropriate when Self is a shallow type (with no
    /// substructure). If you override `Self::IS_SHALLOW` and set it to false,
    /// you should override this method also (it will actually Err if you
    /// don't). This charge does not include shallow copying of `Self` because
    /// that should be taken care of by the caller beforehand, e.g. in
    /// `metered_clone`.
    fn charge_for_substructure(&self, _budget: impl AsBudget) -> Result<(), HostError> {
        if Self::IS_SHALLOW {
            Ok(())
        } else {
            Err((ScErrorType::Object, ScErrorCode::InternalError).into())
        }
    }

    /// Called when cloning the substructures of a slice of Self. When
    /// Self::IS_SHALLOW, this is a no-op. If Self::IS_SHALLOW is false, this
    /// method will do element-wise charging for substructure, assuming it is
    /// paired with element-wise cloning.
    fn bulk_charge_for_substructure(
        clones: &[Self],
        budget: impl AsBudget,
    ) -> Result<(), HostError> {
        if Self::IS_SHALLOW {
            // If we're shallow, just return.
            Ok(())
        } else {
            for elt in clones {
                elt.charge_for_substructure(budget.clone())?;
            }
            Ok(())
        }
    }

    /// Convenience method for charging for a deep clone knowing the type is not
    /// shallow.
    fn charge_deep_clone(&self, budget: impl AsBudget) -> Result<(), HostError> {
        if Self::IS_SHALLOW {
            Err((ScErrorType::Object, ScErrorCode::InternalError).into())
        } else {
            charge_shallow_copy::<Self>(1, budget.clone())?;
            self.charge_for_substructure(budget)
        }
    }

    /// Composite helper handles metering and then, if successful, cloning. It
    /// first charges for the shallow footprint of the type, followed by
    /// charging the substructure, and finally calling `Clone::clone`. The
    /// reason `charge_for_substructure` does not include shallow copying of
    /// `Self` is because it is taken care of here. Typically overriding
    /// `IS_SHALLOW` and `charge_for_substructure` should propertly take care of
    /// custom non-trivial cloning. Thus this method should not need to be
    /// overriden.
    fn metered_clone(&self, budget: impl AsBudget) -> Result<Self, HostError> {
        charge_shallow_copy::<Self>(1, budget.clone())?;
        self.charge_for_substructure(budget)?;
        Ok(self.clone())
    }
}

// region: primitive types
impl MeteredClone for u8 {}
impl MeteredClone for u32 {}
impl MeteredClone for i32 {}
impl MeteredClone for u64 {}
impl MeteredClone for i64 {}
impl MeteredClone for u128 {}
impl MeteredClone for i128 {}
// endregion: primitive types

// region: Val-wrapping types
impl MeteredClone for Val {}
impl MeteredClone for Void {}
impl MeteredClone for Bool {}
impl MeteredClone for VecObject {}
impl MeteredClone for MapObject {}
impl MeteredClone for AddressObject {}
impl MeteredClone for BytesObject {}
impl MeteredClone for U32Val {}
impl MeteredClone for I32Val {}
impl MeteredClone for U64Val {}
impl MeteredClone for U64Small {}
impl MeteredClone for U64Object {}
impl MeteredClone for I64Val {}
impl MeteredClone for I64Small {}
impl MeteredClone for I64Object {}
impl MeteredClone for TimepointVal {}
impl MeteredClone for TimepointSmall {}
impl MeteredClone for TimepointObject {}
impl MeteredClone for DurationVal {}
impl MeteredClone for DurationSmall {}
impl MeteredClone for DurationObject {}
impl MeteredClone for U128Val {}
impl MeteredClone for U128Small {}
impl MeteredClone for U128Object {}
impl MeteredClone for I128Val {}
impl MeteredClone for I128Small {}
impl MeteredClone for I128Object {}
impl MeteredClone for U256Val {}
impl MeteredClone for U256Small {}
impl MeteredClone for U256Object {}
impl MeteredClone for I256Val {}
impl MeteredClone for I256Small {}
impl MeteredClone for I256Object {}
impl MeteredClone for Object {}
impl MeteredClone for Error {}
impl MeteredClone for StringObject {}
impl MeteredClone for Symbol {}
impl MeteredClone for SymbolSmall {}
impl MeteredClone for SymbolObject {}
// endregion: Val-wrapping types

// region: other env types with no substructure
impl MeteredClone for SymbolStr {}
impl MeteredClone for SymbolSmallIter {}
impl MeteredClone for U256 {}
impl MeteredClone for I256 {}
impl MeteredClone for Address {}
impl MeteredClone for AccessType {}
// endregion: other env types with no substructure

// region: xdr types with no substructure
impl MeteredClone for TimePoint {}
impl MeteredClone for Duration {}
impl MeteredClone for Hash {}
impl MeteredClone for Uint256 {}
impl MeteredClone for ContractCodeCostInputs {}
impl MeteredClone for ContractCodeEntryV1 {}
impl MeteredClone for ContractExecutable {}
impl MeteredClone for AccountId {}
impl MeteredClone for ScAddress {}
impl MeteredClone for ScNonceKey {}
impl MeteredClone for PublicKey {}
impl MeteredClone for TrustLineAsset {}

impl MeteredClone for LedgerKeyAccount {}
impl MeteredClone for LedgerKeyTrustLine {}
impl MeteredClone for LedgerKeyContractCode {}

impl MeteredClone for LedgerEntryExt {}
impl MeteredClone for TrustLineEntry {}

impl MeteredClone for CreateContractArgs {}
impl MeteredClone for ContractIdPreimage {}
impl MeteredClone for Asset {}
// endregion: xdr types with no substructure

// region: Rust standard composite types

// cloning Rc is just a ref-count bump
impl<T> MeteredClone for Rc<T> {}

// cloning a RefCell clones its underlying data structure
impl<T: MeteredClone> MeteredClone for RefCell<T> {
    const IS_SHALLOW: bool = T::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.borrow().charge_for_substructure(budget)
    }
}

impl<K, V> MeteredClone for (K, V)
where
    K: MeteredClone,
    V: MeteredClone,
{
    const IS_SHALLOW: bool = K::IS_SHALLOW && V::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        if !K::IS_SHALLOW {
            K::charge_for_substructure(&self.0, budget.clone())?;
        }
        if !V::IS_SHALLOW {
            V::charge_for_substructure(&self.1, budget)?;
        }
        Ok(())
    }
}

impl<C: MeteredClone, const N: usize> MeteredClone for [C; N] {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        C::bulk_charge_for_substructure(self.as_slice(), budget)
    }
}

impl<C: MeteredClone> MeteredClone for Vec<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        // first take care of Vec clone, which involves allocating memory and shallowly cloning the
        // data type into it, then deep clone any substructure.
        charge_heap_alloc::<C>(self.len() as u64, budget.clone())?;
        charge_shallow_copy::<C>(self.len() as u64, budget.clone())?;
        C::bulk_charge_for_substructure(self.as_slice(), budget)?;
        Ok(())
    }
}

impl<C: MeteredClone> MeteredClone for Box<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        // first take care of the Box clone: allocating memory and shallow cloning of data type.
        charge_heap_alloc::<C>(1, budget.clone())?;
        charge_shallow_copy::<C>(1, budget.clone())?;
        // then take care of any substructure clone (recursively)
        let inner: &C = self;
        inner.charge_for_substructure(budget)
    }
}

impl<C: MeteredClone> MeteredClone for Option<C> {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        match self {
            Some(elt) => elt.charge_for_substructure(budget),
            None => Ok(()),
        }
    }
}
// endregion: Rust standard composite types

// region: other env types with substructure
// endregion: other env types with substructure

// region: xdr types with substructure

impl MeteredClone for ScVal {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        // This is the depth limit checkpoint for `ScVal` cloning.
        let mut budget_clone = budget.as_budget().clone();
        budget_clone.with_limited_depth(|_| {
            match self {
                ScVal::Vec(Some(v)) => ScVec::charge_for_substructure(v, budget),
                ScVal::Map(Some(m)) => ScMap::charge_for_substructure(m, budget),
                ScVal::Vec(None) | ScVal::Map(None) => {
                    Err((ScErrorType::Value, ScErrorCode::InvalidInput).into())
                }
                ScVal::Bytes(b) => BytesM::charge_for_substructure(b, budget),
                ScVal::String(s) => StringM::charge_for_substructure(s, budget),
                ScVal::Symbol(s) => StringM::charge_for_substructure(s, budget),
                ScVal::ContractInstance(i) => {
                    ScContractInstance::charge_for_substructure(i, budget)
                }
                // Everything else was handled by the memcpy above.
                ScVal::U64(_)
                | ScVal::I64(_)
                | ScVal::U128(_)
                | ScVal::I128(_)
                | ScVal::Address(_)
                | ScVal::U32(_)
                | ScVal::I32(_)
                | ScVal::Error(_)
                | ScVal::Bool(_)
                | ScVal::Void
                | ScVal::Timepoint(_)
                | ScVal::Duration(_)
                | ScVal::U256(_)
                | ScVal::I256(_)
                | ScVal::LedgerKeyContractInstance
                | ScVal::LedgerKeyNonce(_) => Ok(()),
            }
        })
    }
}

impl MeteredClone for ScValObject {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<ScVal>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScMapEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.key.charge_for_substructure(budget.clone())?;
        self.val.charge_for_substructure(budget)
    }
}

impl MeteredClone for ScVec {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl MeteredClone for ScMap {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl<const C: u32> MeteredClone for BytesM<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl<const C: u32> MeteredClone for StringM<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScBytes {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScString {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScSymbol {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScContractInstance {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.storage.charge_for_substructure(budget)
    }
}

impl MeteredClone for LedgerKey {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        use LedgerKey::*;
        match self {
            ContractData(d) => d.key.charge_for_substructure(budget),
            ContractCode(_) | Account(_) | Trustline(_) => Ok(()),

            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Value, ScErrorCode::InternalError).into()),
        }
    }
}

impl MeteredClone for ContractCodeEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        // self.ext is a former ExtensionEntry; see note on ExtensionEntry in declared_size.rs
        match &self.ext {
            ContractCodeEntryExt::V0 => (),
            ContractCodeEntryExt::V1(v1) => v1.charge_for_substructure(budget.clone())?,
        }
        self.code.charge_for_substructure(budget)
    }
}

impl MeteredClone for LedgerEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        use LedgerEntryData::*;
        match &self.data {
            ContractData(d) => {
                d.val.charge_for_substructure(budget.clone())?;
                d.key.charge_for_substructure(budget)
            }
            ContractCode(c) => c.charge_for_substructure(budget),
            Account(ae) => ae.charge_for_substructure(budget),
            Trustline(_) => Ok(()),

            Offer(_) | Data(_) | ClaimableBalance(_) | LiquidityPool(_) | ConfigSetting(_)
            | Ttl(_) => Err((ScErrorType::Value, ScErrorCode::InternalError).into()),
        }
    }
}

impl MeteredClone for Signer {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        use crate::xdr::SignerKey::*;
        match &self.key {
            Ed25519(_) | PreAuthTx(_) | HashX(_) => Ok(()),
            Ed25519SignedPayload(p) => p.payload.charge_for_substructure(budget),
        }
    }
}

impl MeteredClone for AccountEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.home_domain.charge_for_substructure(budget.clone())?;
        self.signers.charge_for_substructure(budget)
    }
}

impl MeteredClone for SorobanAuthorizedInvocation {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.function.charge_for_substructure(budget.clone())?;
        self.sub_invocations.charge_for_substructure(budget)
    }
}

impl MeteredClone for SorobanAuthorizationEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.root_invocation.charge_for_substructure(budget)
    }
}

impl MeteredClone for SorobanAuthorizedFunction {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        match self {
            // InvokeContractArgs carries a vector as substructure.
            SorobanAuthorizedFunction::ContractFn(c) => c.charge_for_substructure(budget),
            // CreateContractArgs has no substructure.
            SorobanAuthorizedFunction::CreateContractHostFn(_) => Ok(()),
        }
    }
}

impl MeteredClone for InvokeContractArgs {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.function_name.charge_for_substructure(budget.clone())?;
        self.args.charge_for_substructure(budget)
    }
}

// endregion: xdr types with substructure
