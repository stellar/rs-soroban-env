use std::{mem, rc::Rc};

use crate::{
    budget::Budget,
    events::{HostEvent, InternalContractEvent, InternalEvent},
    host::Events,
    host_object::HostObject,
    storage::AccessType,
    xdr::{
        AccountEntry, AccountId, BytesM, ClaimableBalanceEntry, ConfigSettingEntry,
        ContractCodeEntry, ContractCodeEntryBody, ContractCostType, ContractDataEntryBody,
        ContractEvent, ContractEventBody, ContractEventType, ContractIdPreimage,
        CreateContractArgs, DataEntry, Duration, Hash, LedgerEntry, LedgerEntryData,
        LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyClaimableBalance,
        LedgerKeyConfigSetting, LedgerKeyContractCode, LedgerKeyData, LedgerKeyLiquidityPool,
        LedgerKeyOffer, LedgerKeyTrustLine, LiquidityPoolEntry, OfferEntry, PublicKey, ScAddress,
        ScBytes, ScContractExecutable, ScErrorCode, ScErrorType, ScMap, ScMapEntry, ScNonceKey,
        ScString, ScSymbol, ScVal, ScVec, StringM, TimePoint, TrustLineAsset, TrustLineEntry,
        Uint256,
    },
    AddressObject, Bool, BytesObject, ContractExecutableObject, DurationObject, DurationSmall,
    DurationVal, Error, HostError, I128Object, I128Small, I128Val, I256Object, I256Small, I256Val,
    I32Val, I64Object, I64Small, I64Val, LedgerKeyNonceObject, MapObject, Object, RawVal,
    ScValObject, StringObject, Symbol, SymbolObject, SymbolSmall, SymbolSmallIter, SymbolStr,
    TimepointObject, TimepointSmall, TimepointVal, U128Object, U128Small, U128Val, U256Object,
    U256Small, U256Val, U32Val, U64Object, U64Small, U64Val, VecObject, Void, I256, U256,
};

use super::declared_size::DeclaredSizeForMetering;

// Charge for an N-element "shallow copy" of some type, not cloning any substructure. The charge
// unit is number of elements `n_elts` multiplied by a declared size of each element. In a better
// world we would multiply by `size_of<Self>` instead but that's not guaranteed to be stable, which
// might cause metering to differ across compilations, causing problems in concensus and replay.
pub(crate) fn charge_shallow_copy<T: MeteredClone>(
    n_elts: u64,
    budget: &Budget,
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
    budget.charge(
        ContractCostType::HostMemCpy,
        Some(n_elts.saturating_mul(T::DECLARED_SIZE)),
    )
}

// Let it be a free function instead of a trait because charge_heap_alloc maybe called elsewhere,
// not just metered clone, e.g. Box::<T>::new().
pub(crate) fn charge_heap_alloc<T: MeteredClone>(
    n_elts: u64,
    budget: &Budget,
) -> Result<(), HostError> {
    // Here we make a runtime assertion that the type's size is below its promised element size for
    // budget charging.
    debug_assert!(mem::size_of::<T>() as u64 <= T::DECLARED_SIZE);
    budget.charge(
        ContractCostType::HostMemAlloc,
        Some(n_elts.saturating_mul(T::DECLARED_SIZE)),
    )
}

// A convenience method for a container bulk initialization with elements, e.g. a
// `Vec::with_capacity(N)` immediately followed by N element pushes.
pub(crate) fn charge_container_bulk_init_with_elts<C: MeteredClone, T: MeteredClone>(
    n_elts: u64,
    budget: &Budget,
) -> Result<(), HostError> {
    debug_assert!(!C::IS_SHALLOW);
    charge_shallow_copy::<C>(1, budget)?;
    charge_heap_alloc::<T>(n_elts, budget)?;
    charge_shallow_copy::<T>(n_elts, budget)
}

pub trait MeteredClone: Clone + DeclaredSizeForMetering {
    // By default every MeteredClone type just charges as though it's shallow;
    // if a type is non-shallow (has variable-sized substructure to consider) it
    // should override both `IS_SHALLOW` (setting it to `false`) and
    // `charge_for_clone` (correctly charging for cloning the substructure).
    const IS_SHALLOW: bool = true;

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

// primitive types
impl MeteredClone for u8 {}
impl MeteredClone for u32 {}
impl MeteredClone for i32 {}
impl MeteredClone for u64 {}
impl MeteredClone for i64 {}
impl MeteredClone for u128 {}
impl MeteredClone for i128 {}
// RawVal-wrapping types
impl MeteredClone for RawVal {}
impl MeteredClone for Void {}
impl MeteredClone for Bool {}
impl MeteredClone for VecObject {}
impl MeteredClone for MapObject {}
impl MeteredClone for ContractExecutableObject {}
impl MeteredClone for LedgerKeyNonceObject {}
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
// other common types
impl MeteredClone for SymbolStr {}
impl MeteredClone for SymbolSmallIter {}
impl MeteredClone for U256 {}
impl MeteredClone for I256 {}
impl MeteredClone for HostObject {}
// xdr types
impl MeteredClone for TimePoint {}
impl MeteredClone for Duration {}
impl MeteredClone for Hash {}
impl MeteredClone for Uint256 {}
impl MeteredClone for ScContractExecutable {}
impl MeteredClone for AccountId {}
impl MeteredClone for ScAddress {}
impl MeteredClone for ScNonceKey {}
impl MeteredClone for PublicKey {}
impl MeteredClone for TrustLineAsset {}
impl MeteredClone for LedgerKeyAccount {}
impl MeteredClone for LedgerKeyTrustLine {}
impl MeteredClone for LedgerKeyOffer {}
impl MeteredClone for LedgerKeyData {}
impl MeteredClone for LedgerKeyClaimableBalance {}
impl MeteredClone for LedgerKeyLiquidityPool {}
impl MeteredClone for LedgerKeyContractCode {}
impl MeteredClone for LedgerKeyConfigSetting {}
impl MeteredClone for LedgerEntryExt {}
impl MeteredClone for AccountEntry {}
impl MeteredClone for TrustLineEntry {}
impl MeteredClone for OfferEntry {}
impl MeteredClone for DataEntry {}
impl MeteredClone for ClaimableBalanceEntry {}
impl MeteredClone for LiquidityPoolEntry {}
impl MeteredClone for ContractCodeEntry {}
impl MeteredClone for ConfigSettingEntry {}
impl MeteredClone for AccessType {}
impl MeteredClone for InternalContractEvent {}
impl MeteredClone for CreateContractArgs {}
impl MeteredClone for ContractIdPreimage {}
// composite types
impl<T> MeteredClone for Rc<T> {}
impl<T> MeteredClone for &[T] {}
impl<K, V> MeteredClone for (K, V)
where
    K: MeteredClone,
    V: MeteredClone,
{
}

impl<C: MeteredClone, const N: usize> MeteredClone for [C; N] {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        C::bulk_charge_for_substructure(self.as_slice(), budget)
    }
}

impl MeteredClone for ScVal {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            ScVal::Vec(Some(v)) => ScVec::charge_for_substructure(v, budget),
            ScVal::Map(Some(m)) => ScMap::charge_for_substructure(m, budget),
            ScVal::Vec(None) | ScVal::Map(None) => {
                Err((ScErrorType::Value, ScErrorCode::MissingValue).into())
            }
            ScVal::Bytes(b) => BytesM::charge_for_substructure(b, budget),
            ScVal::String(s) => StringM::charge_for_substructure(s, budget),
            ScVal::Symbol(s) => StringM::charge_for_substructure(s, budget),
            // Everything else was handled by the memcpy above.
            ScVal::U64(_)
            | ScVal::I64(_)
            | ScVal::U128(_)
            | ScVal::I128(_)
            | ScVal::ContractExecutable(_)
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
            | ScVal::LedgerKeyContractExecutable
            | ScVal::StorageType(_)
            | ScVal::LedgerKeyNonce(_) => Ok(()),
        }
    }
}

impl MeteredClone for ScValObject {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<ScVal>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScMapEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.key.charge_for_substructure(budget)?;
        self.val.charge_for_substructure(budget)
    }
}

impl MeteredClone for ScVec {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl MeteredClone for ScMap {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget) // self.0 will be deref'ed into Vec
    }
}

impl<const C: u32> MeteredClone for BytesM<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl<const C: u32> MeteredClone for StringM<C> {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScBytes {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScString {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl MeteredClone for ScSymbol {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        <Self as AsRef<Vec<u8>>>::as_ref(self).charge_for_substructure(budget)
    }
}

impl<C: MeteredClone> MeteredClone for Vec<C> {
    const IS_SHALLOW: bool = false;

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

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        // first take care of the Box clone: allocating memory and shallow cloning of data type.
        charge_heap_alloc::<C>(1, budget)?;
        charge_shallow_copy::<C>(1, budget)?;
        // then take care of any substructure clone (recursively)
        let inner: &C = self;
        inner.charge_for_substructure(budget)
    }
}

impl<C: MeteredClone> MeteredClone for Option<C> {
    const IS_SHALLOW: bool = C::IS_SHALLOW;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            Some(elt) => elt.charge_for_substructure(budget),
            None => Ok(()),
        }
    }
}

impl MeteredClone for ContractEvent {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        if let ContractEventType::Diagnostic = self.type_ {
            // Diagnostic events shouldn't be `metered_clone`d
            Err((ScErrorType::Events, ScErrorCode::InternalError).into())
        } else {
            self.contract_id.charge_for_substructure(budget)?;
            let ContractEventBody::V0(event) = &self.body;
            event.topics.charge_for_substructure(budget)?;
            event.data.charge_for_substructure(budget)
        }
    }
}

impl MeteredClone for HostEvent {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.event.charge_for_substructure(budget)
    }
}

impl MeteredClone for Events {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        self.0.charge_for_substructure(budget)
    }
}

impl MeteredClone for InternalEvent {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            InternalEvent::Contract(c) => c.charge_for_substructure(budget),
            InternalEvent::Diagnostic(_) => {
                Err((ScErrorType::Events, ScErrorCode::InternalError).into())
            }
        }
    }
}

impl MeteredClone for LedgerKey {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        match self {
            LedgerKey::ContractData(d) => d.key.charge_for_substructure(budget),
            LedgerKey::Account(_)
            | LedgerKey::Trustline(_)
            | LedgerKey::Offer(_)
            | LedgerKey::Data(_)
            | LedgerKey::ClaimableBalance(_)
            | LedgerKey::LiquidityPool(_)
            | LedgerKey::ContractCode(_)
            | LedgerKey::ConfigSetting(_) => Ok(()),
        }
    }
}

impl MeteredClone for LedgerEntry {
    const IS_SHALLOW: bool = false;

    fn charge_for_substructure(&self, budget: &Budget) -> Result<(), HostError> {
        use LedgerEntryData::*;
        match &self.data {
            ContractData(d) => {
                if let ContractDataEntryBody::DataEntry(e) = &d.body {
                    e.val.charge_for_substructure(budget)?;
                }
                d.key.charge_for_substructure(budget)
            }
            ContractCode(c) => {
                if let ContractCodeEntryBody::DataEntry(d) = &c.body {
                    d.charge_for_substructure(budget)?;
                }
                Ok(())
            }
            Account(_) | Trustline(_) | Offer(_) | Data(_) | ClaimableBalance(_)
            | LiquidityPool(_) | ConfigSetting(_) => Ok(()),
        }
    }
}
