use core::cmp::{min, Ordering};

use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ClaimableBalanceEntry, ConfigSettingEntry, ContractCodeEntry,
        DataEntry, Duration, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
        LedgerKeyAccount, LedgerKeyClaimableBalance, LedgerKeyConfigSetting, LedgerKeyContractCode,
        LedgerKeyData, LedgerKeyLiquidityPool, LedgerKeyOffer, LedgerKeyTrustLine,
        LiquidityPoolEntry, OfferEntry, PublicKey, ScAddress, ScContractExecutable,
        ScHostValErrorCode, ScMap, ScNonceKey, ScVal, ScVec, TimePoint, TrustLineAsset,
        TrustLineEntry, Uint256,
    },
    Compare, SymbolStr, I256, U256,
};

use crate::{
    budget::{AsBudget, Budget, CostType},
    host_object::HostObject,
    Host, HostError,
};

use super::declared_size::DeclaredSizeForMetering;

// We can't use core::mem::discriminant here because it returns an opaque type
// that only supports Eq, not Ord, to reduce the possibility of an API breakage
// based on reordering enums: https://github.com/rust-lang/rust/issues/51561
fn host_obj_discriminant(ho: &HostObject) -> usize {
    match ho {
        HostObject::Vec(_) => 0,
        HostObject::Map(_) => 1,
        HostObject::U64(_) => 2,
        HostObject::I64(_) => 3,
        HostObject::TimePoint(_) => 4,
        HostObject::Duration(_) => 5,
        HostObject::U128(_) => 6,
        HostObject::I128(_) => 7,
        HostObject::U256(_) => 8,
        HostObject::I256(_) => 9,
        HostObject::Bytes(_) => 10,
        HostObject::String(_) => 11,
        HostObject::Symbol(_) => 12,
        HostObject::Address(_) => 13,
        HostObject::ContractExecutable(_) => 14,
        HostObject::NonceKey(_) => 15,
    }
}

impl Compare<HostObject> for Host {
    type Error = HostError;

    fn compare(&self, a: &HostObject, b: &HostObject) -> Result<Ordering, Self::Error> {
        use HostObject::*;
        match (a, b) {
            (U64(a), U64(b)) => self.as_budget().compare(a, b),
            (I64(a), I64(b)) => self.as_budget().compare(a, b),
            (TimePoint(a), TimePoint(b)) => self.as_budget().compare(a, b),
            (Duration(a), Duration(b)) => self.as_budget().compare(a, b),
            (U128(a), U128(b)) => self.as_budget().compare(a, b),
            (I128(a), I128(b)) => self.as_budget().compare(a, b),
            (U256(a), U256(b)) => self.as_budget().compare(a, b),
            (I256(a), I256(b)) => self.as_budget().compare(a, b),
            (Vec(a), Vec(b)) => self.compare(a, b),
            (Map(a), Map(b)) => self.compare(a, b),
            (Bytes(a), Bytes(b)) => self.as_budget().compare(&a.as_slice(), &b.as_slice()),
            (String(a), String(b)) => self.as_budget().compare(&a.as_slice(), &b.as_slice()),
            (Symbol(a), Symbol(b)) => self.as_budget().compare(&a.as_slice(), &b.as_slice()),
            (ContractExecutable(a), ContractExecutable(b)) => self.as_budget().compare(a, b),
            (Address(a), Address(b)) => self.as_budget().compare(a, b),
            (NonceKey(a), NonceKey(b)) => self.as_budget().compare(a, b),

            // List out at least one side of all the remaining cases here so
            // we don't accidentally forget to update this when/if a new
            // HostObject type is added.
            (U64(_), _)
            | (TimePoint(_), _)
            | (Duration(_), _)
            | (I64(_), _)
            | (U128(_), _)
            | (I128(_), _)
            | (U256(_), _)
            | (I256(_), _)
            | (Vec(_), _)
            | (Map(_), _)
            | (Bytes(_), _)
            | (String(_), _)
            | (Symbol(_), _)
            | (ContractExecutable(_), _)
            | (Address(_), _)
            | (NonceKey(_), _) => {
                let a = host_obj_discriminant(a);
                let b = host_obj_discriminant(b);
                Ok(a.cmp(&b))
            }
        }
    }
}

impl Compare<&[u8]> for Budget {
    type Error = HostError;

    fn compare(&self, a: &&[u8], b: &&[u8]) -> Result<Ordering, Self::Error> {
        self.charge(CostType::HostMemCmp, Some(min(a.len(), b.len()) as u64))?;
        Ok(a.cmp(b))
    }
}

impl<const N: usize> Compare<[u8; N]> for Budget {
    type Error = HostError;

    fn compare(&self, a: &[u8; N], b: &[u8; N]) -> Result<Ordering, Self::Error> {
        self.charge(CostType::HostMemCmp, Some(min(a.len(), b.len()) as u64))?;
        Ok(a.cmp(b))
    }
}

// Apparently we can't do a blanket T:Ord impl because there are Ord derivations
// that also go through &T and Option<T> that conflict with our impls above
// (patches welcome from someone who understands trait-system workarounds
// better). But we can list out any concrete Ord instances we want to support
// here.
//
// We only do this for declared-size types, because we want to charge them a constant
// based on their size declared in accordance with their type layout.

struct FixedSizeOrdType<'a, T: Ord + DeclaredSizeForMetering>(&'a T);
impl<T: Ord + DeclaredSizeForMetering> Compare<FixedSizeOrdType<'_, T>> for Budget {
    type Error = HostError;
    fn compare(
        &self,
        a: &FixedSizeOrdType<'_, T>,
        b: &FixedSizeOrdType<'_, T>,
    ) -> Result<Ordering, Self::Error> {
        // Here we make a runtime assertion that the type's size is below its promised element
        // size for budget charging.
        debug_assert!(
            std::mem::size_of::<T>() as u64 <= <T as DeclaredSizeForMetering>::DECLARED_SIZE
        );
        self.charge(
            CostType::HostMemCmp,
            Some(<T as DeclaredSizeForMetering>::DECLARED_SIZE),
        )?;
        Ok(a.0.cmp(&b.0))
    }
}

macro_rules! impl_compare_fixed_size_ord_type {
    ($t:ty) => {
        impl Compare<$t> for Budget {
            type Error = HostError;
            fn compare(&self, a: &$t, b: &$t) -> Result<Ordering, Self::Error> {
                self.compare(&FixedSizeOrdType(a), &FixedSizeOrdType(b))
            }
        }
        impl Compare<$t> for Host {
            type Error = HostError;
            fn compare(&self, a: &$t, b: &$t) -> Result<Ordering, Self::Error> {
                self.as_budget().compare(a, b)
            }
        }
    };
}

impl_compare_fixed_size_ord_type!(bool);
impl_compare_fixed_size_ord_type!(u32);
impl_compare_fixed_size_ord_type!(i32);
impl_compare_fixed_size_ord_type!(u64);
impl_compare_fixed_size_ord_type!(i64);
impl_compare_fixed_size_ord_type!(u128);
impl_compare_fixed_size_ord_type!(i128);

impl_compare_fixed_size_ord_type!(U256);
impl_compare_fixed_size_ord_type!(I256);
impl_compare_fixed_size_ord_type!(TimePoint);
impl_compare_fixed_size_ord_type!(Duration);
impl_compare_fixed_size_ord_type!(Hash);
impl_compare_fixed_size_ord_type!(Uint256);
impl_compare_fixed_size_ord_type!(ScContractExecutable);
impl_compare_fixed_size_ord_type!(AccountId);
impl_compare_fixed_size_ord_type!(ScAddress);
impl_compare_fixed_size_ord_type!(ScNonceKey);
impl_compare_fixed_size_ord_type!(PublicKey);
impl_compare_fixed_size_ord_type!(TrustLineAsset);

impl_compare_fixed_size_ord_type!(LedgerKeyAccount);
impl_compare_fixed_size_ord_type!(LedgerKeyTrustLine);
impl_compare_fixed_size_ord_type!(LedgerKeyOffer);
impl_compare_fixed_size_ord_type!(LedgerKeyData);
impl_compare_fixed_size_ord_type!(LedgerKeyClaimableBalance);
impl_compare_fixed_size_ord_type!(LedgerKeyLiquidityPool);
impl_compare_fixed_size_ord_type!(LedgerKeyContractCode);
impl_compare_fixed_size_ord_type!(LedgerKeyConfigSetting);

impl_compare_fixed_size_ord_type!(LedgerEntryExt);

impl_compare_fixed_size_ord_type!(AccountEntry);
impl_compare_fixed_size_ord_type!(TrustLineEntry);
impl_compare_fixed_size_ord_type!(OfferEntry);
impl_compare_fixed_size_ord_type!(DataEntry);
impl_compare_fixed_size_ord_type!(ClaimableBalanceEntry);
impl_compare_fixed_size_ord_type!(LiquidityPoolEntry);
impl_compare_fixed_size_ord_type!(ContractCodeEntry);
impl_compare_fixed_size_ord_type!(ConfigSettingEntry);

impl Compare<SymbolStr> for Budget {
    type Error = HostError;

    fn compare(&self, a: &SymbolStr, b: &SymbolStr) -> Result<Ordering, Self::Error> {
        self.compare(
            &<SymbolStr as AsRef<[u8]>>::as_ref(a),
            &<SymbolStr as AsRef<[u8]>>::as_ref(b),
        )
    }
}

impl Compare<ScVec> for Budget {
    type Error = HostError;

    fn compare(&self, a: &ScVec, b: &ScVec) -> Result<Ordering, Self::Error> {
        for (a, b) in a.iter().zip(b.iter()) {
            match self.compare(a, b)? {
                Ordering::Equal => (),
                unequal => return Ok(unequal),
            }
        }
        Ok(Ordering::Equal)
    }
}

impl Compare<ScMap> for Budget {
    type Error = HostError;

    fn compare(&self, a: &ScMap, b: &ScMap) -> Result<Ordering, Self::Error> {
        for (a, b) in a.iter().zip(b.iter()) {
            match self.compare(&(&a.key, &a.val), &(&b.key, &b.val))? {
                Ordering::Equal => (),
                unequal => return Ok(unequal),
            }
        }
        Ok(Ordering::Equal)
    }
}

impl Compare<ScVal> for Budget {
    type Error = HostError;

    fn compare(&self, a: &ScVal, b: &ScVal) -> Result<Ordering, Self::Error> {
        use ScVal::*;
        match (a, b) {
            (Vec(Some(a)), Vec(Some(b))) => self.compare(a, b),
            (Map(Some(a)), Map(Some(b))) => self.compare(a, b),

            (Vec(None), _) | (_, Vec(None)) | (Map(None), _) | (_, Map(None)) => {
                Err(ScHostValErrorCode::MissingObject.into())
            }

            (Bytes(a), Bytes(b)) => {
                <Self as Compare<&[u8]>>::compare(self, &a.as_slice(), &b.as_slice())
            }

            (String(a), String(b)) => {
                <Self as Compare<&[u8]>>::compare(self, &a.as_slice(), &b.as_slice())
            }

            (Symbol(a), Symbol(b)) => {
                <Self as Compare<&[u8]>>::compare(self, &a.as_slice(), &b.as_slice())
            }

            (Bool(_), _)
            | (Void, _)
            | (Status(_), _)
            | (U32(_), _)
            | (I32(_), _)
            | (U64(_), _)
            | (I64(_), _)
            | (Timepoint(_), _)
            | (Duration(_), _)
            | (U128(_), _)
            | (I128(_), _)
            | (U256(_), _)
            | (I256(_), _)
            | (Bytes(_), _)
            | (String(_), _)
            | (Symbol(_), _)
            | (Vec(_), _)
            | (Map(_), _)
            | (ContractExecutable(_), _)
            | (Address(_), _)
            | (LedgerKeyContractExecutable, _)
            | (LedgerKeyNonce(_), _) => Ok(a.cmp(b)),
        }
    }
}

impl Compare<LedgerKey> for Budget {
    type Error = HostError;

    fn compare(&self, a: &LedgerKey, b: &LedgerKey) -> Result<Ordering, Self::Error> {
        use LedgerKey::*;
        match (a, b) {
            (Account(a), Account(b)) => self.compare(&a, &b),
            (Trustline(a), Trustline(b)) => self.compare(&a, &b),
            (Offer(a), Offer(b)) => self.compare(&a, &b),
            (Data(a), Data(b)) => self.compare(&a, &b),
            (ClaimableBalance(a), ClaimableBalance(b)) => self.compare(&a, &b),
            (LiquidityPool(a), LiquidityPool(b)) => self.compare(&a, &b),
            (ContractData(a), ContractData(b)) => {
                self.compare(&(&a.contract_id, &a.key), &(&b.contract_id, &b.key))
            }
            (ContractCode(a), ContractCode(b)) => self.compare(&a, &b),
            (ConfigSetting(a), ConfigSetting(b)) => self.compare(&a, &b),

            // List out one side of each remaining unequal-discriminant case so
            // we remember to update this code if LedgerKey changes. We don't
            // charge for these since they're just 1-integer compares.
            (Account(_), _)
            | (Trustline(_), _)
            | (Offer(_), _)
            | (Data(_), _)
            | (ClaimableBalance(_), _)
            | (LiquidityPool(_), _)
            | (ContractData(_), _)
            | (ContractCode(_), _)
            | (ConfigSetting(_), _) => Ok(a.cmp(b)),
        }
    }
}

impl Compare<LedgerEntry> for Budget {
    type Error = HostError;

    fn compare(&self, a: &LedgerEntry, b: &LedgerEntry) -> Result<Ordering, Self::Error> {
        self.compare(
            &(a.last_modified_ledger_seq, &a.data, &a.ext),
            &(b.last_modified_ledger_seq, &b.data, &b.ext),
        )
    }
}

impl Compare<LedgerEntryData> for Budget {
    type Error = HostError;

    fn compare(&self, a: &LedgerEntryData, b: &LedgerEntryData) -> Result<Ordering, Self::Error> {
        use LedgerEntryData::*;
        match (a, b) {
            (Account(a), Account(b)) => self.compare(&a, &b),
            (Trustline(a), Trustline(b)) => self.compare(&a, &b),
            (Offer(a), Offer(b)) => self.compare(&a, &b),
            (Data(a), Data(b)) => self.compare(&a, &b),
            (ClaimableBalance(a), ClaimableBalance(b)) => self.compare(&a, &b),
            (LiquidityPool(a), LiquidityPool(b)) => self.compare(&a, &b),
            (ContractData(a), ContractData(b)) => self.compare(
                &(&a.contract_id, &a.key, &a.val),
                &(&b.contract_id, &b.key, &b.val),
            ),
            (ContractCode(a), ContractCode(b)) => self.compare(&a, &b),
            (ConfigSetting(a), ConfigSetting(b)) => self.compare(&a, &b),

            (Account(_), _)
            | (Trustline(_), _)
            | (Offer(_), _)
            | (Data(_), _)
            | (ClaimableBalance(_), _)
            | (LiquidityPool(_), _)
            | (ContractData(_), _)
            | (ContractCode(_), _)
            | (ConfigSetting(_), _) => Ok(a.cmp(b)),
        }
    }
}
