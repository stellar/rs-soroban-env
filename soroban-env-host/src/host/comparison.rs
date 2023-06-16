use core::cmp::{min, Ordering};

use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ClaimableBalanceEntry, ConfigSettingEntry, ContractCodeEntryBody,
        ContractCostType, ContractDataEntryBody, ContractDataEntryData, ContractDataType,
        ContractLedgerEntryType, CreateContractArgs, DataEntry, Duration, ExtensionPoint, Hash,
        LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey, LedgerKeyAccount,
        LedgerKeyClaimableBalance, LedgerKeyConfigSetting, LedgerKeyContractCode, LedgerKeyData,
        LedgerKeyLiquidityPool, LedgerKeyOffer, LedgerKeyTrustLine, LiquidityPoolEntry, OfferEntry,
        PublicKey, ScAddress, ScContractExecutable, ScErrorCode, ScErrorType, ScMap, ScMapEntry,
        ScNonceKey, ScVal, ScVec, TimePoint, TrustLineAsset, TrustLineEntry, Uint256,
    },
    Compare, SymbolStr, I256, U256,
};

use crate::{
    budget::{AsBudget, Budget},
    host_object::HostObject,
    Host, HostError,
};

use super::declared_size::DeclaredSizeForMetering;

// We can't use core::mem::discriminant here because it returns an opaque type
// that only supports Eq, not Ord, to reduce the possibility of an API breakage
// based on reordering enums: https://github.com/rust-lang/rust/issues/51561
//
// Note that these must have the same order as the impl
// of Ord for ScVal, re https://github.com/stellar/rs-soroban-env/issues/743
fn host_obj_discriminant(ho: &HostObject) -> usize {
    match ho {
        HostObject::U64(_) => 0,
        HostObject::I64(_) => 1,
        HostObject::TimePoint(_) => 2,
        HostObject::Duration(_) => 3,
        HostObject::U128(_) => 4,
        HostObject::I128(_) => 5,
        HostObject::U256(_) => 6,
        HostObject::I256(_) => 7,
        HostObject::Bytes(_) => 8,
        HostObject::String(_) => 9,
        HostObject::Symbol(_) => 10,
        HostObject::Vec(_) => 11,
        HostObject::Map(_) => 12,
        HostObject::ContractExecutable(_) => 13,
        HostObject::Address(_) => 14,
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
        self.charge(
            ContractCostType::HostMemCmp,
            Some(min(a.len(), b.len()) as u64),
        )?;
        Ok(a.cmp(b))
    }
}

impl<const N: usize> Compare<[u8; N]> for Budget {
    type Error = HostError;

    fn compare(&self, a: &[u8; N], b: &[u8; N]) -> Result<Ordering, Self::Error> {
        self.charge(
            ContractCostType::HostMemCmp,
            Some(min(a.len(), b.len()) as u64),
        )?;
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
            std::mem::size_of::<T>() as u64 <= <T as DeclaredSizeForMetering>::DECLARED_SIZE,
            "mem size: {}, declared: {}",
            std::mem::size_of::<T>(),
            <T as DeclaredSizeForMetering>::DECLARED_SIZE
        );
        self.charge(
            ContractCostType::HostMemCmp,
            Some(<T as DeclaredSizeForMetering>::DECLARED_SIZE),
        )?;
        Ok(a.0.cmp(b.0))
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
impl_compare_fixed_size_ord_type!(ContractDataType);
impl_compare_fixed_size_ord_type!(ContractLedgerEntryType);

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
impl_compare_fixed_size_ord_type!(ConfigSettingEntry);
impl_compare_fixed_size_ord_type!(CreateContractArgs);
impl_compare_fixed_size_ord_type!(ExtensionPoint);

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
        let a: &Vec<ScVal> = a;
        let b: &Vec<ScVal> = b;
        self.compare(a, b)
    }
}

impl Compare<ScMap> for Budget {
    type Error = HostError;

    fn compare(&self, a: &ScMap, b: &ScMap) -> Result<Ordering, Self::Error> {
        let a: &Vec<ScMapEntry> = a;
        let b: &Vec<ScMapEntry> = b;
        self.compare(a, b)
    }
}

impl Compare<ScMapEntry> for Budget {
    type Error = HostError;

    fn compare(&self, a: &ScMapEntry, b: &ScMapEntry) -> Result<Ordering, Self::Error> {
        match self.compare(&a.key, &b.key)? {
            Ordering::Equal => self.compare(&a.val, &b.val),
            cmp => Ok(cmp),
        }
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
                Err((ScErrorType::Object, ScErrorCode::MissingValue).into())
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
            | (Error(_), _)
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
            | (StorageType(_), _)
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
            (ContractData(a), ContractData(b)) => self.compare(
                &(&a.contract, &a.key, &a.type_, &a.le_type),
                &(&b.contract, &b.key, &b.type_, &b.le_type),
            ),
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
                &(
                    &a.contract,
                    &a.key,
                    &a.type_,
                    &a.body,
                    &a.expiration_ledger_seq,
                ),
                &(
                    &b.contract,
                    &b.key,
                    &b.type_,
                    &b.body,
                    &b.expiration_ledger_seq,
                ),
            ),
            (ContractCode(a), ContractCode(b)) => self.compare(
                &(&a.ext, &a.hash, &a.body, &a.expiration_ledger_seq),
                &(&b.ext, &b.hash, &b.body, &b.expiration_ledger_seq),
            ),
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

impl Compare<ContractDataEntryData> for Budget {
    type Error = HostError;

    fn compare(
        &self,
        a: &ContractDataEntryData,
        b: &ContractDataEntryData,
    ) -> Result<Ordering, Self::Error> {
        self.compare(&(a.flags, &a.val), &(b.flags, &b.val))
    }
}

impl Compare<ContractDataEntryBody> for Budget {
    type Error = HostError;

    fn compare(
        &self,
        a: &ContractDataEntryBody,
        b: &ContractDataEntryBody,
    ) -> Result<Ordering, Self::Error> {
        match (a, b) {
            (ContractDataEntryBody::DataEntry(a), ContractDataEntryBody::DataEntry(b)) => {
                self.compare(&(a), &(b))
            }
            (ContractDataEntryBody::DataEntry(_), _)
            | (ContractDataEntryBody::ExpirationExtension, _) => Ok(a.cmp(b)),
        }
    }
}

impl Compare<ContractCodeEntryBody> for Budget {
    type Error = HostError;

    fn compare(
        &self,
        a: &ContractCodeEntryBody,
        b: &ContractCodeEntryBody,
    ) -> Result<Ordering, Self::Error> {
        match (a, b) {
            (ContractCodeEntryBody::DataEntry(a), ContractCodeEntryBody::DataEntry(b)) => {
                <Self as Compare<&[u8]>>::compare(self, &a.as_ref(), &b.as_ref())
            }
            (ContractCodeEntryBody::DataEntry(_), _)
            | (ContractCodeEntryBody::ExpirationExtension, _) => Ok(a.cmp(b)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::xdr::ScVal;
    use crate::{Compare, Host, Tag, TryFromVal, Val};
    use itertools::Itertools;
    use soroban_env_common::StorageType;

    #[test]
    fn test_scvec_unequal_lengths() {
        {
            let v1 = ScVec::try_from((0, 1)).unwrap();
            let v2 = ScVec::try_from((0, 1, 2)).unwrap();
            let expected_cmp = Ordering::Less;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScVec::try_from((0, 1, 2)).unwrap();
            let v2 = ScVec::try_from((0, 1)).unwrap();
            let expected_cmp = Ordering::Greater;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScVec::try_from((0, 1)).unwrap();
            let v2 = ScVec::try_from((0, 0, 2)).unwrap();
            let expected_cmp = Ordering::Greater;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScVec::try_from((0, 0, 2)).unwrap();
            let v2 = ScVec::try_from((0, 1)).unwrap();
            let expected_cmp = Ordering::Less;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
    }

    #[test]
    fn test_scmap_unequal_lengths() {
        {
            let v1 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
            ])
            .unwrap();
            let v2 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
                (ScVal::U32(2), ScVal::U32(2)),
            ])
            .unwrap();
            let expected_cmp = Ordering::Less;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
                (ScVal::U32(2), ScVal::U32(2)),
            ])
            .unwrap();
            let v2 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
            ])
            .unwrap();
            let expected_cmp = Ordering::Greater;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
            ])
            .unwrap();
            let v2 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(0)),
                (ScVal::U32(2), ScVal::U32(2)),
            ])
            .unwrap();
            let expected_cmp = Ordering::Greater;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
        {
            let v1 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(0)),
                (ScVal::U32(2), ScVal::U32(2)),
            ])
            .unwrap();
            let v2 = ScMap::sorted_from([
                (ScVal::U32(0), ScVal::U32(0)),
                (ScVal::U32(1), ScVal::U32(1)),
            ])
            .unwrap();
            let expected_cmp = Ordering::Less;
            let budget = Budget::default();
            let actual_cmp = budget.compare(&v1, &v2).unwrap();
            assert_eq!(expected_cmp, actual_cmp);
        }
    }

    #[test]
    fn host_obj_discriminant_order() {
        // The HostObject discriminants need to be ordered the same
        // as the ScVal discriminants so that Compare<HostObject>
        // produces the same results as `Ord for ScVal`,
        // re https://github.com/stellar/rs-soroban-env/issues/743.
        //
        // This test creates pairs of corresponding ScVal/HostObjects,
        // puts them all into a list, and sorts them 2 ways:
        // comparing ScVals, and comparing the HostObject discriminants;
        // then tests that the two lists are the same.

        use crate::ScValObjRef;
        use soroban_env_common::xdr;

        let host = Host::default();

        let xdr_vals = &[
            ScVal::U64(u64::MAX),
            ScVal::I64(i64::MAX),
            ScVal::Timepoint(xdr::TimePoint(u64::MAX)),
            ScVal::Duration(xdr::Duration(u64::MAX)),
            ScVal::U128(xdr::UInt128Parts {
                hi: u64::MAX,
                lo: u64::MAX,
            }),
            ScVal::I128(xdr::Int128Parts {
                hi: i64::MIN,
                lo: u64::MAX,
            }),
            ScVal::U256(xdr::UInt256Parts {
                hi_hi: u64::MAX,
                hi_lo: u64::MAX,
                lo_hi: u64::MAX,
                lo_lo: u64::MAX,
            }),
            ScVal::I256(xdr::Int256Parts {
                hi_hi: i64::MIN,
                hi_lo: u64::MAX,
                lo_hi: u64::MAX,
                lo_lo: u64::MAX,
            }),
            ScVal::Bytes(xdr::ScBytes::try_from(vec![]).unwrap()),
            ScVal::String(xdr::ScString::try_from(vec![]).unwrap()),
            ScVal::Symbol(xdr::ScSymbol::try_from("big-symbol").unwrap()),
            ScVal::Vec(Some(xdr::ScVec::try_from((0,)).unwrap())),
            ScVal::Map(Some(xdr::ScMap::try_from(vec![]).unwrap())),
            ScVal::ContractExecutable(xdr::ScContractExecutable::Token),
            ScVal::Address(xdr::ScAddress::Contract(xdr::Hash([0; 32]))),
        ];

        let pairs: Vec<_> = xdr_vals
            .into_iter()
            .map(|xdr_val| {
                let xdr_obj = ScValObjRef::classify(&xdr_val).unwrap();
                let host_obj = host.to_host_obj(&xdr_obj).unwrap();
                (xdr_obj, host_obj)
            })
            .collect();

        let mut pairs_xdr_sorted = pairs.clone();
        let mut pairs_host_sorted = pairs_xdr_sorted.clone();

        pairs_xdr_sorted.sort_by(|&(v1, _), &(v2, _)| v1.cmp(&v2));

        pairs_host_sorted.sort_by(|&(_, v1), &(_, v2)| unsafe {
            host.unchecked_visit_val_obj(v1, |v1| {
                host.unchecked_visit_val_obj(v2, |v2| {
                    let v1 = v1.unwrap();
                    let v2 = v2.unwrap();
                    let v1d = host_obj_discriminant(v1);
                    let v2d = host_obj_discriminant(v2);
                    Ok(v1d.cmp(&v2d))
                })
            })
            .unwrap()
        });

        let iter = pairs_xdr_sorted
            .into_iter()
            .zip(pairs_host_sorted.into_iter());

        for ((xdr1, _), (xdr2, _)) in iter {
            assert_eq!(xdr1, xdr2);
        }
    }

    /// Test that comparison of an object of one type to a small value of another
    /// type produces the same results as the equivalent ScVal comparison.
    ///
    /// This is a test of the Host::obj_cmp and Tag::get_scval_type methods.
    ///
    /// It works by generating an "example" Val for every possible tag,
    /// with a match on Tag that ensures it will be updated as Tag changes.
    ///
    /// Those examples are then converted to an array of ScVal.
    ///
    /// For both arrays, every pairwise comparison is performed, and must be equal.
    #[test]
    fn compare_obj_to_small() {
        let host = Host::default();
        let rawvals: Vec<Val> = all_tags()
            .into_iter()
            .map(|t| example_for_tag(&host, t))
            .collect();
        let scvals: Vec<ScVal> = rawvals
            .iter()
            .map(|r| ScVal::try_from_val(&host, r).expect("scval"))
            .collect();

        let rawval_pairs = rawvals.iter().cartesian_product(&rawvals);
        let scval_pairs = scvals.iter().cartesian_product(&scvals);

        let pair_pairs = rawval_pairs.zip(scval_pairs);

        for ((rawval1, rawval2), (scval1, scval2)) in pair_pairs {
            let rawval_cmp = host.compare(rawval1, rawval2).expect("compare");
            let scval_cmp = scval1.cmp(scval2);
            assert_eq!(rawval_cmp, scval_cmp);
        }
    }

    fn all_tags() -> Vec<Tag> {
        (0_u8..=255)
            .map(Tag::from_u8)
            .filter(|t| {
                // bad tags can't be converted to ScVal
                !matches!(t, Tag::Bad)
            })
            .filter(|t| {
                // objects of this type can't be instantiated
                !matches!(t, Tag::LedgerKeyNonceObject)
            })
            .collect()
    }

    fn example_for_tag(host: &Host, tag: Tag) -> Val {
        use crate::{xdr, Error};

        let ex = match tag {
            Tag::False => Val::from(false),
            Tag::True => Val::from(true),
            Tag::Void => Val::from(()),
            Tag::Error => Val::from(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::InternalError,
            )),
            Tag::U32Val => Val::from(u32::MAX),
            Tag::I32Val => Val::from(i32::MAX),
            Tag::U64Small => Val::try_from_val(host, &0_u64).unwrap(),
            Tag::I64Small => Val::try_from_val(host, &0_i64).unwrap(),
            Tag::TimepointSmall => {
                Val::try_from_val(host, &ScVal::Timepoint(xdr::TimePoint(0))).unwrap()
            }
            Tag::DurationSmall => {
                Val::try_from_val(host, &ScVal::Duration(xdr::Duration(0))).unwrap()
            }
            Tag::U128Small => Val::try_from_val(host, &0_u128).unwrap(),
            Tag::I128Small => Val::try_from_val(host, &0_i128).unwrap(),
            Tag::U256Small => Val::try_from_val(
                host,
                &ScVal::U256(xdr::UInt256Parts {
                    hi_hi: 0,
                    hi_lo: 0,
                    lo_hi: 0,
                    lo_lo: 0,
                }),
            )
            .unwrap(),
            Tag::I256Small => Val::try_from_val(
                host,
                &ScVal::I256(xdr::Int256Parts {
                    hi_hi: 0,
                    hi_lo: 0,
                    lo_hi: 0,
                    lo_lo: 0,
                }),
            )
            .unwrap(),
            Tag::SymbolSmall => {
                Val::try_from_val(host, &ScVal::Symbol(xdr::ScSymbol::try_from("").unwrap()))
                    .unwrap()
            }
            Tag::LedgerKeyContractExecutable => {
                Val::try_from_val(host, &ScVal::LedgerKeyContractExecutable).unwrap()
            }
            Tag::SmallCodeUpperBound => panic!(),
            Tag::ObjectCodeLowerBound => panic!(),
            Tag::U64Object => Val::try_from_val(host, &u64::MAX).unwrap(),
            Tag::I64Object => Val::try_from_val(host, &i64::MAX).unwrap(),
            Tag::TimepointObject => {
                Val::try_from_val(host, &ScVal::Timepoint(xdr::TimePoint(u64::MAX))).unwrap()
            }
            Tag::DurationObject => {
                Val::try_from_val(host, &ScVal::Duration(xdr::Duration(u64::MAX))).unwrap()
            }
            Tag::U128Object => Val::try_from_val(host, &u128::MAX).unwrap(),
            Tag::I128Object => Val::try_from_val(host, &i128::MAX).unwrap(),
            Tag::U256Object => Val::try_from_val(
                host,
                &ScVal::U256(xdr::UInt256Parts {
                    hi_hi: u64::MAX,
                    hi_lo: u64::MAX,
                    lo_hi: u64::MAX,
                    lo_lo: u64::MAX,
                }),
            )
            .unwrap(),
            Tag::I256Object => Val::try_from_val(
                host,
                &ScVal::I256(xdr::Int256Parts {
                    hi_hi: i64::MIN,
                    hi_lo: u64::MAX,
                    lo_hi: u64::MAX,
                    lo_lo: u64::MAX,
                }),
            )
            .unwrap(),
            Tag::BytesObject => Val::try_from_val(host, &vec![1]).unwrap(),
            Tag::StringObject => Val::try_from_val(host, &"foo").unwrap(),
            Tag::SymbolObject => Val::try_from_val(
                host,
                &ScVal::Symbol(xdr::ScSymbol::try_from("a-big-symbol").unwrap()),
            )
            .unwrap(),
            Tag::VecObject => {
                Val::try_from_val(host, &ScVal::Vec(Some(xdr::ScVec::try_from((0,)).unwrap())))
                    .unwrap()
            }
            Tag::MapObject => Val::try_from_val(
                host,
                &ScVal::Map(Some(xdr::ScMap::try_from(vec![]).unwrap())),
            )
            .unwrap(),
            Tag::ContractExecutableObject => Val::try_from_val(
                host,
                &ScVal::ContractExecutable(xdr::ScContractExecutable::Token),
            )
            .unwrap(),
            Tag::AddressObject => Val::try_from_val(
                host,
                &ScVal::Address(xdr::ScAddress::Contract(xdr::Hash([0; 32]))),
            )
            .unwrap(),
            Tag::StorageType => Val::from(StorageType::PERSISTENT),
            Tag::LedgerKeyNonceObject => panic!(),
            Tag::ObjectCodeUpperBound => panic!(),
            Tag::Bad => panic!(),
            // NB: do not add a fallthrough case here if new Tag variants are added.
            // this test depends on the match being exhaustive in order to ensure
            // the correctness of Tag discriminants.
        };

        assert_eq!(ex.get_tag(), tag);

        ex
    }
}
