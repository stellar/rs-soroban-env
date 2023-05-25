use core::cmp::Ordering;

use stellar_xdr::ContractDataType;

use crate::{declare_tag_based_wrapper, Compare, Env};

declare_tag_based_wrapper!(StorageType);

/// Wrapper for a [RawVal] that is tagged with [Tag::StorageType].
/// The major value corresponds to the integer representation of the
/// [stellar_xdr::ContractDataType] enum.
impl StorageType {
    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        self.as_raw().get_major()
    }

    pub const TEMPORARY: StorageType =
        unsafe { StorageType::from_major_minor(ContractDataType::Temporary as u32, 0) };

    pub const RECREATABLE: StorageType =
        unsafe { StorageType::from_major_minor(ContractDataType::Recreatable as u32, 0) };

    pub const UNIQUE: StorageType =
        unsafe { StorageType::from_major_minor(ContractDataType::Unique as u32, 0) };
}

impl TryFrom<StorageType> for ContractDataType {
    type Error = stellar_xdr::Error;
    fn try_from(st: StorageType) -> Result<Self, Self::Error> {
        ContractDataType::try_from(st.get_code() as i32)
    }
}

impl PartialEq for StorageType {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_raw().get_payload() == other.as_raw().get_payload()
    }
}

impl Eq for StorageType {}

impl PartialOrd for StorageType {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StorageType {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        let self_major = self.as_raw().get_major();
        let other_major = other.as_raw().get_major();
        self_major.cmp(&other_major)
    }
}

impl<E: Env> Compare<StorageType> for E {
    type Error = E::Error;
    fn compare(&self, a: &StorageType, b: &StorageType) -> Result<Ordering, Self::Error> {
        Ok(a.cmp(&b))
    }
}
