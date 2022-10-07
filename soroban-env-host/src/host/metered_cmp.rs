use std::cmp::{min, Ordering};

use soroban_env_common::xdr::{AccountId, PublicKey, ScContractCode, Uint256};

use crate::{
    budget::{Budget, CostType},
    host_object::HostObject,
    HostError,
};

// `MeteredCmp` exists for charging the nontrivial / structural part of an object comparison.
// It returns a `HostError` if the budget limit has been reached before the comparison.
// MeteredCmp is a proxy for, and ultimately delegates to `Ord::cmp`. It just charges a budget
// along the way.
pub(crate) trait MeteredCmp: Ord {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError>;
}

// `MeteredCmp` for host object, recursing through RawVal <-> Object ord-comparisons nested within.
// (via EnvVal::cmp -> Host::obj_cmp -> HostObject::metered_cmp).
impl MeteredCmp for &HostObject {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        match (self, other) {
            (HostObject::Vec(a), HostObject::Vec(b)) => a.metered_cmp(b, budget),
            (HostObject::Map(a), HostObject::Map(b)) => a.metered_cmp(b, budget),
            (HostObject::BigInt(a), HostObject::BigInt(b)) => a.metered_cmp(b, budget),
            (HostObject::Bytes(a), HostObject::Bytes(b)) => a.metered_cmp(b, budget),
            (HostObject::ContractCode(a), HostObject::ContractCode(b)) => a.metered_cmp(b, budget),
            (HostObject::AccountId(a), HostObject::AccountId(b)) => a.metered_cmp(b, budget),
            // if object types are different, the comparison is trivial, no metering.
            _ => Ok(self.cmp(other)),
        }
    }
}

impl<T: MeteredCmp> MeteredCmp for Option<T> {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        match (self, other) {
            (Some(a), Some(b)) => a.metered_cmp(b, budget),
            _ => Ok(self.cmp(other)),
        }
    }
}

impl MeteredCmp for Vec<u8> {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        budget.charge(CostType::BytesCmp, min(self.len(), other.len()) as u64)?;
        Ok(self.cmp(other))
    }
}

impl MeteredCmp for Uint256 {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        budget.charge(CostType::BytesCmp, self.0.len() as u64)?;
        Ok(self.cmp(other))
    }
}

impl MeteredCmp for ScContractCode {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        match (self, other) {
            (ScContractCode::Wasm(a), ScContractCode::Wasm(b)) => a.metered_cmp(b, budget),
            _ => Ok(self.cmp(other)),
        }
    }
}

impl MeteredCmp for AccountId {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        match (&self.0, &other.0) {
            (PublicKey::PublicKeyTypeEd25519(a), PublicKey::PublicKeyTypeEd25519(b)) => {
                a.metered_cmp(&b, budget)
            }
        }
    }
}
