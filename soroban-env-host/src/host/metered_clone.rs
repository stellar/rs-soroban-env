use crate::{
    budget::{Budget, CostType},
    host::Events,
    host::MeteredOrdMap,
    xdr::{AccountId, Hash, PublicKey, ScContractCode, Uint256},
    HostError,
};
use std::rc::Rc;

pub trait MeteredClone: Clone {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError>;
}
impl MeteredClone for Hash {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, 32)?;
        Ok(self.clone())
    }
}
impl MeteredClone for Vec<u8> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.len() as u64)?;
        Ok(self.clone())
    }
}
impl MeteredClone for Uint256 {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, 32 as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for Events {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneEvents, self.0.len() as u64)?;
        Ok(self.clone())
    }
}

impl<K, V> MeteredClone for MeteredOrdMap<K, V> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        assert!(Rc::ptr_eq(&self.budget.0, &budget.0));
        self.charge_new()?;
        Ok(self.clone())
    }
}

impl MeteredClone for ScContractCode {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        if let ScContractCode::Wasm(c) = self {
            budget.charge(CostType::BytesClone, c.len() as u64)?;
        }
        Ok(self.clone())
    }
}

impl MeteredClone for AccountId {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        match self.0 {
            PublicKey::PublicKeyTypeEd25519(_) => budget.charge(CostType::BytesClone, 32 as u64)?,
        }
        Ok(self.clone())
    }
}
