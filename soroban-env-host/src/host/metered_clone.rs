use crate::{
    budget::{Budget, CostType},
    host::Events,
    xdr::{Hash, Uint256},
    HostError,
};

pub trait MeteredClone: Clone {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError>;
}
impl MeteredClone for Hash {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneBytes, 32)?;
        Ok(self.clone())
    }
}
impl MeteredClone for Vec<u8> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneBytes, self.len() as u64)?;
        Ok(self.clone())
    }
}
impl MeteredClone for Uint256 {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneBytes, 32 as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for Events {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneEvents, self.0.len() as u64)?;
        Ok(self.clone())
    }
}
