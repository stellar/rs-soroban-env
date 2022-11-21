use crate::{
    budget::{Budget, CostType},
    host::Events,
    xdr::{AccountId, Hash, PublicKey, ScContractCode, ScVec, Uint256},
    HostError,
};

pub trait MeteredClone: Clone {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError>;
}

impl MeteredClone for Hash {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.0.len() as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for Vec<u8> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.len() as u64)?;
        Ok(self.clone())
    }
}

impl<const N: usize> MeteredClone for [u8; N] {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.len() as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for Uint256 {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.0.len() as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for Events {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::CloneEvents, self.0.len() as u64)?;
        Ok(self.clone())
    }
}

impl MeteredClone for ScContractCode {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        match self {
            ScContractCode::WasmRef(h) => budget.charge(CostType::BytesClone, h.0.len() as u64)?,
            ScContractCode::Token => (),
        };
        Ok(self.clone())
    }
}

impl MeteredClone for ScVec {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, self.0.len() as u64)?;
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
