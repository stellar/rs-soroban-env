use super::Budget;
use crate::{host::error::TryBorrowOrErr, HostError};

#[cfg(test)]
use super::model::ScaledU64;
#[cfg(test)]
use crate::xdr::ContractCostType;

#[cfg(test)]
impl Budget {
    pub fn reset_models(&self) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset_models();
            b.mem_bytes.reset_models();
            Ok(())
        })
    }

    pub(crate) fn override_model_with_scaled_params(
        &self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: ScaledU64,
        const_mem: u64,
        lin_mem: ScaledU64,
    ) -> Result<(), HostError> {
        let mut bgt = self.0.try_borrow_mut_or_err()?;

        let cpu_model = bgt.cpu_insns.get_cost_model_mut(ty);
        cpu_model.const_term = const_cpu;
        cpu_model.lin_term = lin_cpu;

        let mem_model = bgt.mem_bytes.get_cost_model_mut(ty);
        mem_model.const_term = const_mem;
        mem_model.lin_term = lin_mem;
        Ok(())
    }

    pub(crate) fn override_model_with_unscaled_params(
        &self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: u64,
        const_mem: u64,
        lin_mem: u64,
    ) -> Result<(), HostError> {
        self.override_model_with_scaled_params(
            ty,
            const_cpu,
            ScaledU64::from_unscaled_u64(lin_cpu),
            const_mem,
            ScaledU64::from_unscaled_u64(lin_mem),
        )
    }

    pub(crate) fn track_wasm_mem_alloc(&self, delta: u64) -> Result<(), HostError> {
        let mut bgt = self.0.try_borrow_mut_or_err()?;
        bgt.tracker.wasm_memory = bgt.tracker.wasm_memory.saturating_add(delta);
        Ok(())
    }

    pub(crate) fn get_wasm_mem_alloc(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.tracker.wasm_memory)
    }
}

#[cfg(any(test, feature = "testutils"))]
impl Budget {
    pub fn reset_default(&self) -> Result<(), HostError> {
        *self.0.try_borrow_mut_or_err()? = super::BudgetImpl::default();
        Ok(())
    }

    pub fn reset_unlimited(&self) -> Result<(), HostError> {
        self.reset_unlimited_cpu()?;
        self.reset_unlimited_mem()?;
        Ok(())
    }

    pub fn reset_unlimited_cpu(&self) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset(u64::MAX);
            Ok(())
        })?; // panic means multiple-mut-borrow bug
        self.reset_tracker()
    }

    pub fn reset_unlimited_mem(&self) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.mem_bytes.reset(u64::MAX);
            Ok(())
        })?;
        self.reset_tracker()
    }

    pub fn reset_tracker(&self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.tracker.reset();
        Ok(())
    }

    pub fn reset_limits(&self, cpu: u64, mem: u64) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset(cpu);
            b.mem_bytes.reset(mem);
            Ok(())
        })?;
        self.reset_tracker()
    }

    /// Resets the `FuelConfig` we pass into Wasmi before running calibration.
    /// Wasmi instruction calibration requires running the same Wasmi insn
    /// a fixed number of times, record their actual cpu and mem consumption, then
    /// divide those numbers by the number of iterations, which is the fuel count.
    /// Fuel count is kept tracked on the Wasmi side, based on the `FuelConfig`
    /// of a specific fuel category. In order to get the correct, unscaled fuel
    /// count, we have to preset all the `FuelConfig` entries to 1.
    pub fn reset_fuel_config(&self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.fuel_config.reset();
        Ok(())
    }
}
