#[cfg(any(
    test,
    feature = "testutils",
    feature = "bench",
    feature = "recording_mode"
))]
use crate::{budget::Budget, HostError};

#[cfg(any(test, feature = "testutils", feature = "bench"))]
use crate::host::error::TryBorrowOrErr;

#[cfg(any(test, feature = "testutils"))]
use crate::{budget::model::ScaledU64, xdr::ContractCostType};

#[cfg(any(test, feature = "testutils", feature = "bench"))]
impl Budget {
    pub fn reset_models(&self) -> Result<(), HostError> {
        self.with_mut_budget(|mut b| {
            b.cpu_insns.reset_models();
            b.mem_bytes.reset_models();
            Ok(())
        })
    }

    pub(crate) fn track_wasm_mem_alloc(&self, delta: u64) -> Result<(), HostError> {
        let mut bgt = self.0.try_borrow_mut_or_err()?;
        bgt.tracker.wasm_memory = bgt.tracker.wasm_memory.saturating_add(delta);
        Ok(())
    }

    pub fn get_wasm_mem_alloc(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.tracker.wasm_memory)
    }

    pub fn reset_unlimited(&self) -> Result<(), HostError> {
        self.reset_unlimited_cpu()?;
        self.reset_unlimited_mem()?;
        Ok(())
    }

    pub fn reset_unlimited_cpu(&self) -> Result<(), HostError> {
        self.with_mut_budget(|mut b| {
            b.cpu_insns.reset(u64::MAX);
            Ok(())
        })?; // panic means multiple-mut-borrow bug
        self.reset_tracker()
    }

    pub fn reset_unlimited_mem(&self) -> Result<(), HostError> {
        self.with_mut_budget(|mut b| {
            b.mem_bytes.reset(u64::MAX);
            Ok(())
        })?;
        self.reset_tracker()
    }

    pub fn cpu_limit_exceeded(&self) -> Result<bool, HostError> {
        let cpu = &self.0.try_borrow_or_err()?.cpu_insns;
        Ok(cpu.total_count > cpu.limit)
    }

    pub fn mem_limit_exceeded(&self) -> Result<bool, HostError> {
        let mem = &self.0.try_borrow_or_err()?.mem_bytes;
        Ok(mem.total_count > mem.limit)
    }

    pub fn reset_tracker(&self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.tracker.reset();
        Ok(())
    }

    pub fn reset_cpu_limit(&self, cpu: u64) -> Result<(), HostError> {
        self.with_mut_budget(|mut b| {
            b.cpu_insns.reset(cpu);
            Ok(())
        })?;
        self.reset_tracker()
    }

    pub fn reset_limits(&self, cpu: u64, mem: u64) -> Result<(), HostError> {
        self.with_mut_budget(|mut b| {
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
        self.0.try_borrow_mut_or_err()?.fuel_costs = wasmi::FuelCosts::default();
        Ok(())
    }

    pub fn get_shadow_cpu_insns_consumed(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.cpu_insns.shadow_total_count)
    }

    pub fn get_shadow_mem_bytes_consumed(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.mem_bytes.shadow_total_count)
    }

    #[allow(unused)]
    pub fn shadow_cpu_limit_exceeded(&self) -> Result<bool, HostError> {
        let cpu = &self.0.try_borrow_or_err()?.cpu_insns;
        Ok(cpu.shadow_total_count > cpu.shadow_limit)
    }

    pub fn shadow_mem_limit_exceeded(&self) -> Result<bool, HostError> {
        let mem = &self.0.try_borrow_or_err()?.mem_bytes;
        Ok(mem.shadow_total_count > mem.shadow_limit)
    }
}

#[cfg(any(test, feature = "testutils"))]
impl Budget {
    pub(crate) fn override_model_with_scaled_params(
        &self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: ScaledU64,
        const_mem: u64,
        lin_mem: ScaledU64,
    ) -> Result<(), HostError> {
        use crate::xdr::{ScErrorCode, ScErrorType};

        let mut bgt = self.0.try_borrow_mut_or_err()?;

        let Some(cpu_model) = bgt.cpu_insns.get_cost_model_mut(ty) else {
            return Err((ScErrorType::Budget, ScErrorCode::InternalError).into());
        };
        cpu_model.const_term = const_cpu;
        cpu_model.lin_term = lin_cpu;

        let Some(mem_model) = bgt.mem_bytes.get_cost_model_mut(ty) else {
            return Err((ScErrorType::Budget, ScErrorCode::InternalError).into());
        };
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
}

#[cfg(any(test, feature = "recording_mode"))]
impl Budget {
    /// Variant of `with_shadow_mode`, enabled only in testing and
    /// non-production scenarios, that produces a `Result<>` rather than eating
    /// errors and return values the way `with_shadow_mode` does.
    ///
    /// This is undesirable specifically because it has a return value which may
    /// vary between `Ok` and `Err` depending on whether the shadow budget is
    /// exhausted, and the shadow budget varies based on debug status --
    /// something that is not identical from one host to the next.
    ///
    /// However, in testing and non-production workflows, sometimes we need the
    /// convenience of temporarily "turning off" the budget. This can happen for
    /// several reasons: we want the some test logic to not affect the
    /// production budget, or we want to maintain an accurate prediction of
    /// production budget during preflight. In the latter case, we want to
    /// exclude preflight-only logic from the budget. By routing metering to the
    /// shadow budget instead of turning the budget off completely, it offers
    /// some DOS-mitigation.
    ///
    /// If in doubt, do not use this function.
    pub(crate) fn with_observable_shadow_mode<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce() -> Result<T, HostError>,
    {
        let mut prev = false;
        let should_execute = self.with_mut_budget(|mut b| {
            prev = b.is_in_shadow_mode;
            b.is_in_shadow_mode = true;
            b.cpu_insns
                .check_budget_limit(super::dimension::IsShadowMode(true))?;
            b.mem_bytes
                .check_budget_limit(super::dimension::IsShadowMode(true))
        });

        let rt = match should_execute {
            Ok(_) => f(),
            Err(e) => Err(e),
        };

        self.with_mut_budget(|mut b| {
            b.is_in_shadow_mode = prev;
            Ok(())
        })?;

        rt
    }
}
