use crate::{
    budget::{AsBudget, Budget},
    host::error::TryBorrowOrErr,
    xdr::ContractCostType,
    Host, HostError,
};
use wasmi::{CompilationMode, OperatorCost, ResourceLimiter};
use wasmi_core::LimiterError;

pub(crate) struct WasmiLimits {
    pub table_elements: u32,
    pub instances: usize,
    // The `tables` limit is only relevant if `wasmi_reference_type` is enabled
    pub tables: usize,
    // The `memories` limit is irrelevant. At the current version of WASM, at
    // most one memory may be defined or imported in a single module
    pub memories: usize,
}

pub(crate) const WASMI_LIMITS_CONFIG: WasmiLimits = WasmiLimits {
    table_elements: 1000,
    instances: 1,
    tables: 1,
    memories: 1,
};

impl ResourceLimiter for Host {
    fn memory_growing(
        &mut self,
        current: usize,
        desired: usize,
        maximum: Option<usize>,
    ) -> Result<bool, LimiterError> {
        let host_limit = self
            .as_budget()
            .get_mem_bytes_remaining()
            .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?;

        // NB: if desired < current this is considered a 0-cost shrink.
        let delta = (desired as u64).saturating_sub(current as u64);
        let allow = if delta > host_limit {
            false
        } else {
            match maximum {
                Some(max) => desired <= max,
                None => true,
            }
        };

        if allow {
            #[cfg(any(test, feature = "testutils", feature = "bench"))]
            {
                self.as_budget()
                    .track_wasm_mem_alloc(delta)
                    .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?;
            }

            self.as_budget()
                .charge(ContractCostType::MemAlloc, Some(delta))
                .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?;

            // Attribute the (real-budget) linear-memory charge to the frame that
            // owns the growing VM, so it can be refunded when that frame pops.
            // During execution the top frame *is* the growing VM's frame; during
            // instantiation the VM has no frame yet, so this bills the caller and
            // `call_contract_fn` reclaims the delta onto the new frame at push.
            // Shadow-mode charges are never refunded, so they aren't attributed.
            if !self
                .as_budget()
                .is_in_shadow_mode()
                .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?
            {
                let charged = self
                    .as_budget()
                    .get_memory_cost(ContractCostType::MemAlloc, Some(delta))
                    .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?;
                self.charge_current_frame_linear_mem(charged)
                    .map_err(|_| LimiterError::ResourceLimiterDeniedAllocation)?;
            }
            Ok(true)
        } else {
            Err(LimiterError::ResourceLimiterDeniedAllocation)
        }
    }

    fn table_growing(
        &mut self,
        _current: usize,
        desired: usize,
        maximum: Option<usize>,
    ) -> Result<bool, LimiterError> {
        let allow = if desired > WASMI_LIMITS_CONFIG.table_elements as usize {
            false
        } else {
            match maximum {
                Some(max) => desired <= max,
                None => true,
            }
        };
        if allow {
            Ok(allow)
        } else {
            Err(LimiterError::ResourceLimiterDeniedAllocation)
        }
    }

    fn instances(&self) -> usize {
        WASMI_LIMITS_CONFIG.instances
    }

    fn tables(&self) -> usize {
        WASMI_LIMITS_CONFIG.tables
    }

    fn memories(&self) -> usize {
        WASMI_LIMITS_CONFIG.memories
    }
}

// These values are calibrated and set by us. Calibration is done with a given
// wasmi version, and as long as the version is pinned, these values aren't
// expected to change much.
//
// wasmi charges fuel per Wasm operator. The categories below mirror the ones
// that the previous (pre-2.0) wasmi fuel model distinguished, with the same
// per-category values: "base" (1) is wasmi's default for everything not listed
// here; wasm entity operations cost 3, loads cost 2, stores cost 1 and calls
// cost 67.
pub(crate) fn load_calibrated_fuel_costs() -> OperatorCost {
    const ENTITY: u8 = 3;
    const LOAD: u8 = 2;
    const STORE: u8 = 1;
    const CALL: u8 = 67;

    let mut c = OperatorCost::default();

    c.global_get = ENTITY;
    c.global_set = ENTITY;
    c.memory_size = ENTITY;
    c.memory_grow = ENTITY;
    c.memory_init = ENTITY;
    c.memory_copy = ENTITY;
    c.memory_fill = ENTITY;
    c.data_drop = ENTITY;
    c.table_init = ENTITY;
    c.table_copy = ENTITY;
    c.table_fill = ENTITY;
    c.table_get = ENTITY;
    c.table_set = ENTITY;
    c.table_grow = ENTITY;
    c.table_size = ENTITY;
    c.elem_drop = ENTITY;

    c.i32_load = LOAD;
    c.i64_load = LOAD;
    c.f32_load = LOAD;
    c.f64_load = LOAD;
    c.i32_load8_s = LOAD;
    c.i32_load8_u = LOAD;
    c.i32_load16_s = LOAD;
    c.i32_load16_u = LOAD;
    c.i64_load8_s = LOAD;
    c.i64_load8_u = LOAD;
    c.i64_load16_s = LOAD;
    c.i64_load16_u = LOAD;
    c.i64_load32_s = LOAD;
    c.i64_load32_u = LOAD;

    c.i32_store = STORE;
    c.i64_store = STORE;
    c.f32_store = STORE;
    c.f64_store = STORE;
    c.i32_store8 = STORE;
    c.i32_store16 = STORE;
    c.i64_store8 = STORE;
    c.i64_store16 = STORE;
    c.i64_store32 = STORE;

    c.call = CALL;
    c.call_indirect = CALL;
    c.return_call = CALL;
    c.return_call_indirect = CALL;

    c
}

pub(crate) fn get_wasmi_config(budget: &Budget) -> Result<wasmi::Config, HostError> {
    let mut config = wasmi::Config::default();
    let fuel_costs = budget.0.try_borrow_or_err()?.fuel_costs;

    // Turn off most optional wasm features, leaving on some post-MVP features
    // commonly enabled by Rust and Clang. Make sure all unused features are
    // explicited turned off, so that we don't get "opted in" by a future wasmi
    // version. (The `memory64`, `simd` and `relaxed-simd` features are not
    // compiled into wasmi at all, see the workspace Cargo.toml.)
    //
    // Translation is eager: wasmi's default is lazy translation, which defers
    // some failures to first call and is not acceptable for a metered,
    // deterministic VM.
    config
        .consume_fuel(true)
        .compilation_mode(CompilationMode::Eager)
        .wasm_bulk_memory(true)
        .wasm_mutable_global(true)
        .wasm_sign_extension(true)
        .wasm_saturating_float_to_int(false)
        .wasm_multi_value(false)
        .wasm_multi_memory(false)
        .wasm_reference_types(false)
        .wasm_tail_call(false)
        .wasm_extended_const(false)
        .wasm_custom_page_sizes(false)
        .wasm_wide_arithmetic(false)
        .floats(false)
        .allow_start_fn(false)
        .operator_cost(fuel_costs);

    // The stack limits are pinned to the values the pre-2.0 wasmi effectively
    // enforced (1024 frames, 1 MiB of value stack), rather than wasmi 2.0's
    // defaults. wasmi 2.0 rejects a call that would reach the configured
    // recursion depth where the earlier wasmi only rejected one that would
    // exceed it, so the configured depth is one higher than the effective
    // limit (see `test_stack_depth_stability`).
    config
        .set_max_recursion_depth(1025)
        .set_max_stack_height(1024 * 1024)
        .set_min_stack_height(1024);

    Ok(config)
}
