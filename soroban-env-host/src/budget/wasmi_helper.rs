use crate::{
    budget::{AsBudget, Budget},
    host::error::TryBorrowOrErr,
    xdr::ContractCostType,
    Host, HostError,
};
use wasmi::{errors, FuelConsumptionMode, FuelCosts, ResourceLimiter};

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
    ) -> Result<bool, errors::MemoryError> {
        let host_limit = self
            .as_budget()
            .get_mem_bytes_remaining()
            .map_err(|_| errors::MemoryError::OutOfBoundsGrowth)?;

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
                    .map_err(|_| errors::MemoryError::OutOfBoundsGrowth)?;
            }

            self.as_budget()
                .charge(ContractCostType::MemAlloc, Some(delta))
                .map(|_| true)
                .map_err(|_| errors::MemoryError::OutOfBoundsGrowth)
        } else {
            Err(errors::MemoryError::OutOfBoundsGrowth)
        }
    }

    fn table_growing(
        &mut self,
        current: u32,
        desired: u32,
        maximum: Option<u32>,
    ) -> Result<bool, errors::TableError> {
        let allow = if desired > WASMI_LIMITS_CONFIG.table_elements {
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
            Err(errors::TableError::GrowOutOfBounds {
                maximum: maximum.unwrap_or(u32::MAX),
                current,
                delta: desired - current,
            })
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
pub(crate) fn load_calibrated_fuel_costs() -> FuelCosts {
    let mut fuel_costs = FuelCosts::default();
    fuel_costs.base = 1;
    fuel_costs.entity = 3;
    fuel_costs.load = 2;
    fuel_costs.store = 1;
    fuel_costs.call = 67;
    fuel_costs
}

pub(crate) fn get_wasmi_config(budget: &Budget) -> Result<wasmi::Config, HostError> {
    let mut config = wasmi::Config::default();
    let fuel_costs = budget.0.try_borrow_or_err()?.fuel_costs;

    // Turn off most optional wasm features, leaving on some post-MVP features
    // commonly enabled by Rust and Clang. Make sure all unused features are
    // explicited turned off, so that we don't get "opted in" by a future wasmi
    // version.
    config
        .consume_fuel(true)
        .wasm_bulk_memory(true)
        .wasm_mutable_global(true)
        .wasm_sign_extension(true)
        .wasm_saturating_float_to_int(false)
        .wasm_multi_value(false)
        .wasm_reference_types(false)
        .wasm_tail_call(false)
        .wasm_extended_const(false)
        .floats(false)
        .fuel_consumption_mode(FuelConsumptionMode::Eager)
        .set_fuel_costs(fuel_costs);

    Ok(config)
}
