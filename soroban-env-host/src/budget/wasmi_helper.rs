use super::AsBudget;
use crate::{xdr::ContractCostType, Host};
use wasmi::{errors, ResourceLimiter};

/// This is a subset of `wasmi::FuelCosts` which are configurable, because it
/// doesn't derive all the traits we want. These fields (coarsely) define the
/// relative costs of different wasm instruction types and are for wasmi internal
/// fuel metering use only. Units are in "fuels".
#[derive(Clone)]
pub(crate) struct FuelConfig {
    /// The base fuel costs for all instructions.
    pub base: u64,
    /// The fuel cost for instruction operating on Wasm entities.
    ///
    /// # Note
    ///
    /// A Wasm entitiy is one of `func`, `global`, `memory` or `table`.
    /// Those instructions are usually a bit more costly since they need
    /// multiplie indirect accesses through the Wasm instance and store.
    pub entity: u64,
    /// The fuel cost offset for `memory.load` instructions.
    pub load: u64,
    /// The fuel cost offset for `memory.store` instructions.
    pub store: u64,
    /// The fuel cost offset for `call` and `call_indirect` instructions.
    pub call: u64,
}

// These values are calibrated and set by us.
impl Default for FuelConfig {
    fn default() -> Self {
        FuelConfig {
            base: 1,
            entity: 3,
            load: 2,
            store: 1,
            call: 67,
        }
    }
}

impl FuelConfig {
    // These values are the "factory default" and used for calibration.
    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    pub(crate) fn reset(&mut self) {
        self.base = 1;
        self.entity = 1;
        self.load = 1;
        self.store = 1;
        self.call = 1;
    }
}

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
