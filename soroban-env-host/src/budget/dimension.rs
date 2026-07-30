use super::model::{HostCostModel, MeteredCostComponent};
use crate::xdr::{ContractCostParams, ContractCostType, ScErrorCode, ScErrorType};
use crate::{Error, HostError};
use core::fmt::Debug;

/// Helper types to annotate boolean function arguments
#[allow(dead_code)]
pub(crate) struct IsCpu(pub(crate) bool);
pub(crate) struct IsShadowMode(pub(crate) bool);

#[derive(Clone)]
pub(crate) struct BudgetDimension {
    /// A set of cost models that map input values (eg. event counts, object
    /// sizes) from some CostType to whatever concrete resource type is being
    /// tracked by this dimension (eg. cpu or memory). CostType enum values are
    /// used as indexes into this vector, to make runtime lookups as cheap as
    /// possible.
    pub(crate) cost_models: [MeteredCostComponent; ContractCostType::variants().len()],

    /// The limit against-which the count is compared to decide if we're
    /// over budget.
    pub(crate) limit: u64,

    /// Running (net) sum of _output_ values from the cost model, compared
    /// against `limit`. For the memory dimension it is lowered by `refund` when
    /// transient allocations (e.g. a torn-down VM's linear memory) are freed, so
    /// it reflects currently-live usage; the cpu dimension is never refunded.
    pub(crate) net_count: u64,

    /// High-water mark of `net_count`: advanced by `charge`, never lowered by
    /// `refund`. It records the peak the running count ever reached, and is the
    /// provisioning-relevant quantity reported to callers (see
    /// `Budget::get_mem_bytes_consumed`). Equals `net_count` for cpu.
    pub(crate) peak_count: u64,

    /// The shadow limit tracks work done internally that is not exposed to the
    /// external user -- it does not affect fees or decide the invocation outcome
    /// in any way (no error due to exceeding the shadow limit). It exists solely
    /// for dos prevention. Such work include diagnostic logging, or work that
    /// exists only for preflight.
    pub(crate) shadow_limit: u64,

    /// Similar to `net_count`, but towards the `shadow_limit`
    pub(crate) shadow_net_count: u64,
}

impl Default for BudgetDimension {
    fn default() -> Self {
        Self {
            cost_models: [MeteredCostComponent::default(); ContractCostType::variants().len()],
            limit: 0,
            net_count: 0,
            peak_count: 0,
            shadow_limit: 0,
            shadow_net_count: 0,
        }
    }
}

impl Debug for BudgetDimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "limit: {}, net_count: {}, peak_count: {}",
            self.limit, self.net_count, self.peak_count
        )?;

        for ct in ContractCostType::variants() {
            writeln!(f, "CostType {:?}", ct)?;
            writeln!(f, "model: {:?}", self.cost_models[ct as usize])?;
        }

        writeln!(
            f,
            "shadow limit: {}, shadow_net_count: {}",
            self.shadow_limit, self.shadow_net_count
        )?;
        Ok(())
    }
}

impl BudgetDimension {
    pub(crate) fn try_from_config(
        cost_params: ContractCostParams,
        limit: u64,
    ) -> Result<Self, HostError> {
        let mut bd = BudgetDimension::default();
        for (i, cp) in cost_params.0.iter().enumerate() {
            let cm = bd.cost_models.get_mut(i).ok_or_else(|| {
                // the index of ContractCostParams exceeds length of the cost
                // models means attempting to construct the budget from a config
                // that is not yet supported by the protocol
                HostError::from(Error::from_type_and_code(
                    ScErrorType::Budget,
                    ScErrorCode::InternalError,
                ))
            })?;
            *cm = MeteredCostComponent::try_from(cp)?;
        }
        bd.limit = limit;
        bd.shadow_limit = limit;
        Ok(bd)
    }

    pub(crate) fn get_cost_model(
        &self,
        ty: ContractCostType,
    ) -> Result<&MeteredCostComponent, HostError> {
        self.cost_models.get(ty as usize).ok_or_else(|| {
            // cost models are initialized with the static size of the
            // ContractCostType, so this call should always succeed
            HostError::from(Error::from_type_and_code(
                ScErrorType::Budget,
                ScErrorCode::InternalError,
            ))
        })
    }

    pub(crate) fn get_cost_model_mut(
        &mut self,
        ty: ContractCostType,
    ) -> Result<&mut MeteredCostComponent, HostError> {
        self.cost_models.get_mut(ty as usize).ok_or_else(|| {
            // cost models are initialized with the static size of the
            // ContractCostType, so this call should always succeed
            HostError::from(Error::from_type_and_code(
                ScErrorType::Budget,
                ScErrorCode::InternalError,
            ))
        })
    }

    pub(crate) fn get_net_count(&self) -> u64 {
        self.net_count
    }

    pub(crate) fn get_peak_count(&self) -> u64 {
        self.peak_count
    }

    pub(crate) fn get_remaining(&self) -> u64 {
        self.limit.saturating_sub(self.net_count)
    }

    pub(crate) fn reset(&mut self, limit: u64) {
        self.limit = limit;
        self.shadow_limit = limit;
        self.reset_count();
    }

    pub(crate) fn reset_count(&mut self) {
        self.net_count = 0;
        self.peak_count = 0;
        self.shadow_net_count = 0;
    }

    /// Credits `amount` back to the running (net) count, used to return a
    /// transient allocation (e.g. a torn-down VM's linear memory) that has been
    /// freed. Only the net is lowered; the peak high-water mark is retained.
    pub(crate) fn refund(&mut self, amount: u64) {
        self.net_count = self.net_count.saturating_sub(amount);
    }

    pub(crate) fn check_budget_limit(&self, is_shadow: IsShadowMode) -> Result<(), HostError> {
        let over_limit = if is_shadow.0 {
            self.shadow_net_count > self.shadow_limit
        } else {
            self.net_count > self.limit
        };

        if over_limit {
            Err((ScErrorType::Budget, ScErrorCode::ExceededLimit).into())
        } else {
            Ok(())
        }
    }

    /// Performs a bulk charge to the budget under the specified `CostType`.
    /// If the input is `Some`, then the total input charged is iterations *
    /// input, assuming all batched units have the same input size. If input
    /// is `None`, the input is ignored and the model is treated as a constant
    /// model, and amount charged is iterations * const_term.
    /// Returns the amount charged.
    pub(crate) fn charge(
        &mut self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
        _is_cpu: IsCpu,
        is_shadow: IsShadowMode,
    ) -> Result<u64, HostError> {
        let cm = self.get_cost_model(ty)?;
        let amount = cm.evaluate(iterations, input)?;

        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        if _is_cpu.0 {
            let _span = tracy_span!("charge");
            _span.emit_text(ty.name());
            _span.emit_value(amount);
        }

        if is_shadow.0 {
            self.shadow_net_count = self.shadow_net_count.saturating_add(amount);
        } else {
            self.net_count = self.net_count.saturating_add(amount);
            self.peak_count = self.peak_count.max(self.net_count);
        }

        Ok(amount)
    }

    pub(crate) fn get_cost(
        &self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<u64, HostError> {
        self.get_cost_model(ty)?.evaluate(iterations, input)
    }

    // Resets all model parameters to zero (so that we can override and test individual ones later).
    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    pub(crate) fn reset_models(&mut self) {
        for model in &mut self.cost_models {
            model.reset()
        }
    }
}
