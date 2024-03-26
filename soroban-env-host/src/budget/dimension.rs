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

    /// Tracks the sum of _output_ values from the cost model, for purposes
    /// of comparing to limit.
    pub(crate) total_count: u64,

    /// The shadow limit tracks work done internally that is not exposed to the
    /// external user -- it does not affect fees or decide the invocation outcome
    /// in any way (no error due to exceeding the shadow limit). It exists solely
    /// for dos prevention. Such work include diagnostic logging, or work that
    /// exists only for preflight.
    pub(crate) shadow_limit: u64,

    /// Similar to `total_count`, but towards the `shadow_limit`
    pub(crate) shadow_total_count: u64,
}

impl Default for BudgetDimension {
    fn default() -> Self {
        Self {
            cost_models: [MeteredCostComponent::default(); ContractCostType::variants().len()],
            limit: Default::default(),
            total_count: Default::default(),
            shadow_limit: Default::default(),
            shadow_total_count: Default::default(),
        }
    }
}

impl Debug for BudgetDimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "limit: {}, total_count: {}",
            self.limit, self.total_count
        )?;

        for ct in ContractCostType::variants() {
            writeln!(f, "CostType {:?}", ct)?;
            writeln!(f, "model: {:?}", self.cost_models[ct as usize])?;
        }

        writeln!(
            f,
            "shadow limit: {}, shadow_total_count: {}",
            self.shadow_limit, self.shadow_total_count
        )?;
        Ok(())
    }
}

impl BudgetDimension {
    pub(crate) fn try_from_config(cost_params: ContractCostParams) -> Result<Self, HostError> {
        let cost_models = cost_params
            .0
            .iter()
            .map(|p| MeteredCostComponent::try_from(p))
            .collect::<Result<Vec<MeteredCostComponent>, HostError>>()?;

        Ok(Self {
            cost_models: cost_models.try_into().map_err(|_| {
                // error means the cost_params has the wrong dimension
                HostError::from(Error::from_type_and_code(
                    ScErrorType::Budget,
                    ScErrorCode::InternalError,
                ))
            })?,
            limit: Default::default(),
            total_count: Default::default(),
            shadow_limit: Default::default(),
            shadow_total_count: Default::default(),
        })
    }

    pub(crate) fn get_cost_model(&self, ty: ContractCostType) -> Option<&MeteredCostComponent> {
        self.cost_models.get(ty as usize)
    }

    pub(crate) fn get_cost_model_mut(
        &mut self,
        ty: ContractCostType,
    ) -> Option<&mut MeteredCostComponent> {
        self.cost_models.get_mut(ty as usize)
    }

    pub(crate) fn get_total_count(&self) -> u64 {
        self.total_count
    }

    pub(crate) fn get_remaining(&self) -> u64 {
        self.limit.saturating_sub(self.total_count)
    }

    pub(crate) fn reset(&mut self, limit: u64) {
        self.limit = limit;
        self.total_count = 0;
        self.shadow_limit = limit;
        self.shadow_total_count = 0;
    }

    pub(crate) fn check_budget_limit(&self, is_shadow: IsShadowMode) -> Result<(), HostError> {
        let over_limit = if is_shadow.0 {
            self.shadow_total_count > self.shadow_limit
        } else {
            self.total_count > self.limit
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
        let Some(cm) = self.get_cost_model(ty) else {
            return Err((ScErrorType::Budget, ScErrorCode::InternalError).into());
        };
        let amount = cm.evaluate(input)?.saturating_mul(iterations);

        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        if _is_cpu.0 {
            let _span = tracy_span!("charge");
            _span.emit_text(ty.name());
            _span.emit_value(amount);
        }

        if is_shadow.0 {
            self.shadow_total_count = self.shadow_total_count.saturating_add(amount);
        } else {
            self.total_count = self.total_count.saturating_add(amount);
        }

        Ok(amount)
    }

    // Resets all model parameters to zero (so that we can override and test individual ones later).
    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    pub(crate) fn reset_models(&mut self) {
        for model in &mut self.cost_models {
            model.reset()
        }
    }
}
