use std::{
    cell::{RefCell, RefMut},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    host::error::TryBorrowOrErr,
    xdr::{
        ContractCostParamEntry, ContractCostParams, ContractCostType, DepthLimiter, ScErrorCode,
        ScErrorType,
    },
    Error, Host, HostError, DEFAULT_HOST_DEPTH_LIMIT,
};

use wasmi::{errors, FuelCosts, ResourceLimiter};

// These are some sane values, however the embedder should typically customize
// these to match the network config.
const DEFAULT_CPU_INSN_LIMIT: u64 = 100_000_000;
const DEFAULT_MEM_BYTES_LIMIT: u64 = 40 * 1024 * 1024; // 40MB

/// The number of bits to scale the linear term by. The linear coefficient has
/// been scaled by this factor during parameter fitting to retain more significant
/// digits. Thus to get the cost from the raw input, we need to scale the result
/// back by the same factor.
pub const COST_MODEL_LIN_TERM_SCALE_BITS: u32 = 7;

/// We provide a "cost model" object that evaluates a linear expression:
///
///    f(x) = a + b * Option<x>
///
/// Where a, b are "fixed" parameters at construction time (extracted from an
/// on-chain cost schedule, so technically not _totally_ fixed) and Option<x>
/// is some abstract input variable -- say, event counts or object sizes --
/// provided at runtime. If the input cannot be defined, i.e., the cost is
/// constant, input-independent, then pass in `None` as the input.
///
/// The same `CostModel` type, i.e. `CostType` (applied to different parameters
/// and variables) is used for calculating memory as well as CPU time.
///
/// The various `CostType`s are carefully choosen such that 1. their underlying
/// cost characteristics (both cpu and memory) at runtime can be described
/// sufficiently by a linear model and 2. they together encompass the vast
/// majority of available operations done by the `env` -- the host and the VM.
///
/// The parameters for a `CostModel` are calibrated empirically. See this crate's
/// benchmarks for more details.
pub trait HostCostModel {
    fn evaluate(&self, input: Option<u64>) -> Result<u64, HostError>;

    #[cfg(test)]
    fn reset(&mut self);
}

/// A helper type that wraps an u64 to signify the wrapped value have been scaled.
#[derive(Clone)]
pub(crate) struct ScaledU64(u64);

impl ScaledU64 {
    pub const fn unscale(self) -> u64 {
        self.0 >> COST_MODEL_LIN_TERM_SCALE_BITS
    }

    #[cfg(test)]
    pub const fn from_unscaled_u64(u: u64) -> Self {
        ScaledU64(u << COST_MODEL_LIN_TERM_SCALE_BITS)
    }

    pub const fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub const fn saturating_mul(&self, rhs: u64) -> Self {
        ScaledU64(self.0.saturating_mul(rhs))
    }
}

impl Display for ScaledU64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for ScaledU64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Scaled({})", self.0)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct MeteredCostComponent {
    const_term: u64,
    lin_term: ScaledU64,
}

impl TryFrom<&ContractCostParamEntry> for MeteredCostComponent {
    type Error = HostError;

    fn try_from(entry: &ContractCostParamEntry) -> Result<Self, Self::Error> {
        if entry.const_term < 0 || entry.linear_term < 0 {
            return Err((ScErrorType::Context, ScErrorCode::InvalidInput).into());
        }
        Ok(MeteredCostComponent {
            const_term: entry.const_term as u64,
            lin_term: ScaledU64(entry.linear_term as u64),
        })
    }
}

impl TryFrom<ContractCostParamEntry> for MeteredCostComponent {
    type Error = HostError;

    fn try_from(entry: ContractCostParamEntry) -> Result<Self, Self::Error> {
        Self::try_from(&entry)
    }
}

impl HostCostModel for MeteredCostComponent {
    fn evaluate(&self, input: Option<u64>) -> Result<u64, HostError> {
        let const_term = self.const_term;
        match input {
            Some(input) => {
                let mut res = const_term;
                if !self.lin_term.is_zero() {
                    let lin_cost = self.lin_term.saturating_mul(input).unscale();
                    res = res.saturating_add(lin_cost)
                }
                Ok(res)
            }
            None => Ok(const_term),
        }
    }

    #[cfg(test)]
    fn reset(&mut self) {
        self.const_term = 0;
        self.lin_term = ScaledU64(0);
    }
}

#[derive(Clone)]
struct BudgetDimension {
    /// A set of cost models that map input values (eg. event counts, object
    /// sizes) from some CostType to whatever concrete resource type is being
    /// tracked by this dimension (eg. cpu or memory). CostType enum values are
    /// used as indexes into this vector, to make runtime lookups as cheap as
    /// possible.
    cost_models: Vec<MeteredCostComponent>,

    /// The limit against-which the count is compared to decide if we're
    /// over budget.
    limit: u64,

    /// Tracks the output value from individual cost models
    counts: Vec<u64>,

    /// Tracks the sum of _output_ values from the cost model, for purposes
    /// of comparing to limit.
    total_count: u64,
}

impl Debug for BudgetDimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "limit: {}, total_count: {}",
            self.limit, self.total_count
        )?;

        for ct in ContractCostType::variants() {
            writeln!(f, "CostType {:?}, count {}", ct, self.counts[ct as usize])?;
            writeln!(f, "model: {:?}", self.cost_models[ct as usize])?;
        }
        Ok(())
    }
}

impl BudgetDimension {
    fn new() -> Self {
        let mut bd = Self {
            cost_models: Default::default(),
            limit: Default::default(),
            counts: Default::default(),
            total_count: Default::default(),
        };
        for _ct in ContractCostType::variants() {
            bd.cost_models.push(MeteredCostComponent {
                const_term: 0,
                lin_term: ScaledU64(0),
            });
            bd.counts.push(0);
        }
        bd
    }

    fn try_from_config(cost_params: ContractCostParams) -> Result<Self, HostError> {
        let cost_models = cost_params
            .0
            .iter()
            .map(|p| MeteredCostComponent::try_from(p))
            .collect::<Result<Vec<MeteredCostComponent>, HostError>>()?;

        Ok(Self {
            cost_models,
            limit: Default::default(),
            counts: vec![0; cost_params.0.len()],
            total_count: Default::default(),
        })
    }

    pub(crate) fn get_cost_model(&self, ty: ContractCostType) -> &MeteredCostComponent {
        &self.cost_models[ty as usize]
    }

    pub(crate) fn get_cost_model_mut(&mut self, ty: ContractCostType) -> &mut MeteredCostComponent {
        &mut self.cost_models[ty as usize]
    }

    fn get_total_count(&self) -> u64 {
        self.total_count
    }

    fn get_remaining(&self) -> u64 {
        self.limit.saturating_sub(self.total_count)
    }

    fn reset(&mut self, limit: u64) {
        self.limit = limit;
        self.total_count = 0;
        for v in &mut self.counts {
            *v = 0;
        }
    }

    pub fn is_over_budget(&self) -> bool {
        self.total_count > self.limit
    }

    /// Performs a bulk charge to the budget under the specified `CostType`.
    /// If the input is `Some`, then the total input charged is iterations *
    /// input, assuming all batched units have the same input size. If input
    /// is `None`, the input is ignored and the model is treated as a constant
    /// model, and amount charged is iterations * const_term.
    fn charge(
        &mut self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
        _is_cpu: bool,
    ) -> Result<(), HostError> {
        let cm = self.get_cost_model(ty);
        let amount = cm.evaluate(input)?.saturating_mul(iterations);

        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        if _is_cpu {
            let _span = tracy_span!("charge");
            _span.emit_text(ty.name());
            _span.emit_value(amount);
        }

        let cell = self
            .counts
            .get_mut(ty as usize)
            .ok_or_else(|| HostError::from((ScErrorType::Budget, ScErrorCode::InternalError)))?;
        *cell = cell.saturating_add(amount);
        self.total_count = self.total_count.saturating_add(amount);
        if self.is_over_budget() {
            Err((ScErrorType::Budget, ScErrorCode::ExceededLimit).into())
        } else {
            Ok(())
        }
    }

    // Resets all model parameters to zero (so that we can override and test individual ones later).
    #[cfg(test)]
    pub fn reset_models(&mut self) {
        for model in &mut self.cost_models {
            model.reset()
        }
    }
}

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
            entity: 2,
            load: 1,
            store: 1,
            call: 41,
        }
    }
}

impl FuelConfig {
    // These values are the "factory default" and used for calibration.
    #[cfg(any(test, feature = "testutils"))]
    fn reset(&mut self) {
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
    pub tables: usize,
    pub memories: usize,
}

pub(crate) const WASMI_LIMITS_CONFIG: WasmiLimits = WasmiLimits {
    table_elements: 1000,
    instances: 1,
    tables: 1,
    memories: 1,
};

#[derive(Clone, Default)]
struct MeterTracker {
    // Tracks the `(sum_of_iterations, total_input)` for each `CostType`
    cost_tracker: [(u64, Option<u64>); ContractCostType::variants().len()],
    // Total number of times the meter is called
    count: u32,
    #[cfg(test)]
    wasm_memory: u64,
}

impl MeterTracker {
    fn reset(&mut self) {
        self.count = 0;
        for tracker in &mut self.cost_tracker {
            tracker.0 = 0;
            tracker.1 = tracker.1.map(|_| 0);
        }
        #[cfg(test)]
        {
            self.wasm_memory = 0;
        }
    }
}

#[derive(Clone)]
pub(crate) struct BudgetImpl {
    cpu_insns: BudgetDimension,
    mem_bytes: BudgetDimension,
    /// For the purpose o calibration and reporting; not used for budget-limiting per se.
    tracker: MeterTracker,
    enabled: bool,
    fuel_config: FuelConfig,
    depth_limit: u32,
}

impl BudgetImpl {
    /// Initializes the budget from network configuration settings.
    fn try_from_configs(
        cpu_limit: u64,
        mem_limit: u64,
        cpu_cost_params: ContractCostParams,
        mem_cost_params: ContractCostParams,
    ) -> Result<Self, HostError> {
        let mut b = Self {
            cpu_insns: BudgetDimension::try_from_config(cpu_cost_params)?,
            mem_bytes: BudgetDimension::try_from_config(mem_cost_params)?,
            tracker: Default::default(),
            enabled: true,
            fuel_config: Default::default(),
            depth_limit: DEFAULT_HOST_DEPTH_LIMIT,
        };

        b.init_tracker();

        b.cpu_insns.reset(cpu_limit);
        b.mem_bytes.reset(mem_limit);
        Ok(b)
    }

    fn init_tracker(&mut self) {
        for ct in ContractCostType::variants() {
            // Define what inputs actually mean. For any constant-cost types -- whether it is a
            // true constant unit cost type, or empirically assigned (via measurement) constant
            // type -- we leave the input as `None`, otherwise, we initialize the input to 0.
            let mut init_input = |i: usize| {
                self.tracker.cost_tracker[i].1 = Some(0);
            };
            let i = ct as usize;
            match ct {
                ContractCostType::WasmInsnExec => (),
                ContractCostType::MemAlloc => init_input(i), // number of bytes in host memory to allocate
                ContractCostType::MemCpy => init_input(i),   // number of bytes in host to copy
                ContractCostType::MemCmp => init_input(i),   // number of bytes in host to compare
                ContractCostType::DispatchHostFunction => (),
                ContractCostType::VisitObject => (),
                // The inputs for `ValSer` and `ValDeser` are subtly different:
                // `ValSer` works recursively via `WriteXdr`, and each leaf call charges the budget,
                // and the input is the number of bytes of a leaf entity.
                // `ValDeser` charges the budget at the top level. Call to `read_xdr` works through
                // the bytes buffer recursively without worrying about budget charging. So the input
                // is the length of the total buffer.
                // This has implication on how their calibration should be set up.
                ContractCostType::ValSer => init_input(i), // number of bytes in the result buffer
                ContractCostType::ValDeser => init_input(i), // number of bytes in the input buffer
                ContractCostType::ComputeSha256Hash => init_input(i), // number of bytes in the buffer
                ContractCostType::ComputeEd25519PubKey => (),
                ContractCostType::VerifyEd25519Sig => init_input(i), // length of the signed message
                ContractCostType::VmInstantiation => init_input(i),  // length of the wasm bytes,
                ContractCostType::VmCachedInstantiation => init_input(i), // length of the wasm bytes,
                ContractCostType::InvokeVmFunction => (),
                ContractCostType::ComputeKeccak256Hash => init_input(i), // number of bytes in the buffer
                ContractCostType::ComputeEcdsaSecp256k1Sig => (),
                ContractCostType::RecoverEcdsaSecp256k1Key => (),
                ContractCostType::Int256AddSub => (),
                ContractCostType::Int256Mul => (),
                ContractCostType::Int256Div => (),
                ContractCostType::Int256Pow => (),
                ContractCostType::Int256Shift => (),
                ContractCostType::ChaCha20DrawBytes => init_input(i), // number of random bytes to draw
            }
        }
    }

    pub fn charge(
        &mut self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        if !self.enabled {
            return Ok(());
        }

        // update tracker for reporting
        self.tracker.count = self.tracker.count.saturating_add(1);
        let (t_iters, t_inputs) = &mut self
            .tracker
            .cost_tracker
            .get_mut(ty as usize)
            .ok_or_else(|| HostError::from((ScErrorType::Budget, ScErrorCode::InternalError)))?;
        *t_iters = t_iters.saturating_add(iterations);
        match (t_inputs, input) {
            (None, None) => (),
            (Some(t), Some(i)) => *t = t.saturating_add(i.saturating_mul(iterations)),
            // internal logic error, a wrong cost type has been passed in
            _ => return Err((ScErrorType::Budget, ScErrorCode::InternalError).into()),
        };

        // do the actual budget charging
        self.cpu_insns.charge(ty, iterations, input, true)?;
        self.mem_bytes.charge(ty, iterations, input, false)
    }

    fn get_wasmi_fuel_remaining(&self) -> Result<u64, HostError> {
        let cpu_remaining = self.cpu_insns.get_remaining();
        let cpu_per_fuel = self
            .cpu_insns
            .get_cost_model(ContractCostType::WasmInsnExec)
            .const_term
            .max(1);
        // Due to rounding, the amount of cpu converted to fuel will be slightly
        // less than the total cpu available. This is okay because 1. that rounded-off
        // amount should be very small (less than the cpu_per_fuel) 2. it does
        // not cumulate over host function calls (each time the Vm returns back
        // to the host, the host gets back the unspent fuel amount converged
        // back to the cpu). The only way this rounding difference is observable
        // is if the Vm traps due to `OutOfFuel`, this tiny amount would still
        // be withheld from the host. And this may not be the only source of
        // unspendable residual budget (see the other comment in `vm::wrapped_func_call`).
        // So it should be okay.
        Ok(cpu_remaining / cpu_per_fuel)
    }
}

/// Default settings for local/sandbox testing only. The actual operations will use parameters
/// read on-chain from network configuration via [`from_configs`] above.
impl Default for BudgetImpl {
    fn default() -> Self {
        let mut b = Self {
            cpu_insns: BudgetDimension::new(),
            mem_bytes: BudgetDimension::new(),
            tracker: Default::default(),
            enabled: true,
            fuel_config: Default::default(),
            depth_limit: DEFAULT_HOST_DEPTH_LIMIT,
        };

        for ct in ContractCostType::variants() {
            // define the cpu cost model parameters
            let cpu = &mut b.cpu_insns.get_cost_model_mut(ct);
            match ct {
                // This is the host cpu insn cost per wasm "fuel". Every "base" wasm
                // instruction costs 1 fuel (by default), and some particular types of
                // instructions may cost additional amount of fuel based on
                // wasmi's config setting.
                ContractCostType::WasmInsnExec => {
                    cpu.const_term = 6;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::MemAlloc => {
                    cpu.const_term = 1141;
                    cpu.lin_term = ScaledU64(1);
                }
                // We don't use a calibrated number for this because sending a
                // large calibration-buffer to memcpy hits an optimized
                // large-memcpy path in the stdlib, which has both a large
                // overhead and a small per-byte cost. But large buffers aren't
                // really how byte-copies usually get used in metered code. Most
                // calls have to do with small copies of a few tens or hundreds
                // of bytes. So instead we just "reason it out": we can probably
                // copy 8 bytes per instruction on a 64-bit machine, and that
                // therefore a 1-byte copy is considered 1/8th of an
                // instruction. We also add in a nonzero constant overhead, to
                // avoid having anything that can be zero cost and approximate
                // whatever function call, arg-shuffling, spills, reloads or
                // other flotsam accumulates around a typical memory copy.
                ContractCostType::MemCpy => {
                    cpu.const_term = 250;
                    cpu.lin_term = ScaledU64((1 << COST_MODEL_LIN_TERM_SCALE_BITS) / 8);
                }
                ContractCostType::MemCmp => {
                    cpu.const_term = 250;
                    cpu.lin_term = ScaledU64((1 << COST_MODEL_LIN_TERM_SCALE_BITS) / 8);
                }
                ContractCostType::DispatchHostFunction => {
                    cpu.const_term = 263;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::VisitObject => {
                    cpu.const_term = 108;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ValSer => {
                    cpu.const_term = 1000;
                    cpu.lin_term = ScaledU64((1 << COST_MODEL_LIN_TERM_SCALE_BITS) / 8);
                }
                ContractCostType::ValDeser => {
                    cpu.const_term = 1000;
                    cpu.lin_term = ScaledU64((1 << COST_MODEL_LIN_TERM_SCALE_BITS) / 8);
                }
                ContractCostType::ComputeSha256Hash => {
                    cpu.const_term = 2924;
                    cpu.lin_term = ScaledU64(4149);
                }
                ContractCostType::ComputeEd25519PubKey => {
                    cpu.const_term = 25584;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::VerifyEd25519Sig => {
                    cpu.const_term = 376877;
                    cpu.lin_term = ScaledU64(2747);
                }
                ContractCostType::VmInstantiation => {
                    cpu.const_term = 967154;
                    cpu.lin_term = ScaledU64(69991);
                }
                ContractCostType::VmCachedInstantiation => {
                    cpu.const_term = 967154;
                    cpu.lin_term = ScaledU64(69991);
                }
                ContractCostType::InvokeVmFunction => {
                    cpu.const_term = 1125;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeKeccak256Hash => {
                    cpu.const_term = 2890;
                    cpu.lin_term = ScaledU64(3561);
                }
                ContractCostType::ComputeEcdsaSecp256k1Sig => {
                    cpu.const_term = 224;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::RecoverEcdsaSecp256k1Key => {
                    cpu.const_term = 1666155;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256AddSub => {
                    cpu.const_term = 1716;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Mul => {
                    cpu.const_term = 2226;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Div => {
                    cpu.const_term = 2333;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Pow => {
                    cpu.const_term = 5212;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Shift => {
                    cpu.const_term = 412;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ChaCha20DrawBytes => {
                    cpu.const_term = 4907;
                    cpu.lin_term = ScaledU64(2461);
                }
            }

            // define the memory cost model parameters
            let mem = b.mem_bytes.get_cost_model_mut(ct);
            match ct {
                // This type is designated to the cpu cost. By definition, the memory cost
                // of a (cpu) fuel is zero.
                ContractCostType::WasmInsnExec => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::MemAlloc => {
                    mem.const_term = 16;
                    mem.lin_term = ScaledU64(128);
                }
                ContractCostType::MemCpy => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::MemCmp => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::DispatchHostFunction => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::VisitObject => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ValSer => {
                    mem.const_term = 18;
                    mem.lin_term = ScaledU64(384);
                }
                ContractCostType::ValDeser => {
                    mem.const_term = 16;
                    mem.lin_term = ScaledU64(128);
                }
                ContractCostType::ComputeSha256Hash => {
                    mem.const_term = 40;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeEd25519PubKey => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::VerifyEd25519Sig => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::VmInstantiation => {
                    mem.const_term = 131103;
                    mem.lin_term = ScaledU64(5080);
                }
                ContractCostType::VmCachedInstantiation => {
                    mem.const_term = 131103;
                    mem.lin_term = ScaledU64(5080);
                }
                ContractCostType::InvokeVmFunction => {
                    mem.const_term = 14;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeKeccak256Hash => {
                    mem.const_term = 40;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeEcdsaSecp256k1Sig => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::RecoverEcdsaSecp256k1Key => {
                    mem.const_term = 201;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256AddSub => {
                    mem.const_term = 119;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Mul => {
                    mem.const_term = 119;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Div => {
                    mem.const_term = 119;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Pow => {
                    mem.const_term = 119;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Shift => {
                    mem.const_term = 119;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ChaCha20DrawBytes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
            }

            b.init_tracker();
        }

        // define the limits
        b.cpu_insns.reset(DEFAULT_CPU_INSN_LIMIT);
        b.mem_bytes.reset(DEFAULT_MEM_BYTES_LIMIT);
        b
    }
}

impl Debug for BudgetImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:=<165}", "")?;
        writeln!(
            f,
            "Cpu limit: {}; used: {}",
            self.cpu_insns.limit, self.cpu_insns.total_count
        )?;
        writeln!(
            f,
            "Mem limit: {}; used: {}",
            self.mem_bytes.limit, self.mem_bytes.total_count
        )?;
        writeln!(f, "{:=<165}", "")?;
        writeln!(
            f,
            "{:<25}{:<15}{:<15}{:<15}{:<15}{:<20}{:<20}{:<20}{:<20}",
            "CostType",
            "iterations",
            "input",
            "cpu_insns",
            "mem_bytes",
            "const_term_cpu",
            "lin_term_cpu",
            "const_term_mem",
            "lin_term_mem",
        )?;
        for ct in ContractCostType::variants() {
            let i = ct as usize;
            writeln!(
                f,
                "{:<25}{:<15}{:<15}{:<15}{:<15}{:<20}{:<20}{:<20}{:<20}",
                format!("{:?}", ct),
                self.tracker.cost_tracker[i].0,
                format!("{:?}", self.tracker.cost_tracker[i].1),
                self.cpu_insns.counts[i],
                self.mem_bytes.counts[i],
                self.cpu_insns.cost_models[i].const_term,
                format!("{}", self.cpu_insns.cost_models[i].lin_term),
                self.mem_bytes.cost_models[i].const_term,
                format!("{}", self.mem_bytes.cost_models[i].lin_term),
            )?;
        }
        writeln!(f, "{:=<165}", "")?;
        writeln!(f, "Total # times meter was called: {}", self.tracker.count,)?;
        Ok(())
    }
}

impl Display for BudgetImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:=<55}", "")?;
        writeln!(
            f,
            "Cpu limit: {}; used: {}",
            self.cpu_insns.limit, self.cpu_insns.total_count
        )?;
        writeln!(
            f,
            "Mem limit: {}; used: {}",
            self.mem_bytes.limit, self.mem_bytes.total_count
        )?;
        writeln!(f, "{:=<55}", "")?;
        writeln!(
            f,
            "{:<25}{:<15}{:<15}",
            "CostType", "cpu_insns", "mem_bytes",
        )?;
        for ct in ContractCostType::variants() {
            let i = ct as usize;
            writeln!(
                f,
                "{:<25}{:<15}{:<15}",
                format!("{:?}", ct),
                self.cpu_insns.counts[i],
                self.mem_bytes.counts[i],
            )?;
        }
        writeln!(f, "{:=<55}", "")?;
        Ok(())
    }
}

impl DepthLimiter for BudgetImpl {
    type DepthLimiterError = HostError;

    fn enter(&mut self) -> Result<(), HostError> {
        if let Some(depth) = self.depth_limit.checked_sub(1) {
            self.depth_limit = depth;
        } else {
            return Err(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::ExceededLimit,
            )
            .into());
        }
        Ok(())
    }

    // `leave` should be called in tandem with `enter` such that the depth
    // doesn't exceed the initial depth limit.
    fn leave(&mut self) -> Result<(), HostError> {
        self.depth_limit = self.depth_limit.saturating_add(1);
        Ok(())
    }
}

#[derive(Clone)]
pub struct Budget(pub(crate) Rc<RefCell<BudgetImpl>>);

#[allow(clippy::derivable_impls)]
impl Default for Budget {
    fn default() -> Self {
        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        let _client = tracy_client::Client::start();
        Self(Default::default())
    }
}

impl Debug for Budget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.0.try_borrow().map_err(|_| std::fmt::Error)?)
    }
}

impl Display for Budget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0.try_borrow().map_err(|_| std::fmt::Error)?)
    }
}

pub trait AsBudget: Clone {
    fn as_budget(&self) -> &Budget;
}

impl AsBudget for Budget {
    fn as_budget(&self) -> &Budget {
        self
    }
}

impl AsBudget for &Budget {
    fn as_budget(&self) -> &Budget {
        self
    }
}

impl AsBudget for Host {
    fn as_budget(&self) -> &Budget {
        self.budget_ref()
    }
}

impl AsBudget for &Host {
    fn as_budget(&self) -> &Budget {
        self.budget_ref()
    }
}

impl DepthLimiter for Budget {
    type DepthLimiterError = HostError;

    fn enter(&mut self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.enter()
    }

    fn leave(&mut self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.leave()
    }
}

impl Budget {
    /// Initializes the budget from network configuration settings.
    pub fn try_from_configs(
        cpu_limit: u64,
        mem_limit: u64,
        cpu_cost_params: ContractCostParams,
        mem_cost_params: ContractCostParams,
    ) -> Result<Self, HostError> {
        Ok(Self(Rc::new(RefCell::new(BudgetImpl::try_from_configs(
            cpu_limit,
            mem_limit,
            cpu_cost_params,
            mem_cost_params,
        )?))))
    }

    // Helper function to avoid multiple borrow_mut
    fn mut_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(RefMut<BudgetImpl>) -> Result<T, HostError>,
    {
        f(self.0.try_borrow_mut_or_err()?)
    }

    /// Performs a bulk charge to the budget under the specified [`CostType`].
    /// The `iterations` is the batch size. The caller needs to ensure:
    /// 1. the batched charges have identical costs (having the same
    /// [`CostType`] and `input`)
    /// 2. The input passed in (Some/None) is consistent with the [`CostModel`]
    /// underneath the [`CostType`] (linear/constant).
    pub fn bulk_charge(
        &self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        self.0
            .try_borrow_mut_or_err()?
            .charge(ty, iterations, input)
    }

    /// Charges the budget under the specified [`CostType`]. The actual amount
    /// charged is determined by the underlying [`CostModel`] and may depend on
    /// the input. If the input is `None`, the model is assumed to be constant.
    /// Otherwise it is a linear model.  The caller needs to ensure the input
    /// passed is consistent with the inherent model underneath.
    pub fn charge(&self, ty: ContractCostType, input: Option<u64>) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.charge(ty, 1, input)
    }

    pub fn with_free_budget<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce() -> Result<T, HostError>,
    {
        let mut prev = false;
        self.mut_budget(|mut b| {
            prev = b.enabled;
            b.enabled = false;
            Ok(())
        })?;

        let res = f();

        self.mut_budget(|mut b| {
            b.enabled = prev;
            Ok(())
        })?;
        res
    }

    pub fn get_tracker(&self, ty: ContractCostType) -> Result<(u64, Option<u64>), HostError> {
        self.0
            .try_borrow_or_err()?
            .tracker
            .cost_tracker
            .get(ty as usize)
            .map(|x| *x)
            .ok_or_else(|| (ScErrorType::Budget, ScErrorCode::InternalError).into())
    }

    pub fn get_cpu_insns_consumed(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.cpu_insns.get_total_count())
    }

    pub fn get_mem_bytes_consumed(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.mem_bytes.get_total_count())
    }

    pub fn get_cpu_insns_remaining(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.cpu_insns.get_remaining())
    }

    pub fn get_mem_bytes_remaining(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.mem_bytes.get_remaining())
    }

    pub fn reset_default(&self) -> Result<(), HostError> {
        *self.0.try_borrow_mut_or_err()? = BudgetImpl::default();
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

    #[cfg(test)]
    pub fn reset_models(&self) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset_models();
            b.mem_bytes.reset_models();
            Ok(())
        })
    }

    #[cfg(test)]
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

    #[cfg(test)]
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

    #[cfg(test)]
    pub(crate) fn track_wasm_mem_alloc(&self, delta: u64) -> Result<(), HostError> {
        let mut bgt = self.0.try_borrow_mut_or_err()?;
        bgt.tracker.wasm_memory = bgt.tracker.wasm_memory.saturating_add(delta);
        Ok(())
    }

    #[cfg(test)]
    pub fn get_wasm_mem_alloc(&self) -> Result<u64, HostError> {
        Ok(self.0.try_borrow_or_err()?.tracker.wasm_memory)
    }

    /// Resets the `FuelConfig` we pass into Wasmi before running calibration.
    /// Wasmi instruction calibration requires running the same Wasmi insn
    /// a fixed number of times, record their actual cpu and mem consumption, then
    /// divide those numbers by the number of iterations, which is the fuel count.
    /// Fuel count is kept tracked on the Wasmi side, based on the `FuelConfig`
    /// of a specific fuel category. In order to get the correct, unscaled fuel
    /// count, we have to preset all the `FuelConfig` entries to 1.
    #[cfg(any(test, feature = "testutils"))]
    pub fn reset_fuel_config(&self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.fuel_config.reset();
        Ok(())
    }

    pub(crate) fn get_wasmi_fuel_remaining(&self) -> Result<u64, HostError> {
        self.0.try_borrow_mut_or_err()?.get_wasmi_fuel_remaining()
    }

    // generate a wasmi fuel cost schedule based on our calibration
    pub(crate) fn wasmi_fuel_costs(&self) -> Result<FuelCosts, HostError> {
        let config = &self.0.try_borrow_or_err()?.fuel_config;
        let mut costs = FuelCosts::default();
        costs.base = config.base;
        costs.entity = config.entity;
        costs.load = config.load;
        costs.store = config.store;
        costs.call = config.call;
        Ok(costs)
    }
}

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
            #[cfg(test)]
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
