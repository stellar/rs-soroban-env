use crate::HostError;

// TODO: move this to an XDR enum
#[repr(i32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CostType {
    HostVecAllocVec = 0,
    HostVecAllocCell = 1,
    HostMapAllocMap = 2,
    HostMapAllocCell = 3,
    WasmInsnExec = 4,
    WasmMemAlloc = 5,
    HostEventDebug = 6,
}

// TODO: add XDR support for iterating over all the elements of an enum
impl CostType {
    pub fn variants() -> std::slice::Iter<'static, CostType> {
        static VARIANTS: &'static [CostType] = &[
            CostType::HostMapAllocMap,
            CostType::HostMapAllocCell,
            CostType::HostVecAllocVec,
            CostType::HostVecAllocCell,
            CostType::WasmInsnExec,
            CostType::WasmMemAlloc,
            CostType::HostEventDebug,
        ];
        VARIANTS.iter()
    }
}

/// We provide a general "cost model" object that evaluates a general
/// expression:
///
///    f(x) = a + b*log_i(x) + c*x + d*(x^2)
///
/// Where a, b, c, d, and i are all "fixed" parameters at construction time
/// (extracted from an on-chain cost schedule, so technically not _totally_
/// fixed) and x is some abstract input variable -- say, event counts or object
/// sizes -- provided at runtime.
///
/// The log base i is also rounded-up to its nearest power of 2, since we
/// implement that term using bit count instructions.
///
/// The same CostModel type (applied to different parameters and variables) is
/// used for calculating memory as well as CPU time. We _hope_ this expression
/// is capable of modeling any of the costs we want to measure at runtime.
///
/// The parameters for a CostModel are calibrated empirically. See this crate's
/// benchmarks for more details.

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CostModel {
    pub const_param: u64,
    pub log_param: u64,
    pub log_base_param: u64,
    pub lin_param: u64,
    pub quad_param: u64,
}

impl CostModel {
    pub fn evaluate(&self, input: u64) -> u64 {
        let mut res = self.const_param;
        if self.log_param != 0 {
            res = res.saturating_add(
                self.log_param
                    .saturating_mul(ceil_log_base(input, self.log_base_param)),
            );
        }
        if self.lin_param != 0 {
            res = res.saturating_add(self.lin_param.saturating_mul(input));
        }
        if self.quad_param != 0 {
            res = res.saturating_add(self.quad_param.saturating_mul(input.saturating_mul(input)));
        }
        res
    }
}

// Returns the next power of 2 >= the input, avoiding overflow and returning 2
// for any input <= 2 (as we want to use this for logarithm input bases below).
fn next_2(x: u64) -> u64 {
    match x.checked_next_power_of_two() {
        None => 0x1000_0000_0000_0000,
        Some(0) => 2,
        Some(1) => 2,
        Some(n) => n,
    }
}

// Returns the ceil(log_base(next_2(x_in), next_2(base_in))); in other words
// rounds-up the input numbers to their nearest powers-of-two x and base
// respectively and then returns the least k such that base^k >= x.
//
// This is used below when calculating the cost to charge for a logarithmic
// term in the cost model.
fn ceil_log_base(x_in: u64, base_in: u64) -> u64 {
    let x = next_2(x_in);
    let base = next_2(base_in);
    let x_bits = x.trailing_zeros();
    let base_bits = base.trailing_zeros();
    let mut res = x_bits / base_bits;
    if x_bits % base_bits != 0 {
        res += 1;
    }
    assert!(base.pow(res) >= x);
    assert!(base.pow(res) >= x_in);
    res as u64
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BudgetDimension {
    /// A set of cost models that map input values (eg. event counts, object
    /// sizes) from some CostType to whatever concrete resource type is being
    /// tracked by this dimension (eg. cpu or memory). CostType enum values are
    /// used as indexes into this vector, to make runtime lookups as cheap as
    /// possible.
    cost_models: Vec<CostModel>,

    /// The limit against-which the count is compared to decide if we're
    /// over budget.
    limit: u64,

    /// Tracks the sum of _output_ values from the cost model, for purposes
    /// of comparing to limit.
    count: u64,
}

impl BudgetDimension {
    pub fn get_cost_model(&self, ty: CostType) -> &CostModel {
        &self.cost_models[ty as usize]
    }

    pub fn get_cost_model_mut(&mut self, ty: CostType) -> &mut CostModel {
        &mut self.cost_models[ty as usize]
    }

    pub fn get_count(&self) -> u64 {
        self.count
    }

    pub fn get_limit(&self) -> u64 {
        self.limit
    }

    pub fn reset(&mut self, limit: u64) {
        self.limit = limit;
        self.count = 0;
    }

    pub fn is_over_budget(&self) -> bool {
        self.count > self.limit
    }

    pub fn charge(&mut self, ty: CostType, input: u64) -> Result<(), HostError> {
        if self.limit == u64::MAX {
            return Ok(());
        }
        let cm = self.get_cost_model(ty);
        self.count = self.count.saturating_add(cm.evaluate(input));
        if self.is_over_budget() {
            // TODO: convert this to a proper error code type.
            Err(HostError::General("budget limit exceeded"))
        } else {
            Ok(())
        }
    }
}

impl Default for BudgetDimension {
    fn default() -> Self {
        let mut bd = Self {
            cost_models: Default::default(),
            limit: Default::default(),
            count: Default::default(),
        };
        for _ct in CostType::variants() {
            // TODO: load cost model for i from the chain.
            bd.cost_models.push(CostModel::default());
        }
        bd
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Budget {
    pub cpu_insns: BudgetDimension,
    pub mem_bytes: BudgetDimension,
    /// Tracks the sums of _input_ values to the cost models, for purposes of
    /// calibration and reporting; not used for budget-limiting per se.
    inputs: Vec<u64>,
}

impl Budget {
    pub fn charge(&mut self, ty: CostType, input: u64) -> Result<(), HostError> {
        let i = self.get_input_mut(ty);
        *i = i.saturating_add(input);

        self.cpu_insns.charge(ty, input)?;
        self.mem_bytes.charge(ty, input)?;
        Ok(())
    }

    pub fn get_input(&self, ty: CostType) -> u64 {
        self.inputs[ty as usize]
    }

    pub fn get_input_mut(&mut self, ty: CostType) -> &mut u64 {
        &mut self.inputs[ty as usize]
    }

    pub fn reset_unlimited(&mut self) {
        self.cpu_insns.reset(u64::MAX);
        self.mem_bytes.reset(u64::MAX);
        self.reset_inputs()
    }

    pub fn reset_inputs(&mut self) {
        for i in self.inputs.iter_mut() {
            *i = 0;
        }
    }
}

impl Default for Budget {
    fn default() -> Self {
        let mut b = Self {
            cpu_insns: Default::default(),
            mem_bytes: Default::default(),
            inputs: Default::default(),
        };

        for _ct in CostType::variants() {
            b.inputs.push(0);
        }

        // For the time being we don't have "on chain" cost models
        // so we just set some up here that we calibrated manually
        // in the adjacent benchmarks.

        // WASM instructions cost linear CPU instructions: 73 each.
        b.cpu_insns
            .get_cost_model_mut(CostType::WasmInsnExec)
            .lin_param = 73;

        // Some "reasonable defaults": 640k of RAM and 100usec.
        //
        // We don't run for a time unit thought, we run for an estimated
        // (calibrated) number of CPU instructions.
        //
        // Assuming 2ghz chips at 2 instructions per cycle, we can guess about
        // 4bn instructions / sec. So about 4000 instructions per usec, or 400k
        // instructions in a 100usec time budget, or about 5479 wasm instructions
        // using the calibration above. Very roughly!
        b.mem_bytes.limit = 0xa_0000;
        b.cpu_insns.limit = 400_000;
        b
    }
}
