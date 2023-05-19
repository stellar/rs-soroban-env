use std::{
    cell::{RefCell, RefMut},
    fmt::{Debug, Display},
    rc::Rc,
};

use soroban_env_common::xdr::{ScErrorCode, ScErrorType};

use crate::{
    xdr::{ContractCostParamEntry, ContractCostParams, ContractCostType, ExtensionPoint},
    Host, HostError,
};

use wasmi::FuelCosts;

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

impl HostCostModel for ContractCostParamEntry {
    fn evaluate(&self, input: Option<u64>) -> Result<u64, HostError> {
        if self.const_term < 0 || self.linear_term < 0 {
            return Err((ScErrorType::Context, ScErrorCode::InvalidInput).into());
        }

        let const_term = self.const_term as u64;
        let lin_term = self.linear_term as u64;
        match input {
            Some(input) => {
                let mut res = const_term;
                if self.linear_term != 0 {
                    res = res.saturating_add(lin_term.saturating_mul(input));
                }
                Ok(res)
            }
            None => Ok(const_term),
        }
    }

    #[cfg(test)]
    fn reset(&mut self) {
        self.const_term = 0;
        self.linear_term = 0;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BudgetDimension {
    /// A set of cost models that map input values (eg. event counts, object
    /// sizes) from some CostType to whatever concrete resource type is being
    /// tracked by this dimension (eg. cpu or memory). CostType enum values are
    /// used as indexes into this vector, to make runtime lookups as cheap as
    /// possible.
    cost_models: Vec<ContractCostParamEntry>,

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
    pub fn new() -> Self {
        let mut bd = Self {
            cost_models: Default::default(),
            limit: Default::default(),
            counts: Default::default(),
            total_count: Default::default(),
        };
        for _ct in ContractCostType::variants() {
            bd.cost_models.push(ContractCostParamEntry {
                const_term: 0,
                linear_term: 0,
                ext: ExtensionPoint::V0,
            });
            bd.counts.push(0);
        }
        bd
    }

    pub fn from_config(cost_params: ContractCostParams) -> Self {
        Self {
            cost_models: cost_params.0.to_vec(),
            limit: Default::default(),
            counts: vec![0; cost_params.0.len()],
            total_count: Default::default(),
        }
    }

    pub fn get_cost_model(&self, ty: ContractCostType) -> &ContractCostParamEntry {
        &self.cost_models[ty as usize]
    }

    pub fn get_cost_model_mut(&mut self, ty: ContractCostType) -> &mut ContractCostParamEntry {
        &mut self.cost_models[ty as usize]
    }

    pub fn get_count(&self, ty: ContractCostType) -> u64 {
        self.counts[ty as usize]
    }

    pub fn get_total_count(&self) -> u64 {
        self.total_count
    }

    pub fn get_limit(&self) -> u64 {
        self.limit
    }

    pub fn get_remaining(&self) -> u64 {
        self.limit.saturating_sub(self.total_count)
    }

    pub fn reset(&mut self, limit: u64) {
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
    /// model, and amount charged is iterations * const_param.
    pub fn charge(
        &mut self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        let cm = self.get_cost_model(ty);
        let amount = cm.evaluate(input)?.saturating_mul(iterations);
        self.counts[ty as usize] = self.counts[ty as usize].saturating_add(amount);
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BudgetImpl {
    pub cpu_insns: BudgetDimension,
    pub mem_bytes: BudgetDimension,
    /// Tracks the `(sum_of_iterations, total_input)` for each `CostType`, for purposes of
    /// calibration and reporting; not used for budget-limiting per se.
    tracker: Vec<(u64, Option<u64>)>,
    enabled: bool,
}

impl BudgetImpl {
    /// Initializes the budget from network configuration settings.
    fn from_configs(
        cpu_limit: u64,
        mem_limit: u64,
        cpu_cost_params: ContractCostParams,
        mem_cost_params: ContractCostParams,
    ) -> Self {
        let mut b = Self {
            cpu_insns: BudgetDimension::from_config(cpu_cost_params),
            mem_bytes: BudgetDimension::from_config(mem_cost_params),
            tracker: vec![(0, None); ContractCostType::variants().len()],
            enabled: true,
        };

        b.init_tracker();

        b.cpu_insns.reset(cpu_limit);
        b.mem_bytes.reset(mem_limit);
        b
    }

    fn init_tracker(&mut self) {
        for ct in ContractCostType::variants() {
            // Define what inputs actually mean. For any constant-cost types -- whether it is a
            // true constant unit cost type, or empirically assigned (via measurement) constant
            // type -- we leave the input as `None`, otherwise, we initialize the input to 0.
            let i = ct as usize;
            match ct {
                ContractCostType::WasmInsnExec => self.tracker[i].1 = Some(0), // number of "fuel" units wasmi consumes
                ContractCostType::WasmMemAlloc => self.tracker[i].1 = Some(0), // number of "memory fuel" units wasmi consumes
                ContractCostType::HostMemAlloc => self.tracker[i].1 = Some(0), // number of bytes in host memory to allocate
                ContractCostType::HostMemCpy => self.tracker[i].1 = Some(0), // number of bytes in host to copy
                ContractCostType::HostMemCmp => self.tracker[i].1 = Some(0), // number of bytes in host to compare
                ContractCostType::InvokeHostFunction => (),
                ContractCostType::VisitObject => (),
                ContractCostType::ValXdrConv => (),
                ContractCostType::ValSer => self.tracker[i].1 = Some(0), // number of bytes in the result buffer
                ContractCostType::ValDeser => self.tracker[i].1 = Some(0), // number of bytes in the buffer
                ContractCostType::ComputeSha256Hash => self.tracker[i].1 = Some(0), // number of bytes in the buffer
                ContractCostType::ComputeEd25519PubKey => (),
                ContractCostType::MapEntry => (),
                ContractCostType::VecEntry => (),
                ContractCostType::GuardFrame => (),
                ContractCostType::VerifyEd25519Sig => self.tracker[i].1 = Some(0), // length of the signature buffer
                ContractCostType::VmMemRead => self.tracker[i].1 = Some(0), // number of bytes in the linear memory to read
                ContractCostType::VmMemWrite => self.tracker[i].1 = Some(0), // number of bytes in the linear memory to write
                ContractCostType::VmInstantiation => self.tracker[i].1 = Some(0), // length of the wasm bytes
                ContractCostType::InvokeVmFunction => (),
                ContractCostType::ChargeBudget => (),
            }
        }
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
                self.tracker[i].0,
                format!("{:?}", self.tracker[i].1),
                self.cpu_insns.counts[i],
                self.mem_bytes.counts[i],
                self.cpu_insns.cost_models[i].const_term,
                self.cpu_insns.cost_models[i].linear_term,
                self.mem_bytes.cost_models[i].const_term,
                self.mem_bytes.cost_models[i].linear_term,
            )?;
        }
        writeln!(f, "{:=<165}", "")?;
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

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Budget(pub(crate) Rc<RefCell<BudgetImpl>>);

impl Debug for Budget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.0.borrow())
    }
}

impl Display for Budget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0.borrow())
    }
}

pub trait AsBudget {
    fn as_budget(&self) -> &Budget;
}

impl AsBudget for Budget {
    fn as_budget(&self) -> &Budget {
        self
    }
}

impl AsBudget for Host {
    fn as_budget(&self) -> &Budget {
        self.budget_ref()
    }
}

impl Budget {
    /// Initializes the budget from network configuration settings.
    pub fn from_configs(
        cpu_limit: u64,
        mem_limit: u64,
        cpu_cost_params: ContractCostParams,
        mem_cost_params: ContractCostParams,
    ) -> Self {
        Self(Rc::new(RefCell::new(BudgetImpl::from_configs(
            cpu_limit,
            mem_limit,
            cpu_cost_params,
            mem_cost_params,
        ))))
    }

    // Helper function to avoid multiple borrow_mut
    fn mut_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(RefMut<BudgetImpl>) -> Result<T, HostError>,
    {
        f(self.0.borrow_mut())
    }

    fn charge_in_bulk(
        &self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        if !self.0.borrow().enabled {
            return Ok(());
        }

        // NB: charging a cost-amount to the budgeting machinery itself seems to
        // cost a similar amount as a single WASM instruction; so it's quite
        // important to buffer WASM step counts before flushing to budgeting,
        // and we add a constant charge here for "the cost of budget-counting"
        // itself.

        // update tracker for reporting
        self.get_tracker_mut(ty, |(t_iters, t_inputs)| {
            *t_iters = t_iters.saturating_add(iterations);
            match (t_inputs, input) {
                (None, None) => Ok(()),
                (Some(t), Some(i)) => {
                    *t = t.saturating_add(i.saturating_mul(iterations));
                    Ok(())
                }
                // TODO: improve error code "internal error"
                _ => Err((ScErrorType::Context, ScErrorCode::InternalError).into()),
            }
        })?;
        self.get_tracker_mut(ContractCostType::ChargeBudget, |(t_iters, _)| {
            // we already know `ChargeBudget` has undefined input, so here we just add 1 iteration.
            *t_iters = t_iters.saturating_add(1);
            Ok(())
        })?;

        // do the actual budget charging
        self.mut_budget(|mut b| {
            // we already know `ChargeBudget` only affects the cpu budget
            b.cpu_insns
                .charge(ContractCostType::ChargeBudget, 1, None)?;
            b.cpu_insns.charge(ty, iterations, input)?;
            b.mem_bytes.charge(ty, iterations, input)
        })
    }

    /// Charges the budget under the specified [`CostType`]. The actual amount
    /// charged is determined by the underlying [`CostModel`] and may depend on
    /// the input. If the input is `None`, the model is assumed to be constant.
    /// Otherwise it is a linear model.  The caller needs to ensure the input
    /// passed is consistent with the inherent model underneath.
    pub fn charge(&self, ty: ContractCostType, input: Option<u64>) -> Result<(), HostError> {
        self.charge_in_bulk(ty, 1, input)
    }

    pub fn apply_wasmi_fuels(&self, cpu_fuel: u64, mem_fuel: u64) -> Result<(), HostError> {
        self.mut_budget(|mut b| {
            b.cpu_insns
                .charge(ContractCostType::WasmInsnExec, 1, Some(cpu_fuel))?;
            b.mem_bytes
                .charge(ContractCostType::WasmMemAlloc, 1, Some(mem_fuel))
        })
    }

    /// Performs a bulk charge to the budget under the specified [`CostType`].
    /// The `iterations` is the batch size. The caller needs to ensure:
    /// 1. the batched charges have identical costs (having the same
    /// [`CostType`] and `input`)
    /// 2. The input passed in (Some/None) is consistent with the [`CostModel`]
    /// underneath the [`CostType`] (linear/constant).
    pub fn batched_charge(
        &self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        self.charge_in_bulk(ty, iterations, input)
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

    pub fn get_tracker(&self, ty: ContractCostType) -> (u64, Option<u64>) {
        self.0.borrow().tracker[ty as usize]
    }

    pub(crate) fn get_tracker_mut<F>(&self, ty: ContractCostType, f: F) -> Result<(), HostError>
    where
        F: FnOnce(&mut (u64, Option<u64>)) -> Result<(), HostError>,
    {
        f(&mut self.0.borrow_mut().tracker[ty as usize])
    }

    pub fn get_cpu_insns_consumed(&self) -> u64 {
        self.0.borrow().cpu_insns.get_total_count()
    }

    pub fn get_mem_bytes_consumed(&self) -> u64 {
        self.0.borrow().mem_bytes.get_total_count()
    }

    pub fn get_cpu_insns_remaining(&self) -> u64 {
        self.0.borrow().cpu_insns.get_remaining()
    }

    pub fn get_mem_bytes_remaining(&self) -> u64 {
        self.0.borrow().mem_bytes.get_remaining()
    }

    pub fn reset_default(&self) {
        *self.0.borrow_mut() = BudgetImpl::default()
    }

    pub fn reset_unlimited(&self) {
        self.reset_unlimited_cpu();
        self.reset_unlimited_mem();
    }

    pub fn reset_unlimited_cpu(&self) {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset(u64::MAX);
            Ok(())
        })
        .unwrap(); // panic means multiple-mut-borrow bug
        self.reset_tracker()
    }

    pub fn reset_unlimited_mem(&self) {
        self.mut_budget(|mut b| {
            b.mem_bytes.reset(u64::MAX);
            Ok(())
        })
        .unwrap(); // panic means multiple-mut-borrow bug
        self.reset_tracker()
    }

    pub fn reset_tracker(&self) {
        for tracker in self.0.borrow_mut().tracker.iter_mut() {
            tracker.0 = 0;
            tracker.1 = tracker.1.map(|_| 0);
        }
    }

    pub fn reset_limits(&self, cpu: u64, mem: u64) {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset(cpu);
            b.mem_bytes.reset(mem);
            Ok(())
        })
        .unwrap(); // impossible to panic

        self.reset_tracker()
    }

    #[cfg(test)]
    pub fn reset_models(&self) {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset_models();
            b.mem_bytes.reset_models();
            Ok(())
        })
        .unwrap(); // impossible to panic
    }

    fn get_cpu_insns_remaining_as_fuel(&self) -> Result<u64, HostError> {
        let cpu_remaining = self.get_cpu_insns_remaining();
        let cpu_per_fuel = self
            .0
            .borrow()
            .cpu_insns
            .get_cost_model(ContractCostType::WasmInsnExec)
            .linear_term;

        if cpu_per_fuel < 0 {
            return Err((ScErrorType::Context, ScErrorCode::InvalidInput).into());
        }
        let cpu_per_fuel = (cpu_per_fuel as u64).max(1);
        Ok(cpu_remaining / cpu_per_fuel)
    }

    fn get_mem_bytes_remaining_as_fuel(&self) -> Result<u64, HostError> {
        let bytes_remaining = self.get_mem_bytes_remaining();
        let bytes_per_fuel = self
            .0
            .borrow()
            .mem_bytes
            .get_cost_model(ContractCostType::WasmMemAlloc)
            .linear_term;

        if bytes_per_fuel < 0 {
            return Err((ScErrorType::Context, ScErrorCode::InvalidInput).into());
        }
        let bytes_per_fuel = (bytes_per_fuel as u64).max(1);
        Ok(bytes_remaining / bytes_per_fuel)
    }

    pub fn get_fuels_budget(&self) -> Result<(u64, u64), HostError> {
        let cpu_fuel = self.get_cpu_insns_remaining_as_fuel()?;
        let mem_fuel = self.get_mem_bytes_remaining_as_fuel()?;
        Ok((cpu_fuel, mem_fuel))
    }

    // generate a wasmi fuel cost schedule based on our calibration
    pub fn wasmi_fuel_costs(&self) -> FuelCosts {
        let mut costs = FuelCosts::default();
        costs.base = 1;
        costs.entity = 1;
        costs.store = 1;
        costs.call = 10;
        costs
    }
}

/// Default settings for local/sandbox testing only. The actual operations will use parameters
/// read on-chain from network configuration via [`from_configs`] above.
impl Default for BudgetImpl {
    fn default() -> Self {
        let mut b = Self {
            cpu_insns: BudgetDimension::new(),
            mem_bytes: BudgetDimension::new(),
            tracker: vec![(0, None); ContractCostType::variants().len()],
            enabled: true,
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
                    cpu.const_term = 0;
                    cpu.linear_term = 22; // this is calibrated
                }
                // Host cpu insns per wasm "memory fuel". This has to be zero since
                // the fuel (representing cpu cost) has been covered by `WasmInsnExec`.
                // The extra cost of mem processing is accounted for by wasmi's
                // `config.memory_bytes_per_fuel` parameter.
                // This type is designated to the mem cost.
                ContractCostType::WasmMemAlloc => {
                    cpu.const_term = 0;
                    cpu.linear_term = 0;
                }
                ContractCostType::HostMemAlloc => {
                    cpu.const_term = 883;
                    cpu.linear_term = 0;
                }
                ContractCostType::HostMemCpy => {
                    cpu.const_term = 24;
                    cpu.linear_term = 0;
                }
                ContractCostType::HostMemCmp => {
                    cpu.const_term = 42;
                    cpu.linear_term = 1;
                }
                ContractCostType::InvokeHostFunction => {
                    cpu.const_term = 759;
                    cpu.linear_term = 0;
                }
                ContractCostType::VisitObject => {
                    cpu.const_term = 29;
                    cpu.linear_term = 0;
                }
                ContractCostType::ValXdrConv => {
                    cpu.const_term = 177;
                    cpu.linear_term = 0;
                }
                ContractCostType::ValSer => {
                    cpu.const_term = 741;
                    cpu.linear_term = 0;
                }
                ContractCostType::ValDeser => {
                    cpu.const_term = 846;
                    cpu.linear_term = 0;
                }
                ContractCostType::ComputeSha256Hash => {
                    cpu.const_term = 1912;
                    cpu.linear_term = 32;
                }
                ContractCostType::ComputeEd25519PubKey => {
                    cpu.const_term = 25766;
                    cpu.linear_term = 0;
                }
                ContractCostType::MapEntry => {
                    cpu.const_term = 59;
                    cpu.linear_term = 0;
                }
                ContractCostType::VecEntry => {
                    cpu.const_term = 14;
                    cpu.linear_term = 0;
                }
                ContractCostType::GuardFrame => {
                    cpu.const_term = 4512;
                    cpu.linear_term = 0;
                }
                ContractCostType::VerifyEd25519Sig => {
                    cpu.const_term = 368361;
                    cpu.linear_term = 20;
                }
                ContractCostType::VmMemRead => {
                    cpu.const_term = 95;
                    cpu.linear_term = 0;
                }
                ContractCostType::VmMemWrite => {
                    cpu.const_term = 97;
                    cpu.linear_term = 0;
                }
                // This (as well as its mem model params), are not taken from calibration results.
                // if we want to do that we need more sample contracts of various sizes. Right now
                // this is just an eye-balled upperbound.
                ContractCostType::VmInstantiation => {
                    cpu.const_term = 1_000_000;
                    cpu.linear_term = 0;
                }
                ContractCostType::InvokeVmFunction => {
                    cpu.const_term = 6212;
                    cpu.linear_term = 0;
                }
                ContractCostType::ChargeBudget => {
                    cpu.const_term = 198;
                    cpu.linear_term = 0;
                }
            }

            // define the memory cost model parameters
            let mem = b.mem_bytes.get_cost_model_mut(ct);
            match ct {
                // This type is designated to the cpu cost.
                ContractCostType::WasmInsnExec => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                // Bytes per wasmi "memory fuel".
                ContractCostType::WasmMemAlloc => {
                    mem.const_term = 66136; // TODO: this seems to be 1 page overhead?
                    mem.linear_term = 1; // this is always 1 (1-to-1 between bytes in the host and in Wasmi)
                }
                ContractCostType::HostMemAlloc => {
                    mem.const_term = 8;
                    mem.linear_term = 1;
                }
                ContractCostType::HostMemCpy => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::HostMemCmp => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::InvokeHostFunction => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::VisitObject => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::ValXdrConv => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::ValSer => {
                    mem.const_term = 9;
                    mem.linear_term = 3;
                }
                ContractCostType::ValDeser => {
                    mem.const_term = 4;
                    mem.linear_term = 1;
                }
                ContractCostType::ComputeSha256Hash => {
                    mem.const_term = 40;
                    mem.linear_term = 0;
                }
                ContractCostType::ComputeEd25519PubKey => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::MapEntry => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::VecEntry => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::GuardFrame => {
                    mem.const_term = 267;
                    mem.linear_term = 0;
                }
                ContractCostType::VerifyEd25519Sig => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::VmMemRead => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                ContractCostType::VmMemWrite => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
                // This (as well as its cpu model params), are not taken from calibration results.
                // if we want to do that we need more sample contracts of various sizes. Right now
                // this is just an eye-balled upperbound.
                ContractCostType::VmInstantiation => {
                    mem.const_term = 1100000;
                    mem.linear_term = 0;
                }
                ContractCostType::InvokeVmFunction => {
                    mem.const_term = 267;
                    mem.linear_term = 0;
                }
                ContractCostType::ChargeBudget => {
                    mem.const_term = 0;
                    mem.linear_term = 0;
                }
            }

            b.init_tracker();
        }

        // For the time being we don't have "on chain" cost models
        // so we just set some up here that we calibrated manually
        // in the adjacent benchmarks.
        //
        // We don't run for a time unit thought, we run for an estimated
        // (calibrated) number of CPU instructions.
        //
        // Assuming 2ghz chips at 2 instructions per cycle, we can guess about
        // 4bn instructions / sec. So about 4000 instructions per usec, or 400k
        // instructions in a 100usec time budget, or about 5479 wasm instructions
        // using the calibration above (73 CPU insns per wasm insn). Very roughly!
        b.cpu_insns.reset(40_000_000); // 100x the estimation above which corresponds to 10ms
        b.mem_bytes.reset(0x320_0000); // 50MB of memory
        b
    }
}
