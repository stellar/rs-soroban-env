mod dimension;
mod model;
mod util;
mod wasmi_helper;

pub use model::COST_MODEL_LIN_TERM_SCALE_BITS;

use std::{
    cell::{RefCell, RefMut},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    host::error::TryBorrowOrErr,
    xdr::{ContractCostParams, ContractCostType, DepthLimiter, ScErrorCode, ScErrorType},
    Error, Host, HostError, DEFAULT_HOST_DEPTH_LIMIT,
};

use dimension::{BudgetDimension, IsCpu, IsShadowMode};
use model::ScaledU64;
use wasmi_helper::FuelConfig;

// These are some sane values, however the embedder should typically customize
// these to match the network config.
const DEFAULT_CPU_INSN_LIMIT: u64 = 100_000_000;
const DEFAULT_MEM_BYTES_LIMIT: u64 = 40 * 1024 * 1024; // 40MB

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
    #[cfg(any(test, feature = "testutils"))]
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
    /// For the purpose of calibration and reporting; not used for budget-limiting per se.
    tracker: MeterTracker,
    is_in_shadow_mode: bool,
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
            is_in_shadow_mode: false,
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
        if !self.is_in_shadow_mode {
            // update tracker for reporting
            self.tracker.count = self.tracker.count.saturating_add(1);
            let (t_iters, t_inputs) = &mut self
                .tracker
                .cost_tracker
                .get_mut(ty as usize)
                .ok_or_else(|| {
                    HostError::from((ScErrorType::Budget, ScErrorCode::InternalError))
                })?;
            *t_iters = t_iters.saturating_add(iterations);
            match (t_inputs, input) {
                (None, None) => (),
                (Some(t), Some(i)) => *t = t.saturating_add(i.saturating_mul(iterations)),
                // internal logic error, a wrong cost type has been passed in
                _ => return Err((ScErrorType::Budget, ScErrorCode::InternalError).into()),
            };
        }

        self.cpu_insns.charge(
            ty,
            iterations,
            input,
            IsCpu(true),
            IsShadowMode(self.is_in_shadow_mode),
        )?;
        self.mem_bytes.charge(
            ty,
            iterations,
            input,
            IsCpu(false),
            IsShadowMode(self.is_in_shadow_mode),
        )
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
            is_in_shadow_mode: false,
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
        writeln!(
            f,
            "Internal details (diagnostics info, does not affect fees) "
        )?;
        writeln!(f, "Total # times meter was called: {}", self.tracker.count,)?;
        writeln!(
            f,
            "Shadow cpu limit: {}; used: {}",
            self.cpu_insns.shadow_limit, self.cpu_insns.shadow_total_count
        )?;
        writeln!(
            f,
            "Shadow mem limit: {}; used: {}",
            self.mem_bytes.shadow_limit, self.mem_bytes.shadow_total_count
        )?;
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

    /// Runs a user provided closure in shadow mode -- all metering is done through
    /// the shadow budget.
    ///
    /// Because the shadow mode is optional (depending on the configuration,
    /// nodes may or may not trigger this path), any error occured during execution
    /// is swallowed by running a fallback closure provided by the caller.
    ///
    /// # Arguments:
    /// * `f` - A fallible closure to be run in shadow mode. If error occurs,
    ///   fallback closure is run immediately afterwards to replace it
    ///
    /// * `e` - A fallback closure to be run in case of any error occuring
    ///
    /// # Returns:
    ///
    /// Returns a value of type `T`. Any errors arising during the execution are
    /// suppressed.
    pub(crate) fn with_shadow_mode<T, F, E>(&self, f: F, e: E) -> T
    where
        F: FnOnce() -> Result<T, HostError>,
        E: Fn() -> T,
    {
        let mut prev = false;
        let should_execute = self.mut_budget(|mut b| {
            prev = b.is_in_shadow_mode;
            b.is_in_shadow_mode = true;
            b.cpu_insns.check_budget_limit(true)?;
            b.mem_bytes.check_budget_limit(true)
        });

        let rt = if should_execute.is_ok() {
            f().unwrap_or_else(|_| e())
        } else {
            e()
        };

        if let Err(_) = self.mut_budget(|mut b| {
            b.is_in_shadow_mode = prev;
            Ok(())
        }) {
            return e();
        }

        rt
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

    pub(crate) fn get_wasmi_fuel_remaining(&self) -> Result<u64, HostError> {
        self.0.try_borrow_mut_or_err()?.get_wasmi_fuel_remaining()
    }

    // generate a wasmi fuel cost schedule based on our calibration
    pub(crate) fn wasmi_fuel_costs(&self) -> Result<wasmi::FuelCosts, HostError> {
        let config = &self.0.try_borrow_or_err()?.fuel_config;
        let mut costs = wasmi::FuelCosts::default();
        costs.base = config.base;
        costs.entity = config.entity;
        costs.load = config.load;
        costs.store = config.store;
        costs.call = config.call;
        Ok(costs)
    }
}
