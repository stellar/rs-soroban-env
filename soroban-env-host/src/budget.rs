use std::{
    cell::{RefCell, RefMut},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    xdr::{ScUnknownErrorCode, ScVmErrorCode},
    Host, HostError,
};

// TODO: move this to an XDR enum
#[repr(i32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CostType {
    // Cost of running 1 wasm instruction
    WasmInsnExec = 0,
    // Cost of growing wasm linear memory by 1 page
    WasmMemAlloc = 1,
    // Cost of allocating a chuck of host memory (in bytes)
    HostMemAlloc = 2,
    // Cost of copying a chuck of bytes into a pre-allocated host memory
    HostMemCpy = 3,
    // Cost of comparing two slices of host memory
    HostMemCmp = 4,
    // Cost of a host function invocation, not including the actual work done by the function
    InvokeHostFunction = 5,
    // Cost of visiting a host object from the host object storage
    // Only thing to make sure is the guest can't visitObject repeatly without incurring some charges elsewhere.
    VisitObject = 6,
    // Tracks a single Val (RawVal or primative Object like U64) <=> ScVal
    // conversion cost. Most of these Val counterparts in ScVal (except e.g.
    // Symbol) consumes a single int64 and therefore is a constant overhead.
    ValXdrConv = 7,
    // Cost of serializing an xdr object to bytes
    ValSer = 8,
    // Cost of deserializing an xdr object from bytes
    ValDeser = 9,
    // Cost of computing the sha256 hash from bytes
    ComputeSha256Hash = 10,
    // Cost of computing the ed25519 pubkey from bytes
    ComputeEd25519PubKey = 11,
    // Cost of accessing an entry in a Map.
    MapEntry = 12,
    // Cost of accessing an entry in a Vec
    VecEntry = 13,
    // Cost of guarding a frame, which involves pushing and poping a frame and capturing a rollback point.
    GuardFrame = 14,
    // Cost of verifying ed25519 signature of a payload.
    VerifyEd25519Sig = 15,
    // Cost of reading a slice of vm linear memory
    VmMemRead = 16,
    // Cost of writing to a slice of vm linear memory
    VmMemWrite = 17,
    // Cost of instantiation a VM from wasm bytes code.
    VmInstantiation = 18,
    // Roundtrip cost of invoking a VM function from the host.
    InvokeVmFunction = 19,
    // Cost of charging a value to the budgeting system.
    ChargeBudget = 20,
}

// TODO: add XDR support for iterating over all the elements of an enum
impl CostType {
    pub fn variants() -> std::slice::Iter<'static, CostType> {
        static VARIANTS: &'static [CostType] = &[
            CostType::WasmInsnExec,
            CostType::WasmMemAlloc,
            CostType::HostMemAlloc,
            CostType::HostMemCpy,
            CostType::HostMemCmp,
            CostType::InvokeHostFunction,
            CostType::VisitObject,
            CostType::ValXdrConv,
            CostType::ValSer,
            CostType::ValDeser,
            CostType::ComputeSha256Hash,
            CostType::ComputeEd25519PubKey,
            CostType::MapEntry,
            CostType::VecEntry,
            CostType::GuardFrame,
            CostType::VerifyEd25519Sig,
            CostType::VmMemRead,
            CostType::VmMemWrite,
            CostType::VmInstantiation,
            CostType::InvokeVmFunction,
            CostType::ChargeBudget,
        ];
        VARIANTS.iter()
    }
}

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

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CostModel {
    pub(crate) const_param: u64,
    pub(crate) lin_param: u64,
}

impl CostModel {
    /// Evaluates the linear model `f(input) = const_param + lin_param * Option<input>`.
    /// If the input is `None`, then the linear part is ingored, resulting in a constant
    /// model.
    pub fn evaluate(&self, input: Option<u64>) -> u64 {
        match input {
            Some(input) => {
                let mut res = self.const_param;
                if self.lin_param != 0 {
                    res = res.saturating_add(self.lin_param.saturating_mul(input));
                }
                res
            }
            None => self.const_param,
        }
    }

    #[cfg(test)]
    pub fn reset(&mut self) {
        self.const_param = 0;
        self.lin_param = 0;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BudgetDimension {
    trapcode: ScVmErrorCode,

    /// A set of cost models that map input values (eg. event counts, object
    /// sizes) from some CostType to whatever concrete resource type is being
    /// tracked by this dimension (eg. cpu or memory). CostType enum values are
    /// used as indexes into this vector, to make runtime lookups as cheap as
    /// possible.
    cost_models: Vec<CostModel>,

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

        for ct in CostType::variants() {
            writeln!(f, "CostType {:?}, count {}", ct, self.counts[*ct as usize])?;
            writeln!(f, "model: {:?}", self.cost_models[*ct as usize])?;
        }
        Ok(())
    }
}

impl BudgetDimension {
    pub fn new(trapcode: ScVmErrorCode) -> Self {
        let mut bd = Self {
            trapcode,
            cost_models: Default::default(),
            limit: Default::default(),
            counts: Default::default(),
            total_count: Default::default(),
        };
        for _ct in CostType::variants() {
            // TODO: load cost model for i from the chain.
            bd.cost_models.push(CostModel::default());
            bd.counts.push(0);
        }
        bd
    }

    pub fn get_cost_model(&self, ty: CostType) -> &CostModel {
        &self.cost_models[ty as usize]
    }

    pub fn get_cost_model_mut(&mut self, ty: CostType) -> &mut CostModel {
        &mut self.cost_models[ty as usize]
    }

    pub fn get_count(&self, ty: CostType) -> u64 {
        self.counts[ty as usize]
    }

    pub fn get_total_count(&self) -> u64 {
        self.total_count
    }

    pub fn get_limit(&self) -> u64 {
        self.limit
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
        ty: CostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        let cm = self.get_cost_model(ty);
        let amount = cm.evaluate(input).saturating_mul(iterations);
        self.counts[ty as usize] = self.counts[ty as usize].saturating_add(amount);
        self.total_count = self.total_count.saturating_add(amount);
        if self.is_over_budget() {
            // TODO: convert this to a proper error code type.
            Err(self.trapcode.into())
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
            "const_param_cpu",
            "lin_param_cpu",
            "const_param_mem",
            "lin_param_mem",
        )?;
        for ct in CostType::variants() {
            let i = *ct as usize;
            writeln!(
                f,
                "{:<25}{:<15}{:<15}{:<15}{:<15}{:<20}{:<20}{:<20}{:<20}",
                format!("{:?}", ct),
                self.tracker[i].0,
                format!("{:?}", self.tracker[i].1),
                self.cpu_insns.counts[i],
                self.mem_bytes.counts[i],
                self.cpu_insns.cost_models[i].const_param,
                self.cpu_insns.cost_models[i].lin_param,
                self.mem_bytes.cost_models[i].const_param,
                self.mem_bytes.cost_models[i].lin_param,
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
        for ct in CostType::variants() {
            let i = *ct as usize;
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
    // Helper function to avoid multiple borrow_mut
    fn mut_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(RefMut<BudgetImpl>) -> Result<T, HostError>,
    {
        f(self.0.borrow_mut())
    }

    fn charge_in_bulk(
        &self,
        ty: CostType,
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
                (Some(t), Some(i)) => Ok(*t = t.saturating_add(i.saturating_mul(iterations))),
                // TODO: improve error code "unexpected cost model input"
                _ => Err(ScUnknownErrorCode::General.into()),
            }
        })?;
        self.get_tracker_mut(CostType::ChargeBudget, |(t_iters, _)| {
            // we already know `ChargeBudget` has undefined input, so here we just add 1 iteration.
            Ok(*t_iters = t_iters.saturating_add(1))
        })?;

        // do the actual budget charging
        self.mut_budget(|mut b| {
            // we already know `ChargeBudget` only affects the cpu budget
            b.cpu_insns.charge(CostType::ChargeBudget, 1, None)?;
            b.cpu_insns.charge(ty, iterations, input)?;
            b.mem_bytes.charge(ty, iterations, input)
        })
    }

    /// Charges the budget under the specified [`CostType`]. The actual amount
    /// charged is determined by the underlying [`CostModel`] and may depend on
    /// the input. If the input is `None`, the model is assumed to be constant.
    /// Otherwise it is a linear model.  The caller needs to ensure the input
    /// passed is consistent with the inherent model underneath.
    pub fn charge(&self, ty: CostType, input: Option<u64>) -> Result<(), HostError> {
        self.charge_in_bulk(ty, 1, input)
    }

    /// Performs a bulk charge to the budget under the specified [`CostType`].
    /// The `iterations` is the batch size. The caller needs to ensure:
    /// 1. the batched charges have identical costs (having the same
    /// [`CostType`] and `input`)
    /// 2. The input passed in (Some/None) is consistent with the [`CostModel`]
    /// underneath the [`CostType`] (linear/constant).
    pub fn batched_charge(
        &self,
        ty: CostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        self.charge_in_bulk(ty, iterations, input)
    }

    pub fn with_free_budget<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce() -> Result<T, HostError>,
    {
        self.mut_budget(|mut b| {
            b.enabled = false;
            Ok(())
        })?;

        let res = f();

        self.mut_budget(|mut b| {
            b.enabled = true;
            Ok(())
        })?;
        res
    }

    pub fn get_tracker(&self, ty: CostType) -> (u64, Option<u64>) {
        self.0.borrow().tracker[ty as usize]
    }

    pub(crate) fn get_tracker_mut<F>(&self, ty: CostType, f: F) -> Result<(), HostError>
    where
        F: FnOnce(&mut (u64, Option<u64>)) -> Result<(), HostError>,
    {
        f(&mut self.0.borrow_mut().tracker[ty as usize])
    }

    pub fn get_cpu_insns_count(&self) -> u64 {
        self.0.borrow().cpu_insns.get_total_count()
    }

    pub fn get_mem_bytes_count(&self) -> u64 {
        self.0.borrow().mem_bytes.get_total_count()
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
}

impl Default for BudgetImpl {
    fn default() -> Self {
        let mut b = Self {
            cpu_insns: BudgetDimension::new(ScVmErrorCode::TrapCpuLimitExceeded),
            mem_bytes: BudgetDimension::new(ScVmErrorCode::TrapMemLimitExceeded),
            tracker: vec![(0, None); CostType::variants().len()],
            enabled: true,
        };

        for ct in CostType::variants() {
            // define the cpu cost model parameters
            let cpu = &mut b.cpu_insns.get_cost_model_mut(*ct);
            match ct {
                CostType::WasmInsnExec => {
                    cpu.const_param = 22;
                    cpu.lin_param = 0;
                }
                CostType::WasmMemAlloc => {
                    cpu.const_param = 521;
                    cpu.lin_param = 0;
                }
                CostType::HostMemAlloc => {
                    cpu.const_param = 883;
                    cpu.lin_param = 0;
                }
                CostType::HostMemCpy => {
                    cpu.const_param = 24;
                    cpu.lin_param = 0;
                }
                CostType::HostMemCmp => {
                    cpu.const_param = 42;
                    cpu.lin_param = 1;
                }
                CostType::InvokeHostFunction => {
                    cpu.const_param = 759;
                    cpu.lin_param = 0;
                }
                CostType::VisitObject => {
                    cpu.const_param = 29;
                    cpu.lin_param = 0;
                }
                CostType::ValXdrConv => {
                    cpu.const_param = 177;
                    cpu.lin_param = 0;
                }
                CostType::ValSer => {
                    cpu.const_param = 741;
                    cpu.lin_param = 0;
                }
                CostType::ValDeser => {
                    cpu.const_param = 846;
                    cpu.lin_param = 0;
                }
                CostType::ComputeSha256Hash => {
                    cpu.const_param = 1912;
                    cpu.lin_param = 32;
                }
                CostType::ComputeEd25519PubKey => {
                    cpu.const_param = 25766;
                    cpu.lin_param = 0;
                }
                CostType::MapEntry => {
                    cpu.const_param = 59;
                    cpu.lin_param = 0;
                }
                CostType::VecEntry => {
                    cpu.const_param = 14;
                    cpu.lin_param = 0;
                }
                CostType::GuardFrame => {
                    cpu.const_param = 4512;
                    cpu.lin_param = 0;
                }
                CostType::VerifyEd25519Sig => {
                    cpu.const_param = 368361;
                    cpu.lin_param = 20;
                }
                CostType::VmMemRead => {
                    cpu.const_param = 95;
                    cpu.lin_param = 0;
                }
                CostType::VmMemWrite => {
                    cpu.const_param = 97;
                    cpu.lin_param = 0;
                }
                // This (as well as its mem model params), are not taken from calibration results.
                // if we want to do that we need more sample contracts of various sizes. Right now
                // this is just an eye-balled upperbound.
                CostType::VmInstantiation => {
                    cpu.const_param = 1_000_000;
                    cpu.lin_param = 0;
                }
                CostType::InvokeVmFunction => {
                    cpu.const_param = 6212;
                    cpu.lin_param = 0;
                }
                CostType::ChargeBudget => {
                    cpu.const_param = 198;
                    cpu.lin_param = 0;
                }
            }

            // define the memory cost model parameters
            let mem = b.mem_bytes.get_cost_model_mut(*ct);
            match ct {
                CostType::WasmInsnExec => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::WasmMemAlloc => {
                    mem.const_param = 66136;
                    mem.lin_param = 1;
                }
                CostType::HostMemAlloc => {
                    mem.const_param = 8;
                    mem.lin_param = 1;
                }
                CostType::HostMemCpy => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::HostMemCmp => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::InvokeHostFunction => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::VisitObject => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::ValXdrConv => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::ValSer => {
                    mem.const_param = 9;
                    mem.lin_param = 3;
                }
                CostType::ValDeser => {
                    mem.const_param = 4;
                    mem.lin_param = 1;
                }
                CostType::ComputeSha256Hash => {
                    mem.const_param = 40;
                    mem.lin_param = 0;
                }
                CostType::ComputeEd25519PubKey => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::MapEntry => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::VecEntry => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::GuardFrame => {
                    mem.const_param = 267;
                    mem.lin_param = 0;
                }
                CostType::VerifyEd25519Sig => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::VmMemRead => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                CostType::VmMemWrite => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
                // This (as well as its cpu model params), are not taken from calibration results.
                // if we want to do that we need more sample contracts of various sizes. Right now
                // this is just an eye-balled upperbound.
                CostType::VmInstantiation => {
                    mem.const_param = 1100000;
                    mem.lin_param = 0;
                }
                CostType::InvokeVmFunction => {
                    mem.const_param = 267;
                    mem.lin_param = 0;
                }
                CostType::ChargeBudget => {
                    mem.const_param = 0;
                    mem.lin_param = 0;
                }
            }

            // Define what inputs actually mean. For any constant-cost types -- whether it is a
            // true constant unit cost type, or empirically assigned (via measurement) constant
            // type -- we leave the input as `None`, otherwise, we initialize the input to 0.
            let i = *ct as usize;
            match ct {
                CostType::WasmInsnExec => (),
                CostType::WasmMemAlloc => b.tracker[i].1 = Some(0), // number of pages in wasm linear memory to allocate (each page is 64kB)
                CostType::HostMemAlloc => b.tracker[i].1 = Some(0), // number of bytes in host memory to allocate
                CostType::HostMemCpy => b.tracker[i].1 = Some(0), // number of bytes in host to copy
                CostType::HostMemCmp => b.tracker[i].1 = Some(0), // number of bytes in host to compare
                CostType::InvokeHostFunction => (),
                CostType::VisitObject => (),
                CostType::ValXdrConv => (),
                CostType::ValSer => b.tracker[i].1 = Some(0), // number of bytes in the result buffer
                CostType::ValDeser => b.tracker[i].1 = Some(0), // number of bytes in the buffer
                CostType::ComputeSha256Hash => b.tracker[i].1 = Some(0), // number of bytes in the buffer
                CostType::ComputeEd25519PubKey => (),
                CostType::MapEntry => (),
                CostType::VecEntry => (),
                CostType::GuardFrame => (),
                CostType::VerifyEd25519Sig => b.tracker[i].1 = Some(0), // length of the signature buffer
                CostType::VmMemRead => b.tracker[i].1 = Some(0), // number of bytes in the linear memory to read
                CostType::VmMemWrite => b.tracker[i].1 = Some(0), // number of bytes in the linear memory to write
                CostType::VmInstantiation => b.tracker[i].1 = Some(0), // length of the wasm bytes
                CostType::InvokeVmFunction => (),
                CostType::ChargeBudget => (),
            }
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
        //
        // TODO: Set proper limits once all the machinary (including in SDK tests) is in place and models are calibrated.
        // For now we set a generous but finite limit for DOS prevention.
        b.cpu_insns.reset(40_000_000); // 100x the estimation above which corresponds to 10ms
        b.mem_bytes.reset(0x320_0000); // 50MB of memory
        b
    }
}
