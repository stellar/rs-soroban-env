use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{xdr::ScVmErrorCode, Host, HostError};

// TODO: move this to an XDR enum
#[repr(i32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CostType {
    // Cost of running 1 wasm interpreter loop
    WasmInsnExec = 0,
    // Cost of growing wasm linear memory by 1 page
    WasmMemAlloc = 1,
    // Cost of allocating a chuck of host memory (in bytes)
    HostMemAlloc = 2,
    // Cost of copying a chuck of bytes into a pre-allocated host memory
    HostMemCpy = 3,
    // Cost of a host function invocation, not including the actual work done by the function
    InvokeHostFunction = 4,
    // Cost of visiting a host object from the host object storage
    // TODO: consider removing. This is just indexing into an array. Fixed cost around 400 insns.
    // Only thing to make sure is the guest can't visitObject repeatly without incurring some charges elsewhere.
    VisitObject = 5,
    // Tracks a single Val (RawVal or primative Object like U64) <=> ScVal
    // conversion cost. Most of these Val counterparts in ScVal (except e.g.
    // Symbol) consumes a single int64 and therefore is a constant overhead.
    ValXdrConv = 6,
    // Cost of serializing an xdr object to bytes
    ValSer = 7,
    // Cost of deserializing an xdr object from bytes
    ValDeser = 8,
    // Cost of computing the sha256 hash from bytes
    ComputeSha256Hash = 9,
    // Cost of computing the ed25519 pubkey from bytes
    ComputeEd25519PubKey = 10,
    // Cost of constructing an new map. The input is the number
    // of entries allocated.
    MapNew = 11,
    // Cost of accessing an entry in a map. The input is the count of the number of
    // entries examined (which will be the log of the size of the map under binary search).
    MapEntry = 12,
    // Cost of constructing a new vector. The input is the number of entries allocated.
    VecNew = 13,
    // Cost of accessing one or more elements in a Vector. The input is the count of
    // the number of elements accessed.
    VecEntry = 14,
    // Cost of guarding a frame, which involves pushing and poping a frame and capturing a rollback point.
    GuardFrame = 15,
    // Cost of verifying ed25519 signature of a payload.
    VerifyEd25519Sig = 16,
    // Cost of reading a slice of vm linear memory
    VmMemRead = 17,
    // Cost of writing to a slice of vm linear memory
    VmMemWrite = 18,
    // Cost of instantiation a VM from wasm bytes code.
    VmInstantiation = 19,
    // Roundtrip cost of invoking a VM function from the host.
    InvokeVmFunction = 20,
    // Cost of deleting a byte from a bytes array,
    BytesDel = 21,
    // Cost of pushing a byte
    BytesPush = 22,
    // Cost of poping a byte
    BytesPop = 23,
    // Cost of inserting a byte into a bytes array at some index
    BytesInsert = 24,
    // Cost of appending a byte to the end of a bytes array
    BytesAppend = 25,
    // Cost of comparing two bytes arrays
    BytesCmp = 26,
    // Cost of charging a value to the budgeting system.
    ChargeBudget = 27,
}

// TODO: add XDR support for iterating over all the elements of an enum
impl CostType {
    pub fn variants() -> std::slice::Iter<'static, CostType> {
        static VARIANTS: &'static [CostType] = &[
            CostType::WasmInsnExec,
            CostType::WasmMemAlloc,
            CostType::HostMemAlloc,
            CostType::HostMemCpy,
            CostType::InvokeHostFunction,
            CostType::VisitObject,
            CostType::ValXdrConv,
            CostType::ValSer,
            CostType::ValDeser,
            CostType::ComputeSha256Hash,
            CostType::ComputeEd25519PubKey,
            CostType::MapNew,
            CostType::MapEntry,
            CostType::VecNew,
            CostType::VecEntry,
            CostType::GuardFrame,
            CostType::VerifyEd25519Sig,
            CostType::VmMemRead,
            CostType::VmMemWrite,
            CostType::VmInstantiation,
            CostType::InvokeVmFunction,
            CostType::BytesDel,
            CostType::BytesPush,
            CostType::BytesPop,
            CostType::BytesInsert,
            CostType::BytesAppend,
            CostType::BytesCmp,
            CostType::ChargeBudget,
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

    #[cfg(test)]
    pub fn reset(&mut self) {
        self.const_param = 0;
        self.log_param = 0;
        self.log_base_param = 0;
        self.lin_param = 0;
        self.quad_param = 0;
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

    /// Tracks the sum of _output_ values from the cost model, for purposes
    /// of comparing to limit.
    count: u64,
}

impl BudgetDimension {
    pub fn new(trapcode: ScVmErrorCode) -> Self {
        let mut bd = Self {
            trapcode,
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
        let cm = self.get_cost_model(ty);
        self.count = self.count.saturating_add(cm.evaluate(input));
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BudgetImpl {
    pub cpu_insns: BudgetDimension,
    pub mem_bytes: BudgetDimension,
    /// Tracks the sums of _input_ values to the cost models, for purposes of
    /// calibration and reporting; not used for budget-limiting per se.
    inputs: Vec<u64>,
    enabled: bool,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Budget(pub(crate) Rc<RefCell<BudgetImpl>>);

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

    pub fn charge(&self, ty: CostType, input: u64) -> Result<(), HostError> {
        if !self.0.borrow().enabled {
            return Ok(());
        }

        // println!("charging {} of {:?} (cpu budget: {}, mem budget: {})",
        // input, ty, self.get_cpu_insns_count(), self.get_mem_bytes_count());
        //
        // NB: charging a cost-amount to the budgeting machinery itself seems to
        // cost a similar amount as a single WASM instruction; so it's quite
        // important to buffer WASM step counts before flushing to budgeting,
        // and we add a constant charge here for "the cost of budget-counting"
        // itself.
        self.get_input_mut(ty, |i| *i = i.saturating_add(input));
        self.mut_budget(|mut b| {
            b.cpu_insns.charge(CostType::ChargeBudget, 1)?;
            b.cpu_insns.charge(ty, input)?;
            b.mem_bytes.charge(ty, input)
        })
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

    pub fn get_input(&self, ty: CostType) -> u64 {
        self.0.borrow().inputs[ty as usize]
    }

    fn get_input_mut<F>(&self, ty: CostType, f: F)
    where
        F: FnOnce(&mut u64),
    {
        f(&mut self.0.borrow_mut().inputs[ty as usize])
    }

    pub fn get_cpu_insns_count(&self) -> u64 {
        self.0.borrow().cpu_insns.get_count()
    }

    pub fn get_mem_bytes_count(&self) -> u64 {
        self.0.borrow().mem_bytes.get_count()
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
        self.reset_inputs()
    }

    pub fn reset_unlimited_mem(&self) {
        self.mut_budget(|mut b| {
            b.mem_bytes.reset(u64::MAX);
            Ok(())
        })
        .unwrap(); // panic means multiple-mut-borrow bug
        self.reset_inputs()
    }

    pub fn reset_inputs(&self) {
        for i in self.0.borrow_mut().inputs.iter_mut() {
            *i = 0;
        }
    }

    pub fn reset_limits(&self, cpu: u64, mem: u64) {
        self.mut_budget(|mut b| {
            b.cpu_insns.reset(cpu);
            b.mem_bytes.reset(mem);
            Ok(())
        })
        .unwrap(); // impossible to panic

        self.reset_inputs()
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
            inputs: Default::default(),
            enabled: true,
        };

        for ct in CostType::variants() {
            b.inputs.push(0);

            let cpu = &mut b.cpu_insns.get_cost_model_mut(*ct);
            match ct {
                // We might want to split wasm insns into separate cases; some are much more than
                // this and some are much less.
                CostType::WasmInsnExec => cpu.lin_param = 32,
                CostType::WasmMemAlloc => cpu.lin_param = 1000,
                CostType::InvokeHostFunction => {
                    cpu.const_param = 5462;
                    cpu.lin_param = 732
                }
                CostType::VisitObject => cpu.const_param = 600,
                CostType::ValXdrConv => cpu.lin_param = 182,
                CostType::ValSer => cpu.lin_param = 84,
                CostType::ValDeser => cpu.lin_param = 20,

                CostType::ComputeSha256Hash => {
                    cpu.const_param = 3000;
                    cpu.lin_param = 50;
                }

                CostType::ComputeEd25519PubKey => cpu.const_param = 26_000,
                CostType::VerifyEd25519Sig => {
                    cpu.const_param = 366_877;
                    cpu.lin_param = 22;
                }

                CostType::MapNew => cpu.const_param = 94,
                CostType::MapEntry => cpu.lin_param = 62,
                CostType::VecNew => cpu.lin_param = 94,
                CostType::VecEntry => cpu.lin_param = 125,
                CostType::GuardFrame => cpu.const_param = 3521,

                CostType::VmMemRead => {
                    cpu.const_param = 369;
                    cpu.lin_param = 1
                }
                CostType::VmMemWrite => {
                    cpu.const_param = 322;
                    cpu.lin_param = 1
                }
                CostType::VmInstantiation => cpu.const_param = 1_000_000,
                CostType::InvokeVmFunction => cpu.const_param = 4941,
                CostType::BytesAppend => {
                    cpu.const_param = 770;
                    cpu.lin_param = 1
                }
                CostType::BytesCmp => {
                    cpu.const_param = 141;
                    cpu.lin_param = 1
                }
                CostType::BytesDel => {
                    cpu.const_param = 800;
                }
                CostType::BytesInsert => {
                    cpu.const_param = 756;
                    cpu.lin_param = 1;
                }
                CostType::BytesPop => {
                    cpu.const_param = 486;
                }
                CostType::BytesPush => {
                    cpu.const_param = 1500;
                }
                CostType::ChargeBudget => cpu.const_param = 127,
                CostType::HostMemAlloc => cpu.const_param = 872,
                CostType::HostMemCpy => cpu.lin_param = 3,
            }

            let mem = b.mem_bytes.get_cost_model_mut(*ct);
            match ct {
                CostType::WasmInsnExec => (),
                CostType::WasmMemAlloc => mem.lin_param = 1,
                CostType::InvokeHostFunction => mem.const_param = 592,
                CostType::VisitObject => (),
                CostType::ValXdrConv => (),
                CostType::ValSer => mem.lin_param = 2,
                CostType::ValDeser => mem.lin_param = 4,
                CostType::ComputeSha256Hash | CostType::ComputeEd25519PubKey => {
                    mem.const_param = 40
                }
                CostType::MapNew => (),
                CostType::MapEntry => (),
                CostType::VecNew => (),
                CostType::VecEntry => (),
                CostType::GuardFrame => mem.const_param = 267,
                CostType::VerifyEd25519Sig => (),
                CostType::VmInstantiation => mem.const_param = 1_000_000,
                CostType::VmMemRead => mem.lin_param = 1,
                CostType::VmMemWrite => (),
                CostType::BytesAppend => {
                    mem.const_param = 16;
                    mem.lin_param = 1
                }
                CostType::BytesCmp => (),
                CostType::BytesDel => (),
                CostType::BytesInsert => {
                    cpu.const_param = 16;
                    cpu.lin_param = 2;
                }
                CostType::BytesPop => (),
                CostType::BytesPush => {
                    cpu.const_param = 16;
                    cpu.lin_param = 2;
                }
                CostType::InvokeVmFunction => mem.const_param = 267,
                CostType::ChargeBudget => (),
                CostType::HostMemAlloc => mem.lin_param = 1,
                CostType::HostMemCpy => (),
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
        b.mem_bytes.reset(0xa0_0000); // 10MB of memory
        b
    }
}
