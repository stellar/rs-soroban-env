use std::{cell::RefCell, rc::Rc};

use crate::{xdr::ScVmErrorCode, HostError};

// TODO: move this to an XDR enum
#[repr(i32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CostType {
    WasmInsnExec = 0,
    WasmMemAlloc = 1,
    HostEventDebug = 2,
    HostEventContract = 3,
    HostFunction = 4,
    VisitObject = 5,
    PushFrame = 6,
    PopFrame = 7,
    // Tracks a single Val (RawVal or primative Object like U64) <=> ScVal
    // conversion cost. Most of these Val counterparts in ScVal (except e.g.
    // Symbol) consumes a single int64 and therefore is a constant overhead.
    ValXdrConv = 8,
    ValSer = 9,
    ValDeser = 10,
    CloneEvents = 11,
    HostObjAllocSlot = 12,
    HostVecAllocCell = 13,
    HostMapAllocCell = 14,
    HostU64AllocCell = 15,
    HostI64AllocCell = 16,
    HostBinAllocCell = 17,
    HostBigIntAllocCell = 18,
    ComputeSha256Hash = 19,
    ComputeEd25519PubKey = 20,
    ImMapNew = 21,
    ImMapMutEntry = 22,
    ImMapImmutEntry = 23,
    ImVecNew = 24,
    ImVecMutEntry = 25,
    ImVecImmutEntry = 26,
    ScVecFromHostVec = 27,
    ScMapFromHostMap = 28,
    ScVecToHostVec = 29,
    ScMapToHostMap = 30,
    GuardFrame = 31,
    CloneVm = 32,
    VerifyEd25519Sig = 33,
    BigIntNew = 34,
    BigIntAddSub = 35,
    BigIntMul = 36,
    BigIntDivRem = 37,
    BigIntBitwiseOp = 38,
    BigIntShift = 39,
    BigIntCmp = 40,
    BigIntGcdLcm = 41,
    BigIntPow = 42,
    BigIntPowMod = 43,
    BigIntSqrt = 44,
    BigIntFromBytes = 45,
    BigIntToBytes = 46,
    BigIntToRadix = 47,
    VmMemCpy = 48,
    VmInstantiation = 49,
    VmInvokeFunction = 50,
    BytesClone = 51,
    BytesDel = 52,
    BytesPush = 53,
    BytesPop = 54,
    BytesInsert = 55,
    BytesAppend = 56,
    BytesSlice = 57,
    CallArgsUnpack = 58,
}

// TODO: add XDR support for iterating over all the elements of an enum
impl CostType {
    pub fn variants() -> std::slice::Iter<'static, CostType> {
        static VARIANTS: &'static [CostType] = &[
            CostType::WasmInsnExec,
            CostType::WasmMemAlloc,
            CostType::HostEventDebug,
            CostType::HostEventContract,
            CostType::HostFunction,
            CostType::VisitObject,
            CostType::PushFrame,
            CostType::PopFrame,
            CostType::ValXdrConv,
            CostType::ValSer,
            CostType::ValDeser,
            CostType::CloneEvents,
            CostType::HostObjAllocSlot,
            CostType::HostVecAllocCell,
            CostType::HostMapAllocCell,
            CostType::HostU64AllocCell,
            CostType::HostI64AllocCell,
            CostType::HostBinAllocCell,
            CostType::HostBigIntAllocCell,
            CostType::ComputeSha256Hash,
            CostType::ComputeEd25519PubKey,
            CostType::ImMapNew,
            CostType::ImMapMutEntry,
            CostType::ImMapImmutEntry,
            CostType::ImVecNew,
            CostType::ImVecMutEntry,
            CostType::ImVecImmutEntry,
            CostType::ScVecFromHostVec,
            CostType::ScMapFromHostMap,
            CostType::ScVecToHostVec,
            CostType::ScMapToHostMap,
            CostType::GuardFrame,
            CostType::CloneVm,
            CostType::VerifyEd25519Sig,
            CostType::BigIntNew,
            CostType::BigIntAddSub,
            CostType::BigIntMul,
            CostType::BigIntDivRem,
            CostType::BigIntBitwiseOp,
            CostType::BigIntShift,
            CostType::BigIntCmp,
            CostType::BigIntGcdLcm,
            CostType::BigIntPow,
            CostType::BigIntPowMod,
            CostType::BigIntSqrt,
            CostType::BigIntFromBytes,
            CostType::BigIntToBytes,
            CostType::BigIntToRadix,
            CostType::VmMemCpy,
            CostType::VmInstantiation,
            CostType::VmInvokeFunction,
            CostType::BytesClone,
            CostType::BytesDel,
            CostType::BytesPush,
            CostType::BytesPop,
            CostType::BytesInsert,
            CostType::BytesAppend,
            CostType::BytesSlice,
            CostType::CallArgsUnpack,
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
        let cm = self.get_cost_model(ty);
        self.count = self.count.saturating_add(cm.evaluate(input));
        if self.is_over_budget() {
            // TODO: convert this to a proper error code type.
            Err(ScVmErrorCode::TrapMemLimitExceeded.into())
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
pub(crate) struct BudgetImpl {
    pub cpu_insns: BudgetDimension,
    pub mem_bytes: BudgetDimension,
    /// Tracks the sums of _input_ values to the cost models, for purposes of
    /// calibration and reporting; not used for budget-limiting per se.
    inputs: Vec<u64>,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Budget(pub(crate) Rc<RefCell<BudgetImpl>>);

impl Budget {
    pub fn charge(&self, ty: CostType, input: u64) -> Result<(), HostError> {
        self.get_input_mut(ty, |i| *i = i.saturating_add(input));
        self.0.borrow_mut().cpu_insns.charge(ty, input)?;
        self.0.borrow_mut().mem_bytes.charge(ty, input)?;
        Ok(())
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

    pub fn reset_unlimited(&self) {
        self.0.borrow_mut().cpu_insns.reset(u64::MAX);
        self.0.borrow_mut().mem_bytes.reset(u64::MAX);
        self.reset_inputs()
    }

    pub fn reset_inputs(&self) {
        for i in self.0.borrow_mut().inputs.iter_mut() {
            *i = 0;
        }
    }

    #[cfg(test)]
    pub fn reset_limits(&self, cpu: u64, mem: u64) {
        self.0.borrow_mut().cpu_insns.reset(cpu);
        self.0.borrow_mut().mem_bytes.reset(mem);
        self.reset_inputs()
    }

    #[cfg(test)]
    pub fn reset_models(&self) {
        self.0.borrow_mut().cpu_insns.reset_models();
        self.0.borrow_mut().mem_bytes.reset_models();
    }
}

impl Default for BudgetImpl {
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
        // TODO: to be calibrated
        b.cpu_insns
            .get_cost_model_mut(CostType::ValXdrConv)
            .lin_param = 1;
        b.mem_bytes
            .get_cost_model_mut(CostType::ValXdrConv)
            .lin_param = 1;
        b.cpu_insns
            .get_cost_model_mut(CostType::ValXdrConv)
            .const_param = 100;
        b.mem_bytes
            .get_cost_model_mut(CostType::ValXdrConv)
            .const_param = 10;

        // Some "reasonable defaults": 640k of RAM and 100usec.
        //
        // We don't run for a time unit thought, we run for an estimated
        // (calibrated) number of CPU instructions.
        //
        // Assuming 2ghz chips at 2 instructions per cycle, we can guess about
        // 4bn instructions / sec. So about 4000 instructions per usec, or 400k
        // instructions in a 100usec time budget, or about 5479 wasm instructions
        // using the calibration above. Very roughly!
        // b.mem_bytes.limit = 0xa_0000;
        // b.cpu_insns.limit = 400_000;
        // TODO: Set proper limits once all the machinary (including in SDK tests) is in place and models are calibrated.
        b.cpu_insns.reset(u64::MAX);
        b.mem_bytes.reset(u64::MAX);
        b
    }
}
