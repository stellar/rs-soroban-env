use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{xdr::ScVmErrorCode, HostError};

// TODO: move this to an XDR enum
#[repr(i32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CostType {
    // Cost of running 1 wasm interpreter loop
    WasmInsnExec = 0,
    WasmMemAlloc = 1,
    // Cost of forming a debug event and pushing it into the event storage
    HostEventDebug = 2,
    // TODO: consider removing. This is just a `Vec::push`. Cost of pushing a contract event it into the event storage
    // The average of cpu insns is around 300.
    HostEventContract = 3,
    // Cost of a host function invocation, not including the actual work done by the function
    HostFunction = 4,
    // Cost of visiting a host object from the host object storage
    // TODO: consider removing. This is just indexing into an array. Fixed cost around 400 insns.
    // Only thing to make sure is the guest can't visitObject repeatly without incurring some charges elsewhere.
    VisitObject = 5,
    // TODO: remove, covered by GuardFrame. Cost of pushing a frame into the context
    PushFrame = 6,
    // TODO: remove, covered by GuardFrame. Cost of poping a frame out of the context
    PopFrame = 7,
    // Tracks a single Val (RawVal or primative Object like U64) <=> ScVal
    // conversion cost. Most of these Val counterparts in ScVal (except e.g.
    // Symbol) consumes a single int64 and therefore is a constant overhead.
    ValXdrConv = 8,
    // Cost of serializing an xdr object to bytes
    ValSer = 9,
    // Cost of deserializing an xdr object from bytes
    ValDeser = 10,
    // Cost of cloning events
    CloneEvents = 11,
    // Cost of occupying a host object slot
    HostObjAllocSlot = 12,
    // TODO: remove 12-18, 61-62. They are not needed, HostObjAllocSlot is enough.
    // Cost of pushing a new `Vec` object to the object storage.
    HostVecAllocCell = 13,
    // Cost of pushing a new `Map` object to the object storage.
    HostMapAllocCell = 14,
    // Cost of pushing a new `U64` object to the object storage.
    HostU64AllocCell = 15,
    // Cost of pushing a new `I64` object to the object storage.
    HostI64AllocCell = 16,
    // Cost of pushing a new `Bytes` object to the object storage.
    HostBytesAllocCell = 17,
    // Cost of pushing a new `BigInt` object to the object storage.
    HostBigIntAllocCell = 18,
    // Cost of computing the sha256 hash from bytes
    ComputeSha256Hash = 19,
    // Cost of computing the ed25519 pubkey from bytes
    ComputeEd25519PubKey = 20,
    // Cost of constructing an empty new OrdMap
    ImMapNew = 21,
    // Cost of (mutably) accessing an entry in an OrdMap
    ImMapMutEntry = 22,
    // Cost of (immutably) accessing an entry in an OrdMap
    ImMapImmutEntry = 23,
    // Cost of constructing an empty new Vector
    ImVecNew = 24,
    // Cost of (mutably) accessing an entry in a Vector
    ImVecMutEntry = 25,
    // Cost of (immutably) accessing an entry in a Vector
    ImVecImmutEntry = 26,
    //TODO: 27-30 are probably redundent.They are covered elsewhere.
    // Cost of work needed to collect elements from a HostVec into a ScVec. This does not account for the
    // conversion of the elements into its ScVal form.
    ScVecFromHostVec = 27,
    // Cost of work needed to collect elements from a HostMap into a ScMap. This does not account for the
    // conversion of the elements into its ScVal form.
    ScMapFromHostMap = 28,
    // Cost of work needed to collect elements from an ScVec into a HostVec. This does not account for the
    // conversion of the elements from its ScVal form.
    ScVecToHostVec = 29,
    // Cost of work needed to collect elements from an ScMap into a HostMap. This does not account for the
    // conversion of the elements from its ScVal form.
    ScMapToHostMap = 30,
    // Cost of guarding a frame, which involves pushing and poping a frame and capturing a rollback point.
    GuardFrame = 31,
    // Cost of cloning a VM.
    CloneVm = 32,
    // Cost of verifying ed25519 signature of a payload.
    VerifyEd25519Sig = 33,
    //TODO: 34-47 to be done after we decide on the bigint library.
    // Cost of creating a new bigint.
    BigIntNew = 34,
    // Cost of bigint's add, sub ops.
    BigIntAddSub = 35,
    // Cost of bigint's mul op.
    BigIntMul = 36,
    // Cost of bigint's div and rem ops.
    BigIntDivRem = 37,
    // Cost of bigint's bitwise ops (and, or, xor).
    BigIntBitwiseOp = 38,
    // Cost of bigint's shl and shr ops.
    BigIntShift = 39,
    // Cost of bigint's cmp op.
    BigIntCmp = 40,
    // Cost of bigint's gcd, lcm ops.
    BigIntGcdLcm = 41,
    // Cost of bigint's pow op.
    BigIntPow = 42,
    // Cost of bigint's powmod op.
    BigIntPowMod = 43,
    // Cost of bigint's sqrt op.
    BigIntSqrt = 44,
    // Cost of constructing a bigint from bytes.
    BigIntFromBytes = 45,
    // Cost of constructing bytes from a bigint.
    BigIntToBytes = 46,
    // Cost of converting a bigint to a byte array where each byte is a digit in some pre-determined base.
    BigIntToRadix = 47,
    // Cost of accessing vm lineary memory
    VmMemCpy = 48,
    // Cost of instantiation a VM from wasm bytes code.
    VmInstantiation = 49,
    // The overhead cost of invoking a host function.
    // TODO: this is somewhat duplicate from HostFunction
    VmInvokeFunction = 50,
    // Cost of cloning bytes.
    BytesClone = 51,
    // Cost of deleting a byte from a bytes array,
    BytesDel = 52,
    // Cost of pushing a byte
    BytesPush = 53,
    // Cost of poping a byte
    BytesPop = 54,
    // Cost of inserting a byte into a bytes array at some index
    BytesInsert = 55,
    // Cost of appending a byte to the end of a bytes array
    BytesAppend = 56,
    // Cost of slicing a bytes array
    BytesSlice = 57,
    // Cost of concatenating two bytes arrays
    BytesConcat = 58,
    // Cost of unpacking args from a HostVec into a Vec<RawVal>
    CallArgsUnpack = 59,
    // Cost of charging a value to the budgeting system.
    ChargeBudget = 60,
    // TODO: reshuffle to bring the "AllocCell" types together
    // Cost of pushing a new `ContractCode` object to the object storage.
    HostContractCodeAllocCell = 61,
    // Cost of pushing a new `AccountId` object to the object storage.
    HostAccountIdAllocCell = 62,
    ImMapCmp = 63,
    ImVecCmp = 64,
    BytesCmp = 65,
    // Cost of a 25519 scalar multiplication in the Ed25519 library,
    // here for exploring calibration, not a long-term cost we surface
    // separately from signature verification.
    EdwardsPointCurve25519ScalarMul = 66,
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
            CostType::HostBytesAllocCell,
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
            CostType::BytesConcat,
            CostType::CallArgsUnpack,
            CostType::ChargeBudget,
            CostType::HostContractCodeAllocCell,
            CostType::HostAccountIdAllocCell,
            CostType::ImMapCmp,
            CostType::ImVecCmp,
            CostType::BytesCmp,
            CostType::EdwardsPointCurve25519ScalarMul,
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
}

#[derive(Default, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Budget(pub(crate) Rc<RefCell<BudgetImpl>>);

impl Budget {
    // Helper function to avoid multiple borrow_mut
    fn mut_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(RefMut<BudgetImpl>) -> Result<T, HostError>,
    {
        f(self.0.borrow_mut())
    }

    pub fn charge(&self, ty: CostType, input: u64) -> Result<(), HostError> {
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

    #[cfg(test)]
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
        };

        for ct in CostType::variants() {
            b.inputs.push(0);

            let cpu = &mut b.cpu_insns.get_cost_model_mut(*ct);
            match ct {
                // We might want to split wasm insns into separate cases; some are much more than
                // this and some are much less.
                CostType::WasmInsnExec => cpu.lin_param = 32,
                CostType::WasmMemAlloc => cpu.lin_param = 1000,
                CostType::HostEventDebug | CostType::HostEventContract | CostType::HostFunction => {
                    cpu.const_param = 1000
                }
                CostType::VisitObject | CostType::PushFrame | CostType::PopFrame => {
                    cpu.const_param = 100
                }
                CostType::ValXdrConv | CostType::ValSer | CostType::ValDeser => cpu.lin_param = 10,
                CostType::CloneEvents => cpu.lin_param = 10,
                CostType::HostObjAllocSlot => cpu.const_param = 1000,
                CostType::HostVecAllocCell => cpu.lin_param = 300,
                CostType::HostMapAllocCell => {
                    cpu.const_param = 2000;
                    cpu.log_base_param = 64;
                    cpu.log_param = 1;
                }
                CostType::HostU64AllocCell | CostType::HostI64AllocCell => (), // counted in HostObjAllocSlot
                CostType::HostBytesAllocCell => cpu.lin_param = 10,
                CostType::HostBigIntAllocCell => cpu.lin_param = 10,

                CostType::ComputeSha256Hash => {
                    cpu.const_param = 3000;
                    cpu.lin_param = 50;
                }

                CostType::ComputeEd25519PubKey => cpu.const_param = 40_000,
                CostType::VerifyEd25519Sig => {
                    cpu.const_param = 1000;
                    cpu.lin_param = 35;
                }

                CostType::ImMapNew => cpu.const_param = 2000,
                CostType::ImMapMutEntry | CostType::ImMapImmutEntry => {
                    cpu.const_param = 2000;
                    cpu.log_base_param = 64;
                    cpu.log_param = 10;
                }

                CostType::ImVecNew => cpu.const_param = 1500,
                CostType::ImVecMutEntry | CostType::ImVecImmutEntry => cpu.const_param = 300,
                CostType::ScVecFromHostVec => cpu.lin_param = 10,
                CostType::ScMapFromHostMap => cpu.lin_param = 10,
                CostType::ScVecToHostVec => cpu.lin_param = 300,
                CostType::ScMapToHostMap => cpu.lin_param = 2500,
                CostType::GuardFrame => cpu.lin_param = 10,
                CostType::CloneVm => cpu.const_param = 1000,

                CostType::BigIntNew
                | CostType::BigIntAddSub
                | CostType::BigIntBitwiseOp
                | CostType::BigIntShift
                | CostType::BigIntCmp => cpu.lin_param = 10,

                CostType::BigIntGcdLcm
                | CostType::BigIntPow
                | CostType::BigIntPowMod
                | CostType::BigIntMul
                | CostType::BigIntDivRem
                | CostType::BigIntSqrt => cpu.quad_param = 10,

                CostType::BigIntFromBytes => cpu.lin_param = 10,
                CostType::BigIntToBytes => cpu.lin_param = 10,
                CostType::BigIntToRadix => cpu.lin_param = 10,

                CostType::VmMemCpy => cpu.lin_param = 10,
                CostType::VmInstantiation => cpu.const_param = 100_000,
                CostType::VmInvokeFunction => cpu.const_param = 10_000,
                CostType::BytesClone
                | CostType::BytesDel
                | CostType::BytesPush
                | CostType::BytesPop
                | CostType::BytesInsert
                | CostType::BytesAppend
                | CostType::BytesSlice
                | CostType::BytesConcat => cpu.lin_param = 10,
                CostType::CallArgsUnpack => cpu.lin_param = 10,
                CostType::ChargeBudget => cpu.const_param = 50,
                CostType::HostContractCodeAllocCell => cpu.lin_param = 10,
                CostType::HostAccountIdAllocCell => cpu.lin_param = 320,
                CostType::ImMapCmp => cpu.lin_param = 10,
                CostType::ImVecCmp => cpu.lin_param = 10,
                CostType::BytesCmp => cpu.lin_param = 10,
                CostType::EdwardsPointCurve25519ScalarMul => cpu.const_param = 10,
            }

            let mem = b.mem_bytes.get_cost_model_mut(*ct);
            match ct {
                CostType::WasmInsnExec => (),
                CostType::WasmMemAlloc => mem.lin_param = 1,
                CostType::HostEventDebug | CostType::HostEventContract => mem.const_param = 100,
                CostType::HostFunction
                | CostType::VisitObject
                | CostType::PushFrame
                | CostType::PopFrame => mem.const_param = 100,
                CostType::ValXdrConv | CostType::ValSer | CostType::ValDeser => mem.lin_param = 1,
                CostType::CloneEvents => mem.lin_param = 100,
                CostType::HostObjAllocSlot => mem.const_param = 100,
                CostType::HostVecAllocCell
                | CostType::HostMapAllocCell
                | CostType::HostU64AllocCell
                | CostType::HostI64AllocCell => (), // counted in ObjAllocSlot
                CostType::HostBytesAllocCell => mem.lin_param = 1,
                CostType::HostBigIntAllocCell => mem.lin_param = 1,
                CostType::ComputeSha256Hash | CostType::ComputeEd25519PubKey => (),
                CostType::ImMapNew => mem.const_param = 3000,
                CostType::ImMapMutEntry | CostType::ImMapImmutEntry => (),
                CostType::ImVecNew => mem.const_param = 100,
                CostType::ImVecMutEntry | CostType::ImVecImmutEntry => (),
                CostType::ScVecFromHostVec
                | CostType::ScMapFromHostMap
                | CostType::ScVecToHostVec
                | CostType::ScMapToHostMap => mem.lin_param = 100,
                CostType::GuardFrame | CostType::CloneVm => mem.const_param = 100,
                CostType::VerifyEd25519Sig => (),
                CostType::BigIntNew
                | CostType::BigIntAddSub
                | CostType::BigIntMul
                | CostType::BigIntDivRem
                | CostType::BigIntBitwiseOp
                | CostType::BigIntShift
                | CostType::BigIntCmp
                | CostType::BigIntGcdLcm
                | CostType::BigIntPow
                | CostType::BigIntPowMod
                | CostType::BigIntSqrt
                | CostType::BigIntFromBytes
                | CostType::BigIntToBytes
                | CostType::BigIntToRadix => mem.lin_param = 10,
                CostType::VmMemCpy
                | CostType::VmInstantiation
                | CostType::VmInvokeFunction
                | CostType::BytesClone
                | CostType::BytesDel
                | CostType::BytesPush
                | CostType::BytesPop
                | CostType::BytesInsert
                | CostType::BytesAppend
                | CostType::BytesSlice
                | CostType::BytesConcat => mem.lin_param = 1,
                CostType::CallArgsUnpack | CostType::ChargeBudget => (),
                CostType::HostContractCodeAllocCell => mem.lin_param = 1,
                CostType::HostAccountIdAllocCell => mem.lin_param = 32,
                CostType::ImMapCmp => mem.lin_param = 1,
                CostType::ImVecCmp => mem.lin_param = 1,
                CostType::BytesCmp => mem.lin_param = 1,
                CostType::EdwardsPointCurve25519ScalarMul => mem.const_param = 1,
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
