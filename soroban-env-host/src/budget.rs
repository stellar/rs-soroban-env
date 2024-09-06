mod dimension;
mod limits;
mod model;
mod util;
mod wasmi_helper;

pub(crate) use limits::DepthLimiter;
pub use limits::{DEFAULT_HOST_DEPTH_LIMIT, DEFAULT_XDR_RW_LIMITS};
pub use model::{MeteredCostComponent, ScaledU64};
pub(crate) use wasmi_helper::{get_wasmi_config, load_calibrated_fuel_costs};

use std::{
    cell::{RefCell, RefMut},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    host::error::TryBorrowOrErr,
    xdr::{ContractCostParams, ContractCostType, ScErrorCode, ScErrorType},
    Error, Host, HostError,
};

use dimension::{BudgetDimension, IsCpu, IsShadowMode};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct CostTracker {
    pub iterations: u64,
    pub inputs: Option<u64>,
    pub cpu: u64,
    pub mem: u64,
}

#[derive(Clone)]
struct BudgetTracker {
    // Tracker for each `CostType`
    cost_trackers: [CostTracker; ContractCostType::variants().len()],
    // Total number of times the meter is called
    meter_count: u32,
    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    wasm_memory: u64,
    // Tracks the real time (in nsecs) spent on various `CostType`
    time_tracker: [u64; ContractCostType::variants().len()],
}

impl Default for BudgetTracker {
    fn default() -> Self {
        let mut mt = Self {
            cost_trackers: [CostTracker::default(); ContractCostType::variants().len()],
            meter_count: 0,
            #[cfg(any(test, feature = "testutils", feature = "bench"))]
            wasm_memory: 0,
            time_tracker: [0; ContractCostType::variants().len()],
        };
        for (ct, tracker) in ContractCostType::variants()
            .iter()
            .zip(mt.cost_trackers.iter_mut())
        {
            // Define what inputs actually mean. For any constant-cost types --
            // whether it is a true constant unit cost type, or empirically
            // assigned (via measurement) constant type -- we leave the input as
            // `None`, otherwise, we initialize the input to `Some(0)``.
            let mut init_input = || tracker.inputs = Some(0);
            match ct {
                ContractCostType::WasmInsnExec => (),
                ContractCostType::MemAlloc => init_input(), // number of bytes in host memory to allocate
                ContractCostType::MemCpy => init_input(),   // number of bytes in host to copy
                ContractCostType::MemCmp => init_input(),   // number of bytes in host to compare
                ContractCostType::DispatchHostFunction => (),
                ContractCostType::VisitObject => (),
                // The inputs for `ValSer` and `ValDeser` are subtly different:
                // `ValSer` works recursively via `WriteXdr`, and each leaf call charges the budget,
                // and the input is the number of bytes of a leaf entity.
                // `ValDeser` charges the budget at the top level. Call to `read_xdr` works through
                // the bytes buffer recursively without worrying about budget charging. So the input
                // is the length of the total buffer.
                // This has implication on how their calibration should be set up.
                ContractCostType::ValSer => init_input(), // number of bytes in the result buffer
                ContractCostType::ValDeser => init_input(), // number of bytes in the input buffer
                ContractCostType::ComputeSha256Hash => init_input(), // number of bytes in the buffer
                ContractCostType::ComputeEd25519PubKey => (),
                ContractCostType::VerifyEd25519Sig => init_input(), // length of the signed message
                ContractCostType::VmInstantiation => init_input(),  // length of the wasm bytes,
                ContractCostType::VmCachedInstantiation => init_input(), // length of the wasm bytes,
                ContractCostType::InvokeVmFunction => (),
                ContractCostType::ComputeKeccak256Hash => init_input(), // number of bytes in the buffer
                ContractCostType::DecodeEcdsaCurve256Sig => (),
                ContractCostType::RecoverEcdsaSecp256k1Key => (),
                ContractCostType::Int256AddSub => (),
                ContractCostType::Int256Mul => (),
                ContractCostType::Int256Div => (),
                ContractCostType::Int256Pow => (),
                ContractCostType::Int256Shift => (),
                ContractCostType::ChaCha20DrawBytes => init_input(), // number of random bytes to draw

                ContractCostType::ParseWasmInstructions => init_input(),
                ContractCostType::ParseWasmFunctions => init_input(),
                ContractCostType::ParseWasmGlobals => init_input(),
                ContractCostType::ParseWasmTableEntries => init_input(),
                ContractCostType::ParseWasmTypes => init_input(),
                ContractCostType::ParseWasmDataSegments => init_input(),
                ContractCostType::ParseWasmElemSegments => init_input(),
                ContractCostType::ParseWasmImports => init_input(),
                ContractCostType::ParseWasmExports => init_input(),
                ContractCostType::ParseWasmDataSegmentBytes => init_input(),
                ContractCostType::InstantiateWasmInstructions => (),
                ContractCostType::InstantiateWasmFunctions => init_input(),
                ContractCostType::InstantiateWasmGlobals => init_input(),
                ContractCostType::InstantiateWasmTableEntries => init_input(),
                ContractCostType::InstantiateWasmTypes => (),
                ContractCostType::InstantiateWasmDataSegments => init_input(),
                ContractCostType::InstantiateWasmElemSegments => init_input(),
                ContractCostType::InstantiateWasmImports => init_input(),
                ContractCostType::InstantiateWasmExports => init_input(),
                ContractCostType::InstantiateWasmDataSegmentBytes => init_input(),
                ContractCostType::Sec1DecodePointUncompressed => (),
                ContractCostType::VerifyEcdsaSecp256r1Sig => (),
                ContractCostType::Bls12381EncodeFp => (),
                ContractCostType::Bls12381DecodeFp => (),
                ContractCostType::Bls12381G1Validate => (),
                ContractCostType::Bls12381G2Validate => (),
                ContractCostType::Bls12381G1ProjectiveToAffine => (),
                ContractCostType::Bls12381G2ProjectiveToAffine => (),
                ContractCostType::Bls12381G1Add => (),
                ContractCostType::Bls12381G1Mul => (),
                ContractCostType::Bls12381G1Msm => init_input(), // input is number of (G1,Fr) pairs
                ContractCostType::Bls12381MapFpToG1 => (),
                ContractCostType::Bls12381HashToG1 => init_input(),
                ContractCostType::Bls12381G2Add => (),
                ContractCostType::Bls12381G2Mul => (),
                ContractCostType::Bls12381G2Msm => init_input(), // input is number of (G2,Fr) pairs
                ContractCostType::Bls12381MapFp2ToG2 => (),
                ContractCostType::Bls12381HashToG2 => init_input(),
                ContractCostType::Bls12381Pairing => init_input(), // input is number of (G1,G2) pairs
                ContractCostType::Bls12381FrFromU256 => (),
                ContractCostType::Bls12381FrToU256 => (),
                ContractCostType::Bls12381FrAddSub => (),
                ContractCostType::Bls12381FrMul => (),
                ContractCostType::Bls12381FrPow => init_input(), // input is number of bits in the u64 exponent excluding leading zeros
                ContractCostType::Bls12381FrInv => (),
            }
        }
        mt
    }
}

impl BudgetTracker {
    #[cfg(any(test, feature = "testutils", feature = "bench"))]
    fn reset(&mut self) {
        self.meter_count = 0;
        for tracker in &mut self.cost_trackers {
            tracker.iterations = 0;
            tracker.inputs = tracker.inputs.map(|_| 0);
            tracker.cpu = 0;
            tracker.mem = 0;
        }
        self.wasm_memory = 0;
    }

    fn track_time(&mut self, ty: ContractCostType, duration: u64) -> Result<(), HostError> {
        let t = self.time_tracker.get_mut(ty as usize).ok_or_else(|| {
            HostError::from(Error::from_type_and_code(
                ScErrorType::Budget,
                ScErrorCode::InternalError,
            ))
        })?;
        *t += duration;
        Ok(())
    }

    fn get_time(&self, ty: ContractCostType) -> Result<u64, HostError> {
        self.time_tracker
            .get(ty as usize)
            .map(|t| *t)
            .ok_or_else(|| (ScErrorType::Budget, ScErrorCode::InternalError).into())
    }
}

#[derive(Clone)]
pub(crate) struct BudgetImpl {
    cpu_insns: BudgetDimension,
    mem_bytes: BudgetDimension,
    /// For the purpose of calibration and reporting; not used for budget-limiting nor does it affect consensus
    tracker: BudgetTracker,
    is_in_shadow_mode: bool,
    fuel_costs: wasmi::FuelCosts,
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
        Ok(Self {
            cpu_insns: BudgetDimension::try_from_config(cpu_cost_params, cpu_limit)?,
            mem_bytes: BudgetDimension::try_from_config(mem_cost_params, mem_limit)?,
            tracker: BudgetTracker::default(),
            is_in_shadow_mode: false,
            fuel_costs: load_calibrated_fuel_costs(),
            depth_limit: DEFAULT_HOST_DEPTH_LIMIT,
        })
    }

    pub fn charge(
        &mut self,
        ty: ContractCostType,
        iterations: u64,
        input: Option<u64>,
    ) -> Result<(), HostError> {
        let tracker = self
            .tracker
            .cost_trackers
            .get_mut(ty as usize)
            .ok_or_else(|| HostError::from((ScErrorType::Budget, ScErrorCode::InternalError)))?;

        if !self.is_in_shadow_mode {
            // update tracker for reporting
            self.tracker.meter_count = self.tracker.meter_count.saturating_add(1);
            tracker.iterations = tracker.iterations.saturating_add(iterations);
            match (&mut tracker.inputs, input) {
                (None, None) => (),
                (Some(t), Some(i)) => *t = t.saturating_add(i.saturating_mul(iterations)),
                // internal logic error, a wrong cost type has been passed in
                _ => return Err((ScErrorType::Budget, ScErrorCode::InternalError).into()),
            };
        }

        let cpu_charged = self.cpu_insns.charge(
            ty,
            iterations,
            input,
            IsCpu(true),
            IsShadowMode(self.is_in_shadow_mode),
        )?;
        if !self.is_in_shadow_mode {
            tracker.cpu = tracker.cpu.saturating_add(cpu_charged);
        }
        self.cpu_insns
            .check_budget_limit(IsShadowMode(self.is_in_shadow_mode))?;

        let mem_charged = self.mem_bytes.charge(
            ty,
            iterations,
            input,
            IsCpu(false),
            IsShadowMode(self.is_in_shadow_mode),
        )?;
        if !self.is_in_shadow_mode {
            tracker.mem = tracker.mem.saturating_add(mem_charged);
        }
        self.mem_bytes
            .check_budget_limit(IsShadowMode(self.is_in_shadow_mode))
    }

    fn get_wasmi_fuel_remaining(&self) -> Result<u64, HostError> {
        let cpu_remaining = self.cpu_insns.get_remaining();
        let cost_model = self
            .cpu_insns
            .get_cost_model(ContractCostType::WasmInsnExec)?;
        let cpu_per_fuel = cost_model.const_term.max(1);
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
        Ok(cpu_remaining.checked_div(cpu_per_fuel).unwrap_or(0))
    }
}

/// Default settings for local/sandbox testing only. The actual operations will use parameters
/// read on-chain from network configuration via [`from_configs`] above.
impl Default for BudgetImpl {
    fn default() -> Self {
        let mut b = Self {
            cpu_insns: BudgetDimension::default(),
            mem_bytes: BudgetDimension::default(),
            tracker: Default::default(),
            is_in_shadow_mode: false,
            fuel_costs: load_calibrated_fuel_costs(),
            depth_limit: DEFAULT_HOST_DEPTH_LIMIT,
        };

        for ct in ContractCostType::variants() {
            // define the cpu cost model parameters
            let Ok(cpu) = b.cpu_insns.get_cost_model_mut(ct) else {
                continue;
            };
            match ct {
                // This is the host cpu insn cost per wasm "fuel". Every "base" wasm
                // instruction costs 1 fuel (by default), and some particular types of
                // instructions may cost additional amount of fuel based on
                // wasmi's config setting.
                ContractCostType::WasmInsnExec => {
                    cpu.const_term = 4;
                    cpu.lin_term = ScaledU64(0);
                }
                // We don't have a clear way of modeling the linear term of
                // memalloc cost thus we choose a reasonable upperbound which is
                // same as other mem ops.
                ContractCostType::MemAlloc => {
                    cpu.const_term = 434;
                    cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8);
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
                    cpu.const_term = 42;
                    cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8);
                }
                ContractCostType::MemCmp => {
                    cpu.const_term = 44;
                    cpu.lin_term = ScaledU64::from_unscaled_u64(1).safe_div(8);
                }
                ContractCostType::DispatchHostFunction => {
                    cpu.const_term = 310;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::VisitObject => {
                    cpu.const_term = 61;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ValSer => {
                    cpu.const_term = 230;
                    cpu.lin_term = ScaledU64(29);
                }
                ContractCostType::ValDeser => {
                    cpu.const_term = 59052;
                    cpu.lin_term = ScaledU64(4001);
                }
                ContractCostType::ComputeSha256Hash => {
                    cpu.const_term = 3738;
                    cpu.lin_term = ScaledU64(7012);
                }
                ContractCostType::ComputeEd25519PubKey => {
                    cpu.const_term = 40253;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::VerifyEd25519Sig => {
                    cpu.const_term = 377524;
                    cpu.lin_term = ScaledU64(4068);
                }
                // VmInstantiation, VmCachedInstantiation and InvokeVmFunction
                // should all come from _eager_ calibration (i.e. _without_
                // CHECK_LAZY_COMPILATION_COSTS=1). The rationale here is as
                // follows:
                //
                // 1) VmInstantiation: this is used for the parsing-half of
                //    processing a Wasm we don't have a refined cost input model
                //    for, which happens on initial upload _or_ running an old
                //    pre-p21 contract that hasn't been reuploaded.
                //
                //     1.a) In the upload case, we run the parse-and-instantate
                //          in eager mode, because we want to fully validate the
                //          contract before accepting it. So this _has_ to be
                //          charged the eager / expensive cost.
                //
                //     1.b) In the old-contract case, we run the
                //          parse-and-instantiate in lazy mode because that's
                //          what we always do in p22, so we'll be overcharging.
                //          But we have no choice here: we don't have a refined
                //          input model to base a cheaper / lazy charge on (by
                //          definition) and we don't have calibration of the
                //          worst-case costs of lazy compilation "by byte count"
                //          and it would probably be about as bad as the eager
                //          case anyways because the Wasm could be "the
                //          pathological case" where every byte declares a new
                //          function and the function bodies (that lazy mode
                //          gets to elide processing) are all trivial. The only
                //          "choice" we could make here would be to potentially
                //          _run_ this case eagerly instead of lazily -- to have
                //          the real costs match the model costs -- but that
                //          would be weird: we'd be "artificially running slower
                //          in reality in order to avoid disagreeing with the
                //          overcharge happening in the model". Overcharging
                //          while running lazily seems like the least-bad case,
                //          and can be fixed by reuploading a contract.
                //
                // 2) VmCachedInstantiation: this is used for the
                //    instantiation-half of both cases above, and has the same
                //    rationale for both. The only way it differs is that we
                //    "parse" (VmInstantiation) once and instantiate
                //    (VmCachedInstantiation) potentially multiple times within
                //    a tx, and also coincidentally the calibration we have for
                //    VmCachedInstantiation produces almost identical numbers
                //    between lazy and eager modes anyways.
                //
                // 3) InvokeVmFunction: this is used for the _invocation_, from
                //    the host, of each _function_ in a single
                //    parsed-and-instantiated VM. The calibration we have is
                //    _wrong_ if you run it in lazy mode, as it'll artificially
                //    charge the cost of wasmi doing lazy compilation of the
                //    function. That's double charging (since wasmi charges this
                //    itself inside its gas accounting), but it'd actually be
                //    _worse_ than double charging because wasmi only charges on
                //    first call to a given function and we'd be charging on
                //    _every_ call. So instead we calibrate in eager mode,
                //    manually confirm the linear term is zero (it sure should
                //    be!) and hope that the the constant term is mostly the
                //    same as lazy, and that any difference is small enough to
                //    be lost in the noise of the other costs (parsing,
                //    instantiation, lazy compilation, etc).
                ContractCostType::VmInstantiation => {
                    cpu.const_term = 31271;
                    cpu.lin_term = ScaledU64(57504);
                }
                ContractCostType::VmCachedInstantiation => {
                    cpu.const_term = 40828;
                    cpu.lin_term = ScaledU64(680);
                }
                ContractCostType::InvokeVmFunction => {
                    cpu.const_term = 2149;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeKeccak256Hash => {
                    cpu.const_term = 3766;
                    cpu.lin_term = ScaledU64(5969);
                }
                ContractCostType::DecodeEcdsaCurve256Sig => {
                    cpu.const_term = 710;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::RecoverEcdsaSecp256k1Key => {
                    cpu.const_term = 2315295;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256AddSub => {
                    cpu.const_term = 4404;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Mul => {
                    cpu.const_term = 4947;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Div => {
                    cpu.const_term = 4911;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Pow => {
                    cpu.const_term = 4286;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Shift => {
                    cpu.const_term = 913;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::ChaCha20DrawBytes => {
                    cpu.const_term = 1058;
                    cpu.lin_term = ScaledU64(501);
                }
                // All the other parsing and instantiation costs should come
                // from _lazy_ calibration (i.e. _with_
                // CHECK_LAZY_COMPILATION_COSTS=1). The rationale here is as
                // follows:
                //
                //   - These cost types are only charged on paths where we're
                //     sure we're running in lazy mode (i.e. not the eager
                //     intial-upload case). So lazily-calibrated numbers are
                //     _correct_ to charge. But they still might not be
                //     _advantageous_.
                //
                //   - Lazily-calibrated numbers are _advantageous_ to charge
                //     for _these_ cost types (rather than the older cost types)
                //     because these cost types allow us to identify and
                //     charge-less for the one (fairly common) "good case" where
                //     lazy compilation is a big win in reality: when the module
                //     has a smallish number of functions relative to the number
                //     of instructions. Then the cheap cost charged for a
                //     lazy-parse of the instructions (100x cheaper than eager)
                //     isn't swamped by the still-relatively-high per-function
                //     cost of a lazy-parse of the functions. This case wouldn't
                //     be possible to identify if we only had the coarse
                //     byte-count input model -- we'd have to pessimistically
                //     charge as if it were "the pathological Wasm" -- but we
                //     have the refined input model here, so we should use it.
                ContractCostType::ParseWasmInstructions => {
                    cpu.const_term = 37421;
                    cpu.lin_term = ScaledU64(32);
                }
                ContractCostType::ParseWasmFunctions => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(84156);
                }
                ContractCostType::ParseWasmGlobals => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(163415);
                }
                ContractCostType::ParseWasmTableEntries => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(29644);
                }
                ContractCostType::ParseWasmTypes => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(893113);
                }
                ContractCostType::ParseWasmDataSegments => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(184921);
                }
                ContractCostType::ParseWasmElemSegments => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(312369);
                }
                ContractCostType::ParseWasmImports => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(529255);
                }
                ContractCostType::ParseWasmExports => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(361665);
                }
                ContractCostType::ParseWasmDataSegmentBytes => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(14);
                }
                ContractCostType::InstantiateWasmInstructions => {
                    cpu.const_term = 43208;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::InstantiateWasmFunctions => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(8050);
                }
                ContractCostType::InstantiateWasmGlobals => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(10647);
                }
                ContractCostType::InstantiateWasmTableEntries => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(1933);
                }
                ContractCostType::InstantiateWasmTypes => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::InstantiateWasmDataSegments => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(17164);
                }
                ContractCostType::InstantiateWasmElemSegments => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(34261);
                }
                ContractCostType::InstantiateWasmImports => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(746142);
                }
                ContractCostType::InstantiateWasmExports => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(296177);
                }
                ContractCostType::InstantiateWasmDataSegmentBytes => {
                    cpu.const_term = 0;
                    cpu.lin_term = ScaledU64(14);
                }
                ContractCostType::Sec1DecodePointUncompressed => {
                    cpu.const_term = 1882;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::VerifyEcdsaSecp256r1Sig => {
                    cpu.const_term = 3000906;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381EncodeFp => {
                    cpu.const_term = 661;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381DecodeFp => {
                    cpu.const_term = 985;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Validate => {
                    cpu.const_term = 732301;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Validate => {
                    cpu.const_term = 1063432;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1ProjectiveToAffine => {
                    cpu.const_term = 92642;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2ProjectiveToAffine => {
                    cpu.const_term = 100742;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Add => {
                    cpu.const_term = 7689;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Mul => {
                    cpu.const_term = 2458985;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Msm => {
                    cpu.const_term = 2426722;
                    cpu.lin_term = ScaledU64(96397671);
                }
                ContractCostType::Bls12381MapFpToG1 => {
                    cpu.const_term = 1541554;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381HashToG1 => {
                    cpu.const_term = 3211191;
                    cpu.lin_term = ScaledU64(6713);
                }
                ContractCostType::Bls12381G2Add => {
                    cpu.const_term = 25207;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Mul => {
                    cpu.const_term = 7873219;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Msm => {
                    cpu.const_term = 8035968;
                    cpu.lin_term = ScaledU64(309667335);
                }
                ContractCostType::Bls12381MapFp2ToG2 => {
                    cpu.const_term = 2420202;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381HashToG2 => {
                    cpu.const_term = 7050564;
                    cpu.lin_term = ScaledU64(6797);
                }
                ContractCostType::Bls12381Pairing => {
                    cpu.const_term = 10558948;
                    cpu.lin_term = ScaledU64(632860943);
                }
                ContractCostType::Bls12381FrFromU256 => {
                    cpu.const_term = 1994;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrToU256 => {
                    cpu.const_term = 1155;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrAddSub => {
                    cpu.const_term = 74;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrMul => {
                    cpu.const_term = 332;
                    cpu.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrPow => {
                    cpu.const_term = 691;
                    cpu.lin_term = ScaledU64(74558);
                }
                ContractCostType::Bls12381FrInv => {
                    cpu.const_term = 35421;
                    cpu.lin_term = ScaledU64(0);
                }
            }

            // define the memory cost model parameters
            let Ok(mem) = b.mem_bytes.get_cost_model_mut(ct) else {
                continue;
            };
            match ct {
                // This type is designated to the cpu cost. By definition, the memory cost
                // of a (cpu) fuel is zero.
                ContractCostType::WasmInsnExec => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::MemAlloc => {
                    mem.const_term = 16;
                    mem.lin_term = ScaledU64::from_unscaled_u64(1);
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
                // These are derived analytically but based on calibration on
                // highly nested xdr structures
                ContractCostType::ValSer => {
                    mem.const_term = 242;
                    mem.lin_term = ScaledU64::from_unscaled_u64(3);
                }
                ContractCostType::ValDeser => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64::from_unscaled_u64(3);
                }
                ContractCostType::ComputeSha256Hash => {
                    mem.const_term = 0;
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
                    mem.const_term = 130065;
                    mem.lin_term = ScaledU64(5064);
                }
                ContractCostType::VmCachedInstantiation => {
                    mem.const_term = 69472;
                    mem.lin_term = ScaledU64(1478);
                }
                ContractCostType::InvokeVmFunction => {
                    mem.const_term = 15;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ComputeKeccak256Hash => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::DecodeEcdsaCurve256Sig => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::RecoverEcdsaSecp256k1Key => {
                    mem.const_term = 181;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256AddSub => {
                    mem.const_term = 99;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Mul => {
                    mem.const_term = 99;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Div => {
                    mem.const_term = 99;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Pow => {
                    mem.const_term = 99;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Int256Shift => {
                    mem.const_term = 99;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ChaCha20DrawBytes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::ParseWasmInstructions => {
                    mem.const_term = 13980;
                    mem.lin_term = ScaledU64(215);
                }
                ContractCostType::ParseWasmFunctions => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(23056);
                }
                ContractCostType::ParseWasmGlobals => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(11924);
                }
                ContractCostType::ParseWasmTableEntries => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(6121);
                }
                ContractCostType::ParseWasmTypes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(49554);
                }
                ContractCostType::ParseWasmDataSegments => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(5525);
                }
                ContractCostType::ParseWasmElemSegments => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(47034);
                }
                ContractCostType::ParseWasmImports => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(101762);
                }
                ContractCostType::ParseWasmExports => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(35491);
                }
                ContractCostType::ParseWasmDataSegmentBytes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(129);
                }
                ContractCostType::InstantiateWasmInstructions => {
                    mem.const_term = 70792;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::InstantiateWasmFunctions => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(17749);
                }
                ContractCostType::InstantiateWasmGlobals => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(6833);
                }
                ContractCostType::InstantiateWasmTableEntries => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(1025);
                }
                ContractCostType::InstantiateWasmTypes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::InstantiateWasmDataSegments => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(129632);
                }
                ContractCostType::InstantiateWasmElemSegments => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(13665);
                }
                ContractCostType::InstantiateWasmImports => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(98578);
                }
                ContractCostType::InstantiateWasmExports => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(9176);
                }
                ContractCostType::InstantiateWasmDataSegmentBytes => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(126);
                }
                ContractCostType::Sec1DecodePointUncompressed => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::VerifyEcdsaSecp256r1Sig => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381EncodeFp => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381DecodeFp => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Validate => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Validate => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1ProjectiveToAffine => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2ProjectiveToAffine => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Add => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Mul => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G1Msm => {
                    mem.const_term = 109494;
                    mem.lin_term = ScaledU64(354667);
                }
                ContractCostType::Bls12381MapFpToG1 => {
                    mem.const_term = 5552;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381HashToG1 => {
                    mem.const_term = 9424;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Add => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Mul => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381G2Msm => {
                    mem.const_term = 219654;
                    mem.lin_term = ScaledU64(354667);
                }
                ContractCostType::Bls12381MapFp2ToG2 => {
                    mem.const_term = 3344;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381HashToG2 => {
                    mem.const_term = 6816;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381Pairing => {
                    mem.const_term = 2204;
                    mem.lin_term = ScaledU64(9340474);
                }
                ContractCostType::Bls12381FrFromU256 => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrToU256 => {
                    mem.const_term = 248;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrAddSub => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrMul => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
                ContractCostType::Bls12381FrPow => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(128);
                }
                ContractCostType::Bls12381FrInv => {
                    mem.const_term = 0;
                    mem.lin_term = ScaledU64(0);
                }
            }
        }

        // define the limits
        b.cpu_insns.reset(limits::DEFAULT_CPU_INSN_LIMIT);
        b.mem_bytes.reset(limits::DEFAULT_MEM_BYTES_LIMIT);
        b
    }
}

impl Debug for BudgetImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:=<175}", "")?;
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
        writeln!(f, "{:=<175}", "")?;
        writeln!(
            f,
            "{:<35}{:<15}{:<15}{:<15}{:<15}{:<20}{:<20}{:<20}{:<20}",
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
                "{:<35}{:<15}{:<15}{:<15}{:<15}{:<20}{:<20}{:<20}{:<20}",
                format!("{:?}", ct),
                self.tracker.cost_trackers[i].iterations,
                format!("{:?}", self.tracker.cost_trackers[i].inputs),
                self.tracker.cost_trackers[i].cpu,
                self.tracker.cost_trackers[i].mem,
                self.cpu_insns.cost_models[i].const_term,
                format!("{}", self.cpu_insns.cost_models[i].lin_term),
                self.mem_bytes.cost_models[i].const_term,
                format!("{}", self.mem_bytes.cost_models[i].lin_term),
            )?;
        }
        writeln!(f, "{:=<175}", "")?;
        writeln!(
            f,
            "Internal details (diagnostics info, does not affect fees) "
        )?;
        writeln!(
            f,
            "Total # times meter was called: {}",
            self.tracker.meter_count,
        )?;
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
        writeln!(f, "{:=<175}", "")?;
        Ok(())
    }
}

impl Display for BudgetImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:=<65}", "")?;
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
        writeln!(f, "{:=<65}", "")?;
        writeln!(
            f,
            "{:<35}{:<15}{:<15}",
            "CostType", "cpu_insns", "mem_bytes",
        )?;
        for ct in ContractCostType::variants() {
            let i = ct as usize;
            writeln!(
                f,
                "{:<35}{:<15}{:<15}",
                format!("{:?}", ct),
                self.tracker.cost_trackers[i].cpu,
                self.tracker.cost_trackers[i].mem,
            )?;
        }
        writeln!(f, "{:=<65}", "")?;
        Ok(())
    }
}

#[allow(unused)]
#[cfg(test)]
impl BudgetImpl {
    // Utility function for printing default budget cost parameters in cpp format
    // so that it can be ported into stellar-core.
    // When needing it, copy and run the following test
    // ```
    // #[test]
    // fn test() {
    //     let bi = BudgetImpl::default();
    //     bi.print_default_params_in_cpp();
    // }
    // ```
    // and copy the screen output.
    fn print_default_params_in_cpp(&self) {
        // cpu
        println!();
        println!();
        println!();
        for ct in ContractCostType::variants() {
            let Ok(cpu) = self.cpu_insns.get_cost_model(ct) else {
                continue;
            };
            println!("case {}:", ct.name());
            println!(
                "params[val] = ContractCostParamEntry{{ExtensionPoint{{0}}, {}, {}}};",
                cpu.const_term, cpu.lin_term.0
            );
            println!("break;");
        }
        // mem
        println!();
        println!();
        println!();
        for ct in ContractCostType::variants() {
            let Ok(mem) = self.mem_bytes.get_cost_model(ct) else {
                continue;
            };
            println!("case {}:", ct.name());
            println!(
                "params[val] = ContractCostParamEntry{{ExtensionPoint{{0}}, {}, {}}};",
                mem.const_term, mem.lin_term.0
            );
            println!("break;");
        }
        println!();
        println!();
        println!();
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

    // Helper function to avoid panics from multiple borrow_muts
    fn with_mut_budget<T, F>(&self, f: F) -> Result<T, HostError>
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

    /// Runs a user provided closure in shadow mode -- all metering is done
    /// through the shadow budget.
    ///
    /// Because shadow mode is _designed not to be observed_ (indeed it exists
    /// primarily to count actions against the shadow budget that are _optional_
    /// on a given host, such as debug logging, and that therefore must strictly
    /// must not be observed), any error that occurs during execution is
    /// swallowed.
    pub(crate) fn with_shadow_mode<T, F>(&self, f: F)
    where
        F: FnOnce() -> Result<T, HostError>,
    {
        let mut prev = false;

        if self
            .with_mut_budget(|mut b| {
                prev = b.is_in_shadow_mode;
                b.is_in_shadow_mode = true;
                b.cpu_insns.check_budget_limit(IsShadowMode(true))?;
                b.mem_bytes.check_budget_limit(IsShadowMode(true))
            })
            .is_ok()
        {
            let _ = f();
        }

        let _ = self.with_mut_budget(|mut b| {
            b.is_in_shadow_mode = prev;
            Ok(())
        });
    }

    pub(crate) fn is_in_shadow_mode(&self) -> Result<bool, HostError> {
        Ok(self.0.try_borrow_or_err()?.is_in_shadow_mode)
    }

    pub(crate) fn set_shadow_limits(&self, cpu: u64, mem: u64) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.cpu_insns.shadow_limit = cpu;
        self.0.try_borrow_mut_or_err()?.mem_bytes.shadow_limit = mem;
        Ok(())
    }

    pub(crate) fn ensure_shadow_cpu_limit_factor(&self, factor: u64) -> Result<(), HostError> {
        let mut b = self.0.try_borrow_mut_or_err()?;
        b.cpu_insns.shadow_limit = b.cpu_insns.limit.saturating_mul(factor);
        Ok(())
    }

    pub(crate) fn ensure_shadow_mem_limit_factor(&self, factor: u64) -> Result<(), HostError> {
        let mut b = self.0.try_borrow_mut_or_err()?;
        b.mem_bytes.shadow_limit = b.mem_bytes.limit.saturating_mul(factor);
        Ok(())
    }

    pub fn get_tracker(&self, ty: ContractCostType) -> Result<CostTracker, HostError> {
        self.0
            .try_borrow_or_err()?
            .tracker
            .cost_trackers
            .get(ty as usize)
            .map(|x| *x)
            .ok_or_else(|| (ScErrorType::Budget, ScErrorCode::InternalError).into())
    }

    pub fn get_time(&self, ty: ContractCostType) -> Result<u64, HostError> {
        self.0.try_borrow_or_err()?.tracker.get_time(ty)
    }

    pub fn track_time(&self, ty: ContractCostType, duration: u64) -> Result<(), HostError> {
        self.0
            .try_borrow_mut_or_err()?
            .tracker
            .track_time(ty, duration)
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

    pub fn reset_default(&self) -> Result<(), HostError> {
        *self.0.try_borrow_mut_or_err()? = BudgetImpl::default();
        Ok(())
    }
}

#[test]
fn test_budget_initialization() -> Result<(), HostError> {
    use crate::xdr::{ContractCostParamEntry, ExtensionPoint};
    let cpu_cost_params = ContractCostParams(
        vec![
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 35,
                linear_term: 36,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 37,
                linear_term: 38,
            },
        ]
        .try_into()
        .unwrap(),
    );
    let mem_cost_params = ContractCostParams(
        vec![
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 39,
                linear_term: 40,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 41,
                linear_term: 42,
            },
            ContractCostParamEntry {
                ext: ExtensionPoint::V0,
                const_term: 43,
                linear_term: 44,
            },
        ]
        .try_into()
        .unwrap(),
    );

    let budget = Budget::try_from_configs(100, 100, cpu_cost_params, mem_cost_params)?;
    assert_eq!(
        budget.0.try_borrow_or_err()?.cpu_insns.cost_models.len(),
        ContractCostType::variants().len()
    );
    assert_eq!(
        budget.0.try_borrow_or_err()?.mem_bytes.cost_models.len(),
        ContractCostType::variants().len()
    );
    assert_eq!(
        budget.0.try_borrow_or_err()?.tracker.cost_trackers.len(),
        ContractCostType::variants().len()
    );
    assert_eq!(
        budget.0.try_borrow_or_err()?.tracker.time_tracker.len(),
        ContractCostType::variants().len()
    );

    Ok(())
}
