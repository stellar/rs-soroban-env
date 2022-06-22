#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CostLimits {
    pub mem_byte_limit: u64,
    pub cpu_insn_limit: u64,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CostFactors {
    pub cpu_insn_per_wasm_insn: u64,
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EventCounts {
    pub wasm_insns: u64,
    pub wasm_linear_memory_bytes: u64,
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Budget {
    pub cost_limits: CostLimits,
    pub cost_factors: CostFactors,
    pub event_counts: EventCounts,
}

impl Budget {
    pub fn cpu_limit_exceeded(&self) -> bool {
        self.event_counts
            .wasm_insns
            .saturating_mul(self.cost_factors.cpu_insn_per_wasm_insn)
            > self.cost_limits.cpu_insn_limit
    }
    pub fn mem_limit_exceeded(&self) -> bool {
        self.event_counts.wasm_linear_memory_bytes > self.cost_limits.mem_byte_limit
    }
}

impl Default for CostFactors {
    fn default() -> Self {
        Self {
            // Note: This was empirically calibrated very crudely on an x64
            // system based on the `calibrate_wasm_insns` benchmark, using:
            //
            //  $ cargo bench --features vm calibrate_wasm_insns -- --nocapture
            //
            // It should be explored in more detail and stored / retrieved on
            // chain in a CONFIG_SETTING LE.
            cpu_insn_per_wasm_insn: 73,
        }
    }
}

impl Default for CostLimits {
    fn default() -> Self {
        // Some "reasonable defaults": 640k of RAM and 100usec.
        //
        // We don't run for a time unit thought, we run for an estimated
        // (calibrated) number of CPU instructions.
        //
        // Assuming 2ghz chips at 2 instructions per cycle, we can guess about
        // 4bn instructions / sec. So about 4000 instructions per usec, or 400k
        // instructions in a 100usec time budget, or about 5479 wasm instructions
        // using the calibration above. Very roughly!
        Self {
            mem_byte_limit: 0xa_0000,
            cpu_insn_limit: 400_000,
        }
    }
}
