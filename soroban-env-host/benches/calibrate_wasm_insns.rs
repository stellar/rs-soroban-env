// Run this with
// $ cargo bench --features vm calibrate_wasm_insns -- --nocapture

mod common;
use common::*;
use soroban_env_host::budget::CostType;
use soroban_env_host::xdr::{Hash, ScVal, ScVec};
use soroban_env_host::{Host, Vm};
use std::rc::Rc;

#[cfg(all(test, feature = "vm"))]
fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    use parity_wasm::builder;
    use parity_wasm::elements::{
        ExportEntry, Instruction,
        Instruction::{GetLocal, I64Add, I64Const, I64Mul},
        Instructions, Internal, ValueType,
    };
    let mut insns: Vec<Instruction> = Vec::new();
    insns.push(I64Const(1));
    for i in 0..n {
        insns.push(GetLocal(0));
        insns.push(I64Const(i as i64));
        insns.push(I64Mul);
        insns.push(I64Add);
    }
    insns.push(Instruction::Drop);
    insns.push(I64Const(0));
    insns.push(Instruction::End);
    let module = builder::module()
        .function()
        .signature()
        .with_params(vec![ValueType::I64])
        .with_result(ValueType::I64)
        .build()
        .body()
        .with_instructions(Instructions::new(insns))
        .build()
        .build()
        .with_export(ExportEntry::new("test".into(), Internal::Function(0)))
        .build();
    module.to_bytes().unwrap()
}

struct SyntheticWASMRun {
    args: ScVec,
    vm: Rc<Vm>,
}

impl HostCostMeasurement for SyntheticWASMRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;

    fn new(host: &Host, step: u64) -> Self {
        let args = ScVec(vec![ScVal::U63(5)].try_into().unwrap());
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_4n_insns((step as usize) * 10000);
        let vm = Vm::new(&host, id, &code).unwrap();
        Self { args, vm }
    }

    fn run(&mut self, host: &Host) {
        self.vm.invoke_function(host, "test", &self.args).unwrap();
    }
}

#[cfg(all(test, feature = "vm", any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    let mut measurements = measure_costs::<SyntheticWASMRun>(0..20)?;
    measurements.subtract_baseline();
    measurements.report();
    if std::env::var("FIT_MODELS").is_ok() {
        measurements.fit_model_to_cpu();
        measurements.fit_model_to_mem();
    }
    Ok(())
}
