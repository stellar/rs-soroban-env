use crate::common::HostCostMeasurement;
use rand::rngs::StdRng;
use soroban_env_host::budget::CostType;
use soroban_env_host::xdr::{Hash, ScVal, ScVec};
use soroban_env_host::{Host, Vm};
use std::rc::Rc;

#[cfg(all(test, feature = "vm"))]
fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    use parity_wasm::builder;
    use parity_wasm::elements::{CustomSection, Section};
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
        .with_section(Section::Custom(CustomSection::new(
            "contractenvmetav0".to_string(),
            soroban_env_common::meta::XDR.to_vec(),
        )))
        .build();
    module.to_bytes().unwrap()
}

pub(crate) struct WasmInsnExecRun {
    insns: u64,
    args: ScVec,
    vm: Rc<Vm>,
}

// This measures the cost of executing a block of WASM instructions. The
// input value is the length of the instruction block. The CPU cost should
// be linear in the length and the memory should be zero.
impl HostCostMeasurement for WasmInsnExecRun {
    const COST_TYPE: CostType = CostType::WasmInsnExec;

    fn new_random_case(host: &Host, _rng: &mut StdRng, step: u64) -> Self {
        let insns = step * 1000;
        let args = ScVec(vec![ScVal::U63(5)].try_into().unwrap());
        let id: Hash = [0; 32].into();
        let code = wasm_module_with_4n_insns(insns as usize);
        let vm = Vm::new(&host, id, &code).unwrap();
        Self { insns, args, vm }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.insns
    }

    fn run(&mut self, _iter: u64, host: &Host) {
        self.vm.invoke_function(host, "test", &self.args).unwrap();
    }
}
