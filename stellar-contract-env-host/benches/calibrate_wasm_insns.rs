// Run this with
// $ cargo bench --features vm calibrate_wasm_insns -- --nocapture

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

#[cfg(all(test, feature = "vm", target_os = "linux"))]
fn main() -> std::io::Result<()> {
    use std::io::Write;
    use std::time::Instant;
    use stellar_contract_env_host::xdr::{Hash, ScVal, ScVec};
    use stellar_contract_env_host::{Host, Vm};
    use tabwriter::{Alignment, TabWriter};

    let mut counter = perf_event::Builder::new().build()?;

    let host = Host::default();
    let scvec0: ScVec = ScVec(vec![ScVal::U63(5)].try_into().unwrap());

    let mut baseline = 0;

    let mut tw = TabWriter::new(vec![])
        .padding(5)
        .alignment(Alignment::Right);

    write!(
        &mut tw,
        "CPU insns\tusec\tCPU / usec\tCPU - baseline\twasm insns\tCPU / wasm\n"
    )
    .unwrap();

    for i in 0..20 {
        host.modify_budget(|budget| {
            budget.event_counts.wasm_insns = 0;
            budget.cost_factors.cpu_insn_per_wasm_insn = 1;
            budget.cost_limits.cpu_insns = 1000000000000;
        });

        let id: Hash = [0; 32].into();
        let code = wasm_module_with_4n_insns(i * 1000);
        let vm = Vm::new(&host, id, &code).unwrap();
        let start = Instant::now();
        counter.reset()?;
        counter.enable()?;
        vm.invoke_function(&host, "test", &scvec0).unwrap();
        counter.disable()?;
        let stop = Instant::now();
        let insns_total = counter.read()?;
        let usec = stop.duration_since(start).as_micros() as u64;
        if baseline == 0 {
            baseline = insns_total;
        }
        let wasms_total = host.modify_budget(|b| b.event_counts.wasm_insns);
        let wasm_per_insn = (insns_total - baseline) / wasms_total;
        write!(
            &mut tw,
            "{}\t{}us\t{}\t{}\t{}\t{}\n",
            insns_total,
            usec,
            insns_total / usec,
            (insns_total - baseline),
            wasms_total,
            wasm_per_insn
        )
        .unwrap();
    }
    tw.flush().unwrap();
    eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());
    Ok(())
}
