use soroban_env_host::*;
use std::io::Read;

pub fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("usage: run_contract <filename.wasm>")
    }
    println!("running function `main()` in contract {}", args[1]);
    let host = Host::default();
    host.get_budget(|b| b.reset_unlimited());
    let hash = xdr::Hash([0; 32]);
    let mut file = std::fs::File::open(args[1].as_str()).expect("file");
    let mut wasm_code: Vec<u8> = Vec::new();
    file.read_to_end(&mut wasm_code).expect("read");
    let vm = Vm::new(&host, hash, wasm_code.as_slice()).expect("Vm::new");
    let args: xdr::VecM<xdr::ScVal, { xdr::SCVAL_LIMIT as u32 }> =
        Vec::new().try_into().expect("args");
    let before = std::time::Instant::now();
    vm.invoke_function(&host, "main", &args.into())
        .expect("invoke");
    let after = std::time::Instant::now();
    let dur_usecs = after.duration_since(before).as_micros() as u64;
    println!("function executed for {} usecs", dur_usecs);
    println!("budget measured inputs:");
    for i in budget::CostType::variants() {
        let n = host.get_budget(|b| b.get_input(*i));
        println!("    {:?}: {}", i, n);
    }
    let cpu_insns = host.get_budget(|b| b.get_cpu_insns_count());
    println!("budget model calculated cpu insns: {}", cpu_insns);
    println!(
        "budget model calculated mem bytes: {}",
        host.get_budget(|b| b.get_mem_bytes_count())
    );
    // a 1Ghz machine at 2 insns / cycle retires 2bn insns / sec, or 2000 insns / usec.
    let insn_per_usec = cpu_insns / dur_usecs;
    let freq = insn_per_usec as f64 / 2000.0;
    println!("estimated CPU frequency (at ~2 insns / cycle): {}Ghz", freq);
}
