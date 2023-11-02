use soroban_env_host::{Host, Symbol, SymbolSmall, budget::AsBudget, Env};
use soroban_synth_wasm::{Arity, LocalRef, ModEmitter, Operand};

pub fn wasm_module_with_linear_memory_logging() -> Vec<u8> {
    let mut me = ModEmitter::new();
    // log_from_linear_memory
    let f0 = me.import_func("x", "_", Arity(4));
    // the caller
    let mut fe = me.func(Arity(4), 0);
    fe.push(Operand::Local(LocalRef(0)));
    fe.push(Operand::Local(LocalRef(1)));
    fe.push(Operand::Local(LocalRef(2)));
    fe.push(Operand::Local(LocalRef(3)));
    fe.call_func(f0);
    fe.drop();
    // If I swap out the two lines below, it complains about Symbol not able to convert into Operand
    fe.push(Operand::Const64(0));
    // fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}
