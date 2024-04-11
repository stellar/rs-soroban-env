// use soroban_env_host::Symbol;
use soroban_env_host::Symbol;
use soroban_synth_wasm::{Arity, LocalRef, ModEmitter, Operand};

pub fn wasm_module_with_linear_memory_logging() -> Vec<u8> {
    let mut me = ModEmitter::default_with_test_protocol();
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
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}
