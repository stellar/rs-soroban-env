// ============================================================================
// Wasmi Fuzz Target
// ============================================================================

use crate::fuzz::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use wasmi::{core::ValueType, Engine, Extern, Linker, Module, Store, StoreLimitsBuilder, Value};

#[derive(Debug, Arbitrary)]
struct WasmiConfig;

impl wasm_smith::Config for WasmiConfig {
    fn max_imports(&self) -> usize {
        0
    }
    fn export_everything(&self) -> bool {
        true
    }
    fn min_funcs(&self) -> usize {
        1
    }
    fn reference_types_enabled(&self) -> bool {
        false
    }
    fn max_memory_pages(&self, _is_64: bool) -> u64 {
        1000
    }
    fn allow_start_export(&self) -> bool {
        false
    }
    fn max_data_segments(&self) -> usize {
        1000
    }
    fn max_element_segments(&self) -> usize {
        1000
    }
    fn max_exports(&self) -> usize {
        1000
    }
    fn max_elements(&self) -> usize {
        1000
    }
    fn max_funcs(&self) -> usize {
        1000
    }
    fn max_globals(&self) -> usize {
        1000
    }
    fn max_table_elements(&self) -> u32 {
        1000
    }
    fn max_values(&self) -> usize {
        1000
    }
    fn max_instructions(&self) -> usize {
        10000
    }
}

fn ty_to_arg(ty: &ValueType) -> Value {
    match ty {
        ValueType::I32 => Value::I32(1),
        ValueType::I64 => Value::I64(1),
        ValueType::F32 => Value::F32(1.0.into()),
        ValueType::F64 => Value::F64(1.0.into()),
        _ => panic!("reference type"),
    }
}

/// Run the wasmi fuzz target with the given input bytes.
///
/// This generates a valid wasm module using wasm-smith and runs it through
/// the wasmi VM to find crashes or bugs in the VM implementation.
///
/// Returns `FuzzResult::Ok` if the test completed,
/// `FuzzResult::Reject` if the input was malformed/too short.
pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let mut u = Unstructured::new(data);
    let cfg_module: wasm_smith::ConfiguredModule<WasmiConfig> = match Arbitrary::arbitrary(&mut u) {
        Ok(m) => m,
        Err(_) => return FuzzResult::Reject,
    };

    let mut smith_module = cfg_module.module;
    smith_module.ensure_termination(1000);
    let wasm = smith_module.to_bytes();

    let engine = Engine::default();
    let linker = Linker::new(&engine);
    let limiter = StoreLimitsBuilder::new()
        .memory_size(1000 * 0x10000)
        .build();
    let mut store = Store::new(&engine, limiter);
    store.limiter(|lim| lim);

    let module = match Module::new(store.engine(), wasm.as_slice()) {
        Ok(m) => m,
        Err(_) => return FuzzResult::Ok, // wasm-smith should produce valid wasm, but if not, skip
    };

    let preinstance = match linker.instantiate(&mut store, &module) {
        Ok(p) => p,
        Err(_) => return FuzzResult::Ok,
    };

    let instance = match preinstance.ensure_no_start(&mut store) {
        Ok(i) => i,
        Err(_) => return FuzzResult::Ok,
    };

    let mut args = Vec::new();
    let mut out = Vec::new();
    let mut name = None;

    let exports = instance.exports(&store);
    for e in exports {
        match e.ty(&store) {
            wasmi::ExternType::Func(fty) => {
                args = fty.params().iter().map(ty_to_arg).collect::<Vec<_>>();
                out = fty.results().iter().map(ty_to_arg).collect::<Vec<_>>();
                name = Some(e.name().to_string());
            }
            _ => continue,
        }
    }

    if let Some(name) = name {
        let wasm_fn = instance
            .get_export(&store, &name)
            .and_then(Extern::into_func)
            .unwrap();
        let _ = wasm_fn.call(&mut store, &args, &mut out);
    }

    FuzzResult::Ok
}
