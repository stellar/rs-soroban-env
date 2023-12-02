#![no_main]
use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
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

fuzz_target!(|cfg_module: wasm_smith::ConfiguredModule<WasmiConfig>| {
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
    let module = Module::new(store.engine(), wasm.as_slice()).unwrap();
    let Ok(preinstance) = linker.instantiate(&mut store, &module) else {
        return;
    };
    let Ok(instance) = preinstance.ensure_no_start(&mut store) else {
        return;
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
});
