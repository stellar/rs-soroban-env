use crate::{budget::Budget, HostError};

pub(crate) fn get_wasmtime_config(_budget: &Budget) -> Result<wasmtime::Config, HostError> {
    let mut config = wasmtime::Config::new();
    config
        .strategy(wasmtime::Strategy::Winch)
        .debug_info(false)
        .generate_address_map(false)
        .consume_fuel(true)
        .wasm_bulk_memory(true)
        .wasm_multi_value(false)
        .wasm_simd(false)
        .wasm_tail_call(false);
    Ok(config)
}
