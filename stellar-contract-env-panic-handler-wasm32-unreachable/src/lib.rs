#![cfg_attr(target_family = "wasm", no_std)]

#[cfg(target_family = "wasm")]
#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

