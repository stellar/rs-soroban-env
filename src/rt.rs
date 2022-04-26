// provide bits of rust's runtime interface: allocator, panic handling, etc.

#[cfg(all(target_family = "wasm", not(feature = "host_context")))]
#[inline(always)]
pub fn trap() -> ! {
    core::arch::wasm32::unreachable()
}

#[cfg(any(not(target_family = "wasm"), feature = "host_context"))]
pub fn trap() -> ! {
    panic!()
}

#[cfg(all(target_family = "wasm", not(feature = "host_context")))]
#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    trap();
}
