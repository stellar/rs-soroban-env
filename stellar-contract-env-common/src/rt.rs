// provide bits of rust's runtime interface: allocator, panic handling, etc.

#[cfg(all(target_family = "wasm", not(feature = "std")))]
#[inline(always)]
pub fn trap() -> ! {
    core::arch::wasm32::unreachable()
}

#[cfg(any(not(target_family = "wasm"), feature = "std"))]
pub fn trap() -> ! {
    panic!()
}

// Handle panic provides a panic handler that traps using the wasm unreachable
// opcode, which is less expensive than the builtin panic handling.
//
// Not used when std feature is enabled and when targeting wasm because std
// provides a panic handler. Note that std may be loaded even if the std feature
// is off, such as when tests are built. The guard on the wasm target prevents
// the handler from being loaded in those other cases. Note that adding a
// not(test) here would not be sufficient since not(test) is only true for the
// crate that the test resides in, and not for crates running tests dependent on
// this crate.
#[cfg(all(target_family = "wasm", not(feature = "std")))]
#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    trap();
}
