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

//#[cfg(not(feature = "std"))]
//#[panic_handler]
//fn handle_panic(_: &core::panic::PanicInfo) -> ! {
//    trap();
//}
