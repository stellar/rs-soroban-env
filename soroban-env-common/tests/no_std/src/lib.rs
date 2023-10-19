#![no_std]

/// This test crate simulates a package that imports soroban_env_common and adds
/// a panic handler. The importing package will be allowed to do that if no
/// panic handler has been already linked in. If soroban_env_common imports
/// libstd by accident then this crate will fail to build because there will be
/// two panic hanlders.

// Import a type from soroban_env_common so that the compiler includes it in the
// build.
#[allow(unused_imports)]
use soroban_env_common::Env as _;

// Import a panic handler to collide with any accidentally included libstd panic
// handler.
#[cfg(target_family = "wasm")]
#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    unreachable!()
}
