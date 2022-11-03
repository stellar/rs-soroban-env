#![no_std]

/// This integration test simulates a package that imports soroban_env_common
/// and adds a panic handler. The importing package will be allowed to do that
/// if no panic handler has been already linked in. If soroban_env_common
/// imports libstd by accident then this test will fail because there will be
/// two panic hanlders.

#[allow(unused_imports)]
use soroban_env_common::Env as _;

#[cfg(target_family = "wasm")]
#[panic_handler]
fn handle_panic(_: &core::panic::PanicInfo) -> ! {
    unreachable!()
}

fn main() {}
