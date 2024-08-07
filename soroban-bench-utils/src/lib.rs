#[allow(unused_macros)]
#[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
macro_rules! tracy_span {
    () => {
        tracy_client::span!()
    };
    ($name:expr) => {
        tracy_client::span!($name)
    };
}

#[allow(unused_macros)]
#[cfg(any(target_family = "wasm", not(feature = "tracy")))]
macro_rules! tracy_span {
    () => {
        ()
    };
    ($name:expr) => {
        ()
    };
}

pub mod tracker;
pub use tracker::HostTracker;
pub use tracking_allocator;
