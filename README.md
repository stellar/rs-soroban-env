# rs-stellar-contract-host

Rust host interface and (optional) host implementation for Stellar Jump Cannon.

This crate contains elements of the shared interface between smart contract guest and host: the `Host` trait that defines the set of available host functions as well as the `Val` type that can pass back and forth through the WASM calling convention. Additionally small wrappers around subtypes of `Val` are included: `Object`, `Symbol`, `Status`, etc.

If configured with `cfg(feature = "host_context")`, this crate also contains the host-side _implementation_ of the host interface. This can be used either in the real stellar-core host or as a mock host in the SDK.
