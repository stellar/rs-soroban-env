# rs-stellar-contract-env

Rust contract-environment interface and (optional) host implementation for Stellar Jump Cannon.

This crate contains elements of the shared environment-interface between smart contract guest and host: the `Env` trait that defines the set of available environment functions as well as the `Val` type that can pass back and forth through the WASM calling convention. Additionally small wrappers around subtypes of `Val` are included: `Object`, `Symbol`, `Status`, etc.

If configured with `cfg(feature = "guest")`, this crate contains the guest-side _implementation_ of the environment interface dependent on extern fns provided by the host implementation. This can be used in a WASM runtime that provides the extern fns.

If configured with `cfg(feature = "host")`, this crate contains the host-side _implementation_ of the environment interface. This can be used either in the real stellar-core host or for testing in the SDK.
