# rs-soroban-env

Rust contract-environment interface and (optional) host implementation for Soroban.

The `soroban-env-common` crate contains elements of the shared environment-interface between smart contract guest and host: the `Env` trait that defines the set of available environment functions as well as the `Val` type that can pass back and forth through the WASM calling convention. Additionally small wrappers around subtypes of `Val` are included: `Object`, `Symbol`, `Error`, etc.

The `soroban-env-guest` crate contains the guest-side _stub implementation_ of the environment interface called `Guest` dependent on extern fns provided by the host implementation. This can be used in a WASM runtime that provides the extern fns.

The `soroban-env-host` crate contains the host-side _full implementation_ of the environment interface called `Host`. This can be used either in the real blockchain host, or for local testing in the SDK.
