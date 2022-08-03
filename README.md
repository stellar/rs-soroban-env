# Soroban

https://soroban.stellar.org

This repository contains several things for the Soroban smart contract system:

- Rust contract-environment interface and (optional) host implementation for
Soroban.
- Rust SDK for Soroban. Docs: https://docs.rs/soroban-sdk

## Crates

The `soroban-env-common` crate contains elements of the shared environment-interface between smart contract guest and host: the `Env` trait that defines the set of available environment functions as well as the `RawVal` type that can pass back and forth through the WASM calling convention. Additionally small wrappers around subtypes of `RawVal` are included: `Object`, `Symbol`, `Status`, `Bitset`, etc.

The `soroban-env-guest` crate contains the guest-side _stub implementation_ of the environment interface called `Guest` dependent on extern fns provided by the host implementation. This can be used in a WASM runtime that provides the extern fns.

The `soroban-env-host` crate contains the host-side _full implementation_ of the environment interface called `Host`. This can be used either in the real blockchain host or for local testing in the SDK.

The `soroban-sdk` contains the Rust SDK for writing contracts for Soroban.

**This repository contains code that is in early development, incomplete, not tested, and not recommended for use. The API is unstable, experimental, and is receiving breaking changes frequently.**
