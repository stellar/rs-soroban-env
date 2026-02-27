// Copyright 2026 Stellar Development Foundation and contributors. Licensed
// under the Apache License, Version 2.0. See the COPYING file at the root
// of this distribution or at http://www.apache.org/licenses/LICENSE-2.0

//! Soroban fuzz crate for cargo-fuzz integration.
//!
//! The actual fuzz target logic lives in `soroban_env_host::fuzz` in
//! order that it can be run from multiple external fuzzing engines.
//!
//! This crate just provides cargo-fuzz entry points in fuzz_targets/,
//! in case you want to run fuzzing via cargo-fuzz.
