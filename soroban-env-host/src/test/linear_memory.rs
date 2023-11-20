use crate::{
    host_object::HostMap,
    host_object::HostVec,
    xdr::{ScBytes, ScErrorCode, ScErrorType, ScString, ScSymbol},
    Env, Host, HostError, Symbol, SymbolSmall, U32Val, Val,
};
use soroban_synth_wasm::{Arity, LocalRef, ModEmitter, Operand};

use soroban_env_macros::generate_linear_memory_host_fn_tests;

generate_linear_memory_host_fn_tests!("../soroban-env-common/env.json");
