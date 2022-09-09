use super::dispatch;
use crate::Host;
use soroban_env_common::call_macro_with_all_host_functions;
use wasmi::{Func, Store};

pub(crate) struct HostFuncInfo {
    /// String name of the WASM module this host function is importable from.
    pub(crate) mod_str: &'static str,

    /// String name of the WASM function that the host function is importable
    /// as.
    pub(crate) fn_str: &'static str,

    /// Function that takes a wasmi::Store and _wraps_ a dispatch function
    /// for this host function, with the specific type of the dispatch function,
    /// into a Func in the Store.
    pub(crate) wrap: fn(&mut Store<Host>) -> Func,
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: static HOST_FUNCTIONS array of HostFuncInfo
///////////////////////////////////////////////////////////////////////////////

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of
// dispatch-function definitions.
macro_rules! generate_host_function_infos {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            $(#[$mod_attr:meta])*
            mod $mod_id:ident $mod_str:literal
            {
                $(
                    // This inner pattern matches a single function description
                    // inside a 'mod' block in the token-tree passed from the
                    // x-macro to this macro. It is embedded in a `$()*`
                    // pattern-repetition matcher so that it will match all such
                    // descriptions.
                    $(#[$fn_attr:meta])*
                    { $fn_id:literal, fn $func_id:ident $args:tt -> $ret:ty }
                )*
            }
        )*
    }

    =>   // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: a static array of HostFuncInfo, used by
        // two places:
        //
        //   1. The VM WASM-module instantiation step to resolve all import functions to numbers
        //       and typecheck their signatures (represented here by a simple arity number, since
        //       every host function we have just takes N i64 values and returns an i64).
        //
        //   2. The function dispatch path when guest code calls out of the VM, where we
        //      look up the numbered function the guest is requesting in this array and
        //      call its associated dispatch function.
        pub(crate) static HOST_FUNCTIONS: &[HostFuncInfo] =
        &[
           $(
                $(
                    // This generates a HostFuncInfo struct directly
                    // for each function matched in the token-tree (invoking
                    // the arity_helper! macro above to calculate the arity
                    // of each function along the way). It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the a single array of HostFuncInfo structs.
                    HostFuncInfo {
                        mod_str: $mod_str,
                        fn_str: $fn_id,
                        wrap: |store| Func::wrap(store, dispatch::$func_id),
                    },
                )*
            )*
        ];
    };
}

// Here we invoke the x-macro passing generate_host_function_infos as its callback macro.
call_macro_with_all_host_functions! { generate_host_function_infos }
