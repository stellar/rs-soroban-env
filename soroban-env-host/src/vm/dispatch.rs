#[allow(unused_imports)]
use crate::{
    budget::CostType, Host, HostError, Object, RawVal, Status, Symbol, VmCaller, VmCallerCheckedEnv,
};
use soroban_env_common::abi;
use soroban_env_common::call_macro_with_all_host_functions;
#[allow(unused_imports)]
use wasmi::core::{FromValue, Trap, TrapCode::UnexpectedSignature, Value};

fn escalate_status(host: Host, status: Status) -> Trap {
    let escalation: HostError = host.err_status_msg(status, "escalating error '{}' to VM trap");
    let trap: Trap = escalation.into();
    trap
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: dispatch functions
///////////////////////////////////////////////////////////////////////////////

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of
// dispatch-function definitions.
macro_rules! generate_dispatch_functions {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            $(#[$mod_attr:meta])*
            mod $mod_name:ident $mod_str:literal
            {
                $(
                    // This inner pattern matches a single function description
                    // inside a 'mod' block in the token-tree passed from the
                    // x-macro to this macro. It is embedded in a `$()*`
                    // pattern-repetition matcher so that it will match all such
                    // descriptions.
                    $(#[$fn_attr:meta])*
                    { $fn_str:literal, fn $fn_id:ident ($($arg:ident:$type:ty),*) -> $ret:ty }
                )*
            }
        )*
    }

    =>  // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to multiple items: a set of free functions in the
        // current module, which are called by functions registered with the VM
        // to forward calls to the host.
        $(
            $(
                // This defines a "dispatch function" that does several things:
                //
                //  1. charges the budget for the call, failing if over budget.
                //  2. implodes and attempts to convert incoming args to RawVals or
                //     RawVal-wrappers expected by host functions, failing if
                //     any conversions fail.
                //  3. calls the host function
                //  4. checks the result is Ok, or traps the VM on Err
                //  5. writes the result back through any retptr, returns
                //
                // It is embedded in two nested `$()*` pattern-repetition
                // expanders that correspond to the pattern-repetition matchers
                // in the match section, but we ignore the structure of the
                // 'mod' block repetition-level from the outer pattern in the
                // expansion, flattening all functions from all 'mod' blocks
                // into a set of functions.
                soroban_env_macros::dispatch_function!{$mod_name, $fn_str, fn $fn_id($($arg:$type),*) -> $ret;}
            )*
        )*
    };
}

// Here we invoke the x-macro passing generate_dispatch_functions as its callback macro.
call_macro_with_all_host_functions! { generate_dispatch_functions }
