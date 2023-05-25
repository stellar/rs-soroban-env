use crate::{xdr::ContractCostType, Host, HostError, VmCaller, VmCallerEnv};
use crate::{
    AddressObject, BytesObject, Error, I128Object, I256Object, I64Object, MapObject, RawVal,
    StorageType, StringObject, Symbol, SymbolObject, U128Object, U256Object, U32Val, U64Object,
    VecObject,
};
use soroban_env_common::call_macro_with_all_host_functions;
use wasmi::core::{FromValue, Trap, TrapCode::UnexpectedSignature, Value};

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
                //  2. attempts to convert incoming wasmi i64 args to RawVals or
                //     RawVal-wrappers expected by host functions, failing if
                //     any conversions fail.
                //  3. calls the host function
                //  4. checks the result is Ok, or traps the VM on Err
                //  5. converts the result back to an i64 for wasmi
                //
                // It is embedded in two nested `$()*` pattern-repetition
                // expanders that correspond to the pattern-repetition matchers
                // in the match section, but we ignore the structure of the
                // 'mod' block repetition-level from the outer pattern in the
                // expansion, flattening all functions from all 'mod' blocks
                // into a set of functions.
                $(#[$fn_attr])*
                pub(crate) fn $fn_id(caller: wasmi::Caller<Host>, $($arg:i64),*) ->
                    Result<(i64,), Trap>
                {
                    // Notes on metering: a flat charge per host function invocation.
                    // This does not account for the actual work being done in those functions,
                    // which are accounted for individually at the operation level.
                    let host = caller.host_data().clone();
                    host.charge_budget(ContractCostType::InvokeHostFunction, None)?;
                    let mut vmcaller = VmCaller(Some(caller));
                    // The odd / seemingly-redundant use of `wasmi::Value` here
                    // as intermediates -- rather than just passing RawVals --
                    // has to do with the fact that some host functions are
                    // typed as receiving or returning plain _non-Rawval_ i64 or
                    // u64 values. So the call here has to be able to massage
                    // both types into and out of i64, and `wasmi::Value`
                    // happens to be a natural switching point for that: we have
                    // conversions to and from both RawVal and i64 / u64 for
                    // wasmi::Value.
                    let res: Result<_, HostError> = host.$fn_id(&mut vmcaller, $(<$type as FromValue>::from_value(Value::I64($arg)).ok_or(UnexpectedSignature)?),*);
                    let res: Value = match res {
                        Ok(ok) => ok.into(),
                        Err(hosterr) => {
                            // We make a new HostError here to capture the escalation event itself.
                            let escalation: HostError =
                                host.error(hosterr.error,
                                           concat!("escalating error to VM trap from failed host function call: ",
                                                   stringify!($fn_id)), &[]);
                            let trap: Trap = escalation.into();
                            return Err(trap)
                        }
                    };
                    if let Value::I64(v) = res {
                        Ok((v,))
                    } else {
                        Err(UnexpectedSignature.into())
                    }
                }
            )*
        )*
    };
}

// Here we invoke the x-macro passing generate_dispatch_functions as its callback macro.
call_macro_with_all_host_functions! { generate_dispatch_functions }
