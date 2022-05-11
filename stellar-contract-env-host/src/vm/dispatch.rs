use crate::{CheckedEnv, Host, Object, RawVal};
use stellar_contract_env_common::call_macro_with_all_host_functions;
use wasmi::{RuntimeArgs, RuntimeValue};

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: dispatch functions
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_dispatch_functions below. It
// consumes a token-tree of the form:
//
//  {$host:expr, $vmargs:expr, fn $fn_id:ident $args:tt }
//
// in each of 6 valid forms (with 0, 1, ... 6 possible arguments) and produces a
// function-call expression that extracts and converts the correct number of
// arguments from the $vmargs argument (a wasmi::RuntimeArgs structure) and
// forwards them to the corresponding $host.$fn_id function (a method on Host).
macro_rules! dispatch_function_helper {

    {$host:expr, $vmargs:expr, fn $fn_id:ident()} =>
    {$host.$fn_id()};

    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?)};

    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?,
                  $vmargs.nth_checked::<$t1>(1)?)};

    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?,
                  $vmargs.nth_checked::<$t1>(1)?,
                  $vmargs.nth_checked::<$t2>(2)?)};


    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?,
                  $vmargs.nth_checked::<$t1>(1)?,
                  $vmargs.nth_checked::<$t2>(2)?,
                  $vmargs.nth_checked::<$t3>(3)?)};

    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty,
                                                 $a4:ident:$t4:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?,
                  $vmargs.nth_checked::<$t1>(1)?,
                  $vmargs.nth_checked::<$t2>(2)?,
                  $vmargs.nth_checked::<$t3>(3)?,
                  $vmargs.nth_checked::<$t4>(4)?)};


    {$host:expr, $vmargs:expr, fn $fn_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty,
                                                 $a4:ident:$t4:ty,
                                                 $a5:ident:$t5:ty)} =>
    {$host.$fn_id($vmargs.nth_checked::<$t0>(0)?,
                  $vmargs.nth_checked::<$t1>(1)?,
                  $vmargs.nth_checked::<$t2>(2)?,
                  $vmargs.nth_checked::<$t3>(3)?,
                  $vmargs.nth_checked::<$t4>(4)?,
                  $vmargs.nth_checked::<$t5>(5)?)};
}

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
            mod $mod_name:ident $mod_str:literal
            {
                $(
                    // This inner pattern matches a single function description
                    // inside a 'mod' block in the token-tree passed from the
                    // x-macro to this macro. It is embedded in a `$()*`
                    // pattern-repetition matcher so that it will match all such
                    // descriptions.
                    { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
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
                // This defines a function containing a body that includes the
                // expansion of a macro call to the dispatch_function_helper!
                // macro above, called with some expressions from the enclosing
                // function (host and vmargs) as well as the relevant parts of
                // the token-tree function declaration matched by the inner
                // pattern above. It is embedded in two nested `$()*`
                // pattern-repetition expanders that correspond to the
                // pattern-repetition matchers in the match section, but we
                // ignore the structure of the 'mod' block repetition-level from
                // the outer pattern in the expansion, flattening all functions
                // from all 'mod' blocks into a set of functions.
                pub(crate) fn $fn_id(host: &mut Host, _vmargs: RuntimeArgs) ->
                    Result<RuntimeValue, wasmi::Trap>
                {
                    Ok(dispatch_function_helper!{host, _vmargs, fn $fn_id $args }?.into())
                }
            )*
        )*
    };
}

// Here we invoke the x-macro passing generate_dispatch_functions as its callback macro.
call_macro_with_all_host_functions! { generate_dispatch_functions }
