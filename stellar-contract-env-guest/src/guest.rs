#![allow(dead_code)]
#![allow(unused_variables)]

use stellar_contract_env_common::call_macro_with_all_host_functions;

use super::{Env, EnvBase, RawVal};

// In guest code the environment is global/implicit, so is represented as a unit struct.
#[derive(Copy, Clone, Default)]
pub struct Guest;

impl EnvBase for Guest {
    fn as_mut_any(&mut self) -> &mut dyn core::any::Any {
        return self;
    }

    fn check_same_env(&self, other: &Self) {
        ()
    }
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: impl Env for Guest
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by impl_env_for_guest below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method definition to be used in the
// Guest implementation of the Env trait (calling through to the corresponding
// unsafe extern function).
macro_rules! guest_function_helper {
    {$mod_id:ident, fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $($arg:$type),*) -> $ret {
            unsafe {
                $mod_id::$fn_id($($arg),*)
            }
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of
// forwarding-method definitions, which it places in the body of the declaration
// of the implementation of Env for Guest.
macro_rules! impl_env_for_guest {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            mod $mod_id:ident $mod_str:literal
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
        // This macro expands to a single item: the implementation of Env for
        // the Guest struct used by client contract code running in a WASM VM.
        impl Env for Guest
        {
            $(
                $(
                    // This invokes the guest_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the implementation of Env for Guest.
                    guest_function_helper!{$mod_id, fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { impl_env_for_guest }

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: extern mod blocks
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by impl_env_for_guest below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method definition to be used in the
// Guest implementation of the Env trait (calling through to the corresponding
// unsafe extern function).
macro_rules! extern_function_helper {
    {$fn_str:literal, fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {
        #[cfg_attr(target_family = "wasm", link_name = $fn_str)]
        pub(crate) fn $fn_id($($arg:$type),*) -> $ret;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a set of mod
// items containing extern "C" blocks, each containing extern function
// declarations.
macro_rules! generate_extern_modules {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            mod $mod_id:ident $mod_str:literal
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
        // This macro expands to a set of mod items, each declaring all the extern fns
        // available for the `impl Env for Guest` methods above to call through to.
        $(
            // Unlike the other uses of the x-macro that "flatten" the
            // mod-and-fn structure of the matched token-tree, this callback
            // macro's expansion preserves the structure, creating a nested set
            // of mods and fns. There is therefore a mod declaration between the
            // outer and inner `$()*` pattern-repetition expanders.
            mod $mod_id {
                #[allow(unused_imports)]
                use crate::{RawVal,RawObj};
                #[link(wasm_import_module = $mod_str)]
                extern "C" {
                    $(
                        // This invokes the extern_function_helper! macro above
                        // passing only the relevant parts of the declaration
                        // matched by the inner pattern above. It is embedded in
                        // one `$()*` pattern-repetition expander so that it
                        // repeats only for the part of each mod that the
                        // corresponding pattern-repetition matcher.
                        extern_function_helper!{$fn_str, fn $fn_id $args -> $ret}
                    )*
                }
            }
        )*
    };
}

// Here we invoke the x-macro passing generate_extern_modules as its callback macro.
call_macro_with_all_host_functions! { generate_extern_modules }
