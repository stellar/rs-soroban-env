//! This module contains a macro-generated `impl` block that adds a suite of
//! methods to [`FuncEmitter`], each of which imports and emits code to call
//! call one of the host functions defined on [`soroban_env_common::Env`].

use crate::{Arity, FuncEmitter, Operand};
use soroban_env_common::call_macro_with_all_host_functions;

// This is a helper macro that matches simple ident:ty argument list token-trees
// and returns a literal token that is the arity (number of arguments) in the
// list. It is used to convert the supplied token-tree pattern to an arity number.
macro_rules! arity_helper {
    { () } => { 0 };
    { ($a0:ident ) } => { 1 };
    { ($a0:ident , $a1:ident) } => { 2 };
    { ($a0:ident , $a1:ident, $a2:ident) } => { 3 };
    { ($a0:ident , $a1:ident, $a2:ident, $a3:ident) } => { 4 };
    { ($a0:ident , $a1:ident, $a2:ident, $a3:ident, $a4:ident) } => { 5 };
    { ($a0:ident , $a1:ident, $a2:ident, $a3:ident, $a4:ident, $a5:ident) } => { 6 };
}

macro_rules! generate_call_emitter_functions {
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
                    { $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident ($($arg:ident:$type:ty),*) -> $ret:ty }
                )*
            }
        )*
    }

    =>  // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: an impl block with a set of
        // methods for emitting wasm that calls host functions. We do not
        // perform any protocol version checks here, since this module is for
        // synthesizing test wasms calling host functions.
        impl FuncEmitter
        {
        $(
            $(
                $(#[$fn_attr])*
                pub fn $fn_id<$($arg:Into<Operand>),*>(&mut self, $($arg:$arg),*)
                {
                    let fn_id = self.mod_emit.import_func($mod_str, $fn_str, Arity(arity_helper!{($($arg),*)}));
                    let mut _is_first_arg = true;
                    $(self.push_full($arg.into(), &mut _is_first_arg);)*
                    self.call_func(fn_id);
                }
            )*
        )*
        }
    };
}

// Here we invoke the x-macro passing generate_call_emitter_functions as its callback macro.
call_macro_with_all_host_functions! { generate_call_emitter_functions }
