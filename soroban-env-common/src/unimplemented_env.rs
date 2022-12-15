use super::{call_macro_with_all_host_functions, Env, EnvBase, Object, RawVal, Status, Symbol};
use core::any;

/// A dummy implementation of the [Env] trait that fails with `unimplemented!()` in
/// all functions. Useful for certain testing scenarios.
#[derive(Clone, Default)]
pub struct UnimplementedEnv;

impl EnvBase for UnimplementedEnv {
    fn as_mut_any(&mut self) -> &mut dyn any::Any {
        self
    }

    fn check_same_env(&self, _other: &Self) {}

    fn deep_clone(&self) -> Self {
        Self
    }

    fn bytes_copy_from_slice(
        &self,
        _b: Object,
        _b_pos: RawVal,
        _mem: &[u8],
    ) -> Result<Object, Status> {
        unimplemented!()
    }

    fn bytes_copy_to_slice(
        &self,
        _b: Object,
        _b_pos: RawVal,
        _mem: &mut [u8],
    ) -> Result<(), Status> {
        unimplemented!()
    }

    fn bytes_new_from_slice(&self, _mem: &[u8]) -> Result<Object, Status> {
        unimplemented!()
    }

    fn log_static_fmt_val(&self, _fmt: &'static str, _v: RawVal) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_static_str(
        &self,
        _fmt: &'static str,
        _s: &'static str,
    ) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_val_static_str(
        &self,
        _fmt: &'static str,
        _v: RawVal,
        _s: &'static str,
    ) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_general(
        &self,
        _fmt: &'static str,
        _vals: &[RawVal],
        _strs: &[&'static str],
    ) -> Result<(), Status> {
        unimplemented!()
    }
}

// This is a helper macro used only by generate_env_unimplemented below. It
// consumes a token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! host_function_helper {
    {fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $(_:$type),*) -> $ret {
            unimplemented!()
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the declaration of the
// UnimplementedEnv trait.
macro_rules! generate_env_unimplemented {
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
                    { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
                )*
            }
        )*
    }

    => // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: the UnimplementedEnv struct used
        // to define a type that implements the Env trait with all functions
        // unimplemented, intended for use in tests that need an Env that has no
        // implementation.
        impl Env for UnimplementedEnv
        {
            $(
                $(
                    // This invokes the host_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the Env trait.
                    host_function_helper!{fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Invoke the x-macro passing generate_env_unimplemented as its callback macro.
call_macro_with_all_host_functions! { generate_env_unimplemented }
