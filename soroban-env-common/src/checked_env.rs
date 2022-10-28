use crate::call_macro_with_all_host_functions;
use crate::{EnvBase, Object, RawVal, Status, Symbol};
use core::fmt::Debug;

/// The CheckedEnv trait is similar to the Env trait -- it provides all the
/// same-named methods -- but they have a form that returns Result<T,
/// Self::Error> for the trait's associated `Error:Debug` type.
///
/// There is a blanket `impl<T:CheckedEnv+EnvBase> Env for T` so that any type
/// that implements `CheckedEnv` and `EnvBase` automatically also implements
/// `Env`, just by calling the corresponding `CheckedEnv` method and unwrapping
/// it. This allows the host crate to convert errors into WASM traps on the VM
/// causing the host call, while keeping the actual `Env` interface exposed to
/// users simple: any `Env` call that has any sort of error simply doesn't
/// return (panics when run locally, traps when running in a VM).

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: defining trait CheckedEnv
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_checked_env_trait below. It consumes
// a token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! host_function_helper {
    {
        $(#[$attr:meta])*
        fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty
    }
    =>
    {
        $(#[$attr])*
        fn $fn_id(&self, $($arg:$type),*) -> Result<$ret, Self::Error>;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the declaration of the CheckedEnv
// trait.
macro_rules! generate_checked_env_trait {
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
        // This macro expands to a single item: the CheckedEnv trait

        /// This trait is a variant of the [Env](crate::Env) trait used to define the
        /// interface implemented by Host. The WASM VM dispatch functions call
        /// methods on `CheckedEnv` and convert any `Result::Err(...)` return
        /// value into a VM trap, halting VM execution.
        ///
        /// There is also a blanket `impl<T:CheckedEnv> Env for T` that
        /// implements the `Env` interface directly for `CheckedEnv` by
        /// unwrapping all results, in other words "panicking on error". This is
        /// used in local testing mode to adapt the `Host` to mimic the
        /// (non-`Result`, halt-on-error) interface and behavior of `Guest`
        /// when linking a contract to `Host` natively, for local testing.
        pub trait CheckedEnv
        {
            type Error: Debug;
            fn escalate_error_to_panic(&self,e:Self::Error) -> !;
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
                    // into the CheckedEnv trait.
                    host_function_helper!{$(#[$fn_attr])* fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { generate_checked_env_trait }

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: impl<E> Env for CheckedEnv<E>
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_impl_env_for_checked_env below.
// It consumes a token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! panic_function_helper {
    {fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $($arg:$type),*) -> $ret {
            match <Self as CheckedEnv>::$fn_id(self, $($arg),*) {
                Ok(x) => x,
                Err(e) => self.escalate_error_to_panic(e)
            }
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the blanket impl of Env for
// T:CheckedEnv
macro_rules! impl_env_for_checked_env {
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
        // This macro expands to a single item: a blanket impl that makes all
        // `CheckedEnv+EnvBase` types automatically `Env` types, panicking with
        // any non-Ok() result.
        impl<T:CheckedEnv+EnvBase> $crate::Env for T
        {
            $(
                $(
                    // This invokes the panic_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the impl.
                    panic_function_helper!{fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing impl_env_for_checked_env as its callback macro.
call_macro_with_all_host_functions! { impl_env_for_checked_env }
