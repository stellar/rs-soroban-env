use soroban_env_macros::generate_call_macro_with_all_host_functions;

use super::Symbol;
use super::{Object, RawVal, Status};
use core::any;

/// Base trait extended by the [Env](crate::Env) trait, providing various special-case
/// functions that do _not_ simply call across cross the guest/host interface.
pub trait EnvBase: Sized + Clone {
    /// Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    /// Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);

    /// Used to clone an environment deeply, not just a handle to it.
    fn deep_clone(&self) -> Self;

    // Helpers for methods that wish to pass Rust lifetime-qualified _slices_
    // into the environment. These are _not_ done via Env trait methods to avoid
    // the need to convert, and thus trust (or validate) "raw numbers" coming
    // through that interface as "potentially pointers in the same address space
    // as the host". This is a bit of a defense-in-depth approach as we _could_
    // just accept "numbers as pointers in our address space" on a codepath that
    // is sure its input is coming from a "trusted" contract, and arrange enough
    // other static safety checks elsewhere in the calling path (eg. in the SDK)
    // to ensure that "all callers are trusted" .. but we want to minimize the
    // chance of future maintainers accidentally violating such an invariant,
    // since getting it wrong would let guest code violate memory safety. So the
    // _only_ interface to passing contract pointers to the host is going to be
    // in EnvBase, not Env, and as a bonus we get lifetime checking for free.

    /// Copy a slice of bytes from the caller's memory into an existing `Bytes`
    /// object the host, returning a new `Bytes`.
    fn bytes_copy_from_slice(&self, b: Object, b_pos: RawVal, mem: &[u8])
        -> Result<Object, Status>;

    /// Copy a slice of bytes from a `Bytes` object in the host into the
    /// caller's memory.
    fn bytes_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) -> Result<(), Status>;

    /// Form a new `Bytes` object in the host from a slice of memory in the
    /// caller.
    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<Object, Status>;

    // As with the bytes functions above, these take _slices_ with definite
    // lifetimes. The first slice is interpreted as a (very restricted)
    // format-string -- containing literal text interspersed with some number of
    // `{}` markers which must match the number of other args passed -- with
    // actual formatting delayed until someone asks to see the event (which may
    // never happen). Other args may be static strings, [RawVal]s, or a mix.
    //
    // When the SDK is built with Env = Host, both the format string slice and
    // all static string slice args (and any [RawVal] args) will be passed
    // through into the debug-event subsystem of the host and _stored_
    // unformatted in the debug buffer, until/unless someone dumps some portion
    // of that buffer out. They are therefore quite cheap -- just pushing static
    // pointers and numbers into the debug buffer -- and can be called fairly
    // ubiquitously to provide details on any interesting diagnostic events
    // and/or errors that occur in either SDK or contract code.
    //
    // When Env = Guest, these currently compile as no-ops. We may change this
    // to record a VM-relative guest static string pointer (similar to how the
    // bytes functions above work) into the debug buffer in the future, but it
    // is a little involved to do so and we assume that VM code probably does
    // not want to be carrying static strings at all.

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// a single [RawVal] argument that will be inserted at the marker in the
    /// format string.
    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// a single string-slice argument that will be inserted at the marker in
    /// the format string.
    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// both a [RawVal] and a string-slice argument, that will each be inserted
    /// at markers in the format string.
    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// both a slice of [RawVal]s and a slice of string-slice argument, that
    /// will be sequentially inserted at markers in the format string.
    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), Status>;
}

///////////////////////////////////////////////////////////////////////////////
// X-macro definition
///////////////////////////////////////////////////////////////////////////////

// The set of host functions need to be statically reflected-on in a variety of
// contexts (both in this crate and elsewhere in the guest and host crates), so
// we define them through an x-macro (a macro that calls a user-provided macro)
// and call the x-macro from all such contexts.
//
// How this macro works:
//  - It exports a higher-order "x-macro" called
//    call_macro_with_all_host_functions
//  - The x-macro takes the name of some callback macro to call
//  - The x-macro invokes the callback macro once, passing a single large token
//    tree, seen below in the body of the x-macro
//
// To use this macro:
//  - Call sites define a callback macro that matches on the token-tree
//  - Call sites invoke the x-macro passing their callback macro name
//
// The token-tree being passed is arbitrary, but is chosen to satisfy 3
// criteria:
//  - It's relatively easy to read, edit and understand its content
//  - It's easy to decompose with pattern-matching in the callback macros
//  - It contains everything any callback macro wants to match and use
//
// All callback macros have essentially the same token-tree matcher part,
// only their expansion parts differ.

generate_call_macro_with_all_host_functions!("env.json");

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: defining trait Env
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_env_trait below. It consumes
// a token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! host_function_helper {
    {
        $(#[$attr:meta])*
        fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        $(#[$attr])*
        fn $fn_id(&self, $($arg:$type),*) -> $ret;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the declaration of the Env
// trait.
macro_rules! generate_env_trait {
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
        // This macro expands to a single item: the Env trait.

        /// This trait represents the interface between Host and Guest, used by
        /// client contract code and implemented (via [CheckedEnv](crate::CheckedEnv)) by the host.
        /// It consists of functions that take or return only 64-bit values such
        /// as [RawVal] or [u64].
        pub trait Env: EnvBase
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
                    host_function_helper!{$(#[$fn_attr])* fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { generate_env_trait }
