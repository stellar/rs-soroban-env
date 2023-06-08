use soroban_env_macros::generate_call_macro_with_all_host_functions;

use super::Symbol;
use super::{
    AddressObject, Bool, BytesObject, Error, I128Object, I256Object, I64Object, MapObject, Object,
    RawVal, StorageType, StringObject, SymbolObject, U128Object, U256Object, U32Val, U64Object,
    U64Val, VecObject, Void,
};
use core::any;

/// Base trait extended by the [Env](crate::Env) trait, providing various special-case
/// functions that do _not_ simply call across cross the guest/host interface.
pub trait EnvBase: Sized + Clone {
    /// The type of error returned from the environment when the environment
    /// itself fails "unrecoverably", or at least in a way that the user is not
    /// expected to be able to recover from, such as an internal logic error,
    /// exceeding the execution budget, or being passed malformed input in a way
    /// that the user-facing API does not anticipate or allow for. This type is
    /// returned from _all_ environment-interface methods, and will only ever
    /// take on two possible concrete types: either `Infallible` (in the
    /// `Guest`) or `HostError` (in the `Host`).
    ///
    /// The `Guest` can treat all such errors as impossible-to-observe since
    /// they will result in the `Host` _trapping_ the `Guest` before returning
    /// an `Error` to it. Such errors still remain present in the `Env` API so
    /// that we can use the same API in both scenarios, rather than having to
    /// have separate "fallible" or "infallible" environments and separate
    /// conversion routines for each (as was attempted in earlier iterations).
    ///
    /// This type is _not_ the same as an error intended to make it to the
    /// user-facing API: user-facing errors should return `Ok(Error)` at the
    /// environment-interface level, and then either directly handle or escalate
    /// the contained `Error` code to the user as a `Error` or `Result<>` of
    /// some other type, depending on the API.
    type Error: core::fmt::Debug;

    /// Reject an error from the environment, turning it into a panic but on
    /// terms that the environment controls (eg. transforming or logging it).
    /// This should only ever be called by client-side / SDK local-testing code,
    /// never in the `Host`.
    #[cfg(feature = "testutils")]
    fn escalate_error_to_panic(&self, e: Self::Error) -> !;

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

    /// Clone an existing `Bytes` object in the host, replacing the portion of
    /// its memory with bytes supplied by `slice`, returning the new object. The
    /// replaced portion of the original object's memory begins at `b_pos` and
    /// extends for the same length as the new `slice`.
    fn bytes_copy_from_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &[u8],
    ) -> Result<BytesObject, Self::Error>;

    /// Copy a slice of bytes from a `Bytes` object in the host into a slice in
    /// the caller's memory.
    fn bytes_copy_to_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), Self::Error>;

    /// Copy a slice of bytes from a `String` object in the host into a slice in
    /// the caller's memory.
    fn string_copy_to_slice(
        &self,
        b: StringObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), Self::Error>;

    /// Copy a slice of bytes from a `Symbol` object in the host into the
    /// caller's memory.
    fn symbol_copy_to_slice(
        &self,
        b: SymbolObject,
        b_pos: U32Val,
        mem: &mut [u8],
    ) -> Result<(), Self::Error>;

    /// Form a new `Bytes` host object from a slice of client memory.
    fn bytes_new_from_slice(&self, slice: &[u8]) -> Result<BytesObject, Self::Error>;

    /// Form a new `String` host object from a slice of client memory.
    fn string_new_from_slice(&self, slice: &str) -> Result<StringObject, Self::Error>;

    /// Form a new `Symbol` host object from a slice of client memory.
    fn symbol_new_from_slice(&self, slice: &str) -> Result<SymbolObject, Self::Error>;

    /// Form a new `Map` host object from a slice of symbol-names and a slice of values.
    /// Keys must be in sorted order.
    fn map_new_from_slices(&self, keys: &[&str], vals: &[RawVal])
        -> Result<MapObject, Self::Error>;

    /// Unpack a `Map` host object with a specified set of keys to a slice of
    /// `RawVal`s. Keys must be in sorted order and must match the key set of
    /// the unpacked object exactly.
    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [RawVal],
    ) -> Result<Void, Self::Error>;

    /// Form a new `Vec` host object from a slice of values.
    fn vec_new_from_slice(&self, vals: &[RawVal]) -> Result<VecObject, Self::Error>;

    /// Form a new `Vec` host object from a slice of values. The values slice must
    /// be the same length as the host object.
    fn vec_unpack_to_slice(&self, vec: VecObject, vals: &mut [RawVal])
        -> Result<Void, Self::Error>;

    /// Return the index of a `Symbol` in an array of &strs, or error if not found.
    fn symbol_index_in_strs(&self, key: Symbol, strs: &[&str]) -> Result<U32Val, Self::Error>;

    /// Log a string and set of values as a diagnostic event, if diagnostic
    /// events are enabled. When running on host, logs directly; when running on
    /// guest, redirects through log_from_linear_memory.
    fn log_from_slice(&self, msg: &str, vals: &[RawVal]) -> Result<Void, Self::Error>;
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
        fn $fn_id(&self, $($arg:$type),*) -> Result<$ret, Self::Error>;
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
        /// client contract code and implemented (via [Env](crate::Env)) by the host.
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
