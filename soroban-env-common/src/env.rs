use soroban_env_macros::generate_call_macro_with_all_host_functions;

use crate::Object;

use super::Symbol;
use super::{
    AddressObject, Bool, BytesObject, DurationObject, Error, I128Object, I256Object, I256Val,
    I64Object, MapObject, StorageType, StringObject, SymbolObject, TimepointObject, U128Object,
    U256Object, U256Val, U32Val, U64Object, U64Val, Val, VecObject, Void,
};
use crate::xdr::{ScErrorCode, ScErrorType};

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
    type Error: core::fmt::Debug + Into<crate::Error>;

    /// Check that a [`Val`] is good according to the current Env. This is a
    /// superset of calling `Val::good` as it also checks that if the `Val` is
    /// an [`Object`], that the `Object` is good according to
    /// [`Self::check_obj_integrity`].
    fn check_val_integrity(&self, val: Val) -> Result<(), Self::Error> {
        if !val.is_good() {
            return Err(self.error_from_error_val(Error::from_type_and_code(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
            )));
        }
        if let Ok(obj) = Object::try_from(val) {
            self.check_obj_integrity(obj)
        } else {
            Ok(())
        }
    }

    /// Check that an Object handle is good according to the current Env. For
    /// general Val-validity checking one should use Val::good().
    fn check_obj_integrity(&self, _obj: Object) -> Result<(), Self::Error> {
        Ok(())
    }

    /// Convert a [`crate::Error`] into [`EnvBase::Error`]. This is similar to adding
    /// `+ From<crate::Error>` to the associated type bound for `EnvBase::Error`
    /// but it allows us to restrict that conversion in downstream crates, which
    /// is desirable to keep "conversions that panic" (as the guest definition
    /// of `EnvBase::Error` does) out of the common crate and avoid accidentally
    /// triggering them in the host. It also gives the `Env` an opportunity to
    /// log or enrich the error with context (both of which happen in `Host`).
    fn error_from_error_val(&self, e: crate::Error) -> Self::Error;

    /// Reject an error from the environment, turning it into a panic but on
    /// terms that the environment controls (eg. enriching or logging it). This
    /// should only ever be called by client-side / SDK local-testing code,
    /// never in the `Host`.
    #[cfg(feature = "testutils")]
    fn escalate_error_to_panic(&self, e: Self::Error) -> !;

    #[cfg(all(feature = "std", feature = "testutils"))]
    #[deprecated(note = "replaced by trace_env_call")]
    fn env_call_hook(&self, _fname: &'static str, _args: &[String]) -> Result<(), Self::Error> {
        Ok(())
    }

    #[cfg(all(feature = "std", feature = "testutils"))]
    #[deprecated(note = "replaced by trace_env_ret")]
    fn env_ret_hook(
        &self,
        _fname: &'static str,
        _res: &Result<String, &Self::Error>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    /// Return true if the environment wants to receive trace calls and and
    /// returns using [`Self::trace_env_call`] and [`Self::trace_env_ret`].
    #[cfg(feature = "std")]
    fn tracing_enabled(&self) -> bool {
        false
    }

    /// A general interface for tracing all env-method calls, intended to
    /// be called from macros that do dispatch on all such methods.
    #[cfg(feature = "std")]
    fn trace_env_call(
        &self,
        _fname: &'static str,
        _args: &[&dyn core::fmt::Debug],
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    /// A general interface for tracing all env-method returns, intended to
    /// be called from macros that do dispatch on all such methods.
    #[cfg(feature = "std")]
    fn trace_env_ret(
        &self,
        _fname: &'static str,
        _res: &Result<&dyn core::fmt::Debug, &Self::Error>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    /// If `x` is `Err(...)`, ensure as much debug information as possible is
    /// attached to that error; in any case return "essentially the same" `x` --
    /// either `Ok(...)` or `Err(...)` -- just with extra error context.
    ///
    /// This is called on a best-effort basis while propagating errors in the
    /// host, to attach context "as soon as possible", and is necessary because
    /// some errors are generated in contexts that do not have access to a Host,
    /// and so cannot attach error context at the site of error generation.
    fn augment_err_result<T>(&self, x: Result<T, Self::Error>) -> Result<T, Self::Error> {
        x
    }

    /// Used to check two environments are the same, returning Error if not.
    fn check_same_env(&self, other: &Self) -> Result<(), Self::Error>;

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
    fn string_new_from_slice(&self, slice: &[u8]) -> Result<StringObject, Self::Error>;

    /// Form a new `Symbol` host object from a slice of client memory.
    fn symbol_new_from_slice(&self, slice: &[u8]) -> Result<SymbolObject, Self::Error>;

    /// Form a new `Map` host object from a slice of symbol-names and a slice of values.
    /// Keys must be in sorted order.
    fn map_new_from_slices(&self, keys: &[&str], vals: &[Val]) -> Result<MapObject, Self::Error>;

    /// Unpack a `Map` host object with a specified set of keys to a slice of
    /// `Val`s. Keys must be in sorted order and must match the key set of
    /// the unpacked object exactly.
    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [Val],
    ) -> Result<Void, Self::Error>;

    /// Form a new `Vec` host object from a slice of values.
    fn vec_new_from_slice(&self, vals: &[Val]) -> Result<VecObject, Self::Error>;

    /// Form a new `Vec` host object from a slice of values. The values slice must
    /// be the same length as the host object.
    fn vec_unpack_to_slice(&self, vec: VecObject, vals: &mut [Val]) -> Result<Void, Self::Error>;

    /// Return the index of a `Symbol` in an array of &strs, or error if not found.
    fn symbol_index_in_strs(&self, key: Symbol, strs: &[&str]) -> Result<U32Val, Self::Error>;

    /// Log a string and set of values as a diagnostic event, if diagnostic
    /// events are enabled. When running on host, logs directly; when running on
    /// guest, redirects through log_from_linear_memory.
    fn log_from_slice(&self, msg: &str, vals: &[Val]) -> Result<Void, Self::Error>;

    /// Check the current ledger protocol version against a provided lower
    /// bound, error if protocol version is out-of-bound.
    fn check_protocol_version_lower_bound(&self, lower_bound: u32) -> Result<(), Self::Error>;

    /// Check the current ledger protocol version against a provided upper
    /// bound, error if protocol version is out-of-bound.
    fn check_protocol_version_upper_bound(&self, upper_bound: u32) -> Result<(), Self::Error>;
}

/// This trait is used by macro-generated dispatch and forwarding functions to
/// check arguments being passed to the Env. The default implementations call
/// through to the Env integrity-checking functions.

pub trait CheckedEnvArg: Sized {
    fn check_env_arg<E: crate::Env>(self, _e: &E) -> Result<Self, E::Error> {
        Ok(self)
    }
}

// If a new host function is added that uses argument types not yet listed
// below, they will have to be added, otherwise this crate will not compile.

impl CheckedEnvArg for i64 {}
impl CheckedEnvArg for u64 {}
impl CheckedEnvArg for StorageType {}

macro_rules! impl_checkedenvarg_for_val_or_wrapper {
    ($type:ty) => {
        impl CheckedEnvArg for $type {
            fn check_env_arg<E: crate::Env>(self, e: &E) -> Result<Self, E::Error> {
                e.check_val_integrity(Val::from(self.clone()))?;
                Ok(self)
            }
        }
    };
}
impl_checkedenvarg_for_val_or_wrapper!(Val);
impl_checkedenvarg_for_val_or_wrapper!(Symbol);

impl_checkedenvarg_for_val_or_wrapper!(AddressObject);
impl_checkedenvarg_for_val_or_wrapper!(BytesObject);
impl_checkedenvarg_for_val_or_wrapper!(DurationObject);

impl_checkedenvarg_for_val_or_wrapper!(TimepointObject);
impl_checkedenvarg_for_val_or_wrapper!(SymbolObject);
impl_checkedenvarg_for_val_or_wrapper!(StringObject);

impl_checkedenvarg_for_val_or_wrapper!(VecObject);
impl_checkedenvarg_for_val_or_wrapper!(MapObject);

impl_checkedenvarg_for_val_or_wrapper!(I64Object);
impl_checkedenvarg_for_val_or_wrapper!(I128Object);
impl_checkedenvarg_for_val_or_wrapper!(I256Object);

impl_checkedenvarg_for_val_or_wrapper!(U64Object);
impl_checkedenvarg_for_val_or_wrapper!(U128Object);
impl_checkedenvarg_for_val_or_wrapper!(U256Object);

impl_checkedenvarg_for_val_or_wrapper!(U64Val);
impl_checkedenvarg_for_val_or_wrapper!(U256Val);
impl_checkedenvarg_for_val_or_wrapper!(I256Val);

impl_checkedenvarg_for_val_or_wrapper!(Void);
impl_checkedenvarg_for_val_or_wrapper!(Bool);
impl_checkedenvarg_for_val_or_wrapper!(Error);
impl_checkedenvarg_for_val_or_wrapper!(U32Val);

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
        $($min_proto:literal)?, $($max_proto:literal)?,
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
                    { $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident $args:tt -> $ret:ty }
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
        /// as [Val] or [u64].
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
                    host_function_helper!{$($min_proto)?, $($max_proto)?, $(#[$fn_attr])* fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { generate_env_trait }
