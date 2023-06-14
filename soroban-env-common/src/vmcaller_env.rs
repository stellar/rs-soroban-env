#[cfg(feature = "wasmi")]
use stellar_xdr::{ScErrorCode, ScErrorType};

use super::{
    AddressObject, Bool, BytesObject, Error, I128Object, I256Object, I64Object, MapObject, Object,
    RawVal, StorageType, StringObject, SymbolObject, U128Object, U256Object, U32Val, U64Object,
    U64Val, VecObject, Void,
};
use crate::call_macro_with_all_host_functions;
use crate::{EnvBase, Symbol};
#[cfg(not(feature = "wasmi"))]
use core::marker::PhantomData;

/// The VmCallerEnv trait is similar to the Env trait -- it
/// provides all the same-named methods -- but they have a form that takes an
/// initial [`VmCaller`] argument by `&mut` that may or may-not wrap a
/// `wasmi::Caller` structure, depending on whether it was invoked from a wasmi
/// host-function wrapper.
///
/// There is a blanket `impl<T:VmCallerEnv> Env for T` so that any
/// type (eg. `Host`) that implements `VmCallerEnv` automatically also
/// implements `Env`, just by calling the corresponding
/// `VmCallerEnv` method with the [`VmCaller::none()`] argument. This
/// allows code to import and use `Env` directly (such as the native
/// contract) to call host methods without having to write `VmCaller::none()`
/// everywhere.

#[cfg(feature = "wasmi")]
pub struct VmCaller<'a, T>(pub Option<wasmi::Caller<'a, T>>);
#[cfg(feature = "wasmi")]
impl<'a, T> VmCaller<'a, T> {
    pub fn none() -> Self {
        VmCaller(None)
    }
    pub fn try_ref(&self) -> Result<&wasmi::Caller<'a, T>, Error> {
        match &self.0 {
            Some(caller) => Ok(caller),
            None => Err(Error::from_type_and_code(
                ScErrorType::WasmVm,
                ScErrorCode::MissingValue,
            )),
        }
    }
    pub fn try_mut(&mut self) -> Result<&mut wasmi::Caller<'a, T>, Error> {
        match &mut self.0 {
            Some(caller) => Ok(caller),
            None => Err(Error::from_type_and_code(
                ScErrorType::WasmVm,
                ScErrorCode::MissingValue,
            )),
        }
    }
}

#[cfg(not(feature = "wasmi"))]
pub struct VmCaller<'a, T> {
    _nothing: PhantomData<&'a T>,
}
#[cfg(not(feature = "wasmi"))]
impl<'a, T> VmCaller<'a, T> {
    pub fn none() -> Self {
        VmCaller {
            _nothing: PhantomData,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: defining trait VmCallerEnv
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_vmcaller_checked_env_trait
// below. It consumes a token-tree of the form:
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
        fn $fn_id(&self, vmcaller: &mut VmCaller<Self::VmUserState>, $($arg:$type),*) -> Result<$ret, Self::Error>;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the declaration of the
// VmCallerEnv trait.
macro_rules! generate_vmcaller_checked_env_trait {
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
        // This macro expands to a single item: the VmCallerEnv trait

        /// This trait is a variant of the [Env](crate::Env) trait used to
        /// define the interface implemented by Host. The wasmi VM dispatch
        /// functions call methods on `VmCallerEnv`, passing a
        /// [`VmCaller`] that wraps the wasmi Caller context, and then convert
        /// any `Result::Err(...)` return value into a VM trap, halting VM
        /// execution.
        ///
        /// There is also a blanket `impl<T:VmCallerEnv> Env for
        /// T` that implements the `Env` for any `VmCallerEnv` by
        /// passing [`VmCaller::none()`] for the first argument, allowing user
        /// code such as the native contract to avoid writing `VmCaller::none()`
        /// everywhere.
        pub trait VmCallerEnv: EnvBase
        {
            type VmUserState;
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
                    // into the VmCallerEnv trait.
                    host_function_helper!{$(#[$fn_attr])* fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { generate_vmcaller_checked_env_trait }

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: impl<E> Env for VmCallerEnv<E>
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by
// generate_impl_checked_env_for_vmcaller_checked_env below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! vmcaller_none_function_helper {
    {fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $($arg:$type),*) -> Result<$ret, Self::Error> {
            <Self as VmCallerEnv>::$fn_id(self, &mut VmCaller::none(), $($arg),*)
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the blanket impl of Env for
// T:Env
macro_rules! impl_checked_env_for_vmcaller_checked_env {
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
        // `VmCallerEnv` types automatically `Env` types, just
        // passing [`VmCaller::none()`] as their first argument.
        impl<T:VmCallerEnv> $crate::Env for T
        {
            $(
                $(
                    // This invokes the vmcaller_none_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the impl.
                    vmcaller_none_function_helper!{fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing
// generate_checked_env_for_vmcaller_checked_env as its callback macro.
call_macro_with_all_host_functions! { impl_checked_env_for_vmcaller_checked_env }
