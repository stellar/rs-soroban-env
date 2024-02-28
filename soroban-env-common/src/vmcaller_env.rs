#[cfg(feature = "wasmi")]
use crate::xdr::{ScErrorCode, ScErrorType};

use super::{
    AddressObject, Bool, BytesObject, DurationObject, Error, I128Object, I256Object, I256Val,
    I64Object, MapObject, StorageType, StringObject, SymbolObject, TimepointObject, U128Object,
    U256Object, U256Val, U32Val, U64Object, U64Val, Val, VecObject, Void,
};
use crate::call_macro_with_all_host_functions;
use crate::{CheckedEnvArg, EnvBase, Symbol};
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
                ScErrorType::Context,
                ScErrorCode::InternalError,
            )),
        }
    }
    pub fn try_mut(&mut self) -> Result<&mut wasmi::Caller<'a, T>, Error> {
        match &mut self.0 {
            Some(caller) => Ok(caller),
            None => Err(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::InternalError,
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
        $($min_proto:literal)?, $($max_proto:literal)?,
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
                    { $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident $args:tt -> $ret:ty }
                )*
            }
        )*
    }

    => // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: the VmCallerEnv trait

        /// This trait is a variant of the [Env](crate::Env) trait used to
        /// define the interface implemented by Host. The wasmi VM dispatch
        /// functions (in soroban_env_host::dispatch) call methods on
        /// `VmCallerEnv`, passing a [`VmCaller`] that wraps the wasmi Caller
        /// context, and then convert any `Result::Err(...)` return value into a
        /// VM trap, halting VM execution.
        ///
        /// There is also a blanket `impl<T:VmCallerEnv> Env for T` that
        /// implements the `Env` for any `VmCallerEnv` by passing
        /// [`VmCaller::none()`] for the first argument, allowing user code such
        /// as the native contract to avoid writing `VmCaller::none()`
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
                    host_function_helper!{$($min_proto)?, $($max_proto)?, $(#[$fn_attr])* fn $fn_id $args -> $ret}
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
    {$($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        // We call `augment_err_result` here to give the Env a chance to attach
        // context (eg. a backtrace) to any error that was generated by code
        // that didn't have an Env on hand when creating the error. This will at
        // least localize the error to a given Env call.
        fn $fn_id(&self, $($arg:$type),*) -> Result<$ret, Self::Error> {
            // Check the ledger protocol version against the function-specified
            // boundaries, this prevents calling a host function using the host
            // directly as `Env` (i.e. native mode) when the protocol version is
            // out of bound.
            $( self.check_protocol_version_lower_bound($min_proto)?; )?
            $( self.check_protocol_version_upper_bound($max_proto)?; )?

            #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
            let _span = tracy_span!(core::stringify!($fn_id));
            #[cfg(feature = "std")]
            if self.tracing_enabled()
            {
                self.trace_env_call(&core::stringify!($fn_id), &[$(&$arg),*])?;
            }
            let res: Result<_, _> = self.augment_err_result(<Self as VmCallerEnv>::$fn_id(self, &mut VmCaller::none(), $($arg.check_env_arg(self)?),*));
            let res = match res {
                Ok(ok) => Ok(ok.check_env_arg(self)?),
                Err(err) => Err(err)
            };
            #[cfg(feature = "std")]
            if self.tracing_enabled()
            {
                let dyn_res: Result<&dyn core::fmt::Debug,&Self::Error> = match &res {
                    Ok(ref ok) => Ok(ok),
                    Err(err) => Err(err)
                };
                self.trace_env_ret(&core::stringify!($fn_id), &dyn_res)?;
            }
            res
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the blanket impl of Env for
// T:Env
macro_rules! impl_env_for_vmcaller_env {
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
                    vmcaller_none_function_helper!{$($min_proto)?, $($max_proto)?, fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing
// generate_checked_env_for_vmcaller_checked_env as its callback macro.
call_macro_with_all_host_functions! { impl_env_for_vmcaller_env }
