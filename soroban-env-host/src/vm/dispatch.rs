use super::FuelRefillable;
use crate::{xdr::ContractCostType, EnvBase, Host, HostError, VmCaller, VmCallerEnv};
use crate::{
    AddressObject, Bool, BytesObject, DurationObject, Error, I128Object, I256Object, I256Val,
    I32Val, I64Object, MapObject, StorageType, StringObject, Symbol, SymbolObject, TimepointObject,
    U128Object, U256Object, U256Val, U32Val, U64Object, U64Val, Val, VecObject, Void,
};
use soroban_env_common::{call_macro_with_all_host_functions, WasmiMarshal};
use wasmi::{
    core::{Trap, TrapCode::BadSignature},
    Value,
};

pub(crate) trait RelativeObjectConversion: WasmiMarshal {
    fn absolute_to_relative(self, _host: &Host) -> Result<Self, HostError> {
        Ok(self)
    }
    fn relative_to_absolute(self, _host: &Host) -> Result<Self, HostError> {
        Ok(self)
    }
    fn try_marshal_from_relative_value(v: wasmi::Value, host: &Host) -> Result<Self, Trap> {
        let val = Self::try_marshal_from_value(v).ok_or(BadSignature)?;
        Ok(val.relative_to_absolute(host)?)
    }
    fn marshal_relative_from_self(self, host: &Host) -> Result<wasmi::Value, Trap> {
        let rel = self.absolute_to_relative(host)?;
        Ok(Self::marshal_from_self(rel))
    }
}

macro_rules! impl_relative_object_conversion {
    ($T:ty) => {
        impl RelativeObjectConversion for $T {
            fn absolute_to_relative(self, host: &Host) -> Result<Self, HostError> {
                Ok(Self::try_from(host.absolute_to_relative(self.into())?)?)
            }

            fn relative_to_absolute(self, host: &Host) -> Result<Self, HostError> {
                Ok(Self::try_from(host.relative_to_absolute(self.into())?)?)
            }
        }
    };
}

// Define a relative-to-absolute impl for any type that is (a) mentioned
// in a host function type signature in env and (b) might possibly carry an
// object reference. If you miss one, this file won't compile, so it's safe.
impl_relative_object_conversion!(Val);
impl_relative_object_conversion!(Symbol);

impl_relative_object_conversion!(AddressObject);
impl_relative_object_conversion!(BytesObject);
impl_relative_object_conversion!(DurationObject);

impl_relative_object_conversion!(TimepointObject);
impl_relative_object_conversion!(SymbolObject);
impl_relative_object_conversion!(StringObject);

impl_relative_object_conversion!(VecObject);
impl_relative_object_conversion!(MapObject);

impl_relative_object_conversion!(I64Object);
impl_relative_object_conversion!(I128Object);
impl_relative_object_conversion!(I256Object);

impl_relative_object_conversion!(U64Object);
impl_relative_object_conversion!(U128Object);
impl_relative_object_conversion!(U256Object);

impl_relative_object_conversion!(U64Val);
impl_relative_object_conversion!(U256Val);
impl_relative_object_conversion!(I256Val);

// Trivial / non-relativizing impls are ok for types that can't carry objects.
impl RelativeObjectConversion for i64 {}
impl RelativeObjectConversion for u64 {}
impl RelativeObjectConversion for Void {}
impl RelativeObjectConversion for Bool {}
impl RelativeObjectConversion for Error {}
impl RelativeObjectConversion for StorageType {}
impl RelativeObjectConversion for U32Val {}
impl RelativeObjectConversion for I32Val {}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: dispatch functions
///////////////////////////////////////////////////////////////////////////////

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
                    { $fn_str:literal, fn $fn_id:ident ($($arg:ident:$type:ty),*) -> $ret:ty }
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
                // This defines a "dispatch function" that does several things:
                //
                //  1. Transfers the running "VM fuel" balance from wasmi to the
                //     host's CPU budget.
                //  2. Charges the host budget for the call, failing if over.
                //  3. Attempts to convert incoming wasmi i64 args to Vals or
                //     Val-wrappers expected by host functions, failing if any
                //     conversions fail. This step also does
                //     relative-to-absolute object reference conversion.
                //  4. Calls the host function.
                //  5. Augments any error result with this calling context, so
                //     that we get at minimum a "which host function failed"
                //     context on error.
                //  6. Converts the result back to an i64 for wasmi, again
                //     converting from absolute object references to relative
                //     along the way.
                //  7. Checks the result is Ok, or escalates Err to a VM Trap.
                //  8. Transfers the residual CPU budget back to wasmi "VM
                //     fuel".
                //
                // It is embedded in two nested `$()*` pattern-repetition
                // expanders that correspond to the pattern-repetition matchers
                // in the match section, but we ignore the structure of the
                // 'mod' block repetition-level from the outer pattern in the
                // expansion, flattening all functions from all 'mod' blocks
                // into a set of functions.
                $(#[$fn_attr])*
                pub(crate) fn $fn_id(mut caller: wasmi::Caller<Host>, $($arg:i64),*) ->
                    Result<(i64,), Trap>
                {
                    let _span = tracy_span!(core::stringify!($fn_id));

                    let host = caller.data().clone();

                    // This is where the VM -> Host boundary is crossed.
                    // We first return all fuels from the VM back to the host such that
                    // the host maintains control of the budget.
                    FuelRefillable::return_fuel_to_host(&mut caller, &host).map_err(|he| Trap::from(he))?;

                    // Charge for the host function dispatching: conversion between VM fuel and
                    // host budget, marshalling values. This does not account for the actual work
                    // being done in those functions, which are metered individually by the implementation.
                    host.charge_budget(ContractCostType::DispatchHostFunction, None)?;
                    let mut vmcaller = VmCaller(Some(caller));
                    // The odd / seemingly-redundant use of `wasmi::Value` here
                    // as intermediates -- rather than just passing Vals --
                    // has to do with the fact that some host functions are
                    // typed as receiving or returning plain _non-val_ i64 or
                    // u64 values. So the call here has to be able to massage
                    // both types into and out of i64, and `wasmi::Value`
                    // happens to be a natural switching point for that: we have
                    // conversions to and from both Val and i64 / u64 for
                    // wasmi::Value.
                    let res: Result<_, HostError> = host.$fn_id(&mut vmcaller, $(<$type>::try_marshal_from_relative_value(Value::I64($arg), &host)?),*);

                    // On the off chance we got an error with no context, we can
                    // at least attach some here "at each host function call",
                    // fairly systematically. This will cause the context to
                    // propagate back through wasmi to its caller.
                    let res = host.augment_err_result(res);

                    let res = match res {
                        Ok(ok) => {
                            let val: Value = ok.marshal_relative_from_self(&host)?;
                            if let Value::I64(v) = val {
                                Ok((v,))
                            } else {
                                Err(BadSignature.into())
                            }
                        },
                        Err(hosterr) => {
                            // We make a new HostError here to capture the escalation event itself.
                            let escalation: HostError =
                                host.error(hosterr.error,
                                           concat!("escalating error to VM trap from failed host function call: ",
                                                   stringify!($fn_id)), &[]);
                            let trap: Trap = escalation.into();
                            Err(trap)
                        }
                    };

                    // This is where the Host->VM boundary is crossed.
                    // We supply the remaining host budget as fuel to the VM.
                    let caller = vmcaller.try_mut().map_err(|e| Trap::from(HostError::from(e)))?;
                    FuelRefillable::add_fuel_to_vm(caller, &host).map_err(|he| Trap::from(he))?;

                    res
                }
            )*
        )*
    };
}

// Here we invoke the x-macro passing generate_dispatch_functions as its callback macro.
call_macro_with_all_host_functions! { generate_dispatch_functions }
