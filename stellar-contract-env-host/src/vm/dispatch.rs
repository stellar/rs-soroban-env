use crate::{Env, Host, RawVal};
use stellar_contract_env_common::call_macro_with_all_host_functions;
use wasmi::{RuntimeArgs, RuntimeValue};

// Probably there is a way to do this with recursive macros and accumulators but
// it seems tolerable (and much easier to understand) to just write the cases out.
macro_rules! dispatch_call {

    {$host:ident, $args:ident, fn $func_id:ident() } =>
    { $host.$func_id() };

    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?) };

    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?,
                     $args.nth_checked::<$t1>(1)?) };

    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?,
                     $args.nth_checked::<$t1>(1)?,
                     $args.nth_checked::<$t2>(2)?) };


    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?,
                     $args.nth_checked::<$t1>(1)?,
                     $args.nth_checked::<$t2>(2)?,
                     $args.nth_checked::<$t3>(3)?) };

    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty,
                                                 $a4:ident:$t4:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?,
                     $args.nth_checked::<$t1>(1)?,
                     $args.nth_checked::<$t2>(2)?,
                     $args.nth_checked::<$t3>(3)?,
                     $args.nth_checked::<$t4>(4)?) };


    {$host:ident, $args:ident, fn $func_id:ident($a0:ident:$t0:ty,
                                                 $a1:ident:$t1:ty,
                                                 $a2:ident:$t2:ty,
                                                 $a3:ident:$t3:ty,
                                                 $a4:ident:$t4:ty,
                                                 $a5:ident:$t5:ty) } =>
    { $host.$func_id($args.nth_checked::<$t0>(0)?,
                     $args.nth_checked::<$t1>(1)?,
                     $args.nth_checked::<$t2>(2)?,
                     $args.nth_checked::<$t3>(3)?,
                     $args.nth_checked::<$t4>(4)?,
                     $args.nth_checked::<$t5>(5)?) };
}

macro_rules! generate_dispatch_functions {
    {
        $(
            mod $mod_name:ident $mod_str:literal
            {
                $(
                    { $field_str:literal, fn $func_id:ident $selfspec:tt $args:tt -> $ret:ty }
                )*
            }
        )*
    }
        =>
    {
        $(
            $(
                pub(crate) fn $func_id(host: &mut Host, _args: RuntimeArgs) -> Result<RuntimeValue, wasmi::Trap> {
                    Ok(dispatch_call!{host, _args, fn $func_id $args }.into())
                }
            )*
        )*
    };
}

call_macro_with_all_host_functions! { generate_dispatch_functions }
