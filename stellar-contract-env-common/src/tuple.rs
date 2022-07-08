use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, EnvVal, IntoEnvVal, IntoVal, Object, RawVal, RawValConvertible,
    TryFromVal,
};

macro_rules! impl_for_tuple {
    ( $count:literal $($typ:ident $idx:tt)+ ) => {
        impl<E: Env, $($typ),*> TryFrom<EnvVal<E, RawVal>> for ($($typ),*)
        where
            $($typ: TryFrom<EnvVal<E, RawVal>>),*
        {
            type Error = ConversionError<EnvVal<E, RawVal>, ($($typ),*)>;

            fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
                if !Object::val_is_obj_type(ev.val, ScObjectType::Vec) {
                    return Err(ConversionError);
                }
                let env = ev.env.clone();
                let vec = unsafe { Object::unchecked_from_val(ev.val) };
                let len: u32 = env.vec_len(vec).try_into()?;
                if len != $count {
                    return Err(ConversionError);
                }
                Ok((
                    $({
                        let idx: u32 = $idx;
                        let val = env.vec_get(vec, idx.into());
                        $typ::try_from_val(&env, val).map_err(|_| ConversionError)?
                    }),*
                ))
            }
        }

        impl<E: Env, $($typ),*> IntoEnvVal<E, RawVal> for ($($typ),*)
        where
            $($typ: IntoEnvVal<E, RawVal>),*
        {
            fn into_env_val(self, env: &E) -> EnvVal<E, RawVal> {
                let env = env.clone();
                let vec = env.vec_new();
                $(let vec = env.vec_push(vec, self.$idx.into_val(&env));)*
                EnvVal { env, val: vec.to_raw() }
            }
        }
    };
}
impl_for_tuple! {  2 T0 0 T1 1 }
impl_for_tuple! {  3 T0 0 T1 1 T2 2 }
impl_for_tuple! {  4 T0 0 T1 1 T2 2 T3 3 }
impl_for_tuple! {  5 T0 0 T1 1 T2 2 T3 3 T4 4 }
impl_for_tuple! {  6 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 }
impl_for_tuple! {  7 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 }
impl_for_tuple! {  8 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 }
impl_for_tuple! {  9 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 }
impl_for_tuple! { 10 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 }
impl_for_tuple! { 11 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 }
impl_for_tuple! { 12 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 }
