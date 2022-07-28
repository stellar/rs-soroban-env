use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryFromVal,
    TryIntoVal,
};

macro_rules! impl_for_tuple {
    ( $count:literal $($typ:ident $idx:tt)+ ) => {
        impl<E: Env, $($typ),*> TryFrom<EnvVal<E, RawVal>> for ($($typ,)*)
        where
            $($typ: TryFrom<EnvVal<E, RawVal>>),*
        {
            type Error = ConversionError;

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
                    },)*
                ))
            }
        }

        impl<E: Env, $($typ),*> IntoVal<E, RawVal> for ($($typ,)*)
        where
            $($typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> RawVal {
                let env = env.clone();
                let vec = env.vec_new($count.into());
                $(let vec = env.vec_push(vec, self.$idx.into_val(&env));)*
                vec.to_raw()
            }
        }

        impl<E: Env, $($typ),*> IntoVal<E, EnvVal<E, RawVal>> for ($($typ,)*)
        where
            $($typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
                let rv: RawVal = self.into_val(env);
                EnvVal{
                    env: env.clone(),
                    val: rv,
                }
            }
        }

        impl<E: Env, $($typ),*> TryIntoVal<E,  ($($typ,)*)> for RawVal
        where
            $($typ: TryFrom<EnvVal<E, RawVal>>),*
        {
            type Error = ConversionError;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result< ($($typ,)*), Self::Error> {
                EnvVal{ env: env.clone(), val: self }.try_into()
            }
        }
    };
}

impl_for_tuple! {  1_u32 T0 0 }
impl_for_tuple! {  2_u32 T0 0 T1 1 }
impl_for_tuple! {  3_u32 T0 0 T1 1 T2 2 }
impl_for_tuple! {  4_u32 T0 0 T1 1 T2 2 T3 3 }
impl_for_tuple! {  5_u32 T0 0 T1 1 T2 2 T3 3 T4 4 }
impl_for_tuple! {  6_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 }
impl_for_tuple! {  7_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 }
impl_for_tuple! {  8_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 }
impl_for_tuple! {  9_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 }
impl_for_tuple! { 10_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 }
impl_for_tuple! { 11_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 }
impl_for_tuple! { 12_u32 T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 }
