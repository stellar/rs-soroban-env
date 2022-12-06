use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, IntoVal, Object, RawVal, RawValConvertible, TryFromVal, TryIntoVal,
};

macro_rules! impl_for_tuple {
    ( $count:literal $count_usize:literal $($typ:ident $idx:tt)+ ) => {

        // Conversions to and from RawVal.

        impl<E: Env, $($typ),*> TryFromVal<E, RawVal> for ($($typ,)*)
        where
            $($typ: TryFromVal<E, RawVal>),*
        {
            type Error = ConversionError;

            fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
                if !Object::val_is_obj_type(val, ScObjectType::Vec) {
                    return Err(ConversionError);
                }
                let vec = unsafe { Object::unchecked_from_val(val) };
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

        impl<E: Env, $($typ),*> TryIntoVal<E, ($($typ,)*)> for RawVal
        where
            $($typ: TryFromVal<E, RawVal>),*
        {
            type Error = ConversionError;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result<($($typ,)*), Self::Error> {
                <_ as TryFromVal<_, _>>::try_from_val(env, self)
            }
        }

        impl<E: Env, $($typ),*> IntoVal<E, RawVal> for ($($typ,)*)
        where
            $($typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> RawVal {
                let env = env.clone();
                let vec = env.vec_new($count.into());
                $(let vec = env.vec_push_back(vec, self.$idx.into_val(&env));)*
                vec.to_raw()
            }
        }

        impl<E: Env, $($typ),*> IntoVal<E, RawVal> for &($($typ,)*)
        where
            $(for<'a> &'a $typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> RawVal {
                let env = env.clone();
                let vec = env.vec_new($count.into());
                $(let vec = env.vec_push_back(vec, (&self.$idx).into_val(&env));)*
                vec.to_raw()
            }
        }

        // Conversions to and from Array of RawVal.

        impl<E: Env, $($typ),*, const N: usize> TryFromVal<E, &[RawVal; N]> for ($($typ,)*)
        where
            $($typ: TryFromVal<E, RawVal>),*
        {
            type Error = ConversionError;

            fn try_from_val(env: &E, val: &[RawVal; N]) -> Result<Self, Self::Error> {
                Ok((
                    $({
                        $typ::try_from_val(&env, val[$idx]).map_err(|_| ConversionError)?
                    },)*
                ))
            }
        }

        impl<E: Env, $($typ),*, const N: usize> TryIntoVal<E, ($($typ,)*)> for &[RawVal; N]
        where
            $($typ: TryFromVal<E, RawVal>),*
        {
            type Error = ConversionError;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result<($($typ,)*), Self::Error> {
                <_ as TryFromVal<_, _>>::try_from_val(env, self)
            }
        }

        impl<E: Env, $($typ),*, const N: usize> IntoVal<E, [RawVal; N]> for &($($typ,)*)
        where
            $(for<'a> &'a $typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> [RawVal; N] {
                let mut arr = [RawVal::VOID; N];
                $(arr[$idx] = self.$idx.into_val(&env);)*
                arr
            }
        }

        impl<E: Env, $($typ),*, const N: usize> IntoVal<E, [RawVal; N]> for ($($typ,)*)
        where
            $($typ: IntoVal<E, RawVal>),*
        {
            fn into_val(self, env: &E) -> [RawVal; N] {
                let mut arr = [RawVal::VOID; N];
                $(arr[$idx] = self.$idx.into_val(&env);)*
                arr
            }
        }
    };
}

impl_for_tuple! {  1_u32  1_usize T0 0 }
impl_for_tuple! {  2_u32  2_usize T0 0 T1 1 }
impl_for_tuple! {  3_u32  3_usize T0 0 T1 1 T2 2 }
impl_for_tuple! {  4_u32  4_usize T0 0 T1 1 T2 2 T3 3 }
impl_for_tuple! {  5_u32  5_usize T0 0 T1 1 T2 2 T3 3 T4 4 }
impl_for_tuple! {  6_u32  6_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 }
impl_for_tuple! {  7_u32  7_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 }
impl_for_tuple! {  8_u32  8_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 }
impl_for_tuple! {  9_u32  9_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 }
impl_for_tuple! { 10_u32 10_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 }
impl_for_tuple! { 11_u32 11_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 }
impl_for_tuple! { 12_u32 12_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 }

// Unit types are not tuples, but when people write out what they think is an
// empty tuple, it is a unit type. The following conversions have unit types
// behave like tuples in some conversions that are safe to do so, like
// conversions to and from arrays. Note that unit typles convert to
// RawVal::VOID, see raw_val.rs for those conversions.

impl<E: Env> TryFromVal<E, &[RawVal; 0]> for () {
    type Error = ConversionError;

    fn try_from_val(_env: &E, _val: &[RawVal; 0]) -> Result<Self, Self::Error> {
        Ok(())
    }
}

impl<E: Env> TryIntoVal<E, ()> for &[RawVal; 0] {
    type Error = ConversionError;
    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<(), Self::Error> {
        <_ as TryFromVal<_, _>>::try_from_val(env, self)
    }
}

impl<E: Env> IntoVal<E, [RawVal; 0]> for &() {
    fn into_val(self, _env: &E) -> [RawVal; 0] {
        [RawVal::VOID; 0]
    }
}

impl<E: Env> IntoVal<E, [RawVal; 0]> for () {
    fn into_val(self, _env: &E) -> [RawVal; 0] {
        [RawVal::VOID; 0]
    }
}
