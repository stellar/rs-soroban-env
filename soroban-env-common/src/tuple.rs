use crate::{Env, TryFromVal, TryIntoVal, Val, VecObject};

macro_rules! impl_for_tuple {
    ( $count:literal $count_usize:literal $($typ:ident $idx:tt)+ ) => {

        // Conversions to and from Val.

        impl<E: Env, $($typ),*> TryFromVal<E, Val> for ($($typ,)*)
        where
            $($typ: TryFromVal<E, Val>),*
        {
            type Error = crate::Error;

            fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
                let vec: VecObject = val.try_into()?;
                let mut tmp: [Val; $count as usize] = [Val::VOID.to_val(); $count as usize];
                env.vec_unpack_to_slice(vec, &mut tmp).map_err(Into::into)?;
                Ok(($($typ::try_from_val(env, &tmp[$idx]).map_err(Into::into)?,)*))
            }
        }

        impl<E: Env, $($typ),*> TryFromVal<E, ($($typ,)*)> for Val
        where
            $($typ: TryIntoVal<E, Val>),*
        {
            type Error = crate::Error;
            fn try_from_val(env: &E, v: &($($typ,)*)) -> Result<Self, Self::Error> {
                let tmp: [Val; $count as usize] = [
                    $(v.$idx.try_into_val(&env).map_err(Into::into)?,)*
                ];
                let vec = env.vec_new_from_slice(&tmp).map_err(Into::into)?;
                Ok(vec.to_val())
            }
        }


        // Conversions to and from Array of Val.

        impl<E: Env, $($typ),*, const N: usize> TryFromVal<E, [Val; N]> for ($($typ,)*)
        where
            $($typ: TryFromVal<E, Val>),*
        {
            type Error = crate::Error;

            fn try_from_val(env: &E, val: &[Val; N]) -> Result<Self, Self::Error> {
                Ok((
                    $({
                        $typ::try_from_val(&env, &val[$idx]).map_err(Into::into)?
                    },)*
                ))
            }
        }

        impl<E: Env, $($typ),*, const N: usize> TryFromVal<E, ($($typ,)*)> for [Val; N]
        where
            $(Val: TryFromVal<E, $typ>),*
        {
            type Error = crate::Error;

            fn try_from_val(env: &E, val: &($($typ,)*)) -> Result<Self, Self::Error> {
                let mut arr: [Val; N] = [Val::VOID.into(); N];
                $(arr[$idx] = val.$idx.try_into_val(env).map_err(Into::into)?;)*
                Ok(arr)
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
impl_for_tuple! { 13_u32 13_usize T0 0 T1 1 T2 2 T3 3 T4 4 T5 5 T6 6 T7 7 T8 8 T9 9 T10 10 T11 11 T12 12 }

// Unit types are not tuples, but when people write out what they think is an
// empty tuple, it is a unit type. The following conversions on unit types
// behave like tuples in some conversions that are safe to do so, like
// conversions to and from arrays. Note that unit types directly convert to
// Val::VOID, see val.rs for those conversions.

impl<E: Env> TryFromVal<E, [Val; 0]> for () {
    type Error = crate::Error;

    fn try_from_val(_env: &E, _val: &[Val; 0]) -> Result<Self, Self::Error> {
        Ok(())
    }
}

impl<E: Env> TryFromVal<E, ()> for [Val; 0] {
    type Error = crate::Error;

    fn try_from_val(_env: &E, _v: &()) -> Result<Self, Self::Error> {
        Ok([Val::VOID.into(); 0])
    }
}
