use core::convert::Infallible;

use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, Object, RawVal, RawValConvertible, CheckedEnv, Convert,
};

// This module exists to define blanket impls for tuples of
// RawVal-interconvertable types. Unfortunately for us, defining this "the easy
// way" presents a severe UI problem: if we make something like
//
//   impl<E,T,U> Convert<(T,U),RawVal> for E where E: Convert<T,RawVal> +
//      Convert<U,RawVal>
//
// we have produced an impl-derivation rule that _can_ recur infinitely --
// searching for impl<((T, U), _), ...> -- forever, if there are no other
// matches for T or U, and this means that every impl-searching error that
// happens on Convert gets "upgraded" to a completely mysterious
// infinite-recursion error.
//
// So to make this less painful to users, we make a secondary trait here
// RawValTupEltConvert that we manually implement on all non-tuple types that
// interconvert with RawVal, but _not_ on tuple types.

trait TupElt{ type Ty; }
impl<T:RawValConvertible> TupElt for T { type Ty = T; }
impl TupElt for [RawVal;0] { type Ty = [RawVal;0]; }
impl TupElt for u64 { type Ty = u64; }
impl TupElt for i64 {type Ty = i64; }
impl TupElt for u128 {type Ty = u128; }
impl TupElt for i128 {type Ty = i128; }
impl TupElt for crate::xdr::AccountId { type Ty = crate::xdr::AccountId; }
impl TupElt for crate::xdr::ScContractCode { type Ty = crate::xdr::ScContractCode; }

macro_rules! impl_for_tuple {
    ( $count:literal $count_usize:literal $($typ:ident $idx:tt)+ ) => {

        // Conversions to and from RawVal.

        impl<E: Env, $($typ),*> Convert<($($typ,)*), RawVal> for E
        where
        $($typ: TupElt<Ty=$typ>,)*
        $(E: Convert<<$typ as TupElt>::Ty, RawVal, Error=ConversionError>,)*
        {
            type Error = ConversionError;
            fn convert_ref(&self, val: &($($typ,)*)) -> Result<RawVal, ConversionError> {
                let vec = self.vec_new($count.into());
                $(let vec = self.vec_push_back(vec, <E as Convert<<$typ as TupElt>::Ty, RawVal>>::convert_ref(self, &val.$idx)?);)*
                Ok(vec.to_raw())
            }
        }

        impl<E: Env, $($typ),*> Convert<RawVal, ($($typ,)*)> for E
        where
        $($typ: TupElt<Ty=$typ>,)*
        $(E: Convert<RawVal, <$typ as TupElt>::Ty, Error=ConversionError>),*
        {
            type Error = ConversionError;
            fn convert_ref(&self, val: &RawVal) -> Result<($($typ,)*), ConversionError> {
                if !Object::val_is_obj_type(*val, ScObjectType::Vec) {
                    return Err(ConversionError);
                }
                let vec = unsafe { Object::unchecked_from_val(*val) };
                let len: u32 = self.vec_len(vec).try_into()?;
                if len != $count {
                    return Err(ConversionError);
                }
                Ok((
                    $({
                        let idx: u32 = $idx;
                        let val = self.vec_get(vec, idx.into());
                        <E as Convert<RawVal, <$typ as TupElt>::Ty>>::convert_ref(self, &val).map_err(|_| ConversionError)?
                    },)*
                ))
            }
        }

        // Conversions to and from Array of RawVal.
/*
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
        */
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

impl <E:CheckedEnv> Convert<[RawVal;0],()> for E {
    type Error = Infallible;

    fn convert_ref(&self, _f: &[RawVal;0]) -> Result<(), Self::Error> {
        Ok(())
    }
}
impl <E:CheckedEnv> Convert<(),[RawVal;0]> for E {
    type Error = Infallible;

    fn convert_ref(&self, _f: &()) -> Result<[RawVal;0], Self::Error> {
        Ok([RawVal::VOID;0])
    }
}
