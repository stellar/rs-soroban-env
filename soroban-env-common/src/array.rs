use stellar_xdr::ScObjectType;

use crate::{
    ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryFromVal,
    TryIntoVal,
};

use seq_macro::seq;

macro_rules! impl_for_array {
    ( $count:literal ) => {
        impl<E: Env, T> TryFrom<EnvVal<E, RawVal>> for [T; $count]
        where
            T: TryFrom<EnvVal<E, RawVal>>,
        {
            type Error = ConversionError;

            fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
                const COUNT: u32 = $count;
                if !Object::val_is_obj_type(ev.val, ScObjectType::Vec) {
                    return Err(ConversionError);
                }
                let env = ev.env.clone();
                let vec = unsafe { Object::unchecked_from_val(ev.val) };
                let len: u32 = env.vec_len(vec).try_into()?;
                if len != COUNT {
                    return Err(ConversionError);
                }
                Ok(seq!(INDEX in 0..$count {
                    [
                        #({
                            const IDX: u32 = INDEX;
                            T::try_from_val(&env, env.vec_get(vec, IDX.into())).map_err(|_| ConversionError)?
                        },)*
                    ]
                }))
            }
        }

        impl<E: Env, T> IntoVal<E, RawVal> for [T; $count]
        where
            T: IntoVal<E, RawVal>
        {
            fn into_val(self, env: &E) -> RawVal {
                const COUNT: u32 = $count;
                let env = env.clone();
                let mut vec = env.vec_new(COUNT.into());
                for t in self {
                    vec = env.vec_push(vec, t.into_val(&env));
                }
                vec.to_raw()
            }
        }

        impl<E: Env, T> IntoVal<E, EnvVal<E, RawVal>> for [T; $count]
        where
            T: IntoVal<E, RawVal>
        {
            fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
                let rv: RawVal = self.into_val(env);
                EnvVal{
                    env: env.clone(),
                    val: rv,
                }
            }
        }

        impl<E: Env, T> TryIntoVal<E, [T; $count]> for RawVal
        where
            T: TryFrom<EnvVal<E, RawVal>>
        {
            type Error = ConversionError;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result<[T; $count], Self::Error> {
                EnvVal{ env: env.clone(), val: self }.try_into()
            }
        }

        // TODO: Add [u8; $count] impls as well to and from Bytes.
    };
}

seq!(COUNT in 0..31 {
    impl_for_array! { COUNT }
});
impl_for_array! { 32 }
impl_for_array! { 64 }
impl_for_array! { 128 }
impl_for_array! { 256 }
impl_for_array! { 512 }
