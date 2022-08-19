use stellar_xdr::ScObjectType;

use crate::{ConversionError, Env, EnvVal, IntoVal, Object, RawVal, RawValConvertible, TryIntoVal};

use seq_macro::seq;

macro_rules! impl_for_u8_array {
    ( $count:literal ) => {
        impl<E: Env> TryFrom<EnvVal<E, RawVal>> for [u8; $count] {
            type Error = ConversionError;

            fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
                const COUNT: u32 = $count;
                if !Object::val_is_obj_type(ev.val, ScObjectType::Bytes) {
                    return Err(ConversionError);
                }
                let env = ev.env.clone();
                let bin = unsafe { Object::unchecked_from_val(ev.val) };
                let len: u32 = env.binary_len(bin).try_into()?;
                if len != COUNT {
                    return Err(ConversionError);
                }
                // TODO: Use memory copy instead of individual gets.
                Ok(seq!(INDEX in 0..$count {
                    [
                        #({
                            const IDX: u32 = INDEX;
                            let val = env.binary_get(bin, IDX.into());
                            let val_u32 = unsafe { u32::unchecked_from_val(val) };
                            val_u32 as u8
                        },)*
                    ]
                }))
            }
        }

        impl<E: Env> IntoVal<E, RawVal> for [u8; $count] {
            fn into_val(self, env: &E) -> RawVal {
                let env = env.clone();
                let mut bin = env.binary_new();
                for b in self {
                    let b_u32 = Into::<u32>::into(b);
                    bin = env.binary_push(bin, b_u32.into_val(&env));
                }
                bin.to_raw()
            }
        }

        impl<E: Env> IntoVal<E, EnvVal<E, RawVal>> for [u8; $count] {
            fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
                let rv: RawVal = self.into_val(env);
                EnvVal{
                    env: env.clone(),
                    val: rv,
                }
            }
        }

        impl<E: Env> TryIntoVal<E, [u8; $count]> for RawVal {
            type Error = ConversionError;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result<[u8; $count], Self::Error> {
                EnvVal{ env: env.clone(), val: self }.try_into()
            }
        }
    };
}

seq!(COUNT in 0..31 {
    impl_for_u8_array! { COUNT }
});
impl_for_u8_array! { 32 }
impl_for_u8_array! { 64 }
impl_for_u8_array! { 128 }
impl_for_u8_array! { 256 }
impl_for_u8_array! { 512 }
