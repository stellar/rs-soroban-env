use crate::{Env, EnvVal, IntoVal, RawVal};

// We can't add conversions from RawVal to Option<T> because Option<T>
// has a blanket implementation for converting any T to Option<T>.
//
// We drive TryFromVal impls from a TryFrom<EnvVal<>>, and so there is a
// conflict for TryFrom<EnvVal<>> for Option<T> since T could be EnvVal<>.
//
// What to do? There are two options:
//
//   1) Stop driving TryFromVal from TryFrom<EnvVal<>> and impl TryFromVal
//   manually. This is probably fine, just tedious to go and rewrite all the
//   impls. But, the oddity remains that you still can't convert from an
//   EnvVal<> to an Option<T>.
//
//   2) Or, manually impl TryFrom<EnvVal<>> for every type, including user
//   defined types. (See example below of doing u32.) The good news is this can
//   be macrod.
//
// An oddity remains in both cases that converting RawVal => Option<RawVal> will
// simply wrap it, it won't actually unwrap a RawVal/EnvVal Void to None. So
// there's two types that will behave differently to all other types.
//
// Here we go with option (2), and provide a macro that does the impl for any
// type.

#[macro_export]
macro_rules! impl_try_from_option {
    ( $T:ty ) => {
        impl<E: Env> TryFrom<EnvVal<E, RawVal>> for Option<$T> {
            type Error = <$T as TryFrom<EnvVal<E, RawVal>>>::Error;

            fn try_from(ev: EnvVal<E, RawVal>) -> Result<Self, Self::Error> {
                if ev.val.is_void() {
                    Ok(None)
                } else {
                    Ok(Some(<$T>::try_from(ev)?))
                }
            }
        }

        impl<E: Env> $crate::TryIntoVal<E, Option<$T>> for RawVal {
            type Error = <$T as TryFrom<EnvVal<E, RawVal>>>::Error;
            #[inline(always)]
            fn try_into_val(self, env: &E) -> Result<Option<$T>, Self::Error> {
                <_ as TryFrom<EnvVal<E, RawVal>>>::try_from(EnvVal {
                    env: env.clone(),
                    val: self,
                })
            }
        }
    };
}

// TODO: Move this to the appropriate place. It is just here as an example for
// the moment.
impl_try_from_option!(u32);

impl<E: Env, T> IntoVal<E, RawVal> for &Option<T>
where
    for<'a> &'a T: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Some(t) => t.into_val(env),
            None => RawVal::from_void(),
        }
    }
}

impl<E: Env, T> IntoVal<E, EnvVal<E, RawVal>> for &Option<T>
where
    for<'a> &'a Option<T>: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let rv: RawVal = self.into_val(env);
        EnvVal {
            env: env.clone(),
            val: rv,
        }
    }
}

impl<E: Env, T> IntoVal<E, RawVal> for Option<T>
where
    T: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Some(t) => t.into_val(env),
            None => RawVal::from_void(),
        }
    }
}

impl<E: Env, T> IntoVal<E, EnvVal<E, RawVal>> for Option<T>
where
    T: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> EnvVal<E, RawVal> {
        let rv: RawVal = self.into_val(env);
        EnvVal {
            env: env.clone(),
            val: rv,
        }
    }
}
