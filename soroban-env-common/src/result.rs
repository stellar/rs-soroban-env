use crate::{ConversionError, Env, IntoVal, RawVal, Symbol, TryFromVal, TryIntoVal};

const SYMBOL_OK: Symbol = Symbol::from_str("OK");
const SYMBOL_OK_PAYLOAD: u64 = SYMBOL_OK.to_raw().get_payload();

const SYMBOL_ERROR: Symbol = Symbol::from_str("ERROR");
const SYMBOL_ERROR_PAYLOAD: u64 = SYMBOL_ERROR.to_raw().get_payload();

impl<E: Env, T, F> TryFromVal<E, RawVal> for Result<T, F>
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
    F: TryFromVal<E, RawVal, Error = ConversionError>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, val: RawVal) -> Result<Self, Self::Error> {
        let (discriminant, value): (RawVal, RawVal) = val.try_into_val(env)?;
        match discriminant.get_payload() {
            SYMBOL_OK_PAYLOAD => Ok(Ok(T::try_from_val(env, value)?)),
            SYMBOL_ERROR_PAYLOAD => Ok(Err(F::try_from_val(env, value)?)),
            _ => Err(ConversionError),
        }
    }
}

impl<E: Env, T, F> TryIntoVal<E, Result<T, F>> for RawVal
where
    T: TryFromVal<E, RawVal, Error = ConversionError>,
    F: TryFromVal<E, RawVal, Error = ConversionError>,
{
    type Error = ConversionError;

    #[inline(always)]
    fn try_into_val(self, env: &E) -> Result<Result<T, F>, Self::Error> {
        <_ as TryFromVal<E, RawVal>>::try_from_val(env, self)
    }
}

impl<E: Env, T, F> IntoVal<E, RawVal> for Result<T, F>
where
    T: IntoVal<E, RawVal>,
    F: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => (SYMBOL_OK, t).into_val(env),
            Err(f) => (SYMBOL_ERROR, f).into_val(env),
        }
    }
}

impl<E: Env, T, F> IntoVal<E, RawVal> for &Result<T, F>
where
    for<'a> &'a T: IntoVal<E, RawVal>,
    for<'a> &'a F: IntoVal<E, RawVal>,
{
    fn into_val(self, env: &E) -> RawVal {
        match self {
            Ok(t) => (SYMBOL_OK, t).into_val(env),
            Err(f) => (SYMBOL_ERROR, f).into_val(env),
        }
    }
}
