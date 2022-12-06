use crate::{Env, RawVal, Convert};

impl<E,T> Convert<Option<T>,RawVal> for E where E: Env + Convert<T,RawVal> {
    type Error = E::Error;

    fn convert_ref(&self, f: &Option<T>) -> Result<RawVal, Self::Error> {
        match f {
            None => Ok(RawVal::from_void()),
            Some(x) => self.convert_ref(x)
        }
    }
}

impl<E,T> Convert<RawVal,Option<T>> for E where E: Env + Convert<RawVal,T> {
    type Error = E::Error;

    fn convert_ref(&self, f: &RawVal) -> Result<Option<T>, Self::Error> {
        if f.is_void() {
            Ok(None)
        } else {
            Ok(Some(<E as Convert<RawVal,T>>::convert_ref(self, f)?))
        }
    }
}
