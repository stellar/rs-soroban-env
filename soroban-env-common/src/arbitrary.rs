//! Implementations of [`Arbitrary`] for contract types.

extern crate alloc;

use crate::xdr::ScError;
use crate::Error;
use arbitrary::{Arbitrary, Unstructured};

impl<'a> Arbitrary<'a> for Error {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let scerror = ScError::arbitrary(u)?;
        let error = Error::from(scerror);
        Ok(error)
    }
}
