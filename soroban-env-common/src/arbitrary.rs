//! Implementations of [`Arbitrary`] for contract types.

#![cfg(feature = "testutils")]

extern crate alloc;

use crate::xdr::ScError;
use crate::Error;
use arbitrary::{Arbitrary, Unstructured};

impl<'a> Arbitrary<'a> for Error {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let scstatus = ScError::arbitrary(u)?;
        let status = Error::from(scstatus);

        Ok(status)
    }
}
