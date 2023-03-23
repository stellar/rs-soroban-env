//! Implementations of [`Arbitrary`] for contract types.

#![cfg(feature = "testutils")]

extern crate alloc;

use crate::xdr::ScStatus;
use crate::Status;
use arbitrary::{Arbitrary, Unstructured};

impl<'a> Arbitrary<'a> for Status {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let scstatus = ScStatus::arbitrary(u)?;
        let status = Status::from(scstatus);

        Ok(status)
    }
}
