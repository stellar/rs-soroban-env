//! Implementations of [`Arbitrary`] for contract types.

extern crate alloc;

use crate::symbol::MAX_SMALL_CHARS;
use crate::xdr::ScError;
use crate::{Error, StorageType, Symbol, SymbolSmall, Val, Void};
use arbitrary::{Arbitrary, Unstructured};

impl<'a> Arbitrary<'a> for Error {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let scerror = ScError::arbitrary(u)?;
        let error = Error::from(scerror);
        Ok(error)
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <ScError as Arbitrary>::size_hint(depth)
    }
}

impl<'a> Arbitrary<'a> for Void {
    fn arbitrary(_u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Val::VOID)
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

impl<'a> Arbitrary<'a> for Symbol {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let len: usize = u.int_in_range(0..=MAX_SMALL_CHARS)?;
        let mut buf = [0u8; MAX_SMALL_CHARS];
        for i in 0..len {
            buf[i] = (*u.choose(&[
                'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
                'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_',
            ])?) as u8;
        }
        let small =
            SymbolSmall::try_from(&buf[0..len]).map_err(|_| arbitrary::Error::IncorrectFormat)?;
        Ok(small.into())
    }
}

impl<'a> Arbitrary<'a> for StorageType {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        u.choose(&[
            StorageType::Instance,
            StorageType::Persistent,
            StorageType::Temporary,
        ])
        .map(|x| *x)
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <usize as Arbitrary>::size_hint(depth)
    }
}
