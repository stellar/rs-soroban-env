use crate::{require, ConversionError, RawVal, Tag, TagSymbol, TaggedVal};
use core::{
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
    str,
};

#[derive(Debug)]
pub enum SymbolError {
    TooLong(usize),
    BadChar(char),
}

impl From<SymbolError> for ConversionError {
    fn from(_: SymbolError) -> Self {
        ConversionError
    }
}

extern crate static_assertions as sa;

use super::raw_val::BODY_BITS;

const MAX_CHARS: usize = 10;
const CODE_BITS: usize = 6;
const CODE_MASK: u64 = (1u64 << CODE_BITS) - 1;
sa::const_assert!(CODE_MASK == 0x3f);
sa::const_assert!(CODE_BITS * MAX_CHARS == BODY_BITS);

pub type Symbol = TaggedVal<TagSymbol>;

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().get_payload().hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().get_payload() == other.as_ref().get_payload()
    }
}

impl Eq for Symbol {}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        Iterator::cmp(self.into_iter(), other.into_iter())
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let s: SymbolStr = self.into();
        write!(f, "Symbol(")?;
        for c in s.0.iter() {
            if *c == 0 {
                break;
            }
            write!(f, "{}", unsafe { char::from_u32_unchecked(*c as u32) })?;
        }
        write!(f, ")")
    }
}

impl TryFrom<&[u8]> for Symbol {
    type Error = SymbolError;

    fn try_from(b: &[u8]) -> Result<Symbol, SymbolError> {
        Self::try_from_bytes(b)
    }
}

#[cfg(feature = "std")]
use stellar_xdr::VecM;
#[cfg(feature = "std")]
impl<const N: u32> TryFrom<VecM<u8, N>> for Symbol {
    type Error = SymbolError;

    fn try_from(v: VecM<u8, N>) -> Result<Self, Self::Error> {
        v.as_slice().try_into()
    }
}
#[cfg(feature = "std")]
impl<const N: u32> TryFrom<&VecM<u8, N>> for Symbol {
    type Error = SymbolError;

    fn try_from(v: &VecM<u8, N>) -> Result<Self, Self::Error> {
        v.as_slice().try_into()
    }
}

impl Symbol {
    pub const fn try_from_bytes(b: &[u8]) -> Result<Symbol, SymbolError> {
        let mut n = 0;
        let mut accum: u64 = 0;
        while n < b.len() {
            let ch = b[n] as char;
            if n >= MAX_CHARS {
                return Err(SymbolError::TooLong(b.len()));
            }
            n += 1;
            accum <<= CODE_BITS;
            let v = match ch {
                '_' => 1,
                '0'..='9' => 2 + ((ch as u64) - ('0' as u64)),
                'A'..='Z' => 12 + ((ch as u64) - ('A' as u64)),
                'a'..='z' => 38 + ((ch as u64) - ('a' as u64)),
                _ => return Err(SymbolError::BadChar(ch)),
            };
            accum |= v;
        }
        Ok(Self(
            unsafe { RawVal::from_body_and_tag(accum, Tag::Symbol) },
            PhantomData,
        ))
    }

    pub const fn try_from_str(s: &str) -> Result<Symbol, SymbolError> {
        Self::try_from_bytes(s.as_bytes())
    }

    pub const fn from_str(s: &str) -> Symbol {
        match Self::try_from_str(s) {
            Ok(sym) => sym,
            Err(_) => panic!(),
        }
    }

    pub fn to_str(&self) -> SymbolStr {
        let mut chars = [b'\x00'; MAX_CHARS];
        for (i, ch) in self.into_iter().enumerate() {
            chars[i] = ch as u8;
        }
        SymbolStr(chars)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SymbolStr([u8; MAX_CHARS]);

impl SymbolStr {
    pub fn is_empty(&self) -> bool {
        self.0[0] == 0
    }
    pub fn len(&self) -> usize {
        let s: &[u8] = &self.0;
        for (i, x) in s.iter().enumerate() {
            if *x == 0 {
                return i;
            }
        }
        s.len()
    }
}

impl Debug for SymbolStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let s: &str = self.as_ref();
        f.debug_tuple("SymbolStr").field(&s).finish()
    }
}

impl AsRef<[u8]> for SymbolStr {
    fn as_ref(&self) -> &[u8] {
        let s: &[u8] = &self.0;
        &s[..self.len()]
    }
}

impl AsRef<str> for SymbolStr {
    fn as_ref(&self) -> &str {
        let s: &[u8] = self.as_ref();
        unsafe { str::from_utf8_unchecked(s) }
    }
}

impl From<&Symbol> for SymbolStr {
    fn from(s: &Symbol) -> Self {
        s.to_str()
    }
}

impl From<Symbol> for SymbolStr {
    fn from(s: Symbol) -> Self {
        (&s).into()
    }
}

impl From<&str> for SymbolStr {
    fn from(s: &str) -> Self {
        s.into()
    }
}

#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
impl From<Symbol> for String {
    fn from(s: Symbol) -> Self {
        s.to_string()
    }
}
#[cfg(feature = "std")]
impl From<SymbolStr> for String {
    fn from(s: SymbolStr) -> Self {
        s.to_string()
    }
}
#[cfg(feature = "std")]
impl ToString for Symbol {
    fn to_string(&self) -> String {
        self.into_iter().collect()
    }
}
#[cfg(feature = "std")]
impl ToString for SymbolStr {
    fn to_string(&self) -> String {
        let s: &str = self.as_ref();
        s.to_string()
    }
}

impl IntoIterator for Symbol {
    type Item = char;
    type IntoIter = SymbolIter;
    fn into_iter(self) -> Self::IntoIter {
        SymbolIter(self.as_ref().get_body())
    }
}

#[derive(Clone)]
pub struct SymbolIter(u64);

impl Iterator for SymbolIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        while self.0 != 0 {
            let res = match ((self.0 >> ((MAX_CHARS - 1) * CODE_BITS)) & CODE_MASK) as u8 {
                1 => b'_',
                n @ (2..=11) => (b'0' + n - 2),
                n @ (12..=37) => (b'A' + n - 12),
                n @ (38..=63) => (b'a' + n - 38),
                _ => b'\0',
            };
            self.0 <<= CODE_BITS;
            if res != b'\0' {
                return Some(res as char);
            }
        }
        None
    }
}

impl FromIterator<char> for Symbol {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        let mut n = 0;
        let mut accum: u64 = 0;
        for i in iter {
            require(n < MAX_CHARS);
            n += 1;
            accum <<= CODE_BITS;
            let v = match i {
                '_' => 1,
                '0'..='9' => 2 + ((i as u64) - ('0' as u64)),
                'A'..='Z' => 12 + ((i as u64) - ('A' as u64)),
                'a'..='z' => 38 + ((i as u64) - ('a' as u64)),
                _ => break,
            };
            accum |= v;
        }
        unsafe { TaggedVal::from_body_and_tag_type(accum) }
    }
}

#[cfg(feature = "std")]
use crate::xdr::ScVal;

#[cfg(feature = "std")]
impl TryFrom<ScVal> for Symbol {
    type Error = ();
    fn try_from(v: ScVal) -> Result<Self, Self::Error> {
        (&v).try_into()
    }
}
#[cfg(feature = "std")]
impl TryFrom<&ScVal> for Symbol {
    type Error = ();
    fn try_from(v: &ScVal) -> Result<Self, Self::Error> {
        if let ScVal::Symbol(vec) = v {
            vec.try_into().map_err(|_| ())
        } else {
            Err(())
        }
    }
}

#[cfg(feature = "std")]
impl TryFrom<Symbol> for ScVal {
    type Error = ();
    fn try_from(s: Symbol) -> Result<Self, Self::Error> {
        let res: Result<Vec<u8>, _> = s.into_iter().map(<u8 as TryFrom<char>>::try_from).collect();
        Ok(ScVal::Symbol(
            res.map_err(|_| ())?.try_into().map_err(|_| ())?,
        ))
    }
}

#[cfg(feature = "std")]
impl TryFrom<&Symbol> for ScVal {
    type Error = ();
    fn try_from(s: &Symbol) -> Result<Self, Self::Error> {
        s.clone().try_into()
    }
}

#[cfg(test)]
mod test_without_string {
    use super::{Symbol, SymbolStr};

    #[test]
    fn test_roundtrip() {
        let input = "stellar";
        let sym = Symbol::from_str(input);
        let sym_str = SymbolStr::from(sym);
        let s: &str = sym_str.as_ref();
        assert_eq!(s, input);
    }

    #[test]
    fn test_roundtrip_zero() {
        let input = "";
        let sym = Symbol::from_str(input);
        let sym_str = SymbolStr::from(sym);
        let s: &str = sym_str.as_ref();
        assert_eq!(s, input);
    }

    #[test]
    fn test_roundtrip_ten() {
        let input = "0123456789";
        let sym = Symbol::from_str(input);
        let sym_str = SymbolStr::from(sym);
        let s: &str = sym_str.as_ref();
        assert_eq!(s, input);
    }

    #[test]
    fn test_ord() {
        let a_in = "Hello";
        let b_in = "hello";
        let c_in = "hellos";
        let a_sym = Symbol::from_str(a_in);
        let b_sym = Symbol::from_str(b_in);
        let c_sym = Symbol::from_str(c_in);
        assert!(a_sym < b_sym);
        assert!(b_sym < c_sym);
        assert!(a_sym < c_sym);
    }
}

#[cfg(all(test, feature = "std"))]
mod test_with_string {
    use super::Symbol;
    use std::string::{String, ToString};

    #[test]
    fn test_roundtrip() {
        let input = "stellar";
        let sym = Symbol::from_str(input);
        let s: String = sym.to_string();
        assert_eq!(input, &s);
    }

    #[test]
    fn test_roundtrip_zero() {
        let input = "";
        let sym = Symbol::from_str(input);
        let s: String = sym.to_string();
        assert_eq!(input, &s);
    }

    #[test]
    fn test_roundtrip_ten() {
        let input = "0123456789";
        let sym = Symbol::from_str(input);
        let s: String = sym.to_string();
        assert_eq!(input, &s);
    }
}
