use crate::require;
use std::hash::Hash;

extern crate static_assertions as sa;

use super::val::{Tag, Val, ValType, BODY_BITS};

const MAX_CHARS: usize = 10;
const CODE_BITS: usize = 6;
const CODE_MASK: u64 = (1u64 << CODE_BITS) - 1;
sa::const_assert!(CODE_MASK == 0x3f);
sa::const_assert!(CODE_BITS * MAX_CHARS == BODY_BITS);

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Symbol(pub(crate) Val);

impl From<Symbol> for Val {
    #[inline(always)]
    fn from(s: Symbol) -> Self {
        s.0
    }
}

impl ValType for Symbol {
    #[inline(always)]
    unsafe fn unchecked_from_val(v: Val) -> Self {
        Symbol(v)
    }

    fn is_val_type(v: Val) -> bool {
        v.has_tag(Tag::Symbol)
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.get_payload().hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0.get_payload() == other.0.get_payload()
    }
}

impl Eq for Symbol {}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Iterator::cmp(self.into_iter(), other.into_iter())
    }
}

impl Symbol {
    pub const fn try_from_str(s: &str) -> Result<Symbol, ()> {
        let mut n = 0;
        let mut accum: u64 = 0;
        let b: &[u8] = s.as_bytes();
        while n < b.len() {
            let ch = b[n] as char;
            if n >= MAX_CHARS {
                return Err(());
            }
            n += 1;
            accum <<= CODE_BITS;
            let v = match ch {
                '_' => 1,
                '0'..='9' => 2 + ((ch as u64) - ('0' as u64)),
                'A'..='Z' => 12 + ((ch as u64) - ('A' as u64)),
                'a'..='z' => 38 + ((ch as u64) - ('a' as u64)),
                _ => return Err(()),
            };
            accum |= v;
        }
        let v = unsafe { Val::from_body_and_tag(accum, Tag::Symbol) };
        Ok(Symbol(v))
    }

    pub const fn from_str(s: &str) -> Symbol {
        match Self::try_from_str(s) {
            Ok(sym) => sym,
            Err(_) => panic!(),
        }
    }
}

impl IntoIterator for Symbol {
    type Item = char;
    type IntoIter = SymbolIter;
    fn into_iter(self) -> Self::IntoIter {
        SymbolIter(self.0.get_body())
    }
}

#[derive(Clone, Copy)]
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
        let v = unsafe { Val::from_body_and_tag(accum, Tag::Symbol) };
        Symbol(v)
    }
}

#[cfg(test)]
mod test {
    use super::Symbol;
    extern crate std;

    #[test]
    fn test_roundtrip() {
        let input = "stellar";
        let sym = Symbol::from_str(input);
        let s: String = sym.into_iter().collect();
        assert_eq!(input, &s);
    }

    #[test]
    fn test_roundtrip_zero() {
        let input = "";
        let sym = Symbol::from_str(input);
        let s: String = sym.into_iter().collect();
        assert_eq!(input, &s);
    }

    #[test]
    fn test_roundtrip_ten() {
        let input = "0123456789";
        let sym = Symbol::from_str(input);
        let s: String = sym.into_iter().collect();
        assert_eq!(input, &s);
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
