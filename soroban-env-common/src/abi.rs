pub trait BufReadWrite: Sized {
    type MemBuf;
    const ZERO_BUF: Self::MemBuf;
    fn buf_as_slice(b: &Self::MemBuf) -> &[u8];
    fn buf_as_mut_slice(b: &mut Self::MemBuf) -> &mut [u8];
    fn buf_write(self, b: &mut Self::MemBuf);
    fn buf_read(b: &Self::MemBuf) -> Self;
}

pub trait V160: BufReadWrite {
    fn v160_explode(self) -> (u32, u64, u64);
    fn v160_implode(a: u32, b: u64, c: u64) -> Self;
}

pub trait V128: BufReadWrite {
    fn v128_explode(self) -> (u64, u64);
    fn v128_implode(a: u64, b: u64) -> Self;
}

// We implement ABIs for various primitive types here
// so that other impls can use them by ref and so that
// host functions can take primitive types as well.

impl BufReadWrite for u128 {
    type MemBuf = [u8; 16];
    const ZERO_BUF: Self::MemBuf = [0; 16];

    fn buf_write(self, b: &mut Self::MemBuf) {
        *b = u128::to_le_bytes(self)
    }

    fn buf_read(b: &Self::MemBuf) -> Self {
        u128::from_le_bytes(*b)
    }

    fn buf_as_slice(b: &Self::MemBuf) -> &[u8] {
        b.as_slice()
    }

    fn buf_as_mut_slice(b: &mut Self::MemBuf) -> &mut [u8] {
        b.as_mut_slice()
    }
}

impl BufReadWrite for i128 {
    type MemBuf = [u8; 16];
    const ZERO_BUF: Self::MemBuf = [0; 16];

    fn buf_write(self, b: &mut Self::MemBuf) {
        *b = i128::to_le_bytes(self)
    }

    fn buf_read(b: &Self::MemBuf) -> Self {
        i128::from_le_bytes(*b)
    }

    fn buf_as_slice(b: &Self::MemBuf) -> &[u8] {
        b.as_slice()
    }

    fn buf_as_mut_slice(b: &mut Self::MemBuf) -> &mut [u8] {
        b.as_mut_slice()
    }
}

impl V128 for u128 {
    fn v128_explode(self) -> (u64, u64) {
        (self as u64, (self >> 64) as u64)
    }

    fn v128_implode(a: u64, b: u64) -> Self {
        a as u128 | ((b as u128) << 64)
    }
}

impl V128 for i128 {
    fn v128_explode(self) -> (u64, u64) {
        (self as u64, (self >> 64) as u64)
    }

    fn v128_implode(a: u64, b: u64) -> Self {
        a as i128 | ((b as i128) << 64)
    }
}

pub trait Direct {
    type Repr;
    fn to_repr(self) -> Self::Repr;
    fn from_repr(r: Self::Repr) -> Self;
}

macro_rules! decl_trivial_direct_return {
    ($T:ty, $repr:ty) => {
        impl Direct for $T {
            type Repr = $repr;
            fn to_repr(self) -> Self::Repr {
                self as $repr
            }
            fn from_repr(r: Self::Repr) -> Self {
                r as Self
            }
        }
    };
}

decl_trivial_direct_return!(u32, i32);
decl_trivial_direct_return!(i32, i32);
decl_trivial_direct_return!(u64, i64);
decl_trivial_direct_return!(i64, i64);

#[doc(hidden)]
#[macro_export]
macro_rules! decl_wrapper_direct_abi_support {
    ($wrapper:ident, $wrapper_ty:ty, $wasmi_tag:ident, $wasmi_ty:ty) => {
        #[cfg(feature = "vm")]
        impl wasmi::core::FromValue for $wrapper {
            fn from_value(val: wasmi::core::Value) -> Option<Self> {
                <$wrapper_ty as wasmi::core::FromValue>::from_value(val).map(|x| $wrapper(x))
            }
        }
        #[cfg(feature = "vm")]
        impl From<$wrapper> for wasmi::core::Value {
            fn from(v: $wrapper) -> Self {
                wasmi::core::Value::$wasmi_tag(v.0 as $wasmi_ty)
            }
        }
        impl crate::abi::Direct for $wrapper {
            type Repr = $wasmi_ty;
            fn to_repr(self) -> Self::Repr {
                self.0 as Self::Repr
            }
            fn from_repr(r: Self::Repr) -> Self {
                Self(r as $wrapper_ty)
            }
        }
    };
}
