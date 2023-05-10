#[doc(hidden)]
#[macro_export]
macro_rules! impl_wrapper_tag_based_rawvalconvertible {
    ($tagname:ident) => {
        // A RawValConvertible impl for types where the wrapper _has the same
        // name_ as a Tag::case and being-that-wrapper is identical to
        // having-that-tag.
        impl $crate::RawValConvertible for $tagname {
            #[inline(always)]
            fn is_val_type(v: $crate::RawVal) -> bool {
                v.has_tag($crate::Tag::$tagname)
            }
            #[inline(always)]
            unsafe fn unchecked_from_val(v: $crate::RawVal) -> Self {
                $tagname(v)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_wrapper_tag_based_constructors {
    ($tagname:ident) => {
        #[allow(dead_code)]
        impl $tagname {
            #[inline(always)]
            pub(crate) const unsafe fn from_body(body: u64) -> $tagname {
                let rv = $crate::RawVal::from_body_and_tag(body, $crate::Tag::$tagname);
                Self(rv)
            }

            #[inline(always)]
            pub(crate) const unsafe fn from_major_minor(major: u32, minor: u32) -> $tagname {
                let rv =
                    $crate::RawVal::from_major_minor_and_tag(major, minor, $crate::Tag::$tagname);
                Self(rv)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible {
    ($T:ty) => {
        impl TryFrom<$crate::RawVal> for $T {
            type Error = $crate::ConversionError;
            #[inline(always)]
            fn try_from(v: $crate::RawVal) -> Result<Self, Self::Error> {
                if let Some(c) = <Self as $crate::RawValConvertible>::try_convert(v) {
                    Ok(c)
                } else {
                    Err($crate::ConversionError)
                }
            }
        }
        impl TryFrom<&$crate::RawVal> for $T {
            type Error = $crate::ConversionError;
            #[inline(always)]
            fn try_from(v: &$crate::RawVal) -> Result<Self, Self::Error> {
                Self::try_from(*v)
            }
        }
        impl<E: $crate::Env> $crate::TryFromVal<E, $crate::RawVal> for $T {
            type Error = $crate::ConversionError;
            #[inline(always)]
            fn try_from_val(_env: &E, val: &$crate::RawVal) -> Result<Self, Self::Error> {
                Self::try_from(*val)
            }
        }
        impl<E: $crate::Env> $crate::TryFromVal<E, $T> for $crate::RawVal {
            type Error = $crate::ConversionError;
            fn try_from_val(_env: &E, val: &$T) -> Result<Self, Self::Error> {
                Ok((*val).into())
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_wrapper_wasmi_conversions {
    ($wrapper:ty) => {
        // wasmi / VM argument support
        #[cfg(feature = "wasmi")]
        impl wasmi::core::FromValue for $wrapper {
            fn from_value(val: wasmi::core::Value) -> Option<Self> {
                let maybe: Option<u64> = <u64 as wasmi::core::FromValue>::from_value(val);
                match maybe {
                    Some(u) => {
                        let raw = $crate::RawVal::from_payload(u);
                        if <Self as $crate::RawValConvertible>::is_val_type(raw) {
                            Some(unsafe {
                                <Self as $crate::RawValConvertible>::unchecked_from_val(raw)
                            })
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
        }
        #[cfg(feature = "wasmi")]
        impl From<$wrapper> for wasmi::core::Value {
            fn from(v: $wrapper) -> Self {
                wasmi::core::Value::I64(v.as_raw().get_payload() as i64)
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_wrapper_as_and_to_rawval {
    ($wrapper:ty) => {
        // AsRef / AsMut to RawVal.
        impl AsRef<$crate::RawVal> for $wrapper {
            fn as_ref(&self) -> &$crate::RawVal {
                &self.0
            }
        }
        impl AsMut<$crate::RawVal> for $wrapper {
            fn as_mut(&mut self) -> &mut $crate::RawVal {
                &mut self.0
            }
        }
        // Basic conversion support: wrapper to raw, and try-into helper.
        impl From<$wrapper> for $crate::RawVal {
            fn from(b: $wrapper) -> Self {
                b.0
            }
        }

        impl From<&$wrapper> for $crate::RawVal {
            fn from(b: &$wrapper) -> Self {
                b.0
            }
        }

        // Various inherent helper / disambiguation methods which
        // may or may-not ever get used per-type, so allowed-dead.
        #[allow(dead_code)]
        impl $wrapper {
            pub const fn as_raw(&self) -> &$crate::RawVal {
                &self.0
            }

            pub const fn to_raw(&self) -> $crate::RawVal {
                self.0
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! impl_wrapper_from_other_type {
    ($fromty:ty, $wrapper:ty) => {
        impl From<$fromty> for $wrapper {
            fn from(x: $fromty) -> Self {
                Self(x.into())
            }
        }
        impl<E: Env> $crate::TryFromVal<E, $wrapper> for $fromty {
            type Error = $crate::ConversionError;
            #[inline(always)]
            fn try_from_val(_env: &E, val: &$wrapper) -> Result<Self, Self::Error> {
                Self::try_from((*val).to_raw())
            }
        }
    };
}

/// Macro for base implementation of a type wrapping a [`RawVal`]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_rawval_wrapper_base {
    ($T:ident) => {
        impl core::fmt::Debug for $T {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                self.0.fmt(f)
            }
        }
        $crate::impl_wrapper_as_and_to_rawval!($T);
        $crate::impl_wrapper_wasmi_conversions!($T);
        $crate::impl_tryfroms_and_tryfromvals_delegating_to_rawvalconvertible!($T);
    };
}

/// Declares [`RawVal`]-wrapping type [`Tag`] where the wrapper _has the same
/// name_ as a Tag::case and being-that-wrapper is identical to having-that-tag.
#[doc(hidden)]
#[macro_export]
macro_rules! declare_tag_based_wrapper {
    ($T:ident) => {
        #[repr(transparent)]
        #[derive(Copy, Clone)]
        pub struct $T($crate::RawVal);

        $crate::impl_rawval_wrapper_base!($T);
        $crate::impl_wrapper_tag_based_rawvalconvertible!($T);
        $crate::impl_wrapper_tag_based_constructors!($T);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! declare_tag_based_object_wrapper {
    ($T:ident) => {
        $crate::declare_tag_based_wrapper!($T);
        impl From<$T> for $crate::Object {
            fn from(x: $T) -> $crate::Object {
                $crate::Object(x.0)
            }
        }
        impl TryFrom<$crate::Object> for $T {
            type Error = $crate::ConversionError;
            fn try_from(x: $crate::Object) -> Result<Self, Self::Error> {
                if x.as_raw().has_tag($crate::Tag::$T) {
                    Ok($T(x.0))
                } else {
                    Err($crate::ConversionError)
                }
            }
        }
        #[allow(dead_code)]
        impl $T {
            #[inline(always)]
            pub const unsafe fn from_handle(handle: u32) -> Self {
                let rv = $crate::Object::from_handle_and_tag(handle, $crate::Tag::$T);
                Self(rv.to_raw())
            }
            #[inline(always)]
            pub const fn get_handle(&self) -> u32 {
                $crate::Object(self.0).get_handle()
            }
        }
        impl<E: $crate::Env> $crate::Compare<$T> for E {
            type Error = E::Error;
            fn compare(&self, a: &$T, b: &$T) -> Result<core::cmp::Ordering, Self::Error> {
                self.compare(&$crate::Object(a.0), &$crate::Object(b.0))
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! declare_tag_based_small_and_object_wrappers {
    ($GENERAL:ident,$SMALL:ident,$OBJECT:ident) => {
        $crate::declare_tag_based_wrapper!($SMALL);
        $crate::declare_tag_based_object_wrapper!($OBJECT);

        #[repr(transparent)]
        #[derive(Copy, Clone)]
        pub struct $GENERAL($crate::RawVal);
        $crate::impl_rawval_wrapper_base!($GENERAL);

        impl $crate::RawValConvertible for $GENERAL {
            fn is_val_type(v: $crate::RawVal) -> bool {
                v.has_tag($crate::Tag::$SMALL) || v.has_tag($crate::Tag::$OBJECT)
            }

            unsafe fn unchecked_from_val(v: $crate::RawVal) -> Self {
                Self(v)
            }
        }

        impl From<$SMALL> for $GENERAL {
            fn from(s: $SMALL) -> Self {
                Self(s.0)
            }
        }

        impl From<$OBJECT> for $GENERAL {
            fn from(ob: $OBJECT) -> Self {
                Self(ob.0)
            }
        }

        impl TryFrom<$GENERAL> for $SMALL {
            type Error = $crate::ConversionError;
            fn try_from(s: $GENERAL) -> Result<Self, Self::Error> {
                $SMALL::try_from(s.0)
            }
        }

        impl TryFrom<$GENERAL> for $OBJECT {
            type Error = $crate::ConversionError;
            fn try_from(s: $GENERAL) -> Result<Self, Self::Error> {
                $OBJECT::try_from(s.0)
            }
        }

        impl $GENERAL {
            pub const fn from_small(s: $SMALL) -> Self {
                Self(s.to_raw())
            }
        }

        impl core::hash::Hash for $SMALL {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                self.as_raw().get_payload().hash(state);
            }
        }

        impl core::cmp::PartialEq for $SMALL {
            #[inline(always)]
            fn eq(&self, other: &Self) -> bool {
                self.as_raw().get_payload() == other.as_raw().get_payload()
            }
        }

        impl Eq for $SMALL {}

        impl<E: $crate::Env> $crate::Compare<$SMALL> for E {
            type Error = E::Error;
            fn compare(&self, a: &$SMALL, b: &$SMALL) -> Result<core::cmp::Ordering, Self::Error> {
                Ok(a.cmp(&b))
            }
        }

        impl core::cmp::PartialOrd for $SMALL {
            #[inline(always)]
            fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! declare_tag_based_unsigned_small_and_object_wrappers {
    ($GENERAL:ident,$SMALL:ident,$OBJECT:ident) => {
        declare_tag_based_small_and_object_wrappers!($GENERAL, $SMALL, $OBJECT);

        impl core::cmp::Ord for $SMALL {
            #[inline(always)]
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                self.0.get_body().cmp(&other.0.get_body())
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! declare_tag_based_signed_small_and_object_wrappers {
    ($GENERAL:ident,$SMALL:ident,$OBJECT:ident) => {
        declare_tag_based_small_and_object_wrappers!($GENERAL, $SMALL, $OBJECT);

        impl core::cmp::Ord for $SMALL {
            #[inline(always)]
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                self.0.get_signed_body().cmp(&other.0.get_signed_body())
            }
        }
    };
}
