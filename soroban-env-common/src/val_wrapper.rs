#[macro_export]
macro_rules! decl_tagged_val_wrapper {
    ($tagname:ident) => {
        // Declare the wrapper type.
        #[derive(Copy, Clone)]
        pub struct $tagname(RawVal);

        // AsRef / AsMut to RawVal.
        impl AsRef<RawVal> for $tagname {
            fn as_ref(&self) -> &RawVal {
                &self.0
            }
        }
        impl AsMut<RawVal> for $tagname {
            fn as_mut(&mut self) -> &mut RawVal {
                &mut self.0
            }
        }
        // Basic conversion support: wrapper to raw, and try-into helper.
        impl From<$tagname> for RawVal {
            fn from(b: $tagname) -> Self {
                b.0
            }
        }
        impl crate::RawValConvertible for $tagname {
            #[inline(always)]
            fn is_val_type(v: RawVal) -> bool {
                v.has_tag(Tag::$tagname)
            }
            #[inline(always)]
            unsafe fn unchecked_from_val(v: RawVal) -> Self {
                $tagname(v)
            }
        }

        // EnvVal ref/mut/from support
        impl<E: Env> AsRef<$tagname> for EnvVal<E, $tagname> {
            fn as_ref(&self) -> &$tagname {
                &self.val
            }
        }
        impl<E: Env> AsMut<$tagname> for EnvVal<E, $tagname> {
            fn as_mut(&mut self) -> &mut $tagname {
                &mut self.val
            }
        }
        impl<E: Env> From<EnvVal<E, $tagname>> for EnvVal<E, RawVal> {
            fn from(ev: EnvVal<E, $tagname>) -> Self {
                EnvVal {
                    env: ev.env,
                    val: ev.val.to_raw(),
                }
            }
        }
        impl<E: Env> From<EnvVal<E, $tagname>> for RawVal {
            fn from(ev: EnvVal<E, $tagname>) -> Self {
                ev.val.0
            }
        }
        impl<E: Env> From<EnvVal<E, $tagname>> for $tagname {
            fn from(ev: EnvVal<E, $tagname>) -> Self {
                ev.val
            }
        }
        impl<E: Env> EnvVal<E, $tagname> {
            pub fn as_raw(&self) -> &RawVal {
                self.val.as_ref()
            }
            pub fn to_raw(&self) -> RawVal {
                self.val.to_raw()
            }
        }

        // wasmi / VM argument support
        #[cfg(feature = "vm")]
        impl wasmi::FromValue for $tagname {
            fn from_value(val: wasmi::RuntimeValue) -> Option<Self> {
                let maybe: Option<u64> = val.try_into();
                match maybe {
                    Some(u) => {
                        let raw = RawVal::from_payload(u);
                        if <Self as crate::RawValConvertible>::is_val_type(raw) {
                            Some(unsafe {
                                <Self as crate::RawValConvertible>::unchecked_from_val(raw)
                            })
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
        }
        #[cfg(feature = "vm")]
        impl From<$tagname> for wasmi::RuntimeValue {
            fn from(v: $tagname) -> Self {
                wasmi::RuntimeValue::I64(v.as_raw().get_payload() as i64)
            }
        }

        // Various inherent helper / disambiguation methods which
        // may or may-not ever get used per-type, so allowed-dead.
        #[allow(dead_code)]
        impl $tagname {
            pub const fn as_raw(&self) -> &RawVal {
                &self.0
            }

            pub const fn to_raw(&self) -> RawVal {
                self.0
            }

            pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, Self> {
                EnvVal {
                    env: env.clone(),
                    val: self,
                }
            }

            #[inline(always)]
            pub(crate) const unsafe fn from_body(body: u64) -> $tagname {
                let rv = RawVal::from_body_and_tag(body, Tag::$tagname);
                Self(rv)
            }

            #[inline(always)]
            pub(crate) const unsafe fn from_major_minor(major: u32, minor: u32) -> $tagname {
                let rv = RawVal::from_major_minor_and_tag(major, minor, Tag::$tagname);
                Self(rv)
            }
        }
    };
}

#[macro_export]
macro_rules! impl_wrapper_from {
    ($fromty:ty, $tagname:ident) => {
        impl From<$fromty> for $tagname {
            fn from(x: $fromty) -> Self {
                Self(x.into())
            }
        }
        impl<E: Env> TryFrom<EnvVal<E, $tagname>> for $fromty {
            type Error = crate::ConversionError;
            #[inline(always)]
            fn try_from(v: EnvVal<E, $tagname>) -> Result<Self, Self::Error> {
                Self::try_from(v.to_raw())
            }
        }
    };
}
