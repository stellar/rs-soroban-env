#[doc(hidden)]
#[macro_export]
macro_rules! decl_tagged_val_wrapper_methods {
    ($tagname:ident) => {
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
                    val: ev.val.into(),
                }
            }
        }
        impl<E: Env> From<EnvVal<E, $tagname>> for RawVal {
            fn from(ev: EnvVal<E, $tagname>) -> Self {
                ev.val.into()
            }
        }
        impl<E: Env> From<EnvVal<E, $tagname>> for $tagname {
            fn from(ev: EnvVal<E, $tagname>) -> Self {
                ev.val
            }
        }
        impl<E: Env> EnvVal<E, $tagname> {
            pub fn to_raw(&self) -> RawVal {
                self.val.clone().into()
            }
        }

        // Various inherent helper / disambiguation methods which
        // may or may-not ever get used per-type, so allowed-dead.
        #[allow(dead_code)]
        impl $tagname {
            pub fn in_env<E: Env>(self, env: &E) -> EnvVal<E, Self> {
                EnvVal {
                    env: env.clone(),
                    val: self,
                }
            }
        }
    };
}
