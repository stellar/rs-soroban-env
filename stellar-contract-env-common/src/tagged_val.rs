use crate::Env;
use crate::EnvVal;
/// TaggedVal and TaggedEnvVal are a RawVal and EnvVal wrappers that are statically known to be of a specific tag-case.
/// Additional tag-case-specific methods can be hung off such a value, while still allowing access
/// to its inner RawVal/EnvVal using AsRef/AsMut.
use crate::RawVal;
use crate::RawValConvertable;
use crate::Tag;
use core::marker::PhantomData;

pub trait TagType: Copy + Clone {
    const TAG: Tag;
}

#[derive(Copy, Clone)]
pub struct TagU32;
#[derive(Copy, Clone)]
pub struct TagI32;
#[derive(Copy, Clone)]
pub struct TagStatic;
#[derive(Copy, Clone)]
pub struct TagObject;
#[derive(Copy, Clone)]
pub struct TagSymbol;
#[derive(Copy, Clone)]
pub struct TagBitSet;
#[derive(Copy, Clone)]
pub struct TagStatus;

impl TagType for TagU32 {
    const TAG: Tag = Tag::U32;
}
impl TagType for TagI32 {
    const TAG: Tag = Tag::I32;
}
impl TagType for TagStatic {
    const TAG: Tag = Tag::Static;
}
impl TagType for TagObject {
    const TAG: Tag = Tag::Object;
}
impl TagType for TagSymbol {
    const TAG: Tag = Tag::Symbol;
}
impl TagType for TagBitSet {
    const TAG: Tag = Tag::BitSet;
}
impl TagType for TagStatus {
    const TAG: Tag = Tag::Status;
}

#[derive(Copy, Clone, Debug)]
pub struct TaggedVal<T: TagType>(pub(crate) RawVal, pub(crate) PhantomData<T>);

impl<T: TagType> AsRef<RawVal> for TaggedVal<T> {
    fn as_ref(&self) -> &RawVal {
        &self.0
    }
}

impl<T: TagType> AsMut<RawVal> for TaggedVal<T> {
    fn as_mut(&mut self) -> &mut RawVal {
        &mut self.0
    }
}

impl<T: TagType> TaggedVal<T> {
    pub fn in_env<E: Env>(&self, env: &E) -> EnvVal<E, Self> {
        EnvVal {
            env: env.clone(),
            val: *self,
        }
    }

    #[inline(always)]
    pub(crate) unsafe fn from_body_and_tag_type(body: u64) -> TaggedVal<T> {
        let rv = RawVal::from_body_and_tag(body, T::TAG);
        Self(rv, PhantomData)
    }

    #[inline(always)]
    pub(crate) unsafe fn from_major_minor_and_tag_type(major: u32, minor: u32) -> TaggedVal<T> {
        let rv = RawVal::from_major_minor_and_tag(major, minor, T::TAG);
        Self(rv, PhantomData)
    }
}

macro_rules! impl_tagged_from {
    ($fromty:ty, $tagty:ty) => {
        impl From<$fromty> for TaggedVal<$tagty> {
            fn from(x: $fromty) -> Self {
                Self(x.into(), PhantomData)
            }
        }
    };
}

impl_tagged_from!((), TagStatic);
impl_tagged_from!(bool, TagStatic);
impl_tagged_from!(i32, TagI32);
impl_tagged_from!(u32, TagU32);

impl<T: TagType> From<TaggedVal<T>> for RawVal {
    fn from(tv: TaggedVal<T>) -> Self {
        tv.0
    }
}

impl<T: TagType> TryFrom<RawVal> for TaggedVal<T> {
    type Error = ();

    fn try_from(rv: RawVal) -> Result<Self, Self::Error> {
        if rv.has_tag(T::TAG) {
            Ok(Self(rv, PhantomData))
        } else {
            Err(())
        }
    }
}

impl<T: TagType> RawValConvertable for TaggedVal<T> {
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(T::TAG)
    }

    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Self(v, PhantomData)
    }
}

#[cfg(feature = "vm")]
impl<T: TagType> wasmi::FromRuntimeValue for TaggedVal<T> {
    fn from_runtime_value(val: wasmi::RuntimeValue) -> Option<Self> {
        let maybe: Option<RawVal> = val.try_into();
        maybe.map(|x| Self(x, PhantomData))
    }
}

#[cfg(feature = "vm")]
impl<T: TagType> From<TaggedVal<T>> for wasmi::RuntimeValue {
    fn from(v: TaggedVal<T>) -> Self {
        wasmi::RuntimeValue::I64(v.as_ref().get_payload() as i64)
    }
}
