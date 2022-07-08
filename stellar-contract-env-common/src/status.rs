use crate::{ConversionError, Env, EnvVal, RawVal, Tag, TagStatus, TaggedVal, Val};
use core::{
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
    marker::PhantomData,
};
use stellar_xdr::{
    ScHostContextErrorCode, ScHostFnErrorCode, ScHostObjErrorCode, ScHostStorageErrorCode,
    ScHostValErrorCode, ScStatus, ScStatusType, ScVmErrorCode,
};

pub type Status = TaggedVal<TagStatus>;

impl From<RawVal> for Result<RawVal, Status> {
    fn from(v: RawVal) -> Self {
        match Status::try_from(v) {
            Ok(status) => Err(status),
            Err(ConversionError) => Ok(v),
        }
    }
}

impl<E: Env, V: Val, T: TryFrom<EnvVal<E, V>>> TryFrom<EnvVal<E, V>> for Result<T, Status>
where
    Status: TryFrom<EnvVal<E, V>>,
{
    type Error = T::Error;

    fn try_from(v: EnvVal<E, V>) -> Result<Self, Self::Error> {
        let result: Result<Status, _> = Status::try_from(v.clone());
        match result {
            Ok(status) => Ok(Err(status)),
            Err(_) => Ok(Ok(T::try_from(v)?)),
        }
    }
}

pub const UNKNOWN_ERROR: Status = TaggedVal(
    unsafe { RawVal::from_major_minor_and_tag(0, ScStatusType::UnknownError as u32, Tag::Status) },
    PhantomData,
);
pub const OK: Status = TaggedVal(
    unsafe { RawVal::from_major_minor_and_tag(0, ScStatusType::Ok as u32, Tag::Status) },
    PhantomData,
);

impl Hash for Status {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().get_payload().hash(state);
    }
}

impl PartialEq for Status {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().get_payload() == other.as_ref().get_payload()
    }
}

impl Eq for Status {}

impl PartialOrd for Status {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Status {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        let self_tup = (self.as_ref().get_minor(), self.as_ref().get_major());
        let other_tup = (other.as_ref().get_minor(), other.as_ref().get_major());
        self_tup.cmp(&other_tup)
    }
}

// This trait just lets us write `enum_name_or_unknown` below in a generic
// fashion. Ideally there'd be a trait for "codes that can have .name() called
// on them and return a &'static str" in the XDR crate, but that's not available
// yet: there's no trait and the method that does exist returns a non-static
// &str. See https://github.com/stellar/xdrgen/issues/107 and
// https://github.com/stellar/xdrgen/issues/108
trait NamedCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
}

impl NamedCode for ScHostContextErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl NamedCode for ScHostFnErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl NamedCode for ScHostObjErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl NamedCode for ScHostStorageErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl NamedCode for ScHostValErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl NamedCode for ScVmErrorCode {
    fn fmt_code_name(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

fn fmt_named_code<C: NamedCode>(code: u32, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
where
    C: TryFrom<i32>,
{
    match C::try_from(code as i32) {
        Ok(c) => c.fmt_code_name(f),
        Err(_) => write!(f, "UnknownCode"),
    }
}

impl Debug for Status {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let st_res: Result<ScStatusType, _> = (self.0.get_minor() as i32).try_into();
        let code = self.0.get_major();
        let st = match st_res {
            Ok(t) => t,
            Err(_) => return write!(f, "Status(UnknownType)"),
        };
        write!(f, "Status({}(", st.name())?;
        match st {
            ScStatusType::HostContextError => fmt_named_code::<ScHostContextErrorCode>(code, f),
            ScStatusType::Ok => write!(f, "{}", code),
            ScStatusType::UnknownError => write!(f, "{}", code),
            ScStatusType::HostValueError => fmt_named_code::<ScHostValErrorCode>(code, f),
            ScStatusType::HostObjectError => fmt_named_code::<ScHostObjErrorCode>(code, f),
            ScStatusType::HostFunctionError => fmt_named_code::<ScHostFnErrorCode>(code, f),
            ScStatusType::HostStorageError => fmt_named_code::<ScHostStorageErrorCode>(code, f),
            ScStatusType::VmError => fmt_named_code::<ScVmErrorCode>(code, f),
        }?;
        write!(f, "))")
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatusType) -> bool {
        self.0.has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        self.0.get_major()
    }

    #[inline(always)]
    pub const fn is_ok(&self) -> bool {
        self.is_type(ScStatusType::Ok)
    }

    #[inline(always)]
    pub const fn from_type_and_code(ty: ScStatusType, code: u32) -> Status {
        // Unfortunately we can't use from_major_minor_and_tag_type here because
        // it's not const, and making it const requires nightly.
        unsafe {
            Self(
                RawVal::from_major_minor_and_tag(code, ty as u32, Tag::Status),
                PhantomData,
            )
        }
    }

    // TODO: this should be a const fn, waiting on
    // https://github.com/stellar/xdrgen/issues/106 for the discriminant
    // function call to be const.
    #[inline(always)]
    pub fn from_status(sc: ScStatus) -> Status {
        let code: i32 = match sc {
            ScStatus::Ok => 0,
            ScStatus::HostContextError(code) => code as i32,
            ScStatus::HostValueError(code) => code as i32,
            ScStatus::HostObjectError(code) => code as i32,
            ScStatus::HostFunctionError(code) => code as i32,
            ScStatus::HostStorageError(code) => code as i32,
            ScStatus::VmError(code) => code as i32,
            ScStatus::UnknownError(code) => code as i32,
        };
        Self::from_type_and_code(sc.discriminant(), code as u32)
    }
}
