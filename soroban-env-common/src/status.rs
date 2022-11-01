use crate::{
    decl_tagged_val_wrapper_methods, decl_wrapper_direct_abi_support, ConversionError, Env, EnvVal,
    RawVal, SymbolError, Tag,
};
use core::{
    convert::TryFrom,
    fmt::Debug,
    hash::{Hash, Hasher},
};
use stellar_xdr::{
    ScHostContextErrorCode, ScHostFnErrorCode, ScHostObjErrorCode, ScHostStorageErrorCode,
    ScHostValErrorCode, ScStatus, ScStatusType, ScUnknownErrorCode, ScVal, ScVmErrorCode,
};

/// Wrapper for a [RawVal] that is tagged with [Tag::Status], interpreting the
/// [RawVal]'s body as a pair of a 28-bit status-type code and a 32-bit status
/// code. The status-type codes correspond to the enumerated cases of
/// [ScStatusType], and the status codes correspond to the code values stored in
/// each variant of the [ScStatus] union.
#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Status(u64);

decl_wrapper_direct_abi_support!(Status, u64, I64, i64);
decl_tagged_val_wrapper_methods!(Status);

impl Status {
    pub const UNKNOWN_ERROR: Status = Status::from_type_and_code(ScStatusType::UnknownError, 0);
    pub const OK: Status = Status::from_type_and_code(ScStatusType::Ok, 0);
}

impl Hash for Status {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<Status> for RawVal {
    fn from(obj: Status) -> Self {
        unsafe { RawVal::from_lo64_and_tag(obj.0, Tag::Status) }
    }
}

impl crate::RawValConvertible for Status {
    #[inline(always)]
    fn is_val_type(v: RawVal) -> bool {
        v.has_tag(Tag::Status)
    }
    #[inline(always)]
    unsafe fn unchecked_from_val(v: RawVal) -> Self {
        Status(v.get_lo64())
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

fn fmt_named_code<C: NamedCode>(code: i32, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
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
        let st_res: Result<ScStatusType, _> = (self.0 as i32).try_into();
        let code = self.get_code() as i32;
        let st = match st_res {
            Ok(t) => t,
            Err(_) => return write!(f, "Status(UnknownType)"),
        };
        write!(f, "Status({}(", st.name())?;
        match st {
            ScStatusType::Ok => write!(f, "{}", code),
            ScStatusType::UnknownError => write!(f, "{}", code),
            ScStatusType::HostValueError => fmt_named_code::<ScHostValErrorCode>(code, f),
            ScStatusType::HostObjectError => fmt_named_code::<ScHostObjErrorCode>(code, f),
            ScStatusType::HostFunctionError => fmt_named_code::<ScHostFnErrorCode>(code, f),
            ScStatusType::HostStorageError => fmt_named_code::<ScHostStorageErrorCode>(code, f),
            ScStatusType::HostContextError => fmt_named_code::<ScHostContextErrorCode>(code, f),
            ScStatusType::VmError => fmt_named_code::<ScVmErrorCode>(code, f),
            ScStatusType::ContractError => write!(f, "{}", code),
        }?;
        write!(f, "))")
    }
}

impl TryFrom<Status> for ScStatus {
    type Error = stellar_xdr::Error;
    fn try_from(st: Status) -> Result<Self, Self::Error> {
        let ok = {
            if st.is_type(ScStatusType::Ok) {
                ScStatus::Ok
            } else if st.is_type(ScStatusType::UnknownError) {
                ScStatus::UnknownError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::HostValueError) {
                ScStatus::HostValueError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::HostObjectError) {
                ScStatus::HostObjectError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::HostFunctionError) {
                ScStatus::HostFunctionError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::HostStorageError) {
                ScStatus::HostStorageError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::HostContextError) {
                ScStatus::HostContextError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::VmError) {
                ScStatus::VmError((st.get_code() as i32).try_into()?)
            } else if st.is_type(ScStatusType::ContractError) {
                ScStatus::ContractError(st.get_code() as u32)
            } else {
                return Err(stellar_xdr::Error::Invalid);
            }
        };
        Ok(ok)
    }
}

impl TryFrom<Status> for ScVal {
    type Error = stellar_xdr::Error;
    fn try_from(st: Status) -> Result<Self, Self::Error> {
        Ok(ScVal::Status(<_ as TryInto<ScStatus>>::try_into(st)?))
    }
}

impl From<ScStatus> for Status {
    fn from(st: ScStatus) -> Self {
        Status::from_status(st)
    }
}

impl From<ScUnknownErrorCode> for Status {
    fn from(code: ScUnknownErrorCode) -> Self {
        ScStatus::UnknownError(code).into()
    }
}

impl From<ScHostValErrorCode> for Status {
    fn from(code: ScHostValErrorCode) -> Self {
        ScStatus::HostValueError(code).into()
    }
}

impl From<ScHostObjErrorCode> for Status {
    fn from(code: ScHostObjErrorCode) -> Self {
        ScStatus::HostObjectError(code).into()
    }
}

impl From<ScHostFnErrorCode> for Status {
    fn from(code: ScHostFnErrorCode) -> Self {
        ScStatus::HostFunctionError(code).into()
    }
}

impl From<ScHostStorageErrorCode> for Status {
    fn from(code: ScHostStorageErrorCode) -> Self {
        ScStatus::HostStorageError(code).into()
    }
}

impl From<ScHostContextErrorCode> for Status {
    fn from(code: ScHostContextErrorCode) -> Self {
        ScStatus::HostContextError(code).into()
    }
}

impl From<ScVmErrorCode> for Status {
    fn from(code: ScVmErrorCode) -> Self {
        ScStatus::VmError(code).into()
    }
}

impl From<SymbolError> for Status {
    fn from(se: SymbolError) -> Self {
        let s = match se {
            SymbolError::TooLong(_) => ScHostValErrorCode::SymbolTooLong,
            SymbolError::BadChar(_) => ScHostValErrorCode::SymbolBadChar,
        };
        ScStatus::HostValueError(s).into()
    }
}

impl From<ConversionError> for Status {
    fn from(_: ConversionError) -> Self {
        let s = ScHostValErrorCode::UnexpectedValType;
        ScStatus::HostValueError(s).into()
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatusType) -> bool {
        (self.0 as u32) == (ty as u32)
    }

    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        (self.0 >> 32) as u32
    }

    #[inline(always)]
    pub const fn to_raw(&self) -> RawVal {
        unsafe { RawVal::from_lo64_and_tag(self.0, Tag::Status) }
    }

    #[inline(always)]
    pub const fn is_ok(&self) -> bool {
        self.is_type(ScStatusType::Ok)
    }

    #[inline(always)]
    pub const fn from_contract_error(code: u32) -> Status {
        Self::from_type_and_code(ScStatusType::ContractError, code)
    }

    #[inline(always)]
    pub const fn from_type_and_code(ty: ScStatusType, code: u32) -> Status {
        Self((code as u64) << 32 | (ty as u64))
    }

    #[inline(always)]
    pub const fn from_status(sc: ScStatus) -> Status {
        let code = match sc {
            ScStatus::Ok => 0,
            ScStatus::HostContextError(code) => code as i32 as u32,
            ScStatus::HostValueError(code) => code as i32 as u32,
            ScStatus::HostObjectError(code) => code as i32 as u32,
            ScStatus::HostFunctionError(code) => code as i32 as u32,
            ScStatus::HostStorageError(code) => code as i32 as u32,
            ScStatus::VmError(code) => code as i32 as u32,
            ScStatus::UnknownError(code) => code as i32 as u32,
            ScStatus::ContractError(code) => code as u32,
        };
        Self::from_type_and_code(sc.discriminant(), code)
    }
}

impl From<core::convert::Infallible> for crate::Status {
    fn from(_: core::convert::Infallible) -> Self {
        panic!()
    }
}
