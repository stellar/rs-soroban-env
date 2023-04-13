use crate::{
    impl_wrapper_as_and_to_rawval, impl_wrapper_tag_based_constructors,
    impl_wrapper_tag_based_rawvalconvertible, impl_wrapper_wasmi_conversions, Compare,
    ConversionError, Env, RawVal, SymbolError,
};
use core::{
    cmp::Ordering,
    convert::TryFrom,
    fmt::Debug,
    hash::{Hash, Hasher},
};
use stellar_xdr::{
    ScHostAuthErrorCode, ScHostContextErrorCode, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScStatus, ScStatusType, ScUnknownErrorCode, ScVal,
    ScVmErrorCode,
};

/// Wrapper for a [RawVal] that is tagged with [Tag::Status], interpreting the
/// [RawVal]'s body as a pair of a 28-bit status-type code and a 32-bit status
/// code. The status-type codes correspond to the enumerated cases of
/// [ScStatusType], and the status codes correspond to the code values stored in
/// each variant of the [ScStatus] union.
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Status(RawVal);

impl_wrapper_tag_based_rawvalconvertible!(Status);
impl_wrapper_tag_based_constructors!(Status);
impl_wrapper_as_and_to_rawval!(Status);
impl_wrapper_wasmi_conversions!(Status);

impl Status {
    pub const UNKNOWN_ERROR: Status =
        unsafe { Status::from_major_minor(0, ScStatusType::UnknownError as u32) };
    pub const OK: Status = unsafe { Status::from_major_minor(0, ScStatusType::Ok as u32) };
}

impl Hash for Status {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_raw().get_payload().hash(state);
    }
}

impl PartialEq for Status {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_raw().get_payload() == other.as_raw().get_payload()
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
        let self_tup = (self.as_raw().get_minor(), self.as_raw().get_major());
        let other_tup = (other.as_raw().get_minor(), other.as_raw().get_major());
        self_tup.cmp(&other_tup)
    }
}

impl<E: Env> Compare<Status> for E {
    type Error = E::Error;
    fn compare(&self, a: &Status, b: &Status) -> Result<Ordering, Self::Error> {
        Ok(a.cmp(&b))
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

impl NamedCode for ScHostAuthErrorCode {
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
        let st_res: Result<ScStatusType, _> = (self.as_raw().get_minor() as i32).try_into();
        let code = self.as_raw().get_major();
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
            ScStatusType::HostAuthError => fmt_named_code::<ScHostAuthErrorCode>(code, f),
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
                ScStatus::ContractError(st.get_code())
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

impl From<ScHostAuthErrorCode> for Status {
    fn from(code: ScHostAuthErrorCode) -> Self {
        ScStatus::HostAuthError(code).into()
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

impl From<stellar_xdr::Error> for Status {
    fn from(_value: stellar_xdr::Error) -> Self {
        ScStatus::UnknownError(ScUnknownErrorCode::Xdr).into()
    }
}

impl Status {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScStatusType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, ty: ScStatusType) -> bool {
        self.as_raw().has_minor(ty as u32)
    }

    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        self.as_raw().get_major()
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
        unsafe { Self::from_major_minor(code, ty as u32) }
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
            ScStatus::HostAuthError(code) => code as i32 as u32,
            ScStatus::VmError(code) => code as i32 as u32,
            ScStatus::UnknownError(code) => code as i32 as u32,
            ScStatus::ContractError(code) => code as u32,
        };
        Self::from_type_and_code(sc.discriminant(), code)
    }
}

impl From<core::convert::Infallible> for crate::Status {
    fn from(_: core::convert::Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    #[test]
    fn status_ord_same_as_scstatus() {
        // The impl `Ord for Status` must agree with `Ord for ScStatus`,
        // re https://github.com/stellar/rs-soroban-env/issues/743.
        //
        // This test creates pairs of corresponding ScStatus/Status values,
        // puts them all into a list, and sorts them with each comparison function,
        // then checks that both lists are sorted the same.

        use crate::xdr::*;

        let xdr_vals = &[
            ScStatus::Ok,
            ScStatus::UnknownError(ScUnknownErrorCode::General),
            ScStatus::UnknownError(ScUnknownErrorCode::Xdr),
            ScStatus::HostValueError(ScHostValErrorCode::UnknownError),
            ScStatus::HostValueError(ScHostValErrorCode::ReservedTagValue),
            ScStatus::HostObjectError(ScHostObjErrorCode::UnknownError),
            ScStatus::HostObjectError(ScHostObjErrorCode::UnknownReference),
            ScStatus::HostFunctionError(ScHostFnErrorCode::UnknownError),
            ScStatus::HostFunctionError(ScHostFnErrorCode::UnexpectedHostFunctionAction),
            ScStatus::HostStorageError(ScHostStorageErrorCode::UnknownError),
            ScStatus::HostStorageError(ScHostStorageErrorCode::ExpectContractData),
            ScStatus::HostContextError(ScHostContextErrorCode::UnknownError),
            ScStatus::HostContextError(ScHostContextErrorCode::NoContractRunning),
            ScStatus::VmError(ScVmErrorCode::Unknown),
            ScStatus::VmError(ScVmErrorCode::Validation),
            ScStatus::ContractError(0),
            ScStatus::ContractError(1),
            ScStatus::HostAuthError(ScHostAuthErrorCode::UnknownError),
            ScStatus::HostAuthError(ScHostAuthErrorCode::NonceError),
        ];

        let pairs: Vec<_> = xdr_vals
            .into_iter()
            .map(|xdr_val| {
                let host_val = Status::try_from(xdr_val.clone()).unwrap();
                (xdr_val, host_val)
            })
            .collect();

        let mut pairs_xdr_sorted = pairs.clone();
        let mut pairs_host_sorted = pairs_xdr_sorted.clone();

        pairs_xdr_sorted.sort_by(|&(v1, _), &(v2, _)| v1.cmp(&v2));

        pairs_host_sorted.sort_by(|&(_, v1), &(_, v2)| v1.cmp(&v2));

        assert_eq!(pairs_xdr_sorted, pairs_host_sorted);
    }
}
