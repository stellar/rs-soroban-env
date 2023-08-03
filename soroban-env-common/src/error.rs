use crate::{
    impl_wrapper_as_and_to_rawval, impl_wrapper_tag_based_constructors,
    impl_wrapper_tag_based_rawvalconvertible, impl_wrapper_wasmi_conversions, Compare,
    ConversionError, Env, SymbolError, Val,
};
use core::{
    cmp::Ordering,
    convert::TryFrom,
    fmt::Debug,
    hash::{Hash, Hasher},
};
use stellar_xdr::{ScError, ScErrorCode, ScErrorType, ScVal};

/// Wrapper for a [Val] that is tagged with [Tag::Error], interpreting the
/// [Val]'s body as a pair of a 28-bit status-type code and a 32-bit status
/// code. The status-type codes correspond to the enumerated cases of
/// [ScErrorType], and the status codes correspond to the code values stored in
/// each variant of the [ScError] union.
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Error(Val);

impl_wrapper_tag_based_rawvalconvertible!(Error);
impl_wrapper_tag_based_constructors!(Error);
impl_wrapper_as_and_to_rawval!(Error);
impl_wrapper_wasmi_conversions!(Error);

impl Hash for Error {
    #[inline(always)]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_val().get_payload().hash(state);
    }
}

impl PartialEq for Error {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.as_val().get_payload() == other.as_val().get_payload()
    }
}

impl Eq for Error {}

impl PartialOrd for Error {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Error {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        let self_tup = (self.as_val().get_minor(), self.as_val().get_major());
        let other_tup = (other.as_val().get_minor(), other.as_val().get_major());
        self_tup.cmp(&other_tup)
    }
}

impl<E: Env> Compare<Error> for E {
    type Error = E::Error;
    fn compare(&self, a: &Error, b: &Error) -> Result<Ordering, Self::Error> {
        Ok(a.cmp(b))
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let (min, maj) = (
            self.as_val().get_minor() as i32,
            self.as_val().get_major() as i32,
        );
        if let Ok(type_) = ScErrorType::try_from(min) {
            if type_ == ScErrorType::Contract {
                write!(f, "Error({}, #{})", type_.name(), maj)
            } else if let Ok(code) = ScErrorCode::try_from(maj) {
                write!(f, "Error({}, {})", type_.name(), code.name())
            } else {
                write!(f, "Error({}, #{})", type_.name(), maj)
            }
        } else {
            if let Ok(code) = ScErrorCode::try_from(maj) {
                write!(f, "Error(#{}, {})", min, code.name())
            } else {
                write!(f, "Error(#{}, #{})", min, maj)
            }
        }
    }
}

impl<'a> From<&'a Error> for Error {
    fn from(value: &'a Error) -> Self {
        *value
    }
}

impl TryFrom<Error> for ScError {
    type Error = stellar_xdr::Error;
    fn try_from(er: Error) -> Result<Self, Self::Error> {
        let type_: ScErrorType = (er.as_val().get_minor() as i32).try_into()?;
        let u: u32 = er.as_val().get_major();
        Ok(match type_ {
            ScErrorType::Contract => ScError::Contract(u),
            ScErrorType::WasmVm => ScError::WasmVm((u as i32).try_into()?),
            ScErrorType::Context => ScError::Context((u as i32).try_into()?),
            ScErrorType::Storage => ScError::Storage((u as i32).try_into()?),
            ScErrorType::Object => ScError::Object((u as i32).try_into()?),
            ScErrorType::Crypto => ScError::Crypto((u as i32).try_into()?),
            ScErrorType::Events => ScError::Events((u as i32).try_into()?),
            ScErrorType::Budget => ScError::Budget((u as i32).try_into()?),
            ScErrorType::Value => ScError::Value((u as i32).try_into()?),
            ScErrorType::Auth => ScError::Auth((u as i32).try_into()?),
        })
    }
}

impl TryFrom<Error> for ScVal {
    type Error = stellar_xdr::Error;
    fn try_from(st: Error) -> Result<Self, stellar_xdr::Error> {
        Ok(ScVal::Error(<_ as TryInto<ScError>>::try_into(st)?))
    }
}

impl TryFrom<&Error> for ScVal {
    type Error = stellar_xdr::Error;
    fn try_from(value: &Error) -> Result<Self, stellar_xdr::Error> {
        (*value).try_into()
    }
}

impl From<ScError> for Error {
    fn from(er: ScError) -> Self {
        Error::from_scerror(er)
    }
}

impl From<(ScErrorType, ScErrorCode)> for Error {
    fn from(value: (ScErrorType, ScErrorCode)) -> Self {
        Error::from_type_and_code(value.0, value.1)
    }
}

impl From<SymbolError> for Error {
    fn from(_: SymbolError) -> Self {
        Error::from_type_and_code(ScErrorType::Value, ScErrorCode::InvalidInput)
    }
}

impl From<ConversionError> for Error {
    fn from(_: ConversionError) -> Self {
        Error::from_type_and_code(ScErrorType::Value, ScErrorCode::UnexpectedType)
    }
}

impl From<stellar_xdr::Error> for Error {
    fn from(e: stellar_xdr::Error) -> Self {
        match e {
            stellar_xdr::Error::DepthLimitExceeded => {
                Error::from_type_and_code(ScErrorType::Context, ScErrorCode::ExceededLimit)
            }
            _ => Error::from_type_and_code(ScErrorType::Value, ScErrorCode::InvalidInput),
        }
    }
}

#[cfg(feature = "wasmi")]
impl From<wasmi::core::TrapCode> for Error {
    fn from(code: wasmi::core::TrapCode) -> Self {
        let ec = match code {
            wasmi::core::TrapCode::UnreachableCodeReached => ScErrorCode::InvalidAction,

            wasmi::core::TrapCode::MemoryOutOfBounds | wasmi::core::TrapCode::TableOutOfBounds => {
                ScErrorCode::IndexBounds
            }

            wasmi::core::TrapCode::IndirectCallToNull => ScErrorCode::MissingValue,

            wasmi::core::TrapCode::IntegerDivisionByZero
            | wasmi::core::TrapCode::IntegerOverflow
            | wasmi::core::TrapCode::BadConversionToInteger => ScErrorCode::ArithDomain,

            wasmi::core::TrapCode::BadSignature => ScErrorCode::UnexpectedType,

            wasmi::core::TrapCode::StackOverflow
            | wasmi::core::TrapCode::OutOfFuel
            | wasmi::core::TrapCode::GrowthOperationLimited => {
                return Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit)
            }
        };
        return Error::from_type_and_code(ScErrorType::WasmVm, ec);
    }
}

#[cfg(feature = "wasmi")]
impl From<wasmi::Error> for Error {
    fn from(e: wasmi::Error) -> Self {
        if let wasmi::Error::Trap(trap) = e {
            if let Some(code) = trap.trap_code() {
                return code.into();
            }
        }
        Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::InternalError)
    }
}

impl Error {
    // NB: we don't provide a "get_type" to avoid casting a bad bit-pattern into
    // an ScErrorType. Instead we provide an "is_type" to check any specific
    // bit-pattern.
    #[inline(always)]
    pub const fn is_type(&self, type_: ScErrorType) -> bool {
        self.as_val().has_minor(type_ as u32)
    }

    #[inline(always)]
    pub const fn is_code(&self, code: ScErrorCode) -> bool {
        self.as_val().has_major(code as u32)
    }

    #[inline(always)]
    pub const fn get_code(&self) -> u32 {
        self.as_val().get_major()
    }

    #[inline(always)]
    pub const fn from_contract_error(code: u32) -> Error {
        unsafe { Self::from_major_minor(code, ScErrorType::Contract as u32) }
    }

    #[inline(always)]
    pub const fn from_type_and_code(type_: ScErrorType, code: ScErrorCode) -> Error {
        unsafe { Self::from_major_minor(code as u32, type_ as u32) }
    }

    #[inline(always)]
    pub const fn from_scerror(sc: ScError) -> Error {
        match sc {
            ScError::Contract(u) => Self::from_contract_error(u),
            ScError::WasmVm(code) => Self::from_type_and_code(ScErrorType::WasmVm, code),
            ScError::Context(code) => Self::from_type_and_code(ScErrorType::Context, code),
            ScError::Storage(code) => Self::from_type_and_code(ScErrorType::Storage, code),
            ScError::Object(code) => Self::from_type_and_code(ScErrorType::Object, code),
            ScError::Crypto(code) => Self::from_type_and_code(ScErrorType::Crypto, code),
            ScError::Events(code) => Self::from_type_and_code(ScErrorType::Events, code),
            ScError::Budget(code) => Self::from_type_and_code(ScErrorType::Budget, code),
            ScError::Value(code) => Self::from_type_and_code(ScErrorType::Value, code),
            ScError::Auth(code) => Self::from_type_and_code(ScErrorType::Auth, code),
        }
    }
}

impl From<core::convert::Infallible> for crate::Error {
    fn from(_: core::convert::Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    #[test]
    fn status_ord_same_as_scstatus() {
        // The impl `Ord for Error` must agree with `Ord for ScError`,
        // re https://github.com/stellar/rs-soroban-env/issues/743.
        //
        // This test creates pairs of corresponding ScError/Error values,
        // puts them all into a list, and sorts them with each comparison function,
        // then checks that both lists are sorted the same.

        let mut xdr_vals = Vec::new();
        for type_ in crate::xdr::ScErrorType::VARIANTS {
            match type_ {
                ScErrorType::Contract => {
                    for i in 0..=512 {
                        xdr_vals.push(ScError::Contract(i))
                    }
                }
                ScErrorType::WasmVm => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::WasmVm(code))
                    }
                }
                ScErrorType::Context => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Context(code))
                    }
                }
                ScErrorType::Storage => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Storage(code))
                    }
                }
                ScErrorType::Object => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Object(code))
                    }
                }
                ScErrorType::Crypto => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Crypto(code))
                    }
                }
                ScErrorType::Events => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Events(code))
                    }
                }
                ScErrorType::Budget => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Budget(code))
                    }
                }
                ScErrorType::Value => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Value(code))
                    }
                }
                ScErrorType::Auth => {
                    for code in crate::xdr::ScErrorCode::VARIANTS {
                        xdr_vals.push(ScError::Auth(code))
                    }
                }
            }
        }

        let pairs: Vec<_> = xdr_vals
            .iter()
            .map(|xdr_val| {
                let host_val = Error::try_from(xdr_val.clone()).unwrap();
                (xdr_val, host_val)
            })
            .collect();

        let mut pairs_xdr_sorted = pairs.clone();
        let mut pairs_host_sorted = pairs_xdr_sorted.clone();

        pairs_xdr_sorted.sort_by(|&(v1, _), &(v2, _)| v1.cmp(v2));

        pairs_host_sorted.sort_by(|&(_, v1), &(_, v2)| v1.cmp(&v2));

        assert_eq!(pairs_xdr_sorted, pairs_host_sorted);
    }
}
