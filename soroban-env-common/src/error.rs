use crate::xdr::{ScError, ScErrorCode, ScErrorType, ScVal};
use crate::{
    impl_wrapper_as_and_to_val, impl_wrapper_tag_based_constructors,
    impl_wrapper_tag_based_valconvert, impl_wrapper_wasmi_conversions, Compare, ConversionError,
    Env, SymbolError, Val,
};
use core::{
    cmp::Ordering,
    fmt::Debug,
    hash::{Hash, Hasher},
};

/// Wrapper for a [Val] that is tagged with [Tag::Error], interpreting the
/// [Val]'s body as a pair of a 28-bit error-type code and a 32-bit error
/// code. The error-type codes correspond to the enumerated cases of
/// [ScErrorType], and the error codes correspond to the code values stored in
/// each variant of the [ScError] union.
#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct Error(Val);

impl_wrapper_tag_based_valconvert!(Error);
impl_wrapper_tag_based_constructors!(Error);
impl_wrapper_as_and_to_val!(Error);
impl_wrapper_wasmi_conversions!(Error, WasmiMarshal031, wasmi_031::Value);
impl_wrapper_wasmi_conversions!(Error, WasmiMarshal034, wasmi_034::Val);

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
    type Error = crate::xdr::Error;
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
    type Error = crate::xdr::Error;
    fn try_from(st: Error) -> Result<Self, crate::xdr::Error> {
        Ok(ScVal::Error(<_ as TryInto<ScError>>::try_into(st)?))
    }
}

impl TryFrom<&Error> for ScVal {
    type Error = crate::xdr::Error;
    fn try_from(value: &Error) -> Result<Self, crate::xdr::Error> {
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

impl From<crate::xdr::Error> for Error {
    fn from(e: crate::xdr::Error) -> Self {
        match e {
            crate::xdr::Error::DepthLimitExceeded | crate::xdr::Error::LengthLimitExceeded => {
                Error::from_type_and_code(ScErrorType::Context, ScErrorCode::ExceededLimit)
            }
            _ => Error::from_type_and_code(ScErrorType::Value, ScErrorCode::InvalidInput),
        }
    }
}

// This never happens, but it's needed for some impls of TryFromVal downstream
// in the SDK that use the xdr::Error type.
impl From<Error> for crate::xdr::Error {
    fn from(_value: Error) -> Self {
        crate::xdr::Error::Unsupported
    }
}
macro_rules! decl_wasmi_error_conversions {
    ($wasmi_crate:ident, $errkind:ident, $get_errkind:ident,
        $fuelcase:ident, $trapcase:ident,
        $get_trapcode:ident) => {
        #[cfg(feature = "wasmi")]
        impl From<$wasmi_crate::core::TrapCode> for Error {
            fn from(code: $wasmi_crate::core::TrapCode) -> Self {
                let ec = match code {
                    $wasmi_crate::core::TrapCode::UnreachableCodeReached => {
                        ScErrorCode::InvalidAction
                    }

                    $wasmi_crate::core::TrapCode::MemoryOutOfBounds
                    | $wasmi_crate::core::TrapCode::TableOutOfBounds => ScErrorCode::IndexBounds,

                    $wasmi_crate::core::TrapCode::IndirectCallToNull => ScErrorCode::MissingValue,

                    $wasmi_crate::core::TrapCode::IntegerDivisionByZero
                    | $wasmi_crate::core::TrapCode::IntegerOverflow
                    | $wasmi_crate::core::TrapCode::BadConversionToInteger => {
                        ScErrorCode::ArithDomain
                    }

                    $wasmi_crate::core::TrapCode::BadSignature => ScErrorCode::UnexpectedType,

                    $wasmi_crate::core::TrapCode::StackOverflow
                    | $wasmi_crate::core::TrapCode::OutOfFuel
                    | $wasmi_crate::core::TrapCode::GrowthOperationLimited => {
                        return Error::from_type_and_code(
                            ScErrorType::Budget,
                            ScErrorCode::ExceededLimit,
                        )
                    }
                };
                return Error::from_type_and_code(ScErrorType::WasmVm, ec);
            }
        }

        #[cfg(feature = "wasmi")]
        impl From<$wasmi_crate::errors::FuncError> for Error {
            fn from(err: $wasmi_crate::errors::FuncError) -> Self {
                (&err).into()
            }
        }

        #[cfg(feature = "wasmi")]
        impl From<&$wasmi_crate::errors::FuncError> for Error {
            fn from(err: &$wasmi_crate::errors::FuncError) -> Self {
                let ec = match err {
                    $wasmi_crate::errors::FuncError::ExportedFuncNotFound => {
                        ScErrorCode::MissingValue
                    }
                    $wasmi_crate::errors::FuncError::MismatchingParameterType
                    | $wasmi_crate::errors::FuncError::MismatchingResultType => {
                        ScErrorCode::UnexpectedType
                    }
                    $wasmi_crate::errors::FuncError::MismatchingParameterLen
                    | $wasmi_crate::errors::FuncError::MismatchingResultLen => {
                        ScErrorCode::UnexpectedSize
                    }
                };
                return Error::from_type_and_code(ScErrorType::WasmVm, ec);
            }
        }

        #[cfg(feature = "wasmi")]
        impl From<$wasmi_crate::Error> for Error {
            fn from(e: $wasmi_crate::Error) -> Self {
                const EXCEEDED_LIMIT: Error =
                    Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit);
                const INDEX_BOUND: Error =
                    Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::IndexBounds);

                match $get_errkind(&e) {
                    $errkind::Memory(e) => match e {
                        $wasmi_crate::errors::MemoryError::OutOfBoundsAllocation
                        | $wasmi_crate::errors::MemoryError::OutOfBoundsGrowth => {
                            return EXCEEDED_LIMIT
                        }
                        $wasmi_crate::errors::MemoryError::OutOfBoundsAccess => return INDEX_BOUND,
                        _ => (),
                    },
                    $errkind::Table(e) => match e {
                        $wasmi_crate::errors::TableError::GrowOutOfBounds { .. } => {
                            return EXCEEDED_LIMIT
                        }
                        $wasmi_crate::errors::TableError::AccessOutOfBounds { .. }
                        | $wasmi_crate::errors::TableError::CopyOutOfBounds => return INDEX_BOUND,
                        _ => (),
                    },
                    $errkind::Instantiation(e) => match e {
                        $wasmi_crate::errors::InstantiationError::Memory(me) => match me {
                            $wasmi_crate::errors::MemoryError::OutOfBoundsAllocation
                            | $wasmi_crate::errors::MemoryError::OutOfBoundsGrowth => {
                                return EXCEEDED_LIMIT
                            }
                            $wasmi_crate::errors::MemoryError::OutOfBoundsAccess => {
                                return INDEX_BOUND
                            }
                            _ => (),
                        },
                        $wasmi_crate::errors::InstantiationError::Table(te) => match te {
                            $wasmi_crate::errors::TableError::GrowOutOfBounds { .. } => {
                                return EXCEEDED_LIMIT
                            }
                            $wasmi_crate::errors::TableError::AccessOutOfBounds { .. }
                            | $wasmi_crate::errors::TableError::CopyOutOfBounds => {
                                return INDEX_BOUND
                            }
                            _ => (),
                        },
                        _ => (),
                    },
                    $errkind::$fuelcase(e) => {
                        if let $wasmi_crate::errors::FuelError::OutOfFuel = e {
                            return EXCEEDED_LIMIT;
                        }
                    }
                    $errkind::$trapcase(trap) => {
                        if let Some(code) = $get_trapcode(trap) {
                            return code.into();
                        }
                    }
                    $errkind::Func(e) => {
                        return e.into();
                    }
                    $errkind::Global(e) => {
                        if matches!(e, $wasmi_crate::errors::GlobalError::TypeMismatch { .. }) {
                            return Error::from_type_and_code(
                                ScErrorType::WasmVm,
                                ScErrorCode::UnexpectedType,
                            );
                        }
                    }
                    _ => (),
                }

                Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::InvalidAction)
            }
        }
    };
}

#[cfg(feature = "wasmi")]
type WasmiErr031 = wasmi_031::Error;
#[cfg(feature = "wasmi")]
type WasmiErr034 = wasmi_034::errors::ErrorKind;
#[cfg(feature = "wasmi")]
fn get_wasmi_031_error_kind(e: &wasmi_031::Error) -> &WasmiErr031 {
    e
}
#[cfg(feature = "wasmi")]
fn get_wasmi_034_error_kind(e: &wasmi_034::Error) -> &WasmiErr034 {
    e.kind()
}
#[cfg(feature = "wasmi")]
fn get_wasmi_031_trap_code(trap: &wasmi_031::core::Trap) -> Option<wasmi_031::core::TrapCode> {
    trap.trap_code()
}
#[cfg(feature = "wasmi")]
fn get_wasmi_034_trap_code(trap: &wasmi_034::core::TrapCode) -> Option<wasmi_034::core::TrapCode> {
    Some(*trap)
}

decl_wasmi_error_conversions!(
    wasmi_031,
    WasmiErr031,
    get_wasmi_031_error_kind,
    Store,
    Trap,
    get_wasmi_031_trap_code
);
decl_wasmi_error_conversions!(
    wasmi_034,
    WasmiErr034,
    get_wasmi_034_error_kind,
    Fuel,
    TrapCode,
    get_wasmi_034_trap_code
);

#[cfg(feature = "wasmi")]
impl From<wasmparser::BinaryReaderError> for Error {
    fn from(_: wasmparser::BinaryReaderError) -> Self {
        Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::InvalidInput)
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
    fn from(x: core::convert::Infallible) -> Self {
        match x {}
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    #[test]
    fn error_ord_same_as_scerror() {
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
