use std::{borrow::Cow, fmt::Display};

use crate::{xdr, RawVal, Status};
use crate::{
    xdr::{ScUnknownErrorCode, ScVmErrorCode},
    HostError,
};
use tinyvec::TinyVec;

#[derive(Clone, Debug)]
pub enum DebugArg {
    Str(&'static str),
    Val(RawVal),
}

impl From<RawVal> for DebugArg {
    fn from(rv: RawVal) -> Self {
        DebugArg::Val(rv)
    }
}

impl From<&'static str> for DebugArg {
    fn from(s: &'static str) -> Self {
        DebugArg::Str(s)
    }
}

impl Default for DebugArg {
    fn default() -> Self {
        DebugArg::Str("")
    }
}

impl Display for DebugArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DebugArg::Str(s) => write!(f, "{}", s),
            DebugArg::Val(rv) => write!(f, "{:?}", rv),
        }
    }
}

/// A cheap record type to store in the events buffer for diagnostic reporting
/// when something goes wrong. Should cost very little even when enabled. See
/// [host::Host::debug_event](crate::host::Host::debug_event) for normal use.
#[derive(Clone, Debug)]
pub struct DebugEvent {
    pub msg: Option<Cow<'static, str>>,
    pub args: TinyVec<[DebugArg; 2]>,
}

impl core::fmt::Display for DebugEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.msg {
            None => {
                for arg in self.args.iter() {
                    write!(f, "{}", arg)?;
                }
                Ok(())
            }
            Some(fmt) => {
                let args = dyn_fmt::Arguments::new(fmt, self.args.as_slice());
                write!(f, "{}", args)
            }
        }
    }
}

impl DebugEvent {
    pub fn new() -> Self {
        Self {
            msg: None,
            args: Default::default(),
        }
    }

    pub fn msg(mut self, msg: impl Into<Cow<'static, str>>) -> Self {
        self.msg = Some(msg.into());
        self
    }

    pub fn arg<T: Into<DebugArg>>(mut self, arg: T) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn args<T: Into<DebugArg>>(mut self, args: impl IntoIterator<Item = T>) -> Self {
        self.args.extend(args.into_iter().map(|arg| arg.into()));
        self
    }
}

/// Combines a [DebugEvent] with a [Status] that created it, typically
/// used as a transient type when recording a (possibly enriched)
/// debug event for a status and then converting the status to a
/// HostError. See [host::Host::err](crate::host::Host::err) for normal use.
///
/// # Examples
///
/// ```rust
/// use soroban_env_host::{RawVal, xdr::ScHostFnErrorCode, events::DebugError};
///
/// let r: RawVal = 1i32.into();
/// let name: &'static str = "abc";
/// let de = DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
///     .msg("unexpected RawVal {} for input '{}', need U32")
///     .arg(r)
///     .arg(name);
/// assert_eq!(
///     format!("{}", de.event),
///     "unexpected RawVal I32(1) for input 'abc', need U32"
/// );
/// ```
#[derive(Clone, Debug)]
pub struct DebugError {
    pub event: DebugEvent,
    pub status: Status,
}

impl DebugError {
    pub fn new<T>(status: T) -> Self
    where
        Status: From<T>,
    {
        let status: Status = status.into();
        Self {
            event: DebugEvent::new(),
            status,
        }
    }

    pub fn general() -> Self {
        Self::new(xdr::ScUnknownErrorCode::General)
    }

    pub fn msg(mut self, msg: &'static str) -> Self {
        self.event = self.event.msg(msg);
        self
    }

    pub fn arg<T: Into<DebugArg>>(mut self, arg: T) -> Self {
        self.event = self.event.arg(arg);
        self
    }
}

impl From<xdr::Error> for DebugError {
    fn from(err: xdr::Error) -> Self {
        let msg = match err {
            xdr::Error::Invalid => "XDR error: invalid",
            xdr::Error::Unsupported => "XDR error: unsupported",
            xdr::Error::LengthExceedsMax => "XDR error: length exceeds max",
            xdr::Error::LengthMismatch => "XDR error: length mismatch",
            xdr::Error::NonZeroPadding => "XDR error: nonzero padding",
            xdr::Error::Utf8Error(_) => "XDR error: UTF-8 error",
            xdr::Error::InvalidHex => "XDR error: hex error",
            xdr::Error::Io(_) => "XDR error: IO error",
        };
        Self::new(xdr::ScUnknownErrorCode::Xdr).msg(msg)
    }
}

impl From<wasmi::Error> for DebugError {
    fn from(err: wasmi::Error) -> Self {
        // At the moment we have a status code for each of the wasmi error types,
        // but we mighit reduce this to something coarser in the future, split
        // the name-reporting out from the code we return
        //
        // The errors from wasmi actually have much _more_ content (in the form
        // of Strings) that we're already eliding at this level, that we might
        // want to report for diagnostic purposes if we ever get dynamic strings
        // in the diagnostic buffer.
        use wasmi::core::TrapCode::*;
        use wasmi::Error::*;
        let code = match err {
            // TODO: re-reconcile these cases with ScVmErrorCode cases
            Module(_) => ScVmErrorCode::Validation,
            Linker(_) | Instantiation(_) => ScVmErrorCode::Instantiation,
            Func(_) => ScVmErrorCode::Function,
            Table(_) => ScVmErrorCode::Table,
            Memory(_) => ScVmErrorCode::Memory,
            Global(_) => ScVmErrorCode::Global,
            Trap(trap) if trap.is_host() => {
                let err = trap.into_host().expect("trapped HostError");
                let status: Status = match err.downcast_ref::<HostError>() {
                    Some(he) => he.status,
                    None => ScUnknownErrorCode::General.into(),
                };
                return DebugError::new(status).msg("VM trapped with host error");
            }
            Trap(trap) => match trap.as_code().expect("trap code") {
                Unreachable => ScVmErrorCode::TrapUnreachable,
                MemoryAccessOutOfBounds => ScVmErrorCode::TrapMemoryAccessOutOfBounds,
                TableAccessOutOfBounds => ScVmErrorCode::TrapTableAccessOutOfBounds,
                ElemUninitialized => ScVmErrorCode::TrapElemUninitialized,
                DivisionByZero => ScVmErrorCode::TrapDivisionByZero,
                IntegerOverflow => ScVmErrorCode::TrapIntegerOverflow,
                InvalidConversionToInt => ScVmErrorCode::TrapInvalidConversionToInt,
                StackOverflow => ScVmErrorCode::TrapStackOverflow,
                UnexpectedSignature => ScVmErrorCode::TrapUnexpectedSignature,
                MemLimitExceeded => ScVmErrorCode::TrapMemLimitExceeded,
                CpuLimitExceeded => ScVmErrorCode::TrapCpuLimitExceeded,
            },
            _ => ScVmErrorCode::Unknown,
        };
        Self::new(code).msg(code.name())
    }
}
