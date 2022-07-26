use crate::events::{DebugError, DebugEvent};
use crate::{ConversionError, Host, HostError, RawVal, Status};

impl Host {
    /// Records a debug-event from its input in as much detail as possible, then
    /// converts its input to a (often coarser-granularity) [Status] code, and then
    /// forms a [HostError] with it (which also captures a [backtrace::Backtrace]).
    /// This is the method you want to call any time there's a finer-granularity error
    /// type that you want to log the details of and then downgrade fail with.
    pub(crate) fn err<T>(&self, src: T) -> HostError
    where
        DebugError: From<T>,
    {
        let ds: DebugError = src.into();
        if let Err(e) = self.debug_event(ds.event) {
            e
        } else {
            let mut he: HostError = ds.status.into();
            he.events = Some(self.0.events.borrow().clone());
            he
        }
    }

    /// Helper for the simplest status-only error path.
    pub(crate) fn err_status<T>(&self, status: T) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status))
    }

    /// Helper for the simplest string + general-error path.
    pub(crate) fn err_general(&self, msg: &'static str) -> HostError {
        self.err(DebugError::general().msg(msg))
    }

    pub(crate) fn err_conversion_raw_val_to<T>(&self, rv: RawVal) -> HostError {
        self.err(
            DebugError::new(ConversionError)
                .msg("error converting {} into {}")
                .arg(rv)
                .arg(std::any::type_name::<T>()),
        )
    }

    /// Helper for the next-simplest status-and-extended-debug-message error path.
    pub(crate) fn err_status_msg<T>(&self, status: T, msg: &'static str) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status).msg(msg))
    }

    pub(crate) fn err_convert<T>(&self, v: RawVal) -> HostError {
        if let Err(e) = self.debug_event(
            DebugEvent::new()
                .msg("can't convert {} to {}")
                .arg(v)
                .arg(core::any::type_name::<T>()),
        ) {
            return e;
        }
        ConversionError {}.into()
    }

    pub(crate) fn err_convert_general(&self, msg: &'static str) -> HostError {
        if let Err(e) = self.debug_event(DebugEvent::new().msg(msg)) {
            return e;
        }
        ConversionError {}.into()
    }

    /// Given a result carrying some error type that can be converted to a
    /// DebugStatus, calls self.err with it when there's an error. Returns a
    /// result over HostError.
    ///
    /// If you have an error type T you want to record as a detailed debug event
    /// and a less-detailed Status code embedded in a HostError, add an `impl
    /// From<T> for DebugStatus` over in the [events] module and call this where
    /// the error is generated.
    ///
    /// Note: we do _not_ want to `impl From<T> for HostError` for such types,
    /// as doing so will avoid routing them through the host in order to record
    /// their extended diagnostic information into the debug buffer. This means
    /// you will wind up writing `host.map_err(...)?` a bunch in code that you used
    /// to be able to get away with just writing `...?`, there's no way around
    /// this if we want to record the diagnostic information.
    pub(crate) fn map_err<T, E>(&self, res: Result<T, E>) -> Result<T, HostError>
    where
        DebugError: From<E>,
    {
        res.map_err(|e| self.err(e.into()))
    }
}
