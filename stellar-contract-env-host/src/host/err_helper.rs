use crate::events::DebugError;
use crate::{ConversionError, Host, HostError, RawVal, Status};

impl Host {
    /// Records a debug-event from its input in as much detail as possible, then
    /// converts its input to a (often coarser-granularity) [Status] code, and then
    /// forms a [HostError] with it (which also captures a [backtrace::Backtrace]).
    /// This is the method you want to call any time there's a finer-granularity error
    /// type that you want to log the details of and then downgrade fail with.
    pub fn err<T>(&self, src: T) -> HostError
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
    pub fn err_status<T>(&self, status: T) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status))
    }

    /// Helper for the simplest string + general-error path.
    pub fn err_general(&self, msg: &'static str) -> HostError {
        self.err(DebugError::general().msg(msg))
    }

    /// Helper for the next-simplest status-and-extended-debug-message error path.
    pub fn err_status_msg<T>(&self, status: T, msg: &'static str) -> HostError
    where
        Status: From<T>,
    {
        self.err(DebugError::new(status).msg(msg))
    }

    // Helper for a conversion error from any type into a rawval
    pub fn err_conversion_into_rawval<T>(&self, rv: RawVal) -> HostError {
        self.err(
            DebugError::new(ConversionError)
                .msg("error converting {} into {}")
                .arg(rv)
                .arg(std::any::type_name::<T>()),
        )
    }

    // Helper for a simplest conversion error with a provided msg
    pub fn err_conversion_general(&self, msg: &'static str) -> HostError {
        self.err(DebugError::new(ConversionError).msg(msg))
    }

    /// Given a result carrying some error type that can be converted to a
    /// DebugError, calls self.err with it when there's an error. Returns a
    /// result over HostError.
    ///
    /// If you have an error type T you want to record as a detailed debug event
    /// and a less-detailed Status code embedded in a HostError, add an `impl
    /// From<T> for DebugError` over in the [crate::events] module and call this
    /// where the error is generated.
    ///
    /// Note: we do _not_ want to `impl From<T> for HostError` for such types,
    /// as doing so will avoid routing them through the host in order to record
    /// their extended diagnostic information into the debug buffer. This means
    /// you will wind up writing `host.map_err(...)?` a bunch in code that you
    /// used to be able to get away with just writing `...?`, there's no way
    /// around this if we want to record the diagnostic information.
    pub fn map_err<T, E>(&self, res: Result<T, E>) -> Result<T, HostError>
    where
        DebugError: From<E>,
    {
        res.map_err(|e| self.err(e.into()))
    }
}
