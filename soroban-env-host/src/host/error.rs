use crate::{
    events::{Events, HostEvent},
    xdr::{self, ScStatus},
    Status,
};
use backtrace::{Backtrace, BacktraceFrame};
use core::fmt::Debug;

#[derive(Clone)]
pub struct HostError {
    pub(crate) status: Status,
    pub(crate) events: Option<Events>,
    pub(crate) backtrace: backtrace::Backtrace,
}

impl std::error::Error for HostError {}

impl Debug for HostError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // We do a little trimming here, skipping the first two frames (which
        // are always into, from, and one or more Host::err_foo calls) and all
        // the frames _after_ the short-backtrace marker that rust compiles-in.

        fn frame_name_matches(frame: &BacktraceFrame, pat: &str) -> bool {
            for sym in frame.symbols() {
                match sym.name() {
                    Some(sn) if format!("{:}", sn).contains(pat) => {
                        return true;
                    }
                    _ => (),
                }
            }
            false
        }

        fn frame_is_short_backtrace_start(frame: &BacktraceFrame) -> bool {
            frame_name_matches(frame, "__rust_begin_short_backtrace")
        }

        fn frame_is_initial_error_plumbing(frame: &BacktraceFrame) -> bool {
            frame_name_matches(frame, "::from")
                || frame_name_matches(frame, "::into")
                || frame_name_matches(frame, "Host::err")
                || frame_name_matches(frame, "::map_err")
        }

        let mut bt = self.backtrace.clone();
        bt.resolve();
        let frames: Vec<BacktraceFrame> = bt
            .frames()
            .iter()
            .skip_while(|f| frame_is_initial_error_plumbing(f))
            .take_while(|f| !frame_is_short_backtrace_start(f))
            .cloned()
            .collect();
        let bt: Backtrace = frames.into();
        writeln!(f, "HostError")?;
        writeln!(f, "Value: {:?}", self.status)?;
        // TODO: maybe make this something users can adjust?
        const MAX_DEBUG_EVENTS: usize = 10;
        match &self.events {
            None => (),
            Some(ev) => {
                let mut wrote_heading = false;
                for (i, e) in
                    ev.0.iter()
                        .rev()
                        .filter_map(|ev| match ev {
                            HostEvent::Debug(e) => Some(format!("{:}", e)),
                            HostEvent::StructuredDebug(e) => Some(format!("{:?}", e)),
                            _ => None,
                        })
                        .take(MAX_DEBUG_EVENTS)
                        .enumerate()
                {
                    if !wrote_heading {
                        writeln!(f, "")?;
                        writeln!(f, "Debug events (newest first):")?;
                        wrote_heading = true;
                    }
                    writeln!(f, "   {:?}: {:?}", i, e)?;
                }
                if ev.0.len() > MAX_DEBUG_EVENTS {
                    writeln!(f, "   {:?}: ... elided ...", MAX_DEBUG_EVENTS)?;
                }
            }
        }
        writeln!(f, "")?;
        writeln!(f, "Backtrace (newest first):")?;
        writeln!(f, "{:?}", bt)
    }
}

impl HostError {
    #[cfg(test)]
    pub fn result_matches_err_status<T, C>(res: Result<T, HostError>, code: C) -> bool
    where
        Status: From<C>,
    {
        match res {
            Ok(_) => false,
            Err(he) => {
                let status: Status = code.into();
                he.status == status
            }
        }
    }
}

impl<T> From<T> for HostError
where
    Status: From<T>,
{
    fn from(status: T) -> Self {
        let backtrace = backtrace::Backtrace::new_unresolved();
        let status: Status = status.into();
        let events = None;
        Self {
            status,
            events,
            backtrace,
        }
    }
}

impl std::fmt::Display for HostError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <HostError as Debug>::fmt(self, f)
    }
}

impl TryFrom<&HostError> for ScStatus {
    type Error = xdr::Error;
    fn try_from(err: &HostError) -> Result<Self, Self::Error> {
        err.status.try_into()
    }
}

impl From<HostError> for std::io::Error {
    fn from(e: HostError) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, e)
    }
}
