use std::ops::Range;

use crate::events::DebugError;
use crate::xdr::{ScHostFnErrorCode, ScHostObjErrorCode};
use crate::{Host, HostError, RawVal};

impl Host {
    pub(crate) fn validate_index_lt_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize >= bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound, // TODO: need to reconcile between InputArgsInvalid and VecIndexOutOfBound
                "start index out of bound",
            ));
        }
        Ok(())
    }

    pub(crate) fn validate_index_le_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound, // TODO: need to reconcile between InputArgsInvalid and VecIndexOutOfBound
                "start index out of bound",
            ));
        }
        Ok(())
    }

    pub(crate) fn valid_range_from_start_end_bound(
        &self,
        start: u32,
        end: u32,
        bound: usize,
    ) -> Result<Range<usize>, HostError> {
        if start as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "start index out of bound",
            ));
        }
        if end as usize > bound {
            return Err(self.err_status_msg(
                ScHostObjErrorCode::VecIndexOutOfBound,
                "end index out of bound",
            ));
        }
        if start > end {
            return Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsInvalid)
                    .msg("index starts at {} but ends at {}")
                    .arg(RawVal::from_u32(start))
                    .arg(RawVal::from_u32(end)),
            ));
        }
        Ok(Range {
            start: start as usize,
            end: end as usize,
        })
    }

    pub(crate) fn valid_range_from_start_span_bound(
        &self,
        start: u32,
        span: u32,
        bound: usize,
    ) -> Result<Range<usize>, HostError> {
        let end = start.checked_add(span).ok_or_else(|| {
            self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
        })?;
        self.valid_range_from_start_end_bound(start, end, bound)
    }
}
