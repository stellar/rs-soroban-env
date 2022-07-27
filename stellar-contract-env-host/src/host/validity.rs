use crate::xdr::{ScHostFnErrorCode, ScHostObjErrorCode};
use crate::{Host, HostError};

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

    pub(crate) fn validate_start_end_bound(
        &self,
        start: u32,
        end: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if start as usize >= bound {
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
        // we don't check start <= end, as [start..end] is ok in that case
        Ok(())
    }

    pub(crate) fn validate_start_span_bound(
        &self,
        start: u32,
        span: u32,
        bound: usize,
    ) -> Result<u32, HostError> {
        let end = start.checked_add(span).ok_or_else(|| {
            self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
        })?;
        self.validate_start_end_bound(start, end, bound)?;
        Ok(end)
    }
}
