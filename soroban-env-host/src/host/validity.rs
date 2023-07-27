use std::ops::Range;

use crate::{
    xdr::{ScErrorCode, ScErrorType},
    Host, HostError, U32Val,
};

impl Host {
    // Notes on metering: free
    pub(crate) fn validate_index_lt_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize >= bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "index out of bound",
                &[U32Val::from(index).to_val()],
            ));
        }
        Ok(())
    }

    // Notes on metering: free
    pub(crate) fn validate_index_le_bound(
        &self,
        index: u32,
        bound: usize,
    ) -> Result<(), HostError> {
        if index as usize > bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "index out of bound",
                &[U32Val::from(index).to_val()],
            ));
        }
        Ok(())
    }

    // Notes on metering: free
    pub(crate) fn valid_range_from_start_end_bound(
        &self,
        start: u32,
        end: u32,
        bound: usize,
    ) -> Result<Range<usize>, HostError> {
        if start as usize > bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "start index out of bound",
                &[U32Val::from(start).to_val()],
            ));
        }
        if end as usize > bound {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "end index out of bound",
                &[U32Val::from(end).to_val()],
            ));
        }
        if start > end {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::InvalidInput,
                "start index greater than end index",
                &[U32Val::from(start).to_val(), U32Val::from(end).to_val()],
            ));
        }
        Ok(Range {
            start: start as usize,
            end: end as usize,
        })
    }
}
