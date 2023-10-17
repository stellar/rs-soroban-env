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
            return Err(self.err_oob_object_index(Some(index)));
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
            return Err(self.err_oob_object_index(Some(index)));
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
        self.validate_index_le_bound(start, bound)?;
        self.validate_index_le_bound(end, bound)?;
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

    pub(crate) fn validate_usize_sum_fits_in_u32(
        &self,
        a: usize,
        b: usize,
    ) -> Result<usize, HostError> {
        let a = u32::try_from(a).map_err(|_| self.err_arith_overflow())?;
        let b = u32::try_from(b).map_err(|_| self.err_arith_overflow())?;
        a.checked_add(b)
            .ok_or_else(|| self.err_arith_overflow())
            .map(|u| u as usize)
    }
}
