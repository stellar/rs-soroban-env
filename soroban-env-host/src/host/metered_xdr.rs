use crate::{
    budget::Budget,
    host::crypto::sha256_hash_from_bytes_raw,
    xdr::{ContractCostType, Limited, ReadXdr, ScBytes, ScErrorCode, ScErrorType, WriteXdr},
    BytesObject, Host, HostError, DEFAULT_XDR_RW_LIMITS,
};
use std::io::Write;

struct MeteredWrite<'a, W: Write> {
    budget: &'a Budget,
    w: &'a mut W,
}

impl<'a, W> Write for MeteredWrite<'a, W>
where
    W: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.budget
            .charge(ContractCostType::ValSer, Some(buf.len() as u64))
            .map_err(Into::<std::io::Error>::into)?;
        self.w.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.w.flush()
    }
}

impl Host {
    pub fn metered_hash_xdr(&self, obj: &impl WriteXdr) -> Result<[u8; 32], HostError> {
        let _span = tracy_span!("hash xdr");
        let mut buf = vec![];
        metered_write_xdr(self.budget_ref(), obj, &mut buf)?;
        sha256_hash_from_bytes_raw(&buf, self)
    }

    pub fn metered_from_xdr<T: ReadXdr>(&self, bytes: &[u8]) -> Result<T, HostError> {
        let _span = tracy_span!("read xdr");
        self.charge_budget(ContractCostType::ValDeser, Some(bytes.len() as u64))?;
        let mut limits = DEFAULT_XDR_RW_LIMITS;
        limits.len = bytes.len();
        self.map_err(T::from_xdr(bytes, limits))
    }

    pub(crate) fn metered_from_xdr_obj<T: ReadXdr>(
        &self,
        bytes: BytesObject,
    ) -> Result<T, HostError> {
        self.visit_obj(bytes, |hv: &ScBytes| self.metered_from_xdr(hv.as_slice()))
    }
}

pub fn metered_write_xdr(
    budget: &Budget,
    obj: &impl WriteXdr,
    w: &mut Vec<u8>,
) -> Result<(), HostError> {
    let _span = tracy_span!("write xdr");
    let mut w = Limited::new(MeteredWrite { budget, w }, DEFAULT_XDR_RW_LIMITS);
    // MeteredWrite above turned any budget failure into an IO error; we turn it
    // back to a budget failure here, since there's really no "IO error" that can
    // occur when writing to a Vec<u8>.
    obj.write_xdr(&mut w)
        .map_err(|_| (ScErrorType::Budget, ScErrorCode::ExceededLimit).into())
}

// Host-less metered XDR decoding.
// Prefer using `metered_from_xdr` when host is available for better error
// reporting.
pub fn metered_from_xdr_with_budget<T: ReadXdr>(
    bytes: &[u8],
    budget: &Budget,
) -> Result<T, HostError> {
    let _span = tracy_span!("read xdr with budget");
    budget.charge(ContractCostType::ValDeser, Some(bytes.len() as u64))?;
    let mut limits = DEFAULT_XDR_RW_LIMITS;
    limits.len = bytes.len();
    T::from_xdr(bytes, limits).map_err(|e| e.into())
}
