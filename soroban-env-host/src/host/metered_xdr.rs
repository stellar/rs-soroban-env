use crate::{
    xdr::ContractCostType,
    xdr::{ReadXdr, ScBytes, WriteXdr},
    BytesObject, Host, HostError,
};
use std::io::Write;

use sha2::{Digest, Sha256};
use soroban_env_common::xdr::{
    DepthLimitedWrite, ScErrorCode, ScErrorType, DEFAULT_XDR_RW_DEPTH_LIMIT,
};

struct MeteredWrite<'a, W: Write> {
    host: &'a Host,
    w: &'a mut W,
}

impl<'a, W> Write for MeteredWrite<'a, W>
where
    W: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.host
            .charge_budget(ContractCostType::ValSer, Some(buf.len() as u64))
            .map_err(Into::<std::io::Error>::into)?;
        self.w.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.w.flush()
    }
}

impl Host {
    pub(crate) fn metered_write_xdr(
        &self,
        obj: &impl WriteXdr,
        w: &mut Vec<u8>,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("write xdr");
        let mw = MeteredWrite { host: self, w };
        let mut w = DepthLimitedWrite::new(mw, DEFAULT_XDR_RW_DEPTH_LIMIT);
        // MeteredWrite above turned any budget failure into an IO error; we turn it
        // back to a budget failure here, since there's really no "IO error" that can
        // occur when writing to a Vec<u8>.
        obj.write_xdr(&mut w)
            .map_err(|_| (ScErrorType::Budget, ScErrorCode::ExceededLimit).into())
    }

    pub(crate) fn metered_hash_xdr(&self, obj: &impl WriteXdr) -> Result<[u8; 32], HostError> {
        let _span = tracy_span!("hash xdr");
        let mut buf = vec![];
        self.metered_write_xdr(obj, &mut buf)?;
        self.charge_budget(ContractCostType::ComputeSha256Hash, Some(buf.len() as u64))?;
        Ok(Sha256::digest(&buf).try_into()?)
    }

    pub(crate) fn metered_from_xdr<T: ReadXdr>(&self, bytes: &[u8]) -> Result<T, HostError> {
        let _span = tracy_span!("read xdr");
        self.charge_budget(ContractCostType::ValDeser, Some(bytes.len() as u64))?;
        self.map_err(T::from_xdr(bytes))
    }

    pub(crate) fn metered_from_xdr_obj<T: ReadXdr>(
        &self,
        bytes: BytesObject,
    ) -> Result<T, HostError> {
        self.visit_obj(bytes, |hv: &ScBytes| self.metered_from_xdr(hv.as_slice()))
    }
}
