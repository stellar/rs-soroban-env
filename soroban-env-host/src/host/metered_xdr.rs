use crate::{
    xdr::ContractCostType,
    xdr::{ReadXdr, ScBytes, WriteXdr},
    BytesObject, Host, HostError,
};
use std::{error::Error, io::Write};

use sha2::{Digest, Sha256};

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
            .map_err(|e| Into::<std::io::Error>::into(e))?;
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
        let mut w = MeteredWrite { host: &self, w };
        obj.write_xdr(&mut w).map_err(|e| {
            if let Some(e2) = e.source() {
                if let Some(e3) = (*e2).downcast_ref::<std::io::Error>() {
                    if let Some(e4) = e3.get_ref() {
                        if let Some(e5) = e4.downcast_ref::<HostError>() {
                            e5.clone()
                        } else {
                            self.err_general("failed to write xdr")
                        }
                    } else {
                        self.err_general("failed to write xdr")
                    }
                } else {
                    self.err_general("failed to write xdr")
                }
            } else {
                self.err_general("failed to write xdr")
            }
        })
    }

    pub(crate) fn metered_hash_xdr(&self, obj: &impl WriteXdr) -> Result<[u8; 32], HostError> {
        let mut buf = vec![];
        self.metered_write_xdr(obj, &mut buf)?;
        self.charge_budget(ContractCostType::ComputeSha256Hash, Some(buf.len() as u64))?;
        Ok(Sha256::digest(&buf).try_into()?)
    }

    pub(crate) fn metered_from_xdr<T: ReadXdr>(&self, bytes: &[u8]) -> Result<T, HostError> {
        self.charge_budget(ContractCostType::ValDeser, Some(bytes.len() as u64))?;
        T::from_xdr(bytes).map_err(|_| self.err_general("failed to read from xdr"))
    }

    pub(crate) fn metered_from_xdr_obj<T: ReadXdr>(
        &self,
        bytes: BytesObject,
    ) -> Result<T, HostError> {
        self.visit_obj(bytes, |hv: &ScBytes| self.metered_from_xdr(hv.as_slice()))
    }
}
