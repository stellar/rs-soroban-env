use std::io::Write;

use crate::{
    budget::CostType,
    xdr::{ReadXdr, WriteXdr},
    Host, HostError,
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
            .charge_budget(CostType::ValSer, buf.len() as u64)
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
        obj.write_xdr(&mut w)
            .map_err(|e| self.err_general("failed to write xdr"))
    }

    pub(crate) fn metered_from_xdr<T: ReadXdr>(&self, bytes: &[u8]) -> Result<T, HostError> {
        self.charge_budget(CostType::ValDeser, bytes.len() as u64)?;
        // TODO: fish out `HostError` from `io::Error`
        T::from_xdr(bytes).map_err(|_| self.err_general("failed to read from xdr"))
    }
}
