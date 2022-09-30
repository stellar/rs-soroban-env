use crate::{
    budget::CostType,
    xdr::{ReadXdr, WriteXdr},
    Host, HostError,
};

impl Host {
    pub(crate) fn metered_write_xdr(
        &self,
        obj: &impl WriteXdr,
        buf: &mut Vec<u8>,
    ) -> Result<(), HostError> {
        obj.write_xdr(buf)
            .map_err(|_| self.err_general("failed to write xdr"))?;
        // Notes on metering": "write first charge later" means we could potentially underestimate
        // the cost by the largest sized host object. Since we are bounding the memory limit of a
        // host object, it is probably fine.
        // Ideally, `charge` should go before `write_xdr`, which would require us to either 1.
        // make serialization an iterative / chunked operation. Or 2. have a XDR method to
        // calculate the serialized size. Both would require non-trivial XDR changes.
        self.charge_budget(CostType::ValSer, buf.len() as u64)
    }

    pub(crate) fn metered_from_xdr<T: ReadXdr>(&self, bytes: &[u8]) -> Result<T, HostError> {
        self.charge_budget(CostType::ValDeser, bytes.len() as u64)?;
        T::from_xdr(bytes).map_err(|_| self.err_general("failed to read from xdr"))
    }
}
