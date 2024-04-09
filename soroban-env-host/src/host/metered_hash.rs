use crate::{
    budget::Budget,
    host::metered_xdr::metered_write_xdr,
    xdr::{ContractCostType, WriteXdr},
    HostError,
};
use std::hash::{Hash, Hasher};

// Technically we should be metering the cost of the hash function used, but
// this codepath is used only for charging the costs of tracing against the
// shadow budget, and we do not want to add a cost type to the protocol just
// for this purpose (it's not protocol-visible at all).
//
// In practice, Rust's default hasher is SIP-1-3 which is of a similar order
// of magnitude as a ChaCha20 round, so this is a reasonable approximation.
// It's also fine if we overcharge here, since again this is only used to
// ensure that if the hashing code is ever called _outside_ the shadow budget
// it's not a free operation / DoS vector.
const HASH_COST_TYPE: ContractCostType = ContractCostType::ChaCha20DrawBytes;

#[derive(Default)]
pub struct CountingHasher {
    count: usize,
    hasher: std::collections::hash_map::DefaultHasher,
}

impl Hasher for CountingHasher {
    fn finish(&self) -> u64 {
        self.hasher.finish()
    }

    fn write(&mut self, bytes: &[u8]) {
        self.count = self.count.saturating_add(bytes.len());
        self.hasher.write(bytes);
    }
}

pub(crate) trait MeteredHash {
    fn metered_hash(&self, hasher: &mut CountingHasher, budget: &Budget) -> Result<(), HostError>;
}

impl<T: Hash> MeteredHash for T {
    fn metered_hash(&self, hasher: &mut CountingHasher, budget: &Budget) -> Result<(), HostError> {
        self.hash(hasher);
        budget.charge(HASH_COST_TYPE, Some(hasher.count as u64))?;
        Ok(())
    }
}

impl std::io::Write for CountingHasher {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.count = self.count.saturating_add(buf.len());
        self.hasher.write(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub(crate) trait MeteredHashXdr {
    fn metered_hash_xdr(
        &self,
        hasher: &mut CountingHasher,
        budget: &Budget,
    ) -> Result<(), HostError>;
}

impl<T: WriteXdr> MeteredHashXdr for T {
    fn metered_hash_xdr(
        &self,
        hasher: &mut CountingHasher,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let mut buf = Vec::default();
        metered_write_xdr(budget, self, &mut buf)?;
        buf.metered_hash(hasher, budget)?;
        budget.charge(HASH_COST_TYPE, Some(hasher.count as u64))
    }
}
