use std::rc::Rc;

use crate::{budget::AsBudget, host::metered_clone, xdr::LedgerKey, HostError};

#[derive(Clone)]
pub struct LedgerBump {
    pub key: Rc<LedgerKey>,
    pub min_expiration: u32,
}

#[derive(Clone, Default)]
pub struct ExpirationLedgerBumps(Vec<LedgerBump>);

impl ExpirationLedgerBumps {
    pub(crate) fn metered_push(
        &mut self,
        b: impl AsBudget,
        elem: LedgerBump,
    ) -> Result<(), HostError> {
        // The worst case cost of a `vec.push` requires heap reallocation and copying of old data.
        // Thus we use the cost of instantiating a size=1 `Vec` as the estimate of the amortized
        // cost for it.
        metered_clone::charge_container_bulk_init_with_elts::<Vec<LedgerBump>, LedgerBump>(
            1,
            b.as_budget(),
        )?;
        Ok(self.0.push(elem))
    }
}
