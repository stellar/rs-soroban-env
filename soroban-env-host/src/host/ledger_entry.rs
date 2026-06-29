use std::{cell::RefCell, rc::Rc};

use crate::{
    budget::Budget,
    host::{
        metered_clone::MeteredAlloc,
        metered_hash::{CountingHasher, MeteredHash, MeteredHashXdr},
        metered_xdr::metered_from_xdr_with_budget,
    },
    xdr::{LedgerEntry, ScErrorCode, ScErrorType},
    Error, HostError,
};

/// A ledger entry held by the host's storage map, decoded lazily on first
/// access (when necessary).
///
/// The logical content of a `HostLedgerEntry` is immutable: it is either the
/// raw buffer it was loaded from, the decoded entry, or both (after a lazy
/// decode).
/// Decoding happens at most once and is memoized in place. Thanks to that, if
/// the same `HostLedgerEntry` is shared between the containers via `Rc` (which
/// is what the storage does), decoding will happen at most once across all of
/// them.
///
/// This wrapper allows the host to avoid unnecessary XDR decoding in cases
/// when the entry is not accessed during the contract execution (which is what
/// typically happens to the contract code entries, which are normally fetched
/// from the module cache).
pub(crate) struct HostLedgerEntry {
    /// The raw XDR encoding the entry was loaded from. `Some` for entries loaded
    /// from the ledger, `None` for entries created in-host (e.g. via `put`). It
    /// is retained even after a lazy decode so that trace-hash queries stay
    /// decode-free (the trace hash also records the current decode state; see
    /// [`HostLedgerEntry::metered_hash`]).
    encoded: Option<Rc<[u8]>>,
    /// The decoded entry form. Filled at construction for in-host entries,
    /// or on first access for entries loaded from the ledger.
    decoded: RefCell<Option<Rc<LedgerEntry>>>,
}

impl HostLedgerEntry {
    /// Constructs a wrapper around an already-decoded entry (e.g. one created
    /// in-host by a host function).
    pub(crate) fn from_decoded(entry: Rc<LedgerEntry>) -> Self {
        Self {
            encoded: None,
            decoded: RefCell::new(Some(entry)),
        }
    }

    /// Constructs a wrapper around a raw, owned XDR buffer. The entry is not
    /// decoded until [`HostLedgerEntry::decoded`] is first called. The caller is
    /// responsible for having charged for owning `encoded`.
    pub(crate) fn from_encoded(encoded: Rc<[u8]>) -> Self {
        Self {
            encoded: Some(encoded),
            decoded: RefCell::new(None),
        }
    }

    fn internal_err() -> HostError {
        Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::InternalError).into()
    }

    /// Returns the decoded entry, decoding (and charging `ValDeser`) on the
    /// first call and memoizing the result.
    pub(crate) fn decoded(&self, budget: &Budget) -> Result<Rc<LedgerEntry>, HostError> {
        if let Some(entry) = self.decoded.borrow().as_ref() {
            return Ok(Rc::clone(entry));
        }
        let buf = self.encoded.as_ref().ok_or_else(Self::internal_err)?;
        let entry: LedgerEntry = metered_from_xdr_with_budget(buf, budget)?;
        let entry = Rc::metered_new(entry, budget)?;
        *self.decoded.borrow_mut() = Some(Rc::clone(&entry));
        Ok(entry)
    }

    /// Hashes the entry into a tracing hasher without forcing a decode. Used
    /// only by the trace machinery.
    pub(crate) fn metered_hash(
        &self,
        hasher: &mut CountingHasher,
        budget: &Budget,
    ) -> Result<(), HostError> {
        let decoded = self.decoded.borrow();
        // Hash the content from whichever representation is available, without
        // forcing a decode.
        match &self.encoded {
            Some(buf) => buf.metered_hash(hasher, budget)?,
            None => {
                let entry = decoded.as_ref().ok_or_else(Self::internal_err)?;
                entry.as_ref().metered_hash_xdr(hasher, budget)?;
            }
        }
        // Hash the current decode state so the trace captures the actual
        // in-memory entry representation.
        self.encoded.is_some().metered_hash(hasher, budget)?;
        decoded.is_some().metered_hash(hasher, budget)?;
        Ok(())
    }
}
