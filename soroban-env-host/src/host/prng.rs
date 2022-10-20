use rand_chacha::{
    rand_core::{RngCore, SeedableRng},
    ChaCha20Rng,
};
use sha2::{Digest, Sha256};
use std::collections::{hash_map::Entry, HashMap};

use crate::{events::DebugError, xdr::Hash};

/// PRNG subsystem in the host, which provides best-effort pseudo-randomness to
/// guest contracts using a combination of features that guests cannot easily
/// implement themselves:
///
///   - A common seed S needs to be installed by the embedding environment and
///     should be different for each host instance, or roughly "each transaction
///     in a block of transactions" so that transactions from different
///     submitters cannot guess one another's seeds. It is the embedder's
///     responsibility to set this to something hard to predict. In the
///     stellar-core embedding, S is set to the combination of the txset hash
///     and the apply-order _position_ of the transaction in the txset, which is
///     itself defined in terms of an xor of each transaction hash and the
///     previous ledger hash. While it is theoretically possible for a validator
///     to guess or influence this, being able to do so also grants validators
///     the ability to front-run the orderbook and is therefore already an
///     attack vector for the whole stellar network's financial integrity. In
///     other words: reusing it here doesn't make anything worse, we're already
///     obliged to make transaction apply-order hard to guess or control.
///
///   - Within a transaction, each contract ID I has its own persistent PRNG
///     state seeded from (S,I) which is kept persistently in the Prng type here
///     and advanced / accessed when that contract requests PRNG output. The
///     state of some contract I is separate from the state of some other
///     contract J, such that I cannot try to control the state that J gets
///     called with, nor can I call J once, observe some of its pseudorandom
///     behaviour, and then call J again knowing that behavior since J will take
///     different pseudorandom behaviour the next time (owing to J's PRNG state
///     persisting from call to call).
///
///   - To further minimize the potential for a "victim" PRNG-using contract J
///     to leak its PRNG state to some "attacker" contract I in any way that I
///     can use, calls from J to the PRNG carry a flag indicating whether the
///     PRNG should be finalized with this call. When finalized, further calls
///     by J to its PRNG will trap. In general this flag should be set to true
///     unless J expects to make multiple calls to its PRNG over multiple
///     invocations in the same transaction, in which case it may set the flag
///     to false, and may need to take extra caution to avoid leaking PRNG state
///     to its caller.

#[derive(Clone)]
pub(crate) struct PrngSet {
    // The common seed defaults to 0 but should be set to some
    // per-transaction value by the embedder when running in a
    // transaction-processing environment.
    common_seed: [u8; 32],
    // Key in this map is either Some(ID) for a contract ID or
    // None for host-function frames (which all share a PRNG).
    //
    // Value in this map is either Some(R) for a live PRNG or
    // None for one that has been finalized.
    //
    // If an entry is missing, it has not yet been initialized.
    prngs: HashMap<Option<Hash>, Option<ChaCha20Rng>>,
}

impl Default for PrngSet {
    fn default() -> Self {
        Self::from_seed([0; 32])
    }
}

impl PrngSet {
    pub(crate) fn from_seed(common_seed: [u8; 32]) -> Self {
        let prngs = HashMap::new();
        Self { common_seed, prngs }
    }

    /// Return the PRNG for the provided context, or None if the context has had
    /// its PRNG finalized. Context should be Some(ID) if called from a frame
    /// with a contract ID, otherwise None.
    fn get_prng_for_context(&mut self, context: Option<Hash>) -> &mut Option<ChaCha20Rng> {
        match self.prngs.entry(context.clone()) {
            Entry::Occupied(oe) => oe.into_mut(),
            Entry::Vacant(ve) => {
                let mut hasher: Sha256 = Sha256::new();
                hasher.update(self.common_seed.as_slice());
                match context {
                    None => (),
                    Some(hash) => hasher.update(hash),
                }
                let seed = hasher.finalize();
                let seed32: &[u8; 32] = seed.as_ref();
                let prng = ChaCha20Rng::from_seed(seed32.clone());
                ve.insert(Some(prng))
            }
        }
    }

    /// Get the PRNG for the context, if available, and call the provided
    /// callback with it, failing if the PRNG for the context has already been
    /// finalized, and finalizing it once the callback is called, if requested.
    fn with_prng<F, U>(
        &mut self,
        context: Option<Hash>,
        finalize: bool,
        f: F,
    ) -> Result<U, DebugError>
    where
        F: FnOnce(&mut ChaCha20Rng) -> U,
    {
        let opt_prng: &mut Option<ChaCha20Rng> = self.get_prng_for_context(context);
        let u = if let Some(prng) = opt_prng {
            f(prng)
        } else {
            return Err(DebugError::general().msg("access to finalized PRNG"));
        };
        if finalize {
            *opt_prng = None
        }
        Ok(u)
    }

    pub(crate) fn fill_bytes(
        &mut self,
        context: Option<Hash>,
        out: &mut [u8],
        finalize: bool,
    ) -> Result<(), DebugError> {
        self.with_prng(context, finalize, |prng| prng.fill_bytes(out))
    }

    pub(crate) fn next_u32(
        &mut self,
        context: Option<Hash>,
        finalize: bool,
    ) -> Result<u32, DebugError> {
        self.with_prng(context, finalize, |prng| prng.next_u32())
    }

    pub(crate) fn next_u64(
        &mut self,
        context: Option<Hash>,
        finalize: bool,
    ) -> Result<u64, DebugError> {
        self.with_prng(context, finalize, |prng| prng.next_u64())
    }
}
