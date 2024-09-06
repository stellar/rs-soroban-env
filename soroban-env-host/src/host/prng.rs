use super::{declared_size::DeclaredSizeForMetering, metered_clone::MeteredContainer};
use crate::{
    budget::Budget,
    crypto::{chacha20_fill_bytes, unbias_prng_seed},
    host::metered_clone::MeteredClone,
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    HostError,
};
use rand::{distributions::Uniform, prelude::Distribution, seq::SliceRandom, RngCore};
use rand_chacha::{rand_core::SeedableRng, ChaCha20Rng};
use std::ops::RangeInclusive;

/// PRNG subsystem in the host, which provides best-effort pseudo-randomness to
/// guest contracts using a combination of features that guests cannot easily
/// implement themselves:
///
///   - The host itself maintains one "base" PRNG which should be seeded by the
///     embedding environment and should be different for each host instance, or
///     roughly "each transaction in a block of transactions" so that
///     transactions from different submitters cannot guess one another's seeds.
///     It is the embedder's responsibility to set this to something hard to
///     predict. In the stellar-core embedding, S is set to the combination of
///     the txset hash and the apply-order _position_ of the transaction in the
///     txset, which is itself defined in terms of an xor of each transaction
///     hash and the previous ledger hash. While it is theoretically possible
///     for a validator to guess or influence this, being able to do so also
///     grants validators the ability to front-run the orderbook and is
///     therefore already an attack vector for the whole stellar network's
///     financial integrity. In other words: reusing it here doesn't make
///     anything worse, we're already obliged to make transaction apply-order
///     hard to guess or control.
///
///   - Each frame (which is to say: each contract invocation or sub-invocation)
///     will get a new PRNG instance separately seeded from the host's "base"
///     PRNG, and guest code can only access the frame's PRNG, not the "base"
///     PRNG or those of any other frame. This doesn't eliminate _all_ attack
///     vectors or mechanisms for misuse, but it's the best we can give the user
///     for buiding on. In particular it means that a "random" contract will not
///     behave the same way from one call to the next inside the same txset, nor
///     can a caller control the seed for a "random" callee (since they can't
///     observe the state of the "base" PRNG).
///
///   - Users _can_ reseed their frame-local PRNG if they want, which is a
///     useful building block for random-commitment schemes. In particular if a
///     contract is trying to make a "single random decision", and avoid having
///     callers retry that decision repeatedly while aborting the transaction on
///     any random decision the caller doesn't like, the contract can operate as
///     a state machine like so:
///
///       - tx1: write commitment finalizing all inputs to "random action", plus
///              N = current ledger and S = prng_bytes_new(32).
///
///       - tx2: re-read all committed values, if ledger > N, prng_reseed(S),
///              and use PRNG to take "random" action committed-to.
///
///     With this design, assuming the contract does not expose any _online_
///     method for its caller to observe the commitment it made in tx1, the
///     caller (situated in the same transaction) won't know from its position
///     whether it's to its advantage or not to abort tx1, so will naturally let
///     it commit. Once the commitment is saved, it includes a "locked in"
///     choice of seed, essentially propagating PRNG state out of a context the
///     caller might have been able to influence its state via selective aborts
///     (but didn't know whether to) into a context where the caller can no
///     longer influence its state. The contract can then be re-invoked in the
///     next ledger to load and execute the commitment, in tx2.
///
///   - We also include 3 building blocks for _using_ the PRNG: one basic one
///     that just "generates a random BytesObject" (that the user can do
///     anything they like with, including copying to guest linear memory), and
///     two slightly more subtle but very standard operations that are easy to
///     get wrong: an inclusive-range uniform u64 sampler and a Fisher-Yates
///     vector shuffle. The latter is also hard to do cheaply in guest code
///     without copying the vector into the guest and copying it back.
///
///   - All these PRNGs are ChaCha20: a strong, cheap, standard CSPRNG.
///
#[derive(Debug, Clone)]
pub(crate) struct Prng(pub(crate) ChaCha20Rng);

pub type Seed = <rand_chacha::ChaCha20Rng as rand::SeedableRng>::Seed;
pub const SEED_BYTES: u64 = <Seed as DeclaredSizeForMetering>::DECLARED_SIZE;
static_assertions::const_assert_eq!(SEED_BYTES, 32);

impl std::hash::Hash for Prng {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.get_seed().hash(state);
        self.0.get_stream().hash(state);
        self.0.get_word_pos().hash(state);
    }
}

impl Prng {
    fn charge_prng_bytes(&self, budget: &Budget, count: u64) -> Result<(), HostError> {
        budget.charge(ContractCostType::ChaCha20DrawBytes, Some(count))
    }

    pub(crate) fn new_from_seed(seed: Seed, budget: &Budget) -> Result<Self, HostError> {
        let seed = unbias_prng_seed(&seed, budget)?;
        Ok(Self(ChaCha20Rng::from_seed(seed)))
    }

    pub(crate) fn u64_in_inclusive_range(
        &mut self,
        range: RangeInclusive<u64>,
        budget: &Budget,
    ) -> Result<u64, HostError> {
        // rand::Uniform panics if start > end.
        if range.start() > range.end() {
            return Err((ScErrorType::Value, ScErrorCode::InvalidInput).into());
        }

        // We over-estimate the number of bytes drawn by a factor of 2, to
        // account for the fact that a range sample is rejection-sampling which
        // is expected to only do one draw but might do more than one.
        self.charge_prng_bytes(budget, 2 * <u64 as DeclaredSizeForMetering>::DECLARED_SIZE)?;
        let u = Uniform::from(range);
        Ok(u.sample(&mut self.0))
    }

    pub(crate) fn vec_shuffle(
        &mut self,
        v: &HostVec,
        budget: &Budget,
    ) -> Result<HostVec, HostError> {
        // A Fisher-Yates shuffle essentially does one call to u64_in_range for
        // each element of the input vector, followed by an optional swap. Since
        // u64_in_range is itself a rejection sampling operation (to avoid bias)
        // we can't be 100% sure how many draws it'll make, but the expected
        // number of draws is 1. To give ourselves a little more safety we'll
        // double that number. We also give the implementation freedom to draw a
        // 64-bit (8-byte) value per index, meaning we charge for generating 2 *
        // 8 * len bytes.
        let mut v2 = v.metered_clone(budget)?;
        // We charge for both the PRNG draws and the swaps here (as "memcpys").
        self.charge_prng_bytes(budget, 16u64.saturating_mul(v.len() as u64))?;
        budget.charge(ContractCostType::MemCpy, Some(v.len() as u64))?;
        v2.as_mut_slice().shuffle(&mut self.0);
        Ok(v2)
    }

    pub(crate) fn bytes_new(&mut self, size: u32, budget: &Budget) -> Result<ScBytes, HostError> {
        Vec::<u8>::charge_bulk_init_cpy(size as u64, budget)?;
        let mut vec = vec![0u8; size as usize];
        chacha20_fill_bytes(&mut self.0, vec.as_mut_slice(), budget)?;
        Ok(ScBytes::try_from(vec)?)
    }

    pub(crate) fn sub_prng(&mut self, budget: &Budget) -> Result<Prng, HostError> {
        let mut new_seed: Seed = [0; SEED_BYTES as usize];
        chacha20_fill_bytes(&mut self.0, &mut new_seed, budget)?;
        budget.charge(ContractCostType::MemCpy, Some(SEED_BYTES))?;
        Ok(Self(ChaCha20Rng::from_seed(new_seed)))
    }

    pub(crate) fn unmetered_raw_sub_prng(&mut self) -> ChaCha20Rng {
        let mut new_seed: Seed = [0; SEED_BYTES as usize];
        self.0.fill_bytes(&mut new_seed);
        ChaCha20Rng::from_seed(new_seed)
    }
}
