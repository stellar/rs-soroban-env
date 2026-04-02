use crate::{budget::Budget, xdr::ContractCostType, Compare, HostError};
use std::{
    cell::Cell,
    cmp::Ordering,
    hash::{Hash, Hasher},
};

// We approximate the cost of Rust's default hasher (SIP-1-3) using ChaCha20DrawBytes,
// since both are of a similar order of magnitude. This cost type is used for:
// 1. Tracing operations charged against the shadow budget
// 2. MeteredHashMap operations for per-frame contract data caching
//
// This approximation may slightly overcharge, which is acceptable to prevent
// hash operations from becoming a DoS vector.
const HASH_COST_TYPE: ContractCostType = ContractCostType::ChaCha20DrawBytes;

#[derive(Default)]
pub(crate) struct CountingHasher {
    count: usize,
    hasher: std::collections::hash_map::DefaultHasher,
}

impl CountingHasher {
    pub(crate) fn count(&self) -> usize {
        self.count
    }
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
        budget.charge(HASH_COST_TYPE, Some(hasher.count() as u64))?;
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

use hashbrown::HashMap;

use crate::{
    budget::AsBudget,
    host::{declared_size::DeclaredSizeForMetering, metered_clone::MeteredClone},
    xdr::{ScErrorCode, ScErrorType},
    Error,
};

/// A `BuildHasher` that creates `CountingHasher` instances. This ensures that
/// all HashMap operations (including internal ones like `remove`) use the same
/// hash function as our raw_entry operations.
#[derive(Default, Clone)]
struct CountingBuildHasher;

impl std::hash::BuildHasher for CountingBuildHasher {
    type Hasher = CountingHasher;
    fn build_hasher(&self) -> CountingHasher {
        CountingHasher::default()
    }
}

/// A metered hash map that wraps `hashbrown::HashMap` with budget
/// charging.
///
/// This is minimalistic on purpose and only supports the operations needed by
/// the storage layer. Specifically, clones and key removal are not supported as
/// they are not needed. This can be extended in the future if necessary, though
/// keep in mind that this is implementation not going to be efficient for
/// representing persistent data structures.
///
/// This uses hashbrown map instead of std map (which is also actually based on
/// hashbrown) due to its support of `raw_entry` API which allows us to meter
/// hashing and comparison faithfully during the map operations.
pub(crate) struct MeteredHashMap<K, V> {
    map: HashMap<K, V, CountingBuildHasher>,
}

impl<K, V> Default for MeteredHashMap<K, V> {
    fn default() -> Self {
        Self {
            map: HashMap::with_hasher(CountingBuildHasher),
        }
    }
}

/// Hash implementation is only necessary for tracing where we need to hash the
/// entire storage state deterministically.
impl<K, V> std::hash::Hash for MeteredHashMap<K, V>
where
    K: std::hash::Hash + Eq + Ord,
    V: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut entries: Vec<_> = self.map.iter().collect();
        entries.sort_by(|a, b| a.0.cmp(b.0));
        for (k, v) in entries {
            k.hash(state);
            v.hash(state);
        }
    }
}

/// Hashes a key with `CountingHasher` and charges the budget based on the
/// actual byte count via `MeteredHash`. Returns the hash value for use with
/// hashbrown's `raw_entry` API.
fn metered_hash_key<K: MeteredHash>(key: &K, budget: &Budget) -> Result<u64, HostError> {
    let mut hasher = CountingHasher::default();
    key.metered_hash(&mut hasher, budget)?;
    Ok(hasher.finish())
}

/// Computes a hash value for a key using `CountingHasher`. Used as the rehash
/// function for `insert_with_hasher` during table resize (not metered — this
/// is internal housekeeping).
fn rehash_key<K: Hash>(key: &K) -> u64 {
    let mut hasher = CountingHasher::default();
    key.hash(&mut hasher);
    hasher.finish()
}

/// Helper for building a metered equality closure and propagating errors.
///
/// The `raw_entry` API requires a `FnMut(&K) -> bool` for equality comparison,
/// but `Budget::compare()` returns `Result`. This type captures comparison
/// errors in a `Cell` and converts them to `false` (stop probing). After the
/// raw_entry operation, call `check()` to propagate any captured error.
///
/// Since comparison errors from within a raw_entry closure may leave the map
/// in an inconsistent state (e.g. duplicate key on insert), any such error is
/// escalated to an unrecoverable internal error unless it is already a budget
/// exceeded error.
struct MeteredEq<'a, K> {
    budget: &'a Budget,
    key: &'a K,
    err: Cell<Option<HostError>>,
}

impl<'a, K> MeteredEq<'a, K> {
    fn new(budget: &'a Budget, key: &'a K) -> Self {
        Self {
            budget,
            key,
            err: Cell::new(None),
        }
    }

    fn eq(&self, other: &K) -> bool
    where
        Budget: Compare<K, Error = HostError>,
    {
        match self.budget.compare(other, self.key) {
            Ok(ord) => ord == Ordering::Equal,
            Err(e) => {
                self.err.set(Some(e));
                false
            }
        }
    }

    fn check(self) -> Result<(), HostError> {
        if let Some(e) = self.err.into_inner() {
            // Only budget errors are expected during comparison. Any other
            // error type means something unexpected happened; escalate to an
            // unrecoverable internal error to prevent recovery with a
            // potentially inconsistent map state.
            if e.error.is_type(ScErrorType::Budget) && e.error.is_code(ScErrorCode::ExceededLimit) {
                return Err(e);
            }
            return Err(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::InternalError,
            )
            .into());
        }
        Ok(())
    }
}

impl<K, V> MeteredHashMap<K, V>
where
    K: Hash + Eq,
{
    /// Creates a new empty metered hash map.
    pub(crate) fn new() -> Self {
        Self {
            map: HashMap::with_hasher(CountingBuildHasher),
        }
    }

    /// Returns the number of entries in the map.
    /// This is O(1) and not metered.
    pub(crate) fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns an iterator over the map entries. This is a hash map, so the
    /// iteration order is not deterministic.
    /// Not metered so be careful when using this in recording mode.
    #[cfg(any(test, feature = "testutils", feature = "recording_mode"))]
    pub(crate) fn iter_non_metered(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }

    /// Consumes the map and returns an iterator over owned key-value pairs.
    /// Used by production code that consumes the map (during finalization).
    /// This is not metered and thus relies on the caller to use the results in
    /// metered operations.
    pub(crate) fn into_iter_non_metered(self) -> impl Iterator<Item = (K, V)> {
        self.map.into_iter()
    }

    /// Returns a mutable iterator over the map entries.
    /// This function is not metered as it only returns references. Make sure
    /// that the iteration is somehow bounded otherwise (e.g. every entry
    /// access is metered, or the amount of iterations has a strict bound).
    pub(crate) fn iter_mut_unmetered(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.map.iter_mut()
    }

    /// Removes a key from the map.
    /// Test-only method for manipulating storage in tests, hence unmetered.
    #[cfg(test)]
    pub(crate) fn remove(&mut self, key: &K) -> Option<V> {
        self.map.remove(key)
    }
}

impl<K, V> MeteredHashMap<K, V>
where
    K: Hash + Eq + DeclaredSizeForMetering + MeteredClone,
    V: DeclaredSizeForMetering + MeteredClone,
    Budget: Compare<K, Error = HostError>,
{
    const ENTRY_SIZE: u64 = K::DECLARED_SIZE + V::DECLARED_SIZE;

    /// Shared lookup logic: hashes the key (metered), then probes the map with
    /// metered equality. Returns the raw_entry result after error checking.
    fn raw_lookup<'a>(
        &'a self,
        key: &K,
        budget: &Budget,
    ) -> Result<Option<(&'a K, &'a V)>, HostError> {
        let hash = metered_hash_key(key, budget)?;
        let meq = MeteredEq::new(budget, key);
        let result = self.map.raw_entry().from_hash(hash, |k| meq.eq(k));
        meq.check()?;
        Ok(result)
    }

    /// Shared mutable entry logic: hashes the key (metered), then probes the
    /// map with metered equality via `raw_entry_mut`. Returns the
    /// `RawEntryMut` after error checking.
    fn raw_entry_mut<'a>(
        &'a mut self,
        key: &K,
        budget: &Budget,
    ) -> Result<
        (
            hashbrown::hash_map::RawEntryMut<'a, K, V, CountingBuildHasher>,
            u64,
        ),
        HostError,
    > {
        let hash = metered_hash_key(key, budget)?;
        let meq = MeteredEq::new(budget, key);
        let entry = self.map.raw_entry_mut().from_hash(hash, |k| meq.eq(k));
        meq.check()?;
        Ok((entry, hash))
    }

    /// Gets a reference to a value in the map.
    pub(crate) fn get<B: AsBudget>(&self, key: &K, budget: &B) -> Result<Option<&V>, HostError> {
        Ok(self.raw_lookup(key, budget.as_budget())?.map(|(_, v)| v))
    }

    /// Returns a mutable reference to a value in the map.
    pub(crate) fn get_mut<B: AsBudget>(
        &mut self,
        key: &K,
        budget: &B,
    ) -> Result<Option<&mut V>, HostError> {
        let (entry, _hash) = self.raw_entry_mut(key, budget.as_budget())?;
        Ok(match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(e) => Some(e.into_mut()),
            hashbrown::hash_map::RawEntryMut::Vacant(_) => None,
        })
    }

    /// Inserts a key-value pair into the map. Unlike MeteredOrdMap, this mutates
    /// in place rather than creating a new map.
    /// Charges for hash lookup and data movement.
    pub(crate) fn insert<B: AsBudget>(
        &mut self,
        key: K,
        value: V,
        budget: &B,
    ) -> Result<(), HostError> {
        let budget_ref = budget.as_budget();
        let (entry, hash) = self.raw_entry_mut(&key, budget_ref)?;
        budget_ref.charge(ContractCostType::MemCpy, Some(Self::ENTRY_SIZE))?;
        match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(mut e) => {
                e.insert(value);
            }
            hashbrown::hash_map::RawEntryMut::Vacant(e) => {
                e.insert_with_hasher(hash, key, value, rehash_key);
            }
        }
        Ok(())
    }

    /// Inserts a key-value pair and returns a mutable reference to the value.
    /// This avoids a separate get_mut call after insert.
    /// Charges for hash lookup and data movement.
    #[cfg(any(test, feature = "recording_mode"))]
    pub(crate) fn insert_and_get_mut<B: AsBudget>(
        &mut self,
        key: K,
        value: V,
        budget: &B,
    ) -> Result<&mut V, HostError> {
        let budget_ref = budget.as_budget();
        let (entry, hash) = self.raw_entry_mut(&key, budget_ref)?;
        budget_ref.charge(ContractCostType::MemCpy, Some(Self::ENTRY_SIZE))?;
        Ok(match entry {
            hashbrown::hash_map::RawEntryMut::Occupied(mut e) => {
                e.insert(value);
                e.into_mut()
            }
            hashbrown::hash_map::RawEntryMut::Vacant(e) => {
                let (_, v) = e.insert_with_hasher(hash, key, value, rehash_key);
                v
            }
        })
    }
}
