use soroban_env_common::xdr::ScHostObjErrorCode;

use super::MeteredClone;
use crate::{
    budget::{AsBudget, Budget, CostType},
    xdr::ScHostFnErrorCode,
    Compare, Host, HostError,
};
use std::{borrow::Borrow, cmp::Ordering, marker::PhantomData};

pub struct MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    pub(crate) map: Vec<(K, V)>,
    ctx: PhantomData<Ctx>,
}

impl<K, V, Ctx> Clone for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
            ctx: Default::default(),
        }
    }
}

impl<K, V, Ctx> MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    fn charge_new<B: AsBudget>(&self, size: usize, b: &B) -> Result<(), HostError> {
        b.as_budget().charge(CostType::MapNew, size as u64)
    }

    fn charge_access<B: AsBudget>(&self, count: usize, b: &B) -> Result<(), HostError> {
        b.as_budget().charge(CostType::MapEntry, count as u64)
    }

    fn charge_scan<B: AsBudget>(&self, b: &B) -> Result<(), HostError> {
        b.as_budget()
            .charge(CostType::MapEntry, self.map.len() as u64)
    }

    fn charge_binsearch<B: AsBudget>(&self, b: &B) -> Result<(), HostError> {
        let mag = 64 - (self.map.len() as u64).leading_zeros();
        b.as_budget().charge(CostType::MapEntry, 1 + mag as u64)
    }
}

impl<K, V, Ctx> Default for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    fn default() -> Self {
        Self {
            map: Default::default(),
            ctx: Default::default(),
        }
    }
}

// We abstract over Ctx:AsBudget here so that you can operate on MeteredOrdMap
// before you have a Host constructed -- a bit, though only with certain types,
// for example you can't do any lookups on maps keyed by Objects -- so long as
// you at _least_ have a Budget. This is done to allow clients to populate and
// reuse Storage maps keyed by non-Objects such as LedgerKey while only keeping
// a Budget alive, rather than a whole Host.
impl<K, V, Ctx> MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    pub fn new(ctx: &Ctx) -> Result<Self, HostError> {
        ctx.as_budget().charge(CostType::MapNew, 1)?;
        Ok(MeteredOrdMap {
            map: Vec::new(),
            ctx: Default::default(),
        })
    }

    pub fn from_map(map: Vec<(K, V)>, ctx: &Ctx) -> Result<Self, HostError> {
        // Construction cost already paid for by caller, just check
        // that input has sorted and unique keys.
        let m = MeteredOrdMap {
            map,
            ctx: Default::default(),
        };
        m.charge_scan(ctx)?;
        for w in m.map.as_slice().windows(2) {
            match <Ctx as Compare<K>>::compare(ctx, &w[0].0, &w[1].0)? {
                Ordering::Less => (),
                // TODO need a better error code for "duplicate key"
                Ordering::Equal => return Err(ScHostFnErrorCode::UnknownError.into()),
                // TODO need a better error code for "out-of-order keys"
                Ordering::Greater => return Err(ScHostFnErrorCode::UnknownError.into()),
            }
        }
        Ok(m)
    }

    // This doesn't take ExactSizeIterator since that is not implemented for Chain
    // (see https://github.com/rust-lang/rust/issues/34433) but it only works
    // with iterators that report an exact size_hint, and it constructs a new
    // Vec from that iterator with a single allocation-and-copy.
    pub fn from_exact_iter<I: Iterator<Item = (K, V)>>(
        iter: I,
        ctx: &Ctx,
    ) -> Result<Self, HostError> {
        if let (_, Some(sz)) = iter.size_hint() {
            ctx.as_budget().charge(CostType::MapNew, sz as u64)?;
            // TODO need to do a metered_clone bulk-charge here as well.
            // ctx.as_budget().charge(CostType::BytesClone, nbytes)?;
            let map: Vec<(K, V)> = iter.collect();
            Ok(Self {
                map,
                ctx: Default::default(),
            })
        } else {
            // TODO use a better error code for "unbounded input itertors"
            Err(ScHostFnErrorCode::UnknownError.into())
        }
    }

    fn find<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Result<usize, usize>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        self.charge_binsearch(ctx)?;
        let mut err: Option<HostError> = None;
        let res = self.map.binary_search_by(|probe| {
            // We've already hit an error, return Ordering::Equal
            // to terminate search asap.
            if err.is_some() {
                return Ordering::Equal;
            }
            match <Ctx as Compare<Q>>::compare(ctx, probe.0.borrow(), key) {
                Ok(ord) => ord,
                Err(he) => {
                    err = Some(he);
                    Ordering::Equal
                }
            }
        });
        match err {
            Some(he) => Err(he),
            None => Ok(res),
        }
    }

    pub fn insert(&self, key: K, value: V, ctx: &Ctx) -> Result<Self, HostError> {
        self.charge_access(1, ctx)?;
        match self.find(&key, ctx)? {
            Ok(replace_pos) => {
                // [0,1,2] replace_pos == 1
                // take(1) + new + skip(2)
                // [0] + new + [2]
                if replace_pos == usize::MAX - 1 {
                    // TODO: something better for integer overflow.
                    return Err(ScHostObjErrorCode::VecIndexOutOfBound.into());
                }
                let init = self.map.iter().take(replace_pos).cloned();
                let fini = self.map.iter().skip(replace_pos + 1).cloned();
                let iter = init.chain([(key, value)].into_iter()).chain(fini);
                Self::from_exact_iter(iter, ctx)
            }
            Err(insert_pos) => {
                // [0,1,2] insert_pos == 1
                // take(1) + new + skip(1)
                // [0] new [1, 2]
                let init = self.map.iter().take(insert_pos).cloned();
                let fini = self.map.iter().skip(insert_pos).cloned();
                let iter = init.chain([(key, value)].into_iter()).chain(fini);
                Self::from_exact_iter(iter, ctx)
            }
        }
    }

    pub fn get<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Option<&V>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        match self.find(key, ctx)? {
            Ok(found) => {
                self.charge_access(1, ctx)?;
                Ok(Some(&self.map[found].1))
            }
            _ => Ok(None),
        }
    }

    /// Returns a `Some((new_self, val))` pair where `new_self` no longer
    /// contains an entry for `key`, if the key existed, otherwise `None` if
    /// `key` didn't exist (in which case there's no need to clone).
    pub fn remove<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Option<(Self, V)>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        match self.find(key, ctx)? {
            Ok(found) if found > 0 => {
                // There's a nonempty prefix to preserve.
                // [0,1,2] remove_pos == 1
                // take(1) + skip(2)
                // [0] [2]
                // `found` cannot be > `usize::MAX` - 1, since that means the map contains more than
                // `usize::MAX` elements. Therefore `found + 1` is guaranteed to not overflow.
                let init = self.map.iter().take(found).cloned();
                let fini = self.map.iter().skip(found + 1).cloned();
                let iter = init.chain(fini);
                let new = Self::from_exact_iter(iter, ctx)?;
                let res = self.map[found].1.metered_clone(ctx.as_budget())?;
                Ok(Some((new, res)))
            }
            Ok(found) => {
                // No prefix, removing at position 0.
                // If the suffix is empty it's harmless.
                let iter = self.map.iter().skip(1).cloned();
                let new = Self::from_exact_iter(iter, ctx)?;
                let res = self.map[found].1.metered_clone(ctx.as_budget())?;
                Ok(Some((new, res)))
            }
            _ => Ok(None),
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn contains_key<Q>(&self, key: &Q, ctx: &Ctx) -> Result<bool, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        Ok(self.find(key, ctx)?.is_ok())
    }

    pub fn get_prev<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Option<&(K, V)>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        match self.find(key, ctx)? {
            Ok(hit) if hit == 0 => Ok(None),
            Ok(hit) => Ok(Some(&self.map[hit - 1])),
            // Err(miss) means you could insert key at miss
            // to maintain sort order (meaning that the element
            // currently at miss, if it exists, is > than key).
            Err(miss) if miss == 0 => Ok(None),
            Err(miss) if miss - 1 < self.map.len() => Ok(Some(&self.map[miss - 1])),
            Err(_) => Ok(None),
        }
    }

    pub fn get_next<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Option<&(K, V)>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        match self.find(key, ctx)? {
            Ok(hit) if (hit < usize::MAX) && (hit + 1 < self.map.len()) => {
                Ok(Some(&self.map[hit + 1]))
            }
            Ok(hit) => Ok(None),
            Err(miss) if (miss < self.map.len()) => Ok(Some(&self.map[miss])),
            Err(miss) => Ok(None),
        }
    }

    pub fn get_min<Q>(&self, ctx: &Ctx) -> Result<Option<&(K, V)>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        self.charge_access(1, ctx)?;
        Ok(self.map.as_slice().first())
    }

    pub fn get_max<Q>(&self, ctx: &Ctx) -> Result<Option<&(K, V)>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        self.charge_access(1, ctx)?;
        Ok(self.map.as_slice().last())
    }

    pub fn keys(&self, ctx: &Ctx) -> Result<impl Iterator<Item = &K>, HostError> {
        self.charge_scan(ctx)?;
        Ok(self.map.iter().map(|(k, _)| k))
    }

    pub fn values(&self, ctx: &Ctx) -> Result<impl Iterator<Item = &V>, HostError> {
        self.charge_scan(ctx)?;
        Ok(self.map.iter().map(|(_, v)| v))
    }

    pub fn iter(&self, ctx: &Ctx) -> Result<impl Iterator<Item = &(K, V)>, HostError> {
        self.charge_scan(ctx)?;
        Ok(self.map.iter())
    }
}

impl<K, V, Ctx> MeteredClone for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        self.charge_new(self.map.len(), budget)?;
        Ok(self.clone())
    }
}

impl<K, V> Compare<MeteredOrdMap<K, V, Host>> for Host
where
    K: MeteredClone,
    V: MeteredClone,
    Host: Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredOrdMap<K, V, Host>,
        b: &MeteredOrdMap<K, V, Host>,
    ) -> Result<Ordering, Self::Error> {
        self.as_budget()
            .charge(CostType::MapEntry, a.map.len().min(b.map.len()) as u64)?;
        <Self as Compare<Vec<(K, V)>>>::compare(self, &a.map, &b.map)
    }
}

impl<K, V> Compare<MeteredOrdMap<K, V, Budget>> for Budget
where
    K: MeteredClone,
    V: MeteredClone,
    Budget: Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredOrdMap<K, V, Budget>,
        b: &MeteredOrdMap<K, V, Budget>,
    ) -> Result<Ordering, Self::Error> {
        self.charge(CostType::MapEntry, a.map.len().min(b.map.len()) as u64)?;
        <Self as Compare<Vec<(K, V)>>>::compare(self, &a.map, &b.map)
    }
}

impl<'a, K, V, Ctx> IntoIterator for &'a MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Item = &'a (K, V);
    type IntoIter = core::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.map).into_iter()
    }
}

impl<K, V, Ctx> IntoIterator for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget + Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
