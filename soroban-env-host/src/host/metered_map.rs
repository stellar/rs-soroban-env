use crate::{
    budget::{AsBudget, Budget},
    host::{declared_size::DeclaredSizeForMetering, MeteredClone},
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Compare, Error, Host, HostError,
};

use std::{borrow::Borrow, cmp::Ordering, marker::PhantomData};

const MAP_OOB: Error = Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);

pub struct MeteredOrdMap<K, V, Ctx> {
    pub(crate) map: Vec<(K, V)>,
    ctx: PhantomData<Ctx>,
}

/// `Clone` should not be used directly, used `MeteredClone` instead if
/// possible. `Clone` is defined here to satisfy trait requirements.
impl<K, V, Ctx> Clone for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget,
{
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
            ctx: Default::default(),
        }
    }
}

impl<K, V, Ctx> std::hash::Hash for MeteredOrdMap<K, V, Ctx>
where
    K: std::hash::Hash,
    V: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.map.hash(state);
    }
}

impl<K, V, Ctx> MeteredOrdMap<K, V, Ctx>
where
    K: DeclaredSizeForMetering,
    V: DeclaredSizeForMetering,
    Ctx: AsBudget,
{
    const ENTRY_SIZE: u64 = <(K, V) as DeclaredSizeForMetering>::DECLARED_SIZE;

    fn charge_access<B: AsBudget>(&self, count: usize, b: &B) -> Result<(), HostError> {
        b.as_budget().charge(
            ContractCostType::MemCpy,
            Some(Self::ENTRY_SIZE.saturating_mul(count as u64)),
        )
    }

    fn charge_scan<B: AsBudget>(&self, b: &B) -> Result<(), HostError> {
        Self::charge_access(self, self.map.len(), b)
    }

    // Charge binary search includes accessing number of entries expected for
    // finding an entry. Cost of comparison is charged separately and not
    // covered here.
    fn charge_binsearch<B: AsBudget>(&self, b: &B) -> Result<(), HostError> {
        let mag: u32 = 64u32.saturating_sub((self.map.len() as u64).leading_zeros());
        b.as_budget().charge(
            ContractCostType::MemCpy,
            Some(Self::ENTRY_SIZE.saturating_mul(mag.saturating_add(1u32) as u64)),
        )
    }
}

impl<K, V, Ctx> Default for MeteredOrdMap<K, V, Ctx>
where
    Ctx: Default,
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
    Ctx: AsBudget + Compare<K, Error = HostError>,
{
    pub fn new() -> Self {
        MeteredOrdMap {
            map: Vec::new(),
            ctx: Default::default(),
        }
    }

    pub fn from_map(map: Vec<(K, V)>, ctx: &Ctx) -> Result<Self, HostError> {
        if u32::try_from(map.len()).is_err() {
            return Err(MAP_OOB.into());
        }
        // Allocation cost already paid for by caller, here just checks that input
        // has sorted and unique keys.
        let m = MeteredOrdMap {
            map,
            ctx: Default::default(),
        };
        m.charge_scan(ctx)?;
        for w in m.map.as_slice().windows(2) {
            let [a, b] = w else {
                return Err((ScErrorType::Object, ScErrorCode::InternalError).into());
            };
            match <Ctx as Compare<K>>::compare(ctx, &a.0, &b.0)? {
                Ordering::Less => (),
                _ => return Err((ScErrorType::Object, ScErrorCode::InvalidInput).into()),
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
        let _span = tracy_span!("new map");
        if let (_, Some(sz)) = iter.size_hint() {
            if u32::try_from(sz).is_err() {
                Err(MAP_OOB.into())
            } else {
                // It's possible we temporarily go over-budget here before charging, but
                // only by the cost of temporarily allocating twice the size of our largest
                // possible object. In exchange we get to batch all charges associated with
                // the clone into one (when A::IS_SHALLOW==true).
                let map: Vec<(K, V)> = iter.collect();
                map.charge_deep_clone(ctx.as_budget())?;
                // Delegate to from_map here to recheck sort order.
                Self::from_map(map, ctx)
            }
        } else {
            // This is a logic error, we should never get here.
            Err((ScErrorType::Object, ScErrorCode::InternalError).into())
        }
    }

    fn find<Q>(&self, key: &Q, ctx: &Ctx) -> Result<Result<usize, usize>, HostError>
    where
        K: Borrow<Q>,
        Ctx: Compare<Q, Error = HostError>,
    {
        let _span = tracy_span!("map lookup");
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
                    return Err(MAP_OOB.into());
                }
                let init = self.map.iter().take(replace_pos).cloned();
                let fini = self.map.iter().skip(replace_pos.saturating_add(1)).cloned();
                let iter = init.chain([(key, value)]).chain(fini);
                Self::from_exact_iter(iter, ctx)
            }
            Err(insert_pos) => {
                // [0,1,2] insert_pos == 1
                // take(1) + new + skip(1)
                // [0] new [1, 2]
                if self.len() == u32::MAX as usize {
                    return Err(MAP_OOB.into());
                } else {
                    let init = self.map.iter().take(insert_pos).cloned();
                    let fini = self.map.iter().skip(insert_pos).cloned();
                    let iter = init.chain([(key, value)]).chain(fini);
                    Self::from_exact_iter(iter, ctx)
                }
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
                let Some((_, v)) = self.map.get(found) else {
                    return Err((ScErrorType::Object, ScErrorCode::InternalError).into());
                };
                Ok(Some(v))
            }
            _ => Ok(None),
        }
    }

    pub fn get_at_index(&self, index: usize, ctx: &Ctx) -> Result<&(K, V), HostError> {
        self.charge_access(1, ctx)?;
        self.map.get(index).ok_or_else(|| {
            Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds).into()
        })
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
                let fini = self.map.iter().skip(found.saturating_add(1)).cloned();
                let iter = init.chain(fini);
                let new = Self::from_exact_iter(iter, ctx)?;
                let Some((_, res)) = self.map.get(found) else {
                    return Err((ScErrorType::Object, ScErrorCode::InternalError).into());
                };
                Ok(Some((new, res.metered_clone(ctx.as_budget())?)))
            }
            Ok(found) => {
                // No prefix, removing at position 0.
                // If the suffix is empty it's harmless.
                let iter = self.map.iter().skip(1).cloned();
                let new = Self::from_exact_iter(iter, ctx)?;
                let Some((_, res)) = self.map.get(found) else {
                    return Err((ScErrorType::Object, ScErrorCode::InternalError).into());
                };
                Ok(Some((new, res.metered_clone(ctx.as_budget())?)))
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

impl<K, V, Ctx> DeclaredSizeForMetering for MeteredOrdMap<K, V, Ctx>
where
    K: DeclaredSizeForMetering,
    V: DeclaredSizeForMetering,
{
    const DECLARED_SIZE: u64 = <Vec<(K, V)> as DeclaredSizeForMetering>::DECLARED_SIZE;
}

impl<K, V, Ctx> MeteredClone for MeteredOrdMap<K, V, Ctx>
where
    K: MeteredClone,
    V: MeteredClone,
    Ctx: AsBudget,
{
    fn charge_for_substructure(&self, budget: impl AsBudget) -> Result<(), HostError> {
        self.map.charge_for_substructure(budget)
    }
}

impl<K, V> Compare<MeteredOrdMap<K, V, Host>> for Host
where
    K: DeclaredSizeForMetering,
    V: DeclaredSizeForMetering,
    Host: Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredOrdMap<K, V, Host>,
        b: &MeteredOrdMap<K, V, Host>,
    ) -> Result<Ordering, Self::Error> {
        // Here covers the cost of accessing number of map entries. The cost of
        // comparing entries is covered by the `compare` call below.
        self.as_budget().charge(
            ContractCostType::MemCpy,
            Some(
                <(K, V) as DeclaredSizeForMetering>::DECLARED_SIZE
                    .saturating_mul(a.map.len().min(b.map.len()) as u64),
            ),
        )?;
        <Self as Compare<Vec<(K, V)>>>::compare(self, &a.map, &b.map)
    }
}

impl<K, V> Compare<MeteredOrdMap<K, V, Budget>> for Budget
where
    K: DeclaredSizeForMetering,
    V: DeclaredSizeForMetering,
    Budget: Compare<K, Error = HostError> + Compare<V, Error = HostError>,
{
    type Error = HostError;

    fn compare(
        &self,
        a: &MeteredOrdMap<K, V, Budget>,
        b: &MeteredOrdMap<K, V, Budget>,
    ) -> Result<Ordering, Self::Error> {
        // Here covers the cost of accessing number of map entries. The cost of
        // comparing entries is covered by the `compare` call below.
        self.charge(
            ContractCostType::MemCpy,
            Some(
                <(K, V) as DeclaredSizeForMetering>::DECLARED_SIZE
                    .saturating_mul(a.map.len().min(b.map.len()) as u64),
            ),
        )?;
        <Self as Compare<Vec<(K, V)>>>::compare(self, &a.map, &b.map)
    }
}

impl<'a, K, V, Ctx> IntoIterator for &'a MeteredOrdMap<K, V, Ctx> {
    type Item = &'a (K, V);
    type IntoIter = core::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.iter()
    }
}

impl<K, V, Ctx> IntoIterator for MeteredOrdMap<K, V, Ctx> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
