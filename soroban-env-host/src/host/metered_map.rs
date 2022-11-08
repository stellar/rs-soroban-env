use super::{MeteredClone, MeteredCmp};
use crate::{
    budget::{Budget, CostType},
    HostError,
};
use im_rc::ordmap::{ConsumingIter, Iter, Keys, Values};
use im_rc::OrdMap;
use std::{
    borrow::Borrow,
    cmp::{min, Ordering},
    rc::Rc,
};

pub struct MeteredOrdMap<K, V> {
    pub(crate) budget: Budget,
    pub(crate) map: OrdMap<K, V>,
}

impl<K, V> MeteredOrdMap<K, V> {
    fn charge_new(&self) -> Result<(), HostError> {
        self.budget.charge(CostType::ImMapNew, 1)
    }

    fn charge_mut_access(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::ImMapMutEntry, x)
    }

    fn charge_immut_access(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::ImMapImmutEntry, x)
    }

    fn charge_cmp(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::ImMapCmp, x)
    }
}

impl<K, V> Default for MeteredOrdMap<K, V> {
    fn default() -> Self {
        Self {
            budget: Default::default(),
            map: Default::default(),
        }
    }
}

fn check_size_is_small<T>(name: &str) {
    let sz = core::mem::size_of::<T>();
    if sz > 64 {
        panic!("type '{}' is too big: {}", name, sz);
    }
}

impl<K, V> MeteredOrdMap<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    pub fn new(budget: Budget) -> Result<Self, HostError> {
        check_size_is_small::<K>("key");
        check_size_is_small::<V>("val");
        budget.charge(CostType::ImMapNew, 1)?;
        Ok(MeteredOrdMap {
            budget,
            map: OrdMap::new(),
        })
    }

    pub fn from_map(budget: Budget, map: OrdMap<K, V>) -> Result<Self, HostError> {
        check_size_is_small::<K>("key");
        check_size_is_small::<V>("val");
        budget.charge(CostType::ImMapNew, 1)?;
        Ok(MeteredOrdMap { budget, map })
    }

    // Time: O(log n)
    pub fn insert(&mut self, key: K, value: V) -> Result<Option<V>, HostError> {
        self.charge_mut_access(self.map.len() as u64)?;
        Ok(self.map.insert(key, value))
    }

    // Time: O(log n)
    pub fn get<BK>(&self, key: &BK) -> Result<Option<&V>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.get(key))
    }

    // Time: O(log n)
    pub fn get_mut<BK>(&mut self, key: &BK) -> Result<Option<&mut V>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_mut_access(self.map.len() as u64)?;
        Ok(self.map.get_mut(key))
    }

    // Time: O(log n)
    pub fn remove<BK>(&mut self, k: &BK) -> Result<Option<V>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_mut_access(self.map.len() as u64)?;
        Ok(self.map.remove(k))
    }

    // Time: O(1). Free of charge.
    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    // Time: O(log n)
    pub fn contains_key<BK>(&self, k: &BK) -> Result<bool, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.contains_key(k))
    }

    /// Time: O(log n)
    pub fn extract<BK>(&self, k: &BK) -> Result<Option<(V, Self)>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_immut_access(self.map.len() as u64)?;
        if let Some((v, m)) = self.map.extract(k) {
            Ok(Some((v, Self::from_map(self.budget.clone(), m)?)))
        } else {
            Ok(None)
        }
    }

    pub fn get_prev<BK>(&self, key: &BK) -> Result<Option<(&K, &V)>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.get_prev(key))
    }

    pub fn get_next<BK>(&self, key: &BK) -> Result<Option<(&K, &V)>, HostError>
    where
        BK: Ord + ?Sized,
        K: Borrow<BK>,
    {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.get_next(key))
    }

    /// Time: O(log n)
    pub fn get_min(&self) -> Result<Option<&(K, V)>, HostError> {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.get_min())
    }

    /// Time: O(log n)
    pub fn get_max(&self) -> Result<Option<&(K, V)>, HostError> {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.get_max())
    }

    pub fn keys(&self) -> Result<Keys<'_, K, V>, HostError> {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.keys())
    }

    pub fn values(&self) -> Result<Values<'_, K, V>, HostError> {
        self.charge_immut_access(self.map.len() as u64)?;
        Ok(self.map.values())
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.map.iter()
    }
}

impl<K, V> Clone for MeteredOrdMap<K, V> {
    fn clone(&self) -> Self {
        Self {
            budget: self.budget.clone(),
            map: self.map.clone(),
        }
    }
}

impl<K, V> MeteredClone for MeteredOrdMap<K, V> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        assert!(Rc::ptr_eq(&self.budget.0, &budget.0));
        self.charge_new()?;
        Ok(self.clone())
    }
}

impl<K, V> PartialEq for MeteredOrdMap<K, V>
where
    K: Ord + PartialEq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.budget.0, &other.budget.0) && self.map == other.map
    }
}
impl<K: Ord + Eq, V: Eq> Eq for MeteredOrdMap<K, V> {}

impl<K, V> PartialOrd for MeteredOrdMap<K, V>
where
    K: Ord,
    V: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.map.partial_cmp(&other.map)
    }
}

impl<K, V> Ord for MeteredOrdMap<K, V>
where
    K: Ord,
    V: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.map.cmp(&other.map)
    }
}

impl<K, V> MeteredCmp for MeteredOrdMap<K, V>
where
    K: Ord + Clone,
    V: Ord + Clone,
{
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.charge_cmp(min(self.len(), other.len()) as u64)?;
        Ok(self.map.cmp(&other.map))
    }
}

impl<'a, K, V> IntoIterator for &'a MeteredOrdMap<K, V>
where
    K: Ord,
{
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<K, V> IntoIterator for MeteredOrdMap<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    type Item = (K, V);
    type IntoIter = ConsumingIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
