use crate::budget::{Budget, CostType};
use crate::host::metered_clone::MeteredClone;
use crate::host::{Host, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::ScObjectType;
use soroban_env_common::{
    CheckedEnv, ConversionError, EnvBase, EnvVal, IntoVal, Object, RawVal, TryFromVal, TryIntoVal,
};

#[derive(Clone)]
pub struct BigInt(EnvVal<Host, Object>);

impl TryFromVal<Host, Object> for BigInt {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::BigInt) {
            Ok(BigInt(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for BigInt {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, BigInt> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<BigInt, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for BigInt {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl From<BigInt> for Object {
    fn from(bi: BigInt) -> Self {
        bi.0.val
    }
}

impl core::ops::Add for BigInt {
    type Output = Result<BigInt, HostError>;

    fn add(self, rhs: Self) -> Self::Output {
        let res = self.0.env.bigint_add(self.0.val, rhs.0.val)?;
        BigInt::try_from_val(&self.0.env, res)
    }
}

impl core::ops::Sub for BigInt {
    type Output = Result<BigInt, HostError>;

    fn sub(self, rhs: Self) -> Self::Output {
        let res = self.0.env.bigint_sub(self.0.val, rhs.0.val)?;
        BigInt::try_from_val(&self.0.env, res)
    }
}

impl BigInt {
    pub fn from_u64(env: &Host, x: u64) -> Result<Self, HostError> {
        let res = env.bigint_from_u64(x)?;
        BigInt::try_from_val(env, res)
    }

    pub fn compare(&self, other: &BigInt) -> Result<Ordering, HostError> {
        let i = self
            .0
            .env
            .obj_cmp(self.0.val.to_raw(), other.0.val.to_raw())?;
        Ok(i.cmp(&0))
    }
}

#[derive(Clone)]
pub struct Bytes(EnvVal<Host, Object>);

impl TryFromVal<Host, Object> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::Bytes) {
            Ok(Bytes(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, Bytes> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<Bytes, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for Bytes {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl From<Bytes> for Object {
    fn from(b: Bytes) -> Self {
        b.0.val
    }
}

impl<const N: u32> From<BytesN<N>> for Bytes {
    fn from(b: BytesN<N>) -> Self {
        Self(b.0)
    }
}

impl Bytes {
    pub fn push(&mut self, x: u8) -> Result<(), HostError> {
        let x32: u32 = x.into();
        self.0 = self
            .0
            .env
            .bytes_push(self.0.val, x32.into())?
            .in_env(&self.0.env);
        Ok(())
    }

    pub fn append(&mut self, other: Bytes) -> Result<(), HostError> {
        self.0 = self
            .0
            .env
            .bytes_append(self.0.val, other.0.val)?
            .in_env(&self.0.env);
        Ok(())
    }
}

#[derive(Clone)]
pub struct BytesN<const N: u32>(EnvVal<Host, Object>);

impl<const N: u32> TryFromVal<Host, Object> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(val)?.try_into()?;
        if len == N {
            Ok(Self(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl<const N: u32> Eq for BytesN<N> {}

impl<const N: u32> PartialEq for BytesN<N> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl<const N: u32> PartialOrd for BytesN<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl<const N: u32> Ord for BytesN<N> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<const N: u32> TryFromVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl<const N: u32> TryIntoVal<Host, BytesN<N>> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<BytesN<N>, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl<const N: u32> TryIntoVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl<const N: u32> From<BytesN<N>> for Object {
    fn from(bytes: BytesN<N>) -> Self {
        bytes.0.val
    }
}

impl<const N: u32> BytesN<N> {
    pub fn compare(&self, other: &BytesN<N>) -> Result<Ordering, HostError> {
        let res = self.0.env.obj_cmp(self.0.val.into(), other.0.val.into())?;
        Ok(res.cmp(&0))
    }
}

impl<const N: u32> BytesN<N> {
    /// Copy the bytes in [BytesN] into the given slice.
    ///
    /// The minimum number of bytes are copied to either exhaust [BytesN] or
    /// fill slice.
    #[inline(always)]
    pub fn copy_into_slice(&self, slice: &mut [u8]) -> Result<(), HostError> {
        let env = self.0.env();
        env.bytes_copy_to_slice(self.0.to_object(), RawVal::U32_ZERO, slice)
            .map_err(|status| status.into())
    }

    #[inline(always)]
    pub fn from_slice(env: &Host, items: &[u8]) -> Result<BytesN<N>, HostError> {
        Ok(BytesN(env.bytes_new_from_slice(items)?.in_env(env)))
    }
}

impl<const N: u32> MeteredClone for BytesN<N> {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, N.into())?;
        Ok(self.clone())
    }
}

#[derive(Clone)]
pub struct Map(EnvVal<Host, Object>);

impl TryFromVal<Host, Object> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::Map) {
            Ok(Map(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, Map> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<Map, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for Map {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl From<Map> for Object {
    fn from(map: Map) -> Self {
        map.0.val
    }
}

impl Map {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let map = env.map_new()?;
        Ok(Self(map.in_env(env)))
    }

    pub fn get<K: TryIntoVal<Host, RawVal>, V: TryFromVal<Host, RawVal>>(
        &self,
        k: K,
    ) -> Result<V, HostError>
    where
        HostError: From<<K as TryIntoVal<Host, RawVal>>::Error>,
        HostError: From<<V as TryFromVal<Host, RawVal>>::Error>,
    {
        let k_rv = k.try_into_val(&self.0.env)?;
        let v_rv = self.0.env.map_get(self.0.val, k_rv)?;
        Ok(V::try_from_val(&self.0.env, v_rv)?)
    }

    pub fn set<K: TryIntoVal<Host, RawVal>, V: TryIntoVal<Host, RawVal>>(
        &mut self,
        k: K,
        v: V,
    ) -> Result<(), HostError>
    where
        HostError: From<<K as TryIntoVal<Host, RawVal>>::Error>,
        HostError: From<<V as TryIntoVal<Host, RawVal>>::Error>,
    {
        let k_rv = k.try_into_val(&self.0.env)?;
        let v_rv = v.try_into_val(&self.0.env)?;
        self.0.val = self.0.env.map_put(self.0.val, k_rv, v_rv)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct Vec(EnvVal<Host, Object>);

impl TryFromVal<Host, Object> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::Vec) {
            Ok(Vec(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, Vec> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<Vec, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for Vec {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl From<Vec> for Object {
    fn from(vec: Vec) -> Self {
        vec.0.val
    }
}

impl Vec {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let vec = env.vec_new(().into())?;
        Ok(Self(vec.in_env(env)))
    }

    pub fn get<T: TryFromVal<Host, RawVal>>(&self, i: u32) -> Result<T, HostError>
    where
        HostError: From<<T as TryFromVal<Host, RawVal>>::Error>,
    {
        let rv = self.0.env.vec_get(self.0.val, i.into())?;
        Ok(T::try_from_val(&self.0.env, rv)?)
    }

    pub fn len(&self) -> Result<u32, HostError> {
        let rv = self.0.env.vec_len(self.0.val)?;
        Ok(u32::try_from_val(&self.0.env, rv)?)
    }

    pub fn push<T: TryIntoVal<Host, RawVal>>(&mut self, x: T) -> Result<(), HostError>
    where
        HostError: From<<T as TryIntoVal<Host, RawVal>>::Error>,
    {
        let rv = x.try_into_val(&self.0.env)?;
        self.0.val = self.0.env.vec_push_back(self.0.val, rv)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct AccountId(EnvVal<Host, Object>);

impl Eq for AccountId {}

impl PartialEq for AccountId {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for AccountId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for AccountId {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl TryFromVal<Host, Object> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::AccountId) {
            Ok(AccountId(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryIntoVal<Host, AccountId> for Object {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<AccountId, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, self)
    }
}

impl TryFromVal<Host, RawVal> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, AccountId> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<AccountId, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self)
    }
}

impl TryIntoVal<Host, RawVal> for AccountId {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl IntoVal<Host, Object> for AccountId {
    fn into_val(self, _env: &Host) -> Object {
        self.to_object()
    }
}

impl AccountId {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let map = env.map_new()?;
        Ok(Self(map.in_env(env)))
    }

    pub fn to_object(&self) -> Object {
        self.0.to_object()
    }
}

impl MeteredClone for AccountId {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BytesClone, 32)?;
        Ok(self.clone())
    }
}
