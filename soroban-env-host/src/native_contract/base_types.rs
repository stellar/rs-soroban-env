use crate::host::{Host, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::ScObjectType;
use soroban_env_common::{CheckedEnv, ConversionError, EnvVal, Object, RawVal, TryIntoVal};

#[derive(Clone)]
pub struct BigInt(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for BigInt {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::BigInt) {
            Ok(BigInt(value))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for BigInt {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into()?;
        env_obj.try_into()
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
        res.in_env(&self.0.env).try_into()
    }
}

impl core::ops::Sub for BigInt {
    type Output = Result<BigInt, HostError>;

    fn sub(self, rhs: Self) -> Self::Output {
        let res = self.0.env.bigint_sub(self.0.val, rhs.0.val)?;
        res.in_env(&self.0.env).try_into()
    }
}

impl BigInt {
    pub fn from_u64(env: &Host, x: u64) -> Result<Self, HostError> {
        let res = env.bigint_from_u64(x)?;
        res.in_env(env).try_into()
    }

    pub fn compare(&self, other: &BigInt) -> Result<Ordering, HostError> {
        let res = self.0.env.bigint_cmp(self.0.val, other.0.val)?;
        let i = i32::try_from(res)?;
        Ok(i.cmp(&0))
    }
}

#[derive(Clone)]
pub struct Bytes(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for Bytes {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Bytes) {
            Ok(Bytes(value))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Bytes {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into()?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Bytes {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl From<Bytes> for Object {
    fn from(vec: Bytes) -> Self {
        vec.0.val
    }
}

#[derive(Clone)]
pub struct BytesN<const N: u32>(EnvVal<Host, Object>);

impl<const N: u32> TryFrom<EnvVal<Host, Object>> for BytesN<N> {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        let bin = value.val;
        let len: u32 = value.env.binary_len(bin)?.try_into()?;
        if len == N {
            Ok(Self(bin.in_env(&value.env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl<const N: u32> TryFrom<EnvVal<Host, RawVal>> for BytesN<N> {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into()?;
        env_obj.try_into()
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

#[derive(Clone)]
pub struct Map(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for Map {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Map) {
            Ok(Map(value))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Map {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into()?;
        env_obj.try_into()
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

    pub fn get<K: TryIntoVal<Host, RawVal>, V: TryFrom<EnvVal<Host, RawVal>>>(
        &self,
        k: K,
    ) -> Result<V, HostError>
    where
        HostError: From<<K as TryIntoVal<Host, RawVal>>::Error>,
        HostError: From<<V as TryFrom<EnvVal<Host, RawVal>>>::Error>,
    {
        let k_rv = k.try_into_val(&self.0.env)?;
        let v_rv = self.0.env.map_get(self.0.val, k_rv)?;
        Ok(v_rv.in_env(&self.0.env).try_into()?)
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

impl TryFrom<EnvVal<Host, Object>> for Vec {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Vec) {
            Ok(Vec(value))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Vec {
    type Error = HostError;

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into()?;
        env_obj.try_into()
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

    pub fn get<T: TryFrom<EnvVal<Host, RawVal>>>(&self, i: u32) -> Result<T, HostError>
    where
        HostError: From<<T as TryFrom<EnvVal<Host, RawVal>>>::Error>,
    {
        let rv = self.0.env.vec_get(self.0.val, i.into())?;
        Ok(rv.in_env(&self.0.env).try_into()?)
    }

    pub fn len(&self) -> Result<u32, HostError> {
        let rv = self.0.env.vec_len(self.0.val)?;
        Ok(rv.in_env(&self.0.env).try_into()?)
    }

    pub fn push<T: TryIntoVal<Host, RawVal>>(&mut self, x: T) -> Result<(), HostError>
    where
        HostError: From<<T as TryIntoVal<Host, RawVal>>::Error>,
    {
        let rv = x.try_into_val(&self.0.env)?;
        self.0.val = self.0.env.vec_push(self.0.val, rv)?;
        Ok(())
    }
}
