use crate::host::Host;
use core::cmp::Ordering;
use soroban_env_common::xdr::ScObjectType;
use soroban_env_common::{CheckedEnv, EnvVal, Object, RawVal, TryIntoVal};

#[derive(Clone)]
pub struct BigInt(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for BigInt {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::BigInt) {
            Ok(BigInt(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for BigInt {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for BigInt {
    type Error = ();

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
    type Output = Result<BigInt, ()>;

    fn add(self, rhs: Self) -> Self::Output {
        let res = self
            .0
            .env
            .bigint_add(self.0.val, rhs.0.val)
            .map_err(|_| ())?;
        res.in_env(&self.0.env).try_into()
    }
}

impl core::ops::Sub for BigInt {
    type Output = Result<BigInt, ()>;

    fn sub(self, rhs: Self) -> Self::Output {
        let res = self
            .0
            .env
            .bigint_sub(self.0.val, rhs.0.val)
            .map_err(|_| ())?;
        res.in_env(&self.0.env).try_into()
    }
}

impl BigInt {
    pub fn from_u64(env: &Host, x: u64) -> Result<Self, ()> {
        let res = env.bigint_from_u64(x).map_err(|_| ())?;
        res.in_env(env).try_into()
    }

    pub fn compare(&self, other: &BigInt) -> Result<Ordering, ()> {
        let res = self
            .0
            .env
            .bigint_cmp(self.0.val, other.0.val)
            .map_err(|_| ())?;
        let i = i32::try_from(res).map_err(|_| ())?;
        Ok(i.cmp(&0))
    }
}

#[derive(Clone)]
pub struct Bytes(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for Bytes {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Binary) {
            Ok(Bytes(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Bytes {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Bytes {
    type Error = ();

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
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        let bin = value.val;
        let len: u32 = value
            .env
            .binary_len(bin)
            .map_err(|_| ())?
            .try_into()
            .map_err(|_| ())?;
        if len != N {
            Ok(Self(bin.in_env(&value.env)))
        } else {
            Err(())
        }
    }
}

impl<const N: u32> TryFrom<EnvVal<Host, RawVal>> for BytesN<N> {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl<const N: u32> TryIntoVal<Host, RawVal> for BytesN<N> {
    type Error = ();

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
    pub fn compare(&self, other: &BytesN<N>) -> Result<Ordering, ()> {
        let res = self
            .0
            .env
            .obj_cmp(self.0.val.into(), other.0.val.into())
            .map_err(|_| ())?;
        Ok(res.cmp(&0))
    }
}

#[derive(Clone)]
pub struct Map(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for Map {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Map) {
            Ok(Map(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Map {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Map {
    type Error = ();

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
    pub fn new(env: &Host) -> Result<Self, ()> {
        let map = env.map_new().map_err(|_| ())?;
        Ok(Self(map.in_env(env)))
    }

    pub fn get<K: TryIntoVal<Host, RawVal>, V: TryFrom<EnvVal<Host, RawVal>>>(
        &self,
        k: K,
    ) -> Result<V, ()> {
        let k_rv = k.try_into_val(&self.0.env).map_err(|_| ())?;
        let v_rv = self.0.env.map_get(self.0.val, k_rv).map_err(|_| ())?;
        v_rv.in_env(&self.0.env).try_into().map_err(|_| ())
    }

    pub fn set<K: TryIntoVal<Host, RawVal>, V: TryIntoVal<Host, RawVal>>(
        &mut self,
        k: K,
        v: V,
    ) -> Result<(), ()> {
        let k_rv = k.try_into_val(&self.0.env).map_err(|_| ())?;
        let v_rv = v.try_into_val(&self.0.env).map_err(|_| ())?;
        self.0.val = self.0.env.map_put(self.0.val, k_rv, v_rv).map_err(|_| ())?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct Vec(EnvVal<Host, Object>);

impl TryFrom<EnvVal<Host, Object>> for Vec {
    type Error = ();

    fn try_from(value: EnvVal<Host, Object>) -> Result<Self, Self::Error> {
        if value.val.is_obj_type(ScObjectType::Vec) {
            Ok(Vec(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<EnvVal<Host, RawVal>> for Vec {
    type Error = ();

    fn try_from(value: EnvVal<Host, RawVal>) -> Result<Self, Self::Error> {
        let env_obj: EnvVal<Host, Object> = value.try_into().map_err(|_| ())?;
        env_obj.try_into()
    }
}

impl TryIntoVal<Host, RawVal> for Vec {
    type Error = ();

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
    pub fn new(env: &Host) -> Result<Self, ()> {
        let vec = env.vec_new(().into()).map_err(|_| ())?;
        Ok(Self(vec.in_env(env)))
    }

    pub fn get<T: TryFrom<EnvVal<Host, RawVal>>>(&self, i: u32) -> Result<T, ()> {
        let rv = self.0.env.vec_get(self.0.val, i.into()).map_err(|_| ())?;
        rv.in_env(&self.0.env).try_into().map_err(|_| ())
    }

    pub fn len(&self) -> Result<u32, ()> {
        let rv = self.0.env.vec_len(self.0.val).map_err(|_| ())?;
        rv.in_env(&self.0.env).try_into().map_err(|_| ())
    }

    pub fn push<T: TryIntoVal<Host, RawVal>>(&mut self, x: T) -> Result<(), ()> {
        let rv = x.try_into_val(&self.0.env).map_err(|_| ())?;
        self.0.val = self.0.env.vec_push(self.0.val, rv).map_err(|_| ())?;
        Ok(())
    }
}
