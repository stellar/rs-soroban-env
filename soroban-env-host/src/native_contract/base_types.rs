use crate::budget::CostType;
use crate::host::{Host, HostError};

use core::cmp::Ordering;
use soroban_env_common::xdr::{AccountId, ScObjectType};
use soroban_env_common::{
    CheckedEnv, Compare, ConversionError, EnvBase, Object, RawVal, TryFromVal,
};

#[derive(Clone)]
pub struct Bytes {
    host: Host,
    object: Object,
}

impl Compare<Bytes> for Host {
    type Error = HostError;

    fn compare(&self, a: &Bytes, b: &Bytes) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl TryFromVal<Host, Object> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Object) -> Result<Self, Self::Error> {
        let val = *val;
        if val.is_obj_type(ScObjectType::Bytes) {
            Ok(Bytes {
                host: env.clone(),
                object: val,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: Object = val.try_into()?;
        Bytes::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Bytes> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Bytes) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Bytes> for Object {
    fn from(b: Bytes) -> Self {
        b.object
    }
}

impl<const N: usize> From<BytesN<N>> for Bytes {
    fn from(b: BytesN<N>) -> Self {
        Self {
            host: b.host.clone(),
            object: b.object,
        }
    }
}

impl Bytes {
    pub fn push(&mut self, x: u8) -> Result<(), HostError> {
        let x32: u32 = x.into();
        self.object = self.host.bytes_push(self.object, x32.into())?;
        Ok(())
    }

    pub fn append(&mut self, other: Bytes) -> Result<(), HostError> {
        self.object = self.host.bytes_append(self.object, other.object)?;
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn to_vec(&self) -> std::vec::Vec<u8> {
        use soroban_env_common::RawValConvertible;
        let mut res = std::vec::Vec::<u8>::new();
        let size = unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(
                self.host.bytes_len(self.object).unwrap(),
            )
        };
        res.resize(size as usize, 0);
        self.host
            .bytes_copy_to_slice(self.object, RawVal::U32_ZERO, &mut res[..])
            .unwrap();
        res
    }
}

#[derive(Clone)]
pub struct BytesN<const N: usize> {
    host: Host,
    object: Object,
}

impl<const N: usize> TryFromVal<Host, Object> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Object) -> Result<Self, Self::Error> {
        let val = *val;
        let len: u32 = env.bytes_len(val)?.try_into()?;
        if len
            == N.try_into()
                .map_err(|_| env.err_general("bytes buffer overflow"))?
        {
            Ok(Self {
                host: env.clone(),
                object: val,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl<const N: usize> Compare<BytesN<N>> for Host {
    type Error = HostError;

    fn compare(&self, a: &BytesN<N>, b: &BytesN<N>) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl<const N: usize> TryFromVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: Object = val.try_into()?;
        <BytesN<N>>::try_from_val(env, &obj)
    }
}

impl<const N: usize> TryFromVal<Host, BytesN<N>> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &BytesN<N>) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl<const N: usize> From<BytesN<N>> for Object {
    fn from(bytes: BytesN<N>) -> Self {
        bytes.object
    }
}

impl<const N: usize> BytesN<N> {
    pub fn compare(&self, other: &BytesN<N>) -> Result<Ordering, HostError> {
        let res = self.host.obj_cmp(self.object.into(), other.object.into())?;
        Ok(res.cmp(&0))
    }
}

impl<const N: usize> BytesN<N> {
    #[inline(always)]
    pub fn to_array(&self) -> Result<[u8; N], HostError> {
        let mut slice = [0_u8; N];
        self.host.charge_budget(CostType::BytesClone, N as u64)?;
        self.host
            .bytes_copy_to_slice(self.object, RawVal::U32_ZERO, &mut slice)?;
        Ok(slice)
    }

    #[inline(always)]
    pub fn from_slice(env: &Host, items: &[u8]) -> Result<BytesN<N>, HostError> {
        Ok(BytesN {
            host: env.clone(),
            object: env.bytes_new_from_slice(items)?,
        })
    }

    #[cfg(test)]
    pub(crate) fn to_vec(&self) -> std::vec::Vec<u8> {
        Bytes::from(self.clone()).to_vec()
    }
}

#[derive(Clone)]
pub struct Map {
    host: Host,
    object: Object,
}

impl TryFromVal<Host, Object> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Object) -> Result<Self, Self::Error> {
        let val = *val;
        if val.is_obj_type(ScObjectType::Map) {
            Ok(Map {
                host: env.clone(),
                object: val,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl Compare<Map> for Host {
    type Error = HostError;

    fn compare(&self, a: &Map, b: &Map) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl TryFromVal<Host, RawVal> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: Object = val.try_into()?;
        Map::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Map> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Map) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Map> for Object {
    fn from(map: Map) -> Self {
        map.object
    }
}

impl Map {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let map = env.map_new()?;
        Ok(Self {
            host: env.clone(),
            object: map,
        })
    }

    pub fn get<K, V>(&self, k: &K) -> Result<V, HostError>
    where
        RawVal: TryFromVal<Host, K>,
        V: TryFromVal<Host, RawVal>,
        HostError: From<<RawVal as TryFromVal<Host, K>>::Error>,
        HostError: From<<V as TryFromVal<Host, RawVal>>::Error>,
    {
        let k_rv = RawVal::try_from_val(&self.host, k)?;
        let v_rv = self.host.map_get(self.object, k_rv)?;
        Ok(V::try_from_val(&self.host, &v_rv)?)
    }

    pub fn set<K, V>(&mut self, k: &K, v: &V) -> Result<(), HostError>
    where
        RawVal: TryFromVal<Host, K>,
        RawVal: TryFromVal<Host, V>,
        HostError: From<<RawVal as TryFromVal<Host, K>>::Error>,
        HostError: From<<RawVal as TryFromVal<Host, V>>::Error>,
    {
        let k_rv = RawVal::try_from_val(&self.host, k)?;
        let v_rv = RawVal::try_from_val(&self.host, v)?;
        self.object = self.host.map_put(self.object, k_rv, v_rv)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct Vec {
    host: Host,
    object: Object,
}

impl Compare<Vec> for Host {
    type Error = HostError;

    fn compare(&self, a: &Vec, b: &Vec) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl TryFromVal<Host, Object> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Object) -> Result<Self, Self::Error> {
        let val = *val;
        if val.is_obj_type(ScObjectType::Vec) {
            Ok(Vec {
                host: env.clone(),
                object: val,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: Object = val.try_into()?;
        Vec::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Vec> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Vec) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Vec> for Object {
    fn from(vec: Vec) -> Self {
        vec.object
    }
}

impl Vec {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let vec = env.vec_new(().into())?;
        Ok(Self {
            host: env.clone(),
            object: vec,
        })
    }

    pub fn get<T: TryFromVal<Host, RawVal>>(&self, i: u32) -> Result<T, HostError>
    where
        HostError: From<<T as TryFromVal<Host, RawVal>>::Error>,
    {
        let rv = self.host.vec_get(self.object, i.into())?;
        Ok(T::try_from_val(&self.host, &rv)?)
    }

    pub fn len(&self) -> Result<u32, HostError> {
        let rv = self.host.vec_len(self.object)?;
        Ok(u32::try_from_val(&self.host, &rv)?)
    }

    pub fn push<T>(&mut self, x: &T) -> Result<(), HostError>
    where
        RawVal: TryFromVal<Host, T>,
        HostError: From<<RawVal as TryFromVal<Host, T>>::Error>,
    {
        let rv = RawVal::try_from_val(&self.host, x)?;
        self.push_raw(rv)
    }

    pub fn push_raw(&mut self, x: RawVal) -> Result<(), HostError> {
        self.object = self.host.vec_push_back(self.object, x)?;
        Ok(())
    }
}

impl TryFromVal<Host, Object> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Object) -> Result<Self, Self::Error> {
        let val = *val;
        env.visit_obj(val, |acc: &AccountId| Ok(acc.clone()))
    }
}

impl TryFromVal<Host, RawVal> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: Object = val.try_into()?;
        AccountId::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, AccountId> for RawVal {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &AccountId) -> Result<RawVal, Self::Error> {
        Ok(env.add_host_object(val.clone())?.to_raw())
    }
}
