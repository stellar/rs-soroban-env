use crate::budget::CostType;
use crate::host::{Host, HostError};

use core::cmp::Ordering;
use soroban_env_common::xdr::{AccountId, ScAddress};
use soroban_env_common::{
    AddressObject, BytesObject, Compare, ConversionError, Env, EnvBase, MapObject, RawVal,
    TryFromVal, VecObject,
};

#[derive(Clone)]
pub struct Bytes {
    host: Host,
    object: BytesObject,
}

impl Compare<Bytes> for Host {
    type Error = HostError;

    fn compare(&self, a: &Bytes, b: &Bytes) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_raw(), &b.object.to_raw())
    }
}

impl TryFromVal<Host, BytesObject> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &BytesObject) -> Result<Self, Self::Error> {
        Ok(Bytes {
            host: env.clone(),
            object: *val,
        })
    }
}

impl TryFromVal<Host, RawVal> for Bytes {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: BytesObject = val.try_into()?;
        Bytes::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Bytes> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Bytes) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Bytes> for BytesObject {
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
        let mut res = std::vec::Vec::<u8>::new();
        let size: u32 = self.host.bytes_len(self.object).unwrap().into();
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
    object: BytesObject,
}

impl<const N: usize> TryFromVal<Host, BytesObject> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &BytesObject) -> Result<Self, Self::Error> {
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
        self.compare(&a.object.to_raw(), &b.object.to_raw())
    }
}

impl<const N: usize> TryFromVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: BytesObject = val.try_into()?;
        <BytesN<N>>::try_from_val(env, &obj)
    }
}

impl<const N: usize> TryFromVal<Host, BytesN<N>> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &BytesN<N>) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl<const N: usize> From<BytesN<N>> for RawVal {
    fn from(val: BytesN<N>) -> Self {
        val.object.into()
    }
}

impl<const N: usize> From<BytesN<N>> for BytesObject {
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
    object: MapObject,
}

impl TryFromVal<Host, MapObject> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &MapObject) -> Result<Self, Self::Error> {
        let val = *val;
        Ok(Map {
            host: env.clone(),
            object: val,
        })
    }
}

impl Compare<Map> for Host {
    type Error = HostError;

    fn compare(&self, a: &Map, b: &Map) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_raw(), &b.object.to_raw())
    }
}

impl TryFromVal<Host, RawVal> for Map {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: MapObject = val.try_into()?;
        Map::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Map> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Map) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Map> for MapObject {
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
    object: VecObject,
}

impl Compare<Vec> for Host {
    type Error = HostError;

    fn compare(&self, a: &Vec, b: &Vec) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.as_raw(), &b.object.as_raw())
    }
}

impl TryFromVal<Host, VecObject> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &VecObject) -> Result<Self, Self::Error> {
        let val = *val;
        Ok(Vec {
            host: env.clone(),
            object: val,
        })
    }
}

impl TryFromVal<Host, RawVal> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: VecObject = val.try_into()?;
        Vec::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Vec> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Vec) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Vec> for RawVal {
    fn from(val: Vec) -> Self {
        val.object.into()
    }
}

impl From<Vec> for VecObject {
    fn from(vec: Vec) -> Self {
        vec.object
    }
}

impl TryFromVal<Host, std::vec::Vec<RawVal>> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, vals: &std::vec::Vec<RawVal>) -> Result<Self, Self::Error> {
        let mut v = Vec::new(env)?;
        for rv in vals {
            v.push_raw(*rv)?;
        }
        Ok(v)
    }
}

impl TryFromVal<Host, &std::vec::Vec<RawVal>> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, vals: &&std::vec::Vec<RawVal>) -> Result<Self, Self::Error> {
        Vec::try_from_val(env, *vals)
    }
}

impl Vec {
    pub fn new(env: &Host) -> Result<Self, HostError> {
        let vec: VecObject = env.vec_new(().into())?;
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
        Ok(self.host.vec_len(self.object)?.into())
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

#[derive(Clone)]
pub struct Address {
    host: Host,
    object: AddressObject,
}

impl TryFromVal<Host, AddressObject> for Address {
    type Error = HostError;

    fn try_from_val(env: &Host, obj: &AddressObject) -> Result<Self, Self::Error> {
        Ok(Address {
            host: env.clone(),
            object: *obj,
        })
    }
}

impl TryFromVal<Host, RawVal> for Address {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: AddressObject = val.try_into()?;
        Address::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Address> for RawVal {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Address) -> Result<RawVal, Self::Error> {
        Ok(val.object.into())
    }
}

impl Compare<Address> for Host {
    type Error = HostError;

    fn compare(&self, a: &Address, b: &Address) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_raw(), &b.object.to_raw())
    }
}

impl From<Address> for AddressObject {
    fn from(a: Address) -> Self {
        a.object
    }
}

impl Address {
    pub(crate) fn from_account(env: &Host, account_id: &AccountId) -> Result<Self, HostError> {
        Address::try_from_val(
            env,
            &env.add_host_object(ScAddress::Account(account_id.clone()))?,
        )
    }

    pub(crate) fn to_sc_address(&self) -> Result<ScAddress, HostError> {
        self.host
            .visit_obj(self.object, |addr: &ScAddress| Ok(addr.clone()))
    }

    pub(crate) fn require_auth(&self) -> Result<(), HostError> {
        Ok(self.host.require_auth(self.object)?.try_into()?)
    }
}
