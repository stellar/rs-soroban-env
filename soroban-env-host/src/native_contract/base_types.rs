use crate::budget::CostType;
use crate::host::{Host, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{AccountId, ScObjectType};
use soroban_env_common::{
    CheckedEnv, Compare, ConversionError, ConvertFrom, ConvertInto, EnvBase, Object, RawVal,
};
use std::borrow::Borrow;

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

impl ConvertFrom<Host, Object> for Bytes {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, HostError> {
        let val = *t.borrow();
        if val.is_obj_type(ScObjectType::Bytes) {
            Ok(Bytes {
                host: e.clone(),
                object: val,
            })
        } else {
            Err(e.err_convert_value::<Bytes>(val.to_raw()))
        }
    }
}

impl ConvertFrom<Host, Bytes> for Object {
    fn convert_from(e: &Host, t: impl Borrow<Bytes>) -> Result<Self, HostError> {
        Ok(t.borrow().object)
    }
}

impl ConvertFrom<Host, RawVal> for Bytes {
    fn convert_from(e: &Host, t: impl Borrow<RawVal>) -> Result<Self, HostError> {
        <_ as ConvertFrom<_, Object>>::convert_from(e, t.borrow().try_into()?)
    }
}

impl ConvertFrom<Host, Bytes> for RawVal {
    fn convert_from(e: &Host, t: impl Borrow<Bytes>) -> Result<Self, HostError> {
        Ok(t.borrow().into())
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

impl<const N: usize> Compare<BytesN<N>> for Host {
    type Error = HostError;

    fn compare(&self, a: &BytesN<N>, b: &BytesN<N>) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl<const N: usize> ConvertFrom<Host, Object> for BytesN<N> {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, HostError> {
        let val = *t.borrow();
        let len: u32 = e.bytes_len(val)?.try_into()?;
        if len
            == N.try_into()
                .map_err(|_| e.err_general("bytes buffer overflow"))?
        {
            Ok(Self {
                host: e.clone(),
                object: val,
            })
        } else {
            Err(e.err_convert_value::<Bytes>(val))
        }
    }
}

impl<const N: usize> ConvertFrom<Host, BytesN<N>> for Object {
    fn convert_from(e: &Host, t: impl Borrow<BytesN<N>>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(t.borrow().object)
    }
}

impl<const N: usize> ConvertFrom<Host, RawVal> for BytesN<N> {
    fn convert_from(e: &Host, t: impl Borrow<RawVal>) -> Result<Self, HostError> {
        <_ as ConvertFrom<_, Object>>::convert_from(e, t.borrow().try_into()?)
    }
}

impl<const N: usize> ConvertFrom<Host, BytesN<N>> for RawVal {
    fn convert_from(e: &Host, t: impl Borrow<BytesN<N>>) -> Result<Self, HostError> {
        Ok(t.borrow().object.into())
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

impl Compare<Map> for Host {
    type Error = HostError;

    fn compare(&self, a: &Map, b: &Map) -> Result<Ordering, Self::Error> {
        self.compare(&a.object, &b.object)
    }
}

impl ConvertFrom<Host, Object> for Map {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, HostError> {
        let val = *t.borrow();
        if val.is_obj_type(ScObjectType::Map) {
            Ok(Map {
                host: e.clone(),
                object: val,
            })
        } else {
            Err(e.err_convert_value::<Map>(val))
        }
    }
}

impl ConvertFrom<Host, Object> for Map {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(t.borrow().object)
    }
}

impl ConvertFrom<Host, RawVal> for Map {
    fn convert_from(e: &Host, t: impl Borrow<RawVal>) -> Result<Self, HostError> {
        <_ as ConvertFrom<_, Object>>::convert_from(e, t.borrow().try_into()?)
    }
}

impl ConvertFrom<Host, Map> for RawVal {
    fn convert_from(e: &Host, t: impl Borrow<Map>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(t.borrow().object.into())
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

    pub fn get<K: ConvertInto<Host, RawVal>, V: ConvertFrom<Host, RawVal>>(
        &self,
        k: K,
    ) -> Result<V, HostError> {
        let k_rv = K::convert_from(&self.host, k)?;
        let v_rv = self.host.map_get(self.object, k_rv)?;
        Ok(V::convert_from(&self.host, v_rv)?)
    }

    pub fn set<K: ConvertInto<Host, RawVal>, V: ConvertInto<Host, RawVal>>(
        &mut self,
        k: K,
        v: V,
    ) -> Result<(), HostError> {
        let k_rv = K::convert_from(&self.host, k)?;
        let v_rv = V::convert_from(&self.host, v)?;
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

impl ConvertFrom<Host, Object> for Vec {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, <Host as EnvBase>::Error> {
        let val = *t.borrow();
        if val.is_obj_type(ScObjectType::Vec) {
            Ok(Vec {
                host: e.clone(),
                object: val,
            })
        } else {
            Err(e.err_convert_value::<Vec>(val))
        }
    }
}

impl ConvertFrom<Host, Vec> for Object {
    fn convert_from(e: &Host, t: impl Borrow<Vec>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(t.borrow().object)
    }
}

impl ConvertFrom<Host, RawVal> for Vec {
    fn convert_from(e: &Host, t: impl Borrow<RawVal>) -> Result<Self, <Host as EnvBase>::Error> {
        <_ as ConvertFrom<_, Object>>::convert_from(e, t.borrow().try_into()?)
    }
}

impl ConvertFrom<Host, Vec> for RawVal {
    fn convert_from(e: &Host, t: impl Borrow<Vec>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(t.borrow().object.to_raw())
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

    pub fn get<T: ConvertFrom<Host, RawVal>>(&self, i: u32) -> Result<T, HostError> {
        let rv = self.host.vec_get(self.object, i.into())?;
        Ok(T::convert_from(&self.host, rv)?)
    }

    pub fn len(&self) -> Result<u32, HostError> {
        let rv = self.host.vec_len(self.object)?;
        Ok(u32::convert_from(&self.host, rv)?)
    }

    pub fn push<T: ConvertInto<Host, RawVal>>(&mut self, x: T) -> Result<(), HostError> {
        let rv = x.convert_into(&self.host)?;
        self.push_raw(rv)
    }

    pub fn push_raw(&mut self, x: RawVal) -> Result<(), HostError> {
        self.object = self.host.vec_push_back(self.object, x)?;
        Ok(())
    }
}

impl ConvertFrom<Host, Object> for AccountId {
    fn convert_from(e: &Host, t: impl Borrow<Object>) -> Result<Self, <Host as EnvBase>::Error> {
        e.visit_obj(t.borrow(), |acc: &AccountId| Ok(acc.clone()))
    }
}

impl ConvertFrom<Host, AccountId> for Object {
    fn convert_from(e: &Host, t: impl Borrow<AccountId>) -> Result<Self, <Host as EnvBase>::Error> {
        e.add_host_object(t.borrow().clone())
    }
}

impl ConvertFrom<Host, RawVal> for AccountId {
    fn convert_from(e: &Host, t: impl Borrow<RawVal>) -> Result<Self, <Host as EnvBase>::Error> {
        <_ as ConvertFrom<_, Object>>::convert_from(e, t.borrow().try_into()?)
    }
}

impl ConvertFrom<Host, AccountId> for RawVal {
    fn convert_from(e: &Host, t: impl Borrow<AccountId>) -> Result<Self, <Host as EnvBase>::Error> {
        Ok(Object::convert_from(e, t)?.into())
    }
}
