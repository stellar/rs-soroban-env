use crate::budget::CostType;
use crate::host::{Host, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::ScObjectType;
use soroban_env_common::{CheckedEnv, Compare, ConversionError, Convert, EnvBase, Object, RawVal};

#[derive(Clone)]
pub struct Bytes {
    host: Host,
    object: Object,
}

impl From<Bytes> for Object {
    fn from(b: Bytes) -> Self {
        b.object
    }
}

impl Convert<Bytes, Object> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Bytes) -> Result<Object, Self::Error> {
        Ok(f.object)
    }
}

impl Convert<Object, Bytes> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Object) -> Result<Bytes, Self::Error> {
        if f.is_obj_type(ScObjectType::Bytes) {
            Ok(Bytes {
                host: self.clone(),
                object: *f,
            })
        } else {
            Err(self.err_conversion_general("object is not bytes"))
        }
    }
}

impl Convert<Bytes, RawVal> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Bytes) -> Result<RawVal, Self::Error> {
        Ok(f.object.into())
    }
}

impl Convert<RawVal, Bytes> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &RawVal) -> Result<Bytes, Self::Error> {
        let obj: Object = f.try_into()?;
        self.convert(obj)
    }
}

impl<const N: usize> From<BytesN<N>> for Bytes {
    fn from(b: BytesN<N>) -> Self {
        Self {
            host: b.host,
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

impl<const N: usize> BytesN<N> {
    pub fn compare(&self, other: &BytesN<N>) -> Result<Ordering, HostError> {
        let res = self.host.obj_cmp(self.object.into(), other.object.into())?;
        Ok(res.cmp(&0))
    }
}

impl<const N: usize> Convert<Object, BytesN<N>> for Host {
    type Error = HostError;

    fn convert_ref(&self, object: &Object) -> Result<BytesN<N>, Self::Error> {
        let len: u32 = self.bytes_len(*object)?.try_into()?;
        if len
            == N.try_into()
                .map_err(|_| self.err_general("bytes buffer overflow"))?
        {
            Ok(BytesN {
                host: self.clone(),
                object: *object,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}
impl<const N: usize> Convert<BytesN<N>, Object> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &BytesN<N>) -> Result<Object, Self::Error> {
        Ok(f.object)
    }
}

impl<const N: usize> Convert<RawVal, BytesN<N>> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &RawVal) -> Result<BytesN<N>, Self::Error> {
        let obj: Object = f.try_into()?;
        self.convert(obj)
    }
}

impl<const N: usize> Convert<BytesN<N>, RawVal> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &BytesN<N>) -> Result<RawVal, Self::Error> {
        Ok(f.object.to_raw())
    }
}

impl<const N: usize> From<BytesN<N>> for Object {
    fn from(bytes: BytesN<N>) -> Self {
        bytes.object
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
    pub fn from_slice(host: &Host, items: &[u8]) -> Result<BytesN<N>, HostError> {
        Ok(BytesN {
            host: host.clone(),
            object: host.bytes_new_from_slice(items)?,
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

impl Convert<Object, Map> for Host {
    type Error = HostError;

    fn convert_ref(&self, object: &Object) -> Result<Map, Self::Error> {
        if object.is_obj_type(ScObjectType::Map) {
            Ok(Map {
                host: self.clone(),
                object: *object,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl Convert<RawVal, Map> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &RawVal) -> Result<Map, Self::Error> {
        let obj: Object = f.try_into()?;
        self.convert(obj)
    }
}

impl Convert<Map, Object> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Map) -> Result<Object, Self::Error> {
        Ok(f.object)
    }
}

impl Convert<Map, RawVal> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Map) -> Result<RawVal, Self::Error> {
        Ok(f.object.into())
    }
}

impl From<Map> for Object {
    fn from(bytes: Map) -> Self {
        bytes.object
    }
}

impl Map {
    pub fn new(host: &Host) -> Result<Self, HostError> {
        let object = host.map_new()?;
        Ok(Self {
            host: host.clone(),
            object,
        })
    }

    pub fn get<K, V>(&self, k: &K) -> Result<V, HostError>
    where
        Host: Convert<K, RawVal> + Convert<RawVal, V>,
        HostError: From<<Host as Convert<K, RawVal>>::Error>,
        HostError: From<<Host as Convert<RawVal, V>>::Error>,
    {
        let k_rv: RawVal = self.host.convert_ref(k)?;
        let v_rv: RawVal = self.host.map_get(self.object, k_rv)?;
        Ok(self.host.convert_ref(&v_rv)?)
    }

    pub fn set<K, V>(&mut self, k: &K, v: &V) -> Result<(), HostError>
    where
        Host: Convert<K, RawVal> + Convert<V, RawVal>,
        HostError: From<<Host as Convert<K, RawVal>>::Error>,
        HostError: From<<Host as Convert<V, RawVal>>::Error>,
    {
        let k_rv: RawVal = self.host.convert_ref(k)?;
        let v_rv: RawVal = self.host.convert_ref(v)?;
        self.object = self.host.map_put(self.object, k_rv, v_rv)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct Vec {
    host: Host,
    object: Object,
}

impl Convert<Object, Vec> for Host {
    type Error = HostError;

    fn convert_ref(&self, object: &Object) -> Result<Vec, Self::Error> {
        if object.is_obj_type(ScObjectType::Vec) {
            Ok(Vec {
                host: self.clone(),
                object: *object,
            })
        } else {
            Err(ConversionError.into())
        }
    }
}

impl Convert<Vec, Object> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Vec) -> Result<Object, Self::Error> {
        Ok(f.object)
    }
}

impl Convert<RawVal, Vec> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &RawVal) -> Result<Vec, Self::Error> {
        let obj: Object = f.try_into()?;
        self.convert(obj)
    }
}

impl Convert<Vec, RawVal> for Host {
    type Error = HostError;

    fn convert_ref(&self, f: &Vec) -> Result<RawVal, Self::Error> {
        Ok(f.object.to_raw())
    }
}

impl From<Vec> for Object {
    fn from(bytes: Vec) -> Self {
        bytes.object
    }
}

impl From<Vec> for RawVal {
    fn from(bytes: Vec) -> Self {
        bytes.object.to_raw()
    }
}

impl Vec {
    pub fn new(host: &Host) -> Result<Self, HostError> {
        let object = host.vec_new(().into())?;
        Ok(Self {
            host: host.clone(),
            object,
        })
    }

    pub fn get<T>(&self, i: u32) -> Result<T, HostError>
    where
        Host: Convert<RawVal, T>,
        HostError: From<<Host as Convert<RawVal, T>>::Error>,
    {
        let rv: RawVal = self.host.vec_get(self.object, i.into())?;
        Ok(self.host.convert_ref(&rv)?)
    }

    pub fn len(&self) -> Result<u32, HostError> {
        let rv = self.host.vec_len(self.object)?;
        Ok(rv.try_into()?)
    }

    pub fn push<T>(&mut self, x: &T) -> Result<(), HostError>
    where
        Host: Convert<T, RawVal>,
        HostError: From<<Host as Convert<T, RawVal>>::Error>,
    {
        let rv: RawVal = self.host.convert_ref(x)?;
        self.push_raw(rv)
    }

    pub fn push_raw(&mut self, x: RawVal) -> Result<(), HostError> {
        self.object = self.host.vec_push_back(self.object, x)?;
        Ok(())
    }
}
