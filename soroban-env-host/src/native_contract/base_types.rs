use crate::budget::CostType;
use crate::host::metered_clone::MeteredClone;
use crate::host::{Host, HostError};
use core::cmp::Ordering;
use soroban_env_common::xdr::{AccountId, ScAddress, ScObjectType};
use soroban_env_common::{
    CheckedEnv, ConversionError, EnvBase, EnvVal, Object, RawVal, TryFromVal, TryIntoVal,
};

use super::contract_error::ContractError;

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

impl<const N: usize> From<BytesN<N>> for Bytes {
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

    #[cfg(test)]
    pub(crate) fn to_vec(&self) -> std::vec::Vec<u8> {
        use soroban_env_common::RawValConvertible;
        let env = self.0.env();
        let mut res = std::vec::Vec::<u8>::new();
        let size = unsafe {
            <u32 as RawValConvertible>::unchecked_from_val(
                env.bytes_len(self.0.to_object()).unwrap(),
            )
        };
        res.resize(size as usize, 0);
        env.bytes_copy_to_slice(self.0.to_object(), RawVal::U32_ZERO, &mut res[..])
            .unwrap();
        res
    }
}

#[derive(Clone)]
pub struct BytesN<const N: usize>(EnvVal<Host, Object>);

impl<const N: usize> TryFromVal<Host, Object> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        let len: u32 = env.bytes_len(val)?.try_into()?;
        if len
            == N.try_into()
                .map_err(|_| env.err_general("bytes buffer overflow"))?
        {
            Ok(Self(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl<const N: usize> Eq for BytesN<N> {}

impl<const N: usize> PartialEq for BytesN<N> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl<const N: usize> PartialOrd for BytesN<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl<const N: usize> Ord for BytesN<N> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<const N: usize> TryFromVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl<const N: usize> TryIntoVal<Host, BytesN<N>> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<BytesN<N>, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl<const N: usize> TryIntoVal<Host, RawVal> for BytesN<N> {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}

impl<const N: usize> From<BytesN<N>> for Object {
    fn from(bytes: BytesN<N>) -> Self {
        bytes.0.val
    }
}

impl<const N: usize> BytesN<N> {
    pub fn compare(&self, other: &BytesN<N>) -> Result<Ordering, HostError> {
        let res = self.0.env.obj_cmp(self.0.val.into(), other.0.val.into())?;
        Ok(res.cmp(&0))
    }
}

impl<const N: usize> BytesN<N> {
    #[inline(always)]
    pub fn to_array(&self) -> Result<[u8; N], HostError> {
        let env = self.0.env();
        let mut slice = [0_u8; N];
        env.charge_budget(CostType::BytesClone, 4)?;
        env.bytes_copy_to_slice(self.0.to_object(), RawVal::U32_ZERO, &mut slice)?;
        Ok(slice)
    }

    #[inline(always)]
    pub fn from_slice(env: &Host, items: &[u8]) -> Result<BytesN<N>, HostError> {
        Ok(BytesN(env.bytes_new_from_slice(items)?.in_env(env)))
    }

    #[cfg(test)]
    pub(crate) fn to_vec(&self) -> std::vec::Vec<u8> {
        Bytes::from(self.clone()).to_vec()
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
        self.push_raw(rv)
    }

    pub fn push_raw(&mut self, x: RawVal) -> Result<(), HostError> {
        self.0.val = self.0.env.vec_push_back(self.0.val, x)?;
        Ok(())
    }
}

impl TryFromVal<Host, Object> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        env.visit_obj(val, |acc: &AccountId| Ok(acc.clone()))
    }
}

impl TryFromVal<Host, RawVal> for AccountId {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for AccountId {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        Ok(env.add_host_object(self)?.to_raw())
    }
}

impl TryFromVal<Host, Object> for ScAddress {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        env.visit_obj(val, |addr: &ScAddress| Ok(addr.clone()))
    }
}

impl TryFromVal<Host, RawVal> for ScAddress {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}

impl TryIntoVal<Host, RawVal> for ScAddress {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<RawVal, Self::Error> {
        Ok(env.add_host_object(self)?.to_raw())
    }
}

impl TryIntoVal<Host, ScAddress> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<ScAddress, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

pub(crate) fn get_account_id(env: &Host, addr: &ScAddress) -> Result<AccountId, HostError> {
    if let ScAddress::ClassicAccount(acc) = addr {
        Ok(acc.metered_clone(env.budget_ref())?)
    } else {
        Err(env.err_status(ContractError::AccountIsNotClassic))
    }
}

#[derive(Clone)]
pub struct Account(EnvVal<Host, Object>);

impl TryFromVal<Host, Object> for Account {
    type Error = HostError;

    fn try_from_val(env: &Host, val: Object) -> Result<Self, Self::Error> {
        if val.is_obj_type(ScObjectType::Account) {
            Ok(Account(val.in_env(env)))
        } else {
            Err(ConversionError.into())
        }
    }
}

impl TryFromVal<Host, RawVal> for Account {
    type Error = HostError;

    fn try_from_val(env: &Host, val: RawVal) -> Result<Self, Self::Error> {
        <_ as TryFromVal<_, Object>>::try_from_val(env, val.try_into()?)
    }
}
impl TryIntoVal<Host, RawVal> for Account {
    type Error = HostError;

    fn try_into_val(self, _env: &Host) -> Result<RawVal, Self::Error> {
        Ok(self.0.val.into())
    }
}


impl TryIntoVal<Host, Account> for RawVal {
    type Error = HostError;

    fn try_into_val(self, env: &Host) -> Result<Account, Self::Error> {
        <_ as TryFromVal<_, RawVal>>::try_from_val(env, self.try_into()?)
    }
}

impl Account {
    pub(crate) fn address(&self) -> Result<ScAddress, HostError> {
        let e = &self.0.env;
        Ok(ScAddress::try_from_val(
            e,
            e.get_account_address(self.0.val.clone())?,
        )?)
    }

    pub(crate) fn authorize(&self, args: Vec) -> Result<(), HostError> {
        Ok(self
            .0
            .env
            .authorize_account(self.0.val.into(), args.into())?
            .try_into()?)
    }
}
