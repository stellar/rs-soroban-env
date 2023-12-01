use crate::{
    host::{metered_clone::MeteredClone, Host, HostError},
    xdr::{AccountId, ScAddress, ScErrorCode, ScErrorType},
    AddressObject, BytesObject, Compare, Env, EnvBase, StringObject, TryFromVal, U32Val, Val,
    VecObject,
};
use core::cmp::Ordering;

#[derive(Clone)]
pub(crate) struct String {
    host: Host,
    object: StringObject,
}

impl Compare<String> for Host {
    type Error = HostError;

    fn compare(&self, a: &String, b: &String) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_val(), &b.object.to_val())
    }
}

impl TryFromVal<Host, StringObject> for String {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &StringObject) -> Result<Self, Self::Error> {
        Ok(String {
            host: env.clone(),
            object: *val,
        })
    }
}

impl TryFromVal<Host, Val> for String {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: StringObject = val.try_into()?;
        String::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, String> for Val {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &String) -> Result<Val, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<String> for StringObject {
    fn from(b: String) -> Self {
        b.object
    }
}

impl String {
    pub(crate) fn to_array<const N: usize>(&self) -> Result<[u8; N], HostError> {
        let mut slice = [0_u8; N];
        let len = self.host.string_len(self.object)?;
        if u32::from(len) as usize != N {
            return Err(self.host.err(
                ScErrorType::Value,
                ScErrorCode::UnexpectedSize,
                "String had unexpected size",
                &[len.to_val()],
            ));
        }
        self.host
            .string_copy_to_slice(self.object, Val::U32_ZERO, &mut slice)?;
        Ok(slice)
    }

    #[cfg(test)]
    pub(crate) fn to_string(&self) -> std::string::String {
        let mut res = std::vec::Vec::<u8>::new();
        let size: u32 = self.host.string_len(self.object).unwrap().into();
        res.resize(size as usize, 0);
        self.host
            .string_copy_to_slice(self.object, Val::U32_ZERO, &mut res[..])
            .unwrap();
        std::string::String::from_utf8(res).unwrap()
    }
}

#[derive(Clone)]
pub(crate) struct BytesN<const N: usize> {
    host: Host,
    object: BytesObject,
}

impl<const N: usize> TryFromVal<Host, BytesObject> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &BytesObject) -> Result<Self, Self::Error> {
        let val = *val;
        let len: u32 = env.bytes_len(val)?.try_into()?;
        if len == u32::try_from(N).map_err(|_| env.err_arith_overflow())? {
            Ok(Self {
                host: env.clone(),
                object: val,
            })
        } else {
            return Err(env.err(
                ScErrorType::Value,
                ScErrorCode::UnexpectedSize,
                "Bytes had unexpected size",
                &[U32Val::from(len).to_val()],
            ));
        }
    }
}

impl<const N: usize> Compare<BytesN<N>> for Host {
    type Error = HostError;

    fn compare(&self, a: &BytesN<N>, b: &BytesN<N>) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_val(), &b.object.to_val())
    }
}

impl<const N: usize> TryFromVal<Host, Val> for BytesN<N> {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: BytesObject = val.try_into()?;
        <BytesN<N>>::try_from_val(env, &obj)
    }
}

impl<const N: usize> TryFromVal<Host, BytesN<N>> for Val {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &BytesN<N>) -> Result<Val, Self::Error> {
        Ok(val.object.into())
    }
}

impl<const N: usize> From<BytesN<N>> for Val {
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
    pub(crate) fn compare(&self, other: &BytesN<N>) -> Result<Ordering, HostError> {
        let res = self.host.obj_cmp(self.object.into(), other.object.into())?;
        Ok(res.cmp(&0))
    }
}

impl<const N: usize> BytesN<N> {
    #[inline(always)]
    pub(crate) fn to_array(&self) -> Result<[u8; N], HostError> {
        let mut slice = [0_u8; N];
        self.host
            .bytes_copy_to_slice(self.object, Val::U32_ZERO, &mut slice)?;
        Ok(slice)
    }

    #[inline(always)]
    pub(crate) fn from_slice(env: &Host, items: &[u8]) -> Result<BytesN<N>, HostError> {
        Ok(BytesN {
            host: env.clone(),
            object: env.bytes_new_from_slice(items)?,
        })
    }

    pub(crate) fn as_object(&self) -> BytesObject {
        self.object
    }
}

#[derive(Clone)]
pub(crate) struct Vec {
    host: Host,
    object: VecObject,
}

impl Compare<Vec> for Host {
    type Error = HostError;

    fn compare(&self, a: &Vec, b: &Vec) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.as_val(), &b.object.as_val())
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

impl TryFromVal<Host, Val> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: VecObject = val.try_into()?;
        Vec::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Vec> for Val {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Vec) -> Result<Val, Self::Error> {
        Ok(val.object.into())
    }
}

impl From<Vec> for Val {
    fn from(val: Vec) -> Self {
        val.object.into()
    }
}

impl From<Vec> for VecObject {
    fn from(vec: Vec) -> Self {
        vec.object
    }
}

impl TryFromVal<Host, std::vec::Vec<Val>> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, vals: &std::vec::Vec<Val>) -> Result<Self, Self::Error> {
        Vec::from_slice(env, &vals)
    }
}

impl TryFromVal<Host, &std::vec::Vec<Val>> for Vec {
    type Error = HostError;

    fn try_from_val(env: &Host, vals: &&std::vec::Vec<Val>) -> Result<Self, Self::Error> {
        Vec::try_from_val(env, *vals)
    }
}

impl Vec {
    pub(crate) fn new(env: &Host) -> Result<Self, HostError> {
        let vec = env.vec_new()?;
        Ok(Self {
            host: env.clone(),
            object: vec,
        })
    }

    pub(crate) fn from_slice(env: &Host, slice: &[Val]) -> Result<Self, HostError> {
        let vec = env.vec_new_from_slice(slice)?;
        Ok(Self {
            host: env.clone(),
            object: vec,
        })
    }

    pub(crate) fn get<T: TryFromVal<Host, Val>>(&self, i: u32) -> Result<T, HostError>
    where
        HostError: From<<T as TryFromVal<Host, Val>>::Error>,
    {
        let rv = self.host.vec_get(self.object, i.into())?;
        Ok(T::try_from_val(&self.host, &rv)?)
    }

    pub(crate) fn len(&self) -> Result<u32, HostError> {
        Ok(self.host.vec_len(self.object)?.into())
    }

    pub(crate) fn push<T>(&mut self, x: &T) -> Result<(), HostError>
    where
        Val: TryFromVal<Host, T>,
        HostError: From<<Val as TryFromVal<Host, T>>::Error>,
    {
        let rv = Val::try_from_val(&self.host, x)?;
        self.push_val(rv)
    }

    pub(crate) fn push_val(&mut self, x: Val) -> Result<(), HostError> {
        self.object = self.host.vec_push_back(self.object, x)?;
        Ok(())
    }

    pub(crate) fn as_object(&self) -> VecObject {
        self.object
    }
}

#[derive(Clone)]
pub(crate) struct Address {
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

impl TryFromVal<Host, Val> for Address {
    type Error = HostError;

    fn try_from_val(env: &Host, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        let obj: AddressObject = val.try_into()?;
        Address::try_from_val(env, &obj)
    }
}

impl TryFromVal<Host, Address> for Val {
    type Error = HostError;

    fn try_from_val(_env: &Host, val: &Address) -> Result<Val, Self::Error> {
        Ok(val.object.into())
    }
}

impl Compare<Address> for Host {
    type Error = HostError;

    fn compare(&self, a: &Address, b: &Address) -> Result<Ordering, Self::Error> {
        self.compare(&a.object.to_val(), &b.object.to_val())
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
            &env.add_host_object(ScAddress::Account(
                account_id.metered_clone(env.budget_ref())?,
            ))?,
        )
    }

    pub(crate) fn to_sc_address(&self) -> Result<ScAddress, HostError> {
        self.host.scaddress_from_address(self.object)
    }

    pub(crate) fn require_auth(&self) -> Result<(), HostError> {
        self.host.require_auth(self.object)?;
        Ok(())
    }

    pub(crate) fn as_object(&self) -> AddressObject {
        self.object
    }
}
