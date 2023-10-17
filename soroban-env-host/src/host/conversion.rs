use std::rc::Rc;

use super::metered_clone::{
    charge_shallow_copy, MeteredAlloc, MeteredClone, MeteredContainer, MeteredIterator,
};
use crate::budget::AsBudget;
use crate::err;
use crate::host_object::{HostMap, HostObject, HostVec};
use crate::xdr::{Hash, LedgerKey, LedgerKeyContractData, ScVal, ScVec, Uint256};
use crate::{xdr::ContractCostType, Host, HostError, Val};
use soroban_env_common::num::{
    i256_from_pieces, i256_into_pieces, u256_from_pieces, u256_into_pieces,
};
use soroban_env_common::xdr::{
    self, int128_helpers, AccountId, ContractDataDurability, DepthLimiter, Int128Parts,
    Int256Parts, ScAddress, ScBytes, ScErrorCode, ScErrorType, ScMap, ScMapEntry, UInt128Parts,
    UInt256Parts, VecM,
};
use soroban_env_common::{
    AddressObject, BytesObject, Convert, Object, ScValObjRef, ScValObject, TryFromVal, TryIntoVal,
    U32Val, VecObject,
};

impl Host {
    // Notes on metering: free
    pub(crate) fn usize_to_u32(&self, u: usize) -> Result<u32, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v),
            Err(_) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::ArithDomain,
                "provided usize does not fit in u32",
                &[],
            )),
        }
    }

    // Notes on metering: free
    pub(crate) fn usize_to_u32val(&self, u: usize) -> Result<U32Val, HostError> {
        self.usize_to_u32(u).map(|v| v.into())
    }

    pub(crate) fn u256_from_account(&self, account_id: &AccountId) -> Result<Uint256, HostError> {
        let crate::xdr::PublicKey::PublicKeyTypeEd25519(ed25519) =
            account_id.metered_clone(self)?.0;
        Ok(ed25519)
    }

    // Notes on metering: free
    pub(crate) fn u8_from_u32val_input(
        &self,
        name: &'static str,
        r: U32Val,
    ) -> Result<u8, HostError> {
        let u: u32 = r.into();
        match u8::try_from(u) {
            Ok(v) => Ok(v),
            Err(_) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "expecting U32Val less than 256",
                &[r.to_val(), name.try_into_val(self)?],
            )),
        }
    }

    pub(crate) fn hash_from_bytesobj_input(
        &self,
        name: &'static str,
        hash: BytesObject,
    ) -> Result<Hash, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<Hash, 32>(name, hash)
    }

    pub(crate) fn u256_from_bytesobj_input(
        &self,
        name: &'static str,
        u256: BytesObject,
    ) -> Result<Uint256, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<Uint256, 32>(name, u256)
    }

    pub(crate) fn fixed_length_bytes_from_slice<T, const N: usize>(
        &self,
        name: &'static str,
        bytes_arr: &[u8],
    ) -> Result<T, HostError>
    where
        T: From<[u8; N]>,
    {
        match <[u8; N]>::try_from(bytes_arr) {
            Ok(arr) => {
                self.charge_budget(ContractCostType::MemCpy, Some(N as u64))?;
                Ok(arr.into())
            }
            Err(_) => Err(err!(
                self,
                (ScErrorType::Object, ScErrorCode::UnexpectedSize),
                "expected fixed-length bytes slice, got slice with different size",
                name,
                N,
                bytes_arr.len()
            )),
        }
    }

    pub(crate) fn fixed_length_bytes_from_bytesobj_input<T, const N: usize>(
        &self,
        name: &'static str,
        obj: BytesObject,
    ) -> Result<T, HostError>
    where
        T: From<[u8; N]>,
    {
        self.visit_obj(obj, |bytes: &ScBytes| {
            self.fixed_length_bytes_from_slice(name, bytes.as_slice())
        })
    }

    pub(crate) fn account_id_from_bytesobj(&self, k: BytesObject) -> Result<AccountId, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            Ok(AccountId(xdr::PublicKey::PublicKeyTypeEd25519(
                self.fixed_length_bytes_from_slice("account_id", bytes.as_slice())?,
            )))
        })
    }

    /// Converts a [`Val`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    // Notes on metering: covered by components.
    pub fn storage_key_from_rawval(
        &self,
        k: Val,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        self.storage_key_from_scval(self.from_host_val(k)?, durability)
    }

    pub(crate) fn storage_key_for_address(
        &self,
        contract_address: ScAddress,
        key: ScVal,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        Rc::metered_new(
            LedgerKey::ContractData(LedgerKeyContractData {
                contract: contract_address,
                key,
                durability,
            }),
            self,
        )
    }

    pub(crate) fn storage_key_from_scval(
        &self,
        key: ScVal,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let contract_id = self.get_current_contract_id_internal()?;
        self.storage_key_for_address(ScAddress::Contract(contract_id), key, durability)
    }

    // Notes on metering: covered by components.
    pub(crate) fn contract_data_key_from_rawval(
        &self,
        k: Val,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let key_scval = self.from_host_val(k)?;
        // FIXME: also check for ScVal::ContractInstance here?
        if let ScVal::LedgerKeyContractInstance | ScVal::LedgerKeyNonce(_) = key_scval {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "value type cannot be used as contract data key",
                &[k],
            ));
        }
        self.storage_key_from_scval(key_scval, durability)
    }

    /// Converts a binary search result into a u64. `res` is `Some(index)` if
    /// the value was found at `index`, or `Err(index)` if the value was not
    /// found and would've needed to be inserted at `index`. Returns a
    /// Some(res_u64) where :
    /// - the high 32 bits is 0x0000_0001 if element existed or 0x0000_0000 if
    ///   it didn't
    /// - the low 32 bits contains the u32 representation of the `index` Err(_)
    /// if the `index` fails to be converted to an u32.
    pub(crate) fn u64_from_binary_search_result(
        &self,
        res: Result<usize, usize>,
    ) -> Result<u64, HostError> {
        match res {
            Ok(u) => {
                let v = self.usize_to_u32(u)?;
                Ok(u64::from(v) | (1 << u32::BITS))
            }
            Err(u) => {
                let v = self.usize_to_u32(u)?;
                Ok(u64::from(v))
            }
        }
    }

    pub(crate) fn call_args_from_obj(&self, args: VecObject) -> Result<Vec<Val>, HostError> {
        self.visit_obj(args, |hv: &HostVec| hv.to_vec(self.as_budget()))
    }

    // Metering: covered by rawvals_to_vec
    pub(crate) fn call_args_to_sc_val_vec(
        &self,
        args: VecObject,
    ) -> Result<VecM<ScVal>, HostError> {
        self.visit_obj(args, |hv: &HostVec| {
            self.rawvals_to_sc_val_vec(hv.as_slice())
        })
    }

    pub(crate) fn rawvals_to_sc_val_vec(&self, raw_vals: &[Val]) -> Result<VecM<ScVal>, HostError> {
        raw_vals
            .iter()
            .map(|v| self.from_host_val(*v))
            .metered_collect::<Result<Vec<ScVal>, HostError>>(self)??
            .try_into()
            .map_err(|_| {
                err!(
                    self,
                    (ScErrorType::Object, ScErrorCode::ExceededLimit),
                    "vector size limit exceeded",
                    raw_vals.len()
                )
            })
    }

    pub(crate) fn rawvals_to_sc_val_vec_non_metered(
        &self,
        raw_vals: &[Val],
    ) -> Result<VecM<ScVal>, HostError> {
        raw_vals
            .iter()
            .map(|v| v.try_into_val(self)?)
            .collect::<Result<Vec<ScVal>, HostError>>()?
            .try_into()
            .map_err(|_| {
                err!(
                    self,
                    (ScErrorType::Object, ScErrorCode::ExceededLimit),
                    "vector size limit exceeded",
                    raw_vals.len()
                )
            })
    }

    pub(crate) fn scvals_to_rawvals(&self, sc_vals: &[ScVal]) -> Result<Vec<Val>, HostError> {
        sc_vals
            .iter()
            .map(|scv| self.to_host_val(scv))
            .metered_collect::<Result<Vec<Val>, HostError>>(self)?
    }

    pub(crate) fn bytesobj_from_internal_contract_id(
        &self,
    ) -> Result<Option<BytesObject>, HostError> {
        if let Some(id) = self.get_current_contract_id_opt_internal()? {
            let obj = self.add_host_object::<ScBytes>(
                self.metered_slice_to_vec(id.as_slice())?.try_into()?,
            )?;
            Ok(Some(obj))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn scbytes_from_vec(&self, v: Vec<u8>) -> Result<ScBytes, HostError> {
        Ok(ScBytes(v.try_into()?))
    }

    pub(crate) fn metered_slice_to_vec(&self, s: &[u8]) -> Result<Vec<u8>, HostError> {
        Vec::<u8>::charge_bulk_init_cpy(s.len() as u64, self)?;
        Ok(s.to_vec())
    }

    // metering: covered
    pub(crate) fn scbytes_from_slice(&self, s: &[u8]) -> Result<ScBytes, HostError> {
        self.scbytes_from_vec(self.metered_slice_to_vec(s)?)
    }

    pub(crate) fn scbytes_from_hash(&self, hash: &Hash) -> Result<ScBytes, HostError> {
        self.scbytes_from_slice(hash.as_slice())
    }

    pub(crate) fn scaddress_from_address(
        &self,
        address: AddressObject,
    ) -> Result<ScAddress, HostError> {
        self.visit_obj(address, |addr: &ScAddress| addr.metered_clone(self))
    }

    pub(crate) fn host_map_to_scmap(&self, map: &HostMap) -> Result<ScMap, HostError> {
        Vec::<ScMapEntry>::charge_bulk_init_cpy(map.len() as u64, self)?;
        let mut mv = Vec::with_capacity(map.len());
        for (k, v) in map.iter(self)? {
            let key = self.from_host_val(*k)?;
            let val = self.from_host_val(*v)?;
            mv.push(ScMapEntry { key, val });
        }
        Ok(ScMap(self.map_err(mv.try_into())?))
    }
}

impl Convert<&Object, ScValObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: &Object) -> Result<ScValObject, Self::Error> {
        self.from_host_obj(*ob)
    }
}

impl Convert<Object, ScValObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: Object) -> Result<ScValObject, Self::Error> {
        self.from_host_obj(ob)
    }
}

impl<'a> Convert<&ScValObjRef<'a>, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: &ScValObjRef<'a>) -> Result<Object, Self::Error> {
        self.to_host_obj(ob)
    }
}

impl<'a> Convert<ScValObjRef<'a>, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: ScValObjRef<'a>) -> Result<Object, Self::Error> {
        self.to_host_obj(&ob)
    }
}

impl Host {
    pub(crate) fn from_host_val(&self, val: Val) -> Result<ScVal, HostError> {
        // This is the depth limit checkpoint for `Val`->`ScVal` conversion.
        // Metering of val conversion happens only if an object is encountered,
        // and is done inside `from_host_obj`.
        let _span = tracy_span!("Val to ScVal");
        self.budget_cloned().with_limited_depth(|_| {
            ScVal::try_from_val(self, &val).map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "failed to convert host value to ScVal",
                    &[val],
                )
            })
        })
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<Val, HostError> {
        let _span = tracy_span!("ScVal to Val");
        // This is the depth limit checkpoint for `ScVal`->`Val` conversion.
        // Metering of val conversion happens only if an object is encountered,
        // and is done inside `to_host_obj`.
        self.budget_cloned().with_limited_depth(|_| {
            v.try_into_val(self).map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InternalError,
                    "failed to convert ScVal to host value",
                    &[],
                )
            })
        })
    }

    pub(crate) fn from_host_obj(&self, ob: impl Into<Object>) -> Result<ScValObject, HostError> {
        unsafe {
            let objref: Object = ob.into();
            self.visit_obj_untyped(objref, |ho| {
                let val = match ho {
                    HostObject::Vec(vv) => {
                        Vec::<ScVal>::charge_bulk_init_cpy(vv.len() as u64, self)?;
                        let sv = vv.iter().map(|e| self.from_host_val(*e)).collect::<Result<
                            Vec<ScVal>,
                            HostError,
                        >>(
                        )?;
                        ScVal::Vec(Some(ScVec(self.map_err(sv.try_into())?)))
                    }
                    HostObject::Map(mm) => ScVal::Map(Some(self.host_map_to_scmap(mm)?)),
                    HostObject::U64(u) => {
                        charge_shallow_copy::<u64>(1, self)?;
                        ScVal::U64(*u)
                    }
                    HostObject::I64(i) => {
                        charge_shallow_copy::<i64>(1, self)?;
                        ScVal::I64(*i)
                    }
                    HostObject::TimePoint(tp) => ScVal::Timepoint(tp.metered_clone(self)?),
                    HostObject::Duration(d) => ScVal::Duration(d.metered_clone(self)?),
                    HostObject::U128(u) => {
                        charge_shallow_copy::<u128>(1, self)?;
                        ScVal::U128(UInt128Parts {
                            hi: int128_helpers::u128_hi(*u),
                            lo: int128_helpers::u128_lo(*u),
                        })
                    }
                    HostObject::I128(i) => {
                        charge_shallow_copy::<i128>(1, self)?;
                        ScVal::I128(Int128Parts {
                            hi: int128_helpers::i128_hi(*i),
                            lo: int128_helpers::i128_lo(*i),
                        })
                    }
                    HostObject::U256(u) => {
                        charge_shallow_copy::<u128>(2, self)?;
                        let (hi_hi, hi_lo, lo_hi, lo_lo) = u256_into_pieces(*u);
                        ScVal::U256(UInt256Parts {
                            hi_hi,
                            hi_lo,
                            lo_hi,
                            lo_lo,
                        })
                    }
                    HostObject::I256(i) => {
                        charge_shallow_copy::<i128>(2, self)?;
                        let (hi_hi, hi_lo, lo_hi, lo_lo) = i256_into_pieces(*i);
                        ScVal::I256(Int256Parts {
                            hi_hi,
                            hi_lo,
                            lo_hi,
                            lo_lo,
                        })
                    }
                    HostObject::Bytes(b) => ScVal::Bytes(b.metered_clone(self)?),
                    HostObject::String(s) => ScVal::String(s.metered_clone(self)?),
                    HostObject::Symbol(s) => ScVal::Symbol(s.metered_clone(self)?),
                    HostObject::Address(addr) => ScVal::Address(addr.metered_clone(self)?), // For any future `HostObject` types we add, make sure to add some metering.
                };
                Ok(ScValObject::unchecked_from_val(val))
            })
        }
    }

    pub(crate) fn to_host_obj(&self, ob: &ScValObjRef<'_>) -> Result<Object, HostError> {
        let val: &ScVal = (*ob).into();
        match val {
            // Here we have to make sure host object conversion is charged in each variant
            // below. There is no otherwise ubiquitous metering for ScVal->Val conversion,
            // since most of them happens in the "common" crate with no access to the host.
            ScVal::Vec(Some(v)) => {
                Vec::<Val>::charge_bulk_init_cpy(v.len() as u64, self)?;
                let mut vv = Vec::with_capacity(v.len());
                for e in v.iter() {
                    vv.push(self.to_host_val(e)?)
                }
                Ok(self.add_host_object(HostVec::from_vec(vv)?)?.into())
            }
            ScVal::Map(Some(m)) => {
                Vec::<(Val, Val)>::charge_bulk_init_cpy(m.len() as u64, self)?;
                let mut mm = Vec::with_capacity(m.len());
                for pair in m.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.push((k, v))
                }
                Ok(self.add_host_object(HostMap::from_map(mm, self)?)?.into())
            }
            ScVal::Vec(None) => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::MissingValue,
                "vector body missing",
                &[],
            )),
            ScVal::Map(None) => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::MissingValue,
                "map body missing",
                &[],
            )),
            ScVal::U64(u) => {
                charge_shallow_copy::<u64>(1, self)?;
                Ok(self.add_host_object(*u)?.into())
            }
            ScVal::I64(i) => {
                charge_shallow_copy::<i64>(1, self)?;
                Ok(self.add_host_object(*i)?.into())
            }
            ScVal::Timepoint(t) => Ok(self.add_host_object(t.metered_clone(self)?)?.into()),
            ScVal::Duration(d) => Ok(self.add_host_object(d.metered_clone(self)?)?.into()),
            ScVal::U128(u) => {
                charge_shallow_copy::<u128>(1, self)?;
                Ok(self
                    .add_host_object(int128_helpers::u128_from_pieces(u.hi, u.lo))?
                    .into())
            }
            ScVal::I128(i) => {
                charge_shallow_copy::<i128>(1, self)?;
                Ok(self
                    .add_host_object(int128_helpers::i128_from_pieces(i.hi, i.lo))?
                    .into())
            }
            ScVal::U256(u) => {
                charge_shallow_copy::<u128>(2, self)?;
                Ok(self
                    .add_host_object(u256_from_pieces(u.hi_hi, u.hi_lo, u.lo_hi, u.lo_lo))?
                    .into())
            }
            ScVal::I256(i) => {
                charge_shallow_copy::<i128>(2, self)?;
                Ok(self
                    .add_host_object(i256_from_pieces(i.hi_hi, i.hi_lo, i.lo_hi, i.lo_lo))?
                    .into())
            }
            ScVal::Bytes(b) => Ok(self.add_host_object(b.metered_clone(self)?)?.into()),
            ScVal::String(s) => Ok(self.add_host_object(s.metered_clone(self)?)?.into()),
            ScVal::Symbol(s) => Ok(self.add_host_object(s.metered_clone(self)?)?.into()),
            ScVal::Address(addr) => Ok(self.add_host_object(addr.metered_clone(self)?)?.into()),
            ScVal::Bool(_)
            | ScVal::Void
            | ScVal::Error(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::LedgerKeyNonce(_)
            | ScVal::ContractInstance(_)
            | ScVal::LedgerKeyContractInstance => Err(err!(
                self,
                (ScErrorType::Object, ScErrorCode::InvalidInput),
                "converting unsupported value to object",
                *val
            )), // For any future `HostObject` types we add, make sure to add some metering.
        }
    }
}
