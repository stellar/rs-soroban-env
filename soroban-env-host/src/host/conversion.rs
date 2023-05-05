use std::rc::Rc;

use super::metered_clone::{self, MeteredClone};
use crate::budget::AsBudget;
use crate::host_object::{HostMap, HostObject, HostVec};
use crate::xdr::{
    Hash, LedgerKey, LedgerKeyContractData, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostValErrorCode, ScVal, ScVec, Uint256,
};
use crate::{events::DebugError, xdr::ContractCostType, Host, HostError, RawVal};
use ed25519_dalek::{PublicKey, Signature, SIGNATURE_LENGTH};
use sha2::{Digest, Sha256};
use soroban_env_common::num::{
    i256_from_pieces, i256_into_pieces, u256_from_pieces, u256_into_pieces,
};
use soroban_env_common::xdr::{
    self, int128_helpers, AccountId, Int128Parts, Int256Parts, ScBytes, ScMap, ScMapEntry,
    UInt128Parts, UInt256Parts,
};
use soroban_env_common::{
    BytesObject, Convert, Object, ScValObjRef, ScValObject, TryFromVal, TryIntoVal, U32Val,
    VecObject,
};

impl Host {
    // Notes on metering: free
    pub(crate) fn usize_to_u32(&self, u: usize) -> Result<u32, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v),
            Err(_) => Err(self.err_status(ScHostValErrorCode::U32OutOfRange)),
        }
    }

    // Notes on metering: free
    pub(crate) fn usize_to_u32val(&self, u: usize) -> Result<U32Val, HostError> {
        self.usize_to_u32(u).map(|v| v.into())
    }

    // Notes on metering: free
    pub(crate) fn usize_from_rawval_u32_input(
        &self,
        name: &'static str,
        r: RawVal,
    ) -> Result<usize, HostError> {
        self.u32_from_rawval_input(name, r).map(|u| u as usize)
    }

    // Notes on metering: free
    pub(crate) fn u32_from_rawval_input(
        &self,
        name: &'static str,
        r: RawVal,
    ) -> Result<u32, HostError> {
        match u32::try_from(r) {
            Ok(v) => Ok(v),
            Err(cvt) => Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
                    .msg("unexpected RawVal {} for input '{}', need U32")
                    .arg(r)
                    .arg(name),
            )),
        }
    }

    pub(crate) fn to_u256_from_account(
        &self,
        account_id: &AccountId,
    ) -> Result<Uint256, HostError> {
        let crate::xdr::PublicKey::PublicKeyTypeEd25519(ed25519) =
            account_id.metered_clone(&self.0.budget)?.0;
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
            Err(cvt) => Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
                    .msg("unexpected U32Val {} for input '{}', need u32 no greater than 255")
                    .arg(r.to_raw())
                    .arg(name),
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

    pub(crate) fn uint256_from_bytesobj_input(
        &self,
        name: &'static str,
        u256: BytesObject,
    ) -> Result<Uint256, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<Uint256, 32>(name, u256)
    }

    pub(crate) fn signature_from_bytes(
        &self,
        name: &'static str,
        bytes: &[u8],
    ) -> Result<Signature, HostError> {
        self.fixed_length_bytes_from_slice::<Signature, SIGNATURE_LENGTH>(name, bytes)
    }

    pub(crate) fn signature_from_bytesobj_input(
        &self,
        name: &'static str,
        sig: BytesObject,
    ) -> Result<Signature, HostError> {
        self.fixed_length_bytes_from_bytesobj_input::<Signature, SIGNATURE_LENGTH>(name, sig)
    }

    fn fixed_length_bytes_from_slice<T, const N: usize>(
        &self,
        name: &'static str,
        bytes_arr: &[u8],
    ) -> Result<T, HostError>
    where
        T: From<[u8; N]>,
    {
        match <[u8; N]>::try_from(bytes_arr) {
            Ok(arr) => {
                self.charge_budget(ContractCostType::HostMemCpy, Some(N as u64))?;
                Ok(arr.into())
            }
            Err(cvt) => Err(self.err(
                // TODO: This is a wrong error code to use here, we should replace
                // it with a more generic one.
                DebugError::new(ScHostObjErrorCode::ContractHashWrongLength) // TODO: this should be renamed to be more generic
                    .msg("{} has wrong length for input '{}'")
                    .arg(std::any::type_name::<T>())
                    .arg(name),
            )),
        }
    }

    fn fixed_length_bytes_from_bytesobj_input<T, const N: usize>(
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

    pub(crate) fn ed25519_pub_key_from_bytes(&self, bytes: &[u8]) -> Result<PublicKey, HostError> {
        debug_assert!(bytes.len() == 32);
        self.charge_budget(ContractCostType::ComputeEd25519PubKey, None)?;
        PublicKey::from_bytes(bytes).map_err(|_| {
            self.err_status_msg(ScHostObjErrorCode::UnexpectedType, "invalid public key")
        })
    }

    pub fn ed25519_pub_key_from_bytesobj_input(
        &self,
        k: BytesObject,
    ) -> Result<PublicKey, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            self.ed25519_pub_key_from_bytes(bytes.as_slice())
        })
    }

    pub(crate) fn account_id_from_bytesobj(&self, k: BytesObject) -> Result<AccountId, HostError> {
        self.visit_obj(k, |bytes: &ScBytes| {
            Ok(AccountId(xdr::PublicKey::PublicKeyTypeEd25519(
                self.fixed_length_bytes_from_slice("account_id", bytes.as_slice())?,
            )))
        })
    }

    pub(crate) fn sha256_hash_from_bytes(&self, bytes: &[u8]) -> Result<Vec<u8>, HostError> {
        self.charge_budget(
            ContractCostType::ComputeSha256Hash,
            Some(bytes.len() as u64),
        )?;
        Ok(Sha256::digest(bytes).as_slice().to_vec())
    }

    pub fn sha256_hash_from_bytesobj_input(&self, x: BytesObject) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bytes: &ScBytes| {
            let hash = self.sha256_hash_from_bytes(bytes.as_slice())?;
            if hash.len() != 32 {
                return Err(self.err_general("incorrect hash size"));
            }
            Ok(hash)
        })
    }

    /// Converts a [`RawVal`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    // Notes on metering: covered by components.
    pub fn storage_key_from_rawval(&self, k: RawVal) -> Result<Rc<LedgerKey>, HostError> {
        Ok(Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: self.get_current_contract_id_internal()?,
            key: self.from_host_val(k)?,
        })))
    }

    pub(crate) fn storage_key_for_contract(&self, contract_id: Hash, key: ScVal) -> Rc<LedgerKey> {
        Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key,
        }))
    }

    pub fn storage_key_from_scval(&self, key: ScVal) -> Result<Rc<LedgerKey>, HostError> {
        Ok(Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: self.get_current_contract_id_internal()?,
            key,
        })))
    }

    // Notes on metering: covered by components.
    pub fn contract_data_key_from_rawval(&self, k: RawVal) -> Result<Rc<LedgerKey>, HostError> {
        let key_scval = self.from_host_val(k)?;
        match &key_scval {
            ScVal::LedgerKeyContractExecutable => {
                return Err(self.err_status_msg(
                    ScHostFnErrorCode::InputArgsInvalid,
                    "cannot update contract code",
                ));
            }
            ScVal::LedgerKeyNonce(_) => {
                return Err(self.err_status_msg(
                    ScHostFnErrorCode::InputArgsInvalid,
                    "cannot access internal nonce",
                ));
            }
            _ => (),
        };
        self.storage_key_from_scval(key_scval)
    }

    /// Converts a binary search result into a u64. `res` is `Some(index)`
    /// if the value was found at `index`, or `Err(index)` if the value was not found
    /// and would've needed to be inserted at `index`.
    /// Returns a Some(res_u64) where :
    /// - the high-32 bits is 0x0001 if element existed or 0x0000 if it didn't
    /// - the low-32 bits contains the u32 representation of the `index`
    /// Err(_) if the `index` fails to be converted to an u32.
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

    pub(crate) fn call_args_from_obj(&self, args: VecObject) -> Result<Vec<RawVal>, HostError> {
        self.visit_obj(args, |hv: &HostVec| {
            // Metering: free
            Ok(hv.iter().cloned().collect())
        })
    }

    // Metering: free?
    pub(crate) fn call_args_to_scvec(&self, args: VecObject) -> Result<ScVec, HostError> {
        self.visit_obj(args, |hv: &HostVec| self.rawvals_to_scvec(hv.iter()))
    }

    // Metering: free?
    pub(crate) fn rawvals_to_scvec(
        &self,
        raw_vals: std::slice::Iter<RawVal>,
    ) -> Result<ScVec, HostError> {
        Ok(ScVec(
            raw_vals
                .map(|v| {
                    ScVal::try_from_val(self, v)
                        .map_err(|_| self.err_general("couldn't convert RawVal"))
                })
                .collect::<Result<Vec<ScVal>, HostError>>()?
                .try_into()
                .map_err(|_| self.err_general("too many args"))?,
        ))
    }

    pub(crate) fn scvals_to_rawvals(&self, sc_vals: &[ScVal]) -> Result<Vec<RawVal>, HostError> {
        sc_vals
            .iter()
            .map(|scv| self.to_host_val(scv))
            .collect::<Result<Vec<RawVal>, HostError>>()
    }

    pub(crate) fn bytesobj_from_internal_contract_id(
        &self,
    ) -> Result<Option<BytesObject>, HostError> {
        if let Some(id) = self.get_current_contract_id_opt_internal()? {
            let obj = self.add_host_object::<ScBytes>(id.as_slice().to_vec().try_into()?)?;
            Ok(Some(obj))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn scbytes_from_vec(&self, v: Vec<u8>) -> Result<ScBytes, HostError> {
        Ok(ScBytes(v.try_into()?))
    }

    pub(crate) fn scbytes_from_slice(&self, slice: &[u8]) -> Result<ScBytes, HostError> {
        self.scbytes_from_vec(slice.to_vec())
    }

    pub(crate) fn scbytes_from_hash(&self, hash: &Hash) -> Result<ScBytes, HostError> {
        self.scbytes_from_slice(hash.as_slice())
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
    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        // Charges a single unit to for the RawVal -> ScVal conversion.
        // The actual conversion logic occurs in the `common` crate, which
        // translates a u64 into another form defined by the xdr.
        // For an `Object`, the actual structural conversion (such as byte
        // cloning) occurs in `from_host_obj` and is metered there.
        self.charge_budget(ContractCostType::ValXdrConv, None)?;
        ScVal::try_from_val(self, &val)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        // `ValXdrConv` is const cost in both cpu and mem. The input=0 will be ignored.
        self.charge_budget(ContractCostType::ValXdrConv, None)?;
        v.try_into_val(self)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn from_host_obj(&self, ob: impl Into<Object>) -> Result<ScValObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| {
                // This accounts for conversion of "primitive" objects (e.g U64)
                // and the "shell" of a complex object (ScMap). Any non-trivial
                // work such as byte cloning, has to be accounted for and
                // metered in indivial match arms.
                // `ValXdrConv` is const cost in both cpu and mem. The input=0 will be ignored.
                self.charge_budget(ContractCostType::ValXdrConv, None)?;
                let val = match ob {
                    None => {
                        return Err(self.err_status(ScHostObjErrorCode::UnknownReference));
                    }
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            metered_clone::charge_heap_alloc::<ScVal>(
                                vv.len() as u64,
                                self.as_budget(),
                            )?;
                            let sv = vv.iter().map(|e| self.from_host_val(*e)).collect::<Result<
                                Vec<ScVal>,
                                HostError,
                            >>(
                            )?;
                            ScVal::Vec(Some(ScVec(self.map_err(sv.try_into())?)))
                        }
                        HostObject::Map(mm) => {
                            metered_clone::charge_heap_alloc::<ScMapEntry>(
                                mm.len() as u64,
                                self.as_budget(),
                            )?;
                            let mut mv = Vec::with_capacity(mm.len());
                            for (k, v) in mm.iter(self)? {
                                let key = self.from_host_val(*k)?;
                                let val = self.from_host_val(*v)?;
                                mv.push(ScMapEntry { key, val });
                            }
                            ScVal::Map(Some(ScMap(self.map_err(mv.try_into())?)))
                        }
                        HostObject::U64(u) => ScVal::U64(*u),
                        HostObject::I64(i) => ScVal::I64(*i),
                        HostObject::TimePoint(tp) => {
                            ScVal::Timepoint(tp.metered_clone(self.as_budget())?)
                        }
                        HostObject::Duration(d) => {
                            ScVal::Duration(d.metered_clone(self.as_budget())?)
                        }
                        HostObject::U128(u) => ScVal::U128(UInt128Parts {
                            hi: int128_helpers::u128_hi(*u),
                            lo: int128_helpers::u128_lo(*u),
                        }),
                        HostObject::I128(i) => ScVal::I128(Int128Parts {
                            hi: int128_helpers::i128_hi(*i),
                            lo: int128_helpers::i128_lo(*i),
                        }),
                        HostObject::U256(u) => {
                            let (hi_hi, hi_lo, lo_hi, lo_lo) = u256_into_pieces(*u);
                            ScVal::U256(UInt256Parts {
                                hi_hi,
                                hi_lo,
                                lo_hi,
                                lo_lo,
                            })
                        }
                        HostObject::I256(i) => {
                            let (hi_hi, hi_lo, lo_hi, lo_lo) = i256_into_pieces(*i);
                            ScVal::I256(Int256Parts {
                                hi_hi,
                                hi_lo,
                                lo_hi,
                                lo_lo,
                            })
                        }
                        HostObject::Bytes(b) => ScVal::Bytes(b.metered_clone(self.as_budget())?),
                        HostObject::String(s) => ScVal::String(s.metered_clone(self.as_budget())?),
                        HostObject::Symbol(s) => ScVal::Symbol(s.metered_clone(self.as_budget())?),
                        HostObject::ContractExecutable(cc) => {
                            ScVal::ContractExecutable(cc.metered_clone(self.as_budget())?)
                        }
                        HostObject::Address(addr) => {
                            ScVal::Address(addr.metered_clone(self.as_budget())?)
                        }
                        HostObject::NonceKey(nk) => {
                            ScVal::LedgerKeyNonce(nk.metered_clone(self.as_budget())?)
                        }
                    },
                };
                Ok(ScValObject::unchecked_from_val(val))
            })
        }
    }

    pub(crate) fn to_host_obj(&self, ob: &ScValObjRef<'_>) -> Result<Object, HostError> {
        // `ValXdrConv` is const cost in both cpu and mem. The input=0 will be ignored.
        self.charge_budget(ContractCostType::ValXdrConv, None)?;
        let val: &ScVal = (*ob).into();
        match val {
            ScVal::Vec(Some(v)) => {
                metered_clone::charge_heap_alloc::<RawVal>(v.len() as u64, self.as_budget())?;
                let mut vv = Vec::with_capacity(v.len());
                for e in v.iter() {
                    vv.push(self.to_host_val(e)?)
                }
                Ok(self.add_host_object(HostVec::from_vec(vv)?)?.into())
            }
            ScVal::Map(Some(m)) => {
                metered_clone::charge_heap_alloc::<(RawVal, RawVal)>(
                    m.len() as u64,
                    self.as_budget(),
                )?;
                let mut mm = Vec::with_capacity(m.len());
                for pair in m.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.push((k, v))
                }
                Ok(self.add_host_object(HostMap::from_map(mm, self)?)?.into())
            }
            ScVal::Vec(None) | ScVal::Map(None) => {
                Err(self.err_status(ScHostValErrorCode::MissingObject))
            }
            ScVal::U64(u) => Ok(self.add_host_object(*u)?.into()),
            ScVal::I64(i) => Ok(self.add_host_object(*i)?.into()),
            ScVal::Timepoint(t) => Ok(self
                .add_host_object(t.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::Duration(d) => Ok(self
                .add_host_object(d.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::U128(u) => Ok(self
                .add_host_object(int128_helpers::u128_from_pieces(u.hi, u.lo))?
                .into()),
            ScVal::I128(i) => Ok(self
                .add_host_object(int128_helpers::i128_from_pieces(i.hi, i.lo))?
                .into()),
            ScVal::U256(u) => Ok(self
                .add_host_object(u256_from_pieces(u.hi_hi, u.hi_lo, u.lo_hi, u.lo_lo))?
                .into()),
            ScVal::I256(i) => Ok(self
                .add_host_object(i256_from_pieces(i.hi_hi, i.hi_lo, i.lo_hi, i.lo_lo))?
                .into()),
            ScVal::Bytes(b) => Ok(self
                .add_host_object(b.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::String(s) => Ok(self
                .add_host_object(s.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::Symbol(s) => Ok(self
                .add_host_object(s.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::ContractExecutable(cc) => Ok(self
                .add_host_object(cc.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::LedgerKeyNonce(_) => {
                Err(self.err_general("nonce keys aren't allowed to be used directly"))
            }
            ScVal::Address(addr) => Ok(self
                .add_host_object(addr.metered_clone(self.as_budget())?)?
                .into()),

            ScVal::Bool(_)
            | ScVal::Void
            | ScVal::Status(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::LedgerKeyContractExecutable => {
                Err(self.err_status(ScHostObjErrorCode::UnexpectedType))
            }
        }
    }
}
