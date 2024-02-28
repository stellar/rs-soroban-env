use std::rc::Rc;

use crate::host_object::MemHostObjectType;
use crate::{
    budget::{AsBudget, DepthLimiter},
    err,
    host::metered_clone::{
        charge_shallow_copy, MeteredAlloc, MeteredClone, MeteredContainer, MeteredIterator,
    },
    host_object::{HostMap, HostObject, HostVec},
    num::{i256_from_pieces, i256_into_pieces, u256_from_pieces, u256_into_pieces},
    xdr::{
        self, int128_helpers, AccountId, ContractCostType, ContractDataDurability, Hash,
        Int128Parts, Int256Parts, LedgerKey, LedgerKeyContractData, ScAddress, ScBytes,
        ScErrorCode, ScErrorType, ScMap, ScMapEntry, ScSymbol, ScVal, ScVec, UInt128Parts,
        UInt256Parts, Uint256, VecM,
    },
    AddressObject, BytesObject, Convert, Host, HostError, Object, ScValObjRef, ScValObject, Symbol,
    SymbolObject, TryFromVal, TryIntoVal, U32Val, Val, VecObject,
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
                ScErrorCode::ArithDomain,
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
        static_assertions::assert_eq_size!([u8; 32], Hash);
        self.fixed_length_bytes_from_bytesobj_input::<Hash, 32>(name, hash)
    }

    pub(crate) fn u256_from_bytesobj_input(
        &self,
        name: &'static str,
        u256: BytesObject,
    ) -> Result<Uint256, HostError> {
        static_assertions::assert_eq_size!([u8; 32], Uint256);
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

    pub(crate) fn storage_key_for_address(
        &self,
        contract: ScAddress,
        key: ScVal,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        Rc::metered_new(
            LedgerKey::ContractData(LedgerKeyContractData {
                contract,
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

    /// Converts a [`Val`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    // Notes on metering: covered by components.
    pub(crate) fn storage_key_from_val(
        &self,
        k: Val,
        durability: ContractDataDurability,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let key_scval = self.from_host_val(k)?;
        self.storage_key_from_scval(key_scval, durability)
    }

    /// Converts a binary search result into a u64. `res` is `Some(index)` if
    /// the value was found at `index`, or `Err(index)` if the value was not
    /// found and would've needed to be inserted at `index`. Returns a
    /// Some(res_u64) where:
    /// - The high 32 bits is 0x0000_0001 if element existed or 0x0000_0000 if
    ///   it didn't
    /// - The low 32 bits contains the u32 representation of the `index` Err(_)
    ///   if the `index` fails to be converted to an u32.
    pub(crate) fn u64_from_binary_search_result(
        &self,
        res: Result<usize, usize>,
    ) -> Result<u64, HostError> {
        match res {
            Ok(u) => {
                let v = self.usize_to_u32(u)?;
                Ok(u64::from(v) | (1_u64 << u32::BITS))
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

    // Metering: covered by vals_to_vec
    pub(crate) fn vecobject_to_scval_vec(&self, args: VecObject) -> Result<VecM<ScVal>, HostError> {
        self.visit_obj(args, |hv: &HostVec| self.vals_to_scval_vec(hv.as_slice()))
    }

    pub(crate) fn vals_to_scval_vec(&self, vals: &[Val]) -> Result<VecM<ScVal>, HostError> {
        vals.iter()
            .map(|v| self.from_host_val(*v))
            .metered_collect::<Result<Vec<ScVal>, HostError>>(self)??
            .try_into()
            .map_err(|_| {
                err!(
                    self,
                    (ScErrorType::Object, ScErrorCode::ExceededLimit),
                    "vector size limit exceeded",
                    vals.len()
                )
            })
    }

    pub(crate) fn scvals_to_val_vec(&self, scvals: &[ScVal]) -> Result<Vec<Val>, HostError> {
        scvals
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

    pub fn scaddress_from_address(&self, address: AddressObject) -> Result<ScAddress, HostError> {
        self.visit_obj(address, |addr: &ScAddress| addr.metered_clone(self))
    }

    pub(crate) fn scsymbol_from_symbol(&self, symbol: Symbol) -> Result<ScSymbol, HostError> {
        if let Ok(sobj) = SymbolObject::try_from(symbol) {
            self.visit_obj(sobj, |sym: &ScSymbol| sym.metered_clone(self))
        } else {
            self.map_err(ScSymbol::try_from_val(self, &symbol))
        }
    }

    pub(crate) fn host_map_to_scmap(&self, map: &HostMap) -> Result<ScMap, HostError> {
        let mut mv = Vec::<ScMapEntry>::with_metered_capacity(map.len(), self)?;
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
    pub(crate) fn check_val_representable_scval(&self, scval: &ScVal) -> Result<(), HostError> {
        if Val::can_represent_scval(&scval) {
            Ok(())
        } else {
            Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InternalError,
                "unexpected non-Val-representable ScVal type",
                &[Val::from_u32(scval.discriminant() as u32).into()],
            ))
        }
    }

    pub(crate) fn from_host_val(&self, val: Val) -> Result<ScVal, HostError> {
        // This is the depth limit checkpoint for `Val`->`ScVal` conversion.
        // Metering of val conversion happens only if an object is encountered,
        // and is done inside `from_host_obj`.
        let _span = tracy_span!("Val to ScVal");
        let scval = self.budget_cloned().with_limited_depth(|_| {
            ScVal::try_from_val(self, &val)
                .map_err(|cerr| self.error(cerr, "failed to convert host value to ScVal", &[val]))
        })?;
        // This is a check of internal logical consistency: we came _from_ a Val
        // so the ScVal definitely should have been representable.
        self.check_val_representable_scval(&scval)?;
        Ok(scval)
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<Val, HostError> {
        let _span = tracy_span!("ScVal to Val");
        // This is the depth limit checkpoint for `ScVal`->`Val` conversion.
        // Metering of val conversion happens only if an object is encountered,
        // and is done inside `to_host_obj`.
        self.budget_cloned().with_limited_depth(|_| {
            v.try_into_val(self)
                .map_err(|cerr| self.error(cerr, "failed to convert ScVal to host value", &[]))
        })
    }

    // Version of `to_host_val` for the internal cases where the value has to
    // be valid by construction (e.g. read from ledger).
    pub(crate) fn to_valid_host_val(&self, v: &ScVal) -> Result<Val, HostError> {
        self.to_host_val(v).map_err(|e| {
            if e.error.is_type(ScErrorType::Budget) {
                e
            } else {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InternalError,
                    "unexpected non-Val-representable ScVal in internal conversion",
                    &[],
                )
            }
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
                    HostObject::Address(addr) => ScVal::Address(addr.metered_clone(self)?),
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
            // since most of them happen in the "common" crate with no access to the host.
            ScVal::Vec(Some(v)) => {
                let mut vv = Vec::<Val>::with_metered_capacity(v.len(), self)?;
                for e in v.iter() {
                    vv.push(self.to_host_val(e)?)
                }
                Ok(self.add_host_object(HostVec::from_vec(vv)?)?.into())
            }
            ScVal::Map(Some(m)) => {
                let mut mm = Vec::<(Val, Val)>::with_metered_capacity(m.len(), self)?;
                for pair in m.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.push((k, v))
                }
                Ok(self.add_host_object(HostMap::from_map(mm, self)?)?.into())
            }
            ScVal::Vec(None) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "ScVal::Vec body missing",
                &[],
            )),
            ScVal::Map(None) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "ScVal::Map body missing",
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
            // Similarly to `ScMap`, not every `SCSymbol` XDR is valid. Thus it has to be
            // created with the respective fallible conversion method.
            ScVal::Symbol(s) => Ok(self
                .add_host_object(ScSymbol::try_from_bytes(
                    self,
                    s.0.metered_clone(self.as_budget())?.into(),
                )?)?
                .into()),

            ScVal::Address(addr) => Ok(self.add_host_object(addr.metered_clone(self)?)?.into()),

            // None of the following cases should have made it into this function, they
            // are excluded by the ScValObjRef::classify function.
            ScVal::Bool(_)
            | ScVal::Void
            | ScVal::Error(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::LedgerKeyNonce(_)
            | ScVal::ContractInstance(_)
            | ScVal::LedgerKeyContractInstance => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InternalError,
                "converting ScValObjRef on non-object ScVal type",
                &[],
            )),
        }
    }
}
