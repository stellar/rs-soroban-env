use crate::xdr::{
    Hash, LedgerKey, LedgerKeyContractData, ScBigInt, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostValErrorCode, ScObject, ScStatic, ScVal, ScVec, Uint256,
};
use crate::{
    budget::CostType,
    events::{DebugError, CONTRACT_EVENT_TOPICS_LIMIT, TOPIC_BYTES_LENGTH_LIMIT},
    host_object::HostObject,
    Host, HostError, Object, RawVal, Tag,
};
use ed25519_dalek::{PublicKey, Signature, SIGNATURE_LENGTH};
use num_bigint::{BigInt, Sign};
use sha2::{Digest, Sha256};

impl Host {
    // Notes on metering: free
    pub(crate) fn usize_to_u32(&self, u: usize, msg: &'static str) -> Result<u32, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v),
            Err(_) => Err(self.err_general(msg)), // FIXME: need error status
        }
    }

    // Notes on metering: free
    pub(crate) fn usize_to_rawval_u32(&self, u: usize) -> Result<RawVal, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v.into()),
            Err(_) => Err(self.err_status(ScHostValErrorCode::U32OutOfRange)),
        }
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

    pub(crate) fn to_u256(&self, a: Object) -> Result<Uint256, HostError> {
        self.visit_obj(a, |bin: &Vec<u8>| {
            bin.try_into()
                .map_err(|_| self.err_general("bad u256 length"))
        })
    }

    // Notes on metering: free
    pub(crate) fn u8_from_rawval_input(
        &self,
        name: &'static str,
        r: RawVal,
    ) -> Result<u8, HostError> {
        let u = self.u32_from_rawval_input(name, r)?;
        match u8::try_from(u) {
            Ok(v) => Ok(v),
            Err(cvt) => Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
                    .msg("unexpected RawVal {} for input '{}', need u32 no greater than 255")
                    .arg(r)
                    .arg(name),
            )),
        }
    }

    pub(crate) fn hash_from_obj_input(
        &self,
        name: &'static str,
        hash: Object,
    ) -> Result<Hash, HostError> {
        self.fixed_length_binary_from_obj_input::<Hash, 32>(name, hash)
    }

    pub(crate) fn uint256_from_obj_input(
        &self,
        name: &'static str,
        u256: Object,
    ) -> Result<Uint256, HostError> {
        self.fixed_length_binary_from_obj_input::<Uint256, 32>(name, u256)
    }

    pub(crate) fn signature_from_obj_input(
        &self,
        name: &'static str,
        sig: Object,
    ) -> Result<Signature, HostError> {
        self.fixed_length_binary_from_obj_input::<Signature, SIGNATURE_LENGTH>(name, sig)
    }

    fn fixed_length_binary_from_obj_input<T, const N: usize>(
        &self,
        name: &'static str,
        obj: Object,
    ) -> Result<T, HostError>
    where
        T: From<[u8; N]>,
    {
        self.visit_obj(obj, |bin: &Vec<u8>| {
            match <[u8; N]>::try_from(bin.as_slice()) {
                Ok(arr) => Ok(arr.into()),
                Err(cvt) => Err(self.err(
                    DebugError::new(ScHostObjErrorCode::ContractHashWrongLength) // TODO: this should be renamed to be more generic
                        .msg("{} {} has wrong length for input {}")
                        .arg(std::any::type_name::<T>())
                        .arg(obj.to_raw())
                        .arg(name),
                )),
            }
        })
    }

    pub fn ed25519_pub_key_from_obj_input(&self, k: Object) -> Result<PublicKey, HostError> {
        self.visit_obj(k, |bin: &Vec<u8>| {
            PublicKey::from_bytes(bin).map_err(|_| {
                self.err_status_msg(ScHostObjErrorCode::UnexpectedType, "invalid public key")
            })
        })
    }

    pub fn sha256_hash_from_binary_input(&self, x: Object) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bin: &Vec<u8>| {
            let hash = Sha256::digest(bin).as_slice().to_vec();
            if hash.len() != 32 {
                return Err(self.err_general("incorrect hash size"));
            }
            Ok(hash)
        })
    }

    /// Converts a [`RawVal`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    pub fn storage_key_from_rawval(&self, k: RawVal) -> Result<LedgerKey, HostError> {
        Ok(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: self.get_current_contract_id()?,
            key: self.from_host_val(k)?,
        }))
    }

    pub fn contract_data_key_from_rawval(&self, k: RawVal) -> Result<LedgerKey, HostError> {
        if self.from_host_val(k)? == ScVal::Static(ScStatic::LedgerKeyContractCode) {
            return Err(self.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "cannot update contract code",
            ));
        }
        self.storage_key_from_rawval(k)
    }

    // TODO: impl a `TryFrom` trait once the "metered_" class is ready
    pub(crate) fn scobj_from_bigint(&self, bi: &BigInt) -> Result<ScObject, HostError> {
        let (sign, data) = bi.to_bytes_be();
        match sign {
            Sign::Minus => Ok(ScObject::BigInt(ScBigInt::Negative(
                self.map_err(data.try_into())?,
            ))),
            Sign::NoSign => Ok(ScObject::BigInt(ScBigInt::Zero)),
            Sign::Plus => Ok(ScObject::BigInt(ScBigInt::Positive(
                self.map_err(data.try_into())?,
            ))),
        }
    }

    fn event_topic_from_rawval(&self, topic: RawVal) -> Result<ScVal, HostError> {
        self.charge_budget(CostType::ValXdrConv, 1)?;
        if topic.is_u63() {
            Ok(ScVal::U63(unsafe { topic.unchecked_as_u63() }))
        } else {
            match topic.get_tag() {
                Tag::Object => {
                    unsafe {
                        self.unchecked_visit_val_obj(topic, |ob| {
                            // charge budget
                            let sco = match ob {
                                None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                                Some(ho) => match ho {
                                    // TODO: use more event-specific error codes than `UnexpectedType`
                                    HostObject::Vec(_)
                                    | HostObject::Map(_)
                                    | HostObject::ContractCode(_) => {
                                        Err(self.err_status(ScHostObjErrorCode::UnexpectedType))
                                    }
                                    HostObject::Bin(b) => {
                                        if b.len() > TOPIC_BYTES_LENGTH_LIMIT {
                                            // TODO: use more event-specific error codes than `UnexpectedType`.
                                            // Something like "topic binary exceeds length limit"
                                            return Err(
                                                self.err_status(ScHostObjErrorCode::UnexpectedType)
                                            );
                                        }
                                        Ok(ScObject::Bytes(self.map_err(b.clone().try_into())?))
                                    }
                                    HostObject::U64(u) => Ok(ScObject::U64(*u)),
                                    HostObject::I64(i) => Ok(ScObject::I64(*i)),
                                    HostObject::BigInt(bi) => self.scobj_from_bigint(bi),
                                    HostObject::Hash(h) => Ok(ScObject::Hash(h.clone())),
                                    HostObject::PublicKey(pk) => {
                                        Ok(ScObject::PublicKey(pk.clone()))
                                    }
                                },
                            }?;
                            Ok(ScVal::Object(Some(sco)))
                        })
                    }
                }
                _ => self.from_host_val(topic),
            }
        }
    }

    pub(crate) fn event_topics_from_host_obj(&self, topics: Object) -> Result<ScVec, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(topics.into(), |ob| {
                self.charge_budget(CostType::ValXdrConv, 1)?;
                match ob {
                    None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            if vv.len() > CONTRACT_EVENT_TOPICS_LIMIT {
                                // TODO: proper error code "event topics exceeds count limit"
                                return Err(self.err_status(ScHostObjErrorCode::UnknownError));
                            }
                            let mut sv = Vec::new();
                            for e in vv.iter() {
                                sv.push(self.event_topic_from_rawval(e.val)?);
                            }
                            Ok(ScVec(self.map_err(sv.try_into())?))
                        }
                        _ => Err(self.err_status(ScHostObjErrorCode::UnexpectedType)),
                    },
                }
            })
        }
    }
}
