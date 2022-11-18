use super::metered_bigint::MeteredBigInt;
use super::metered_clone::MeteredClone;
use crate::xdr::{
    Hash, LedgerKey, LedgerKeyContractData, ScBigInt, ScHostFnErrorCode, ScHostObjErrorCode,
    ScHostValErrorCode, ScObject, ScStatic, ScVal, ScVec, Uint256,
};
use crate::{
    budget::CostType,
    events::{DebugError, CONTRACT_EVENT_TOPICS_LIMIT},
    host_object::{HostObject, HostVec},
    Host, HostError, Object, RawVal,
};
use ed25519_dalek::{PublicKey, Signature, SIGNATURE_LENGTH};
use num_bigint::Sign;
use sha2::{Digest, Sha256};
use soroban_env_common::xdr::AccountId;

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

    pub(crate) fn to_account_id(&self, a: Object) -> Result<AccountId, HostError> {
        self.visit_obj(a, |account_id: &AccountId| {
            Ok(account_id.metered_clone(&self.0.budget)?)
        })
    }

    pub(crate) fn to_u256_from_account(
        &self,
        account_id: &AccountId,
    ) -> Result<Uint256, HostError> {
        let crate::xdr::PublicKey::PublicKeyTypeEd25519(ed25519) =
            account_id.metered_clone(&self.0.budget)?.0;
        Ok(ed25519)
    }

    pub(crate) fn to_u256(&self, a: Object) -> Result<Uint256, HostError> {
        self.visit_obj(a, |bytes: &Vec<u8>| {
            self.charge_budget(CostType::BytesClone, 32)?;
            bytes
                .try_into()
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
        self.fixed_length_bytes_from_obj_input::<Hash, 32>(name, hash)
    }

    pub(crate) fn uint256_from_obj_input(
        &self,
        name: &'static str,
        u256: Object,
    ) -> Result<Uint256, HostError> {
        self.fixed_length_bytes_from_obj_input::<Uint256, 32>(name, u256)
    }

    pub(crate) fn signature_from_obj_input(
        &self,
        name: &'static str,
        sig: Object,
    ) -> Result<Signature, HostError> {
        self.fixed_length_bytes_from_obj_input::<Signature, SIGNATURE_LENGTH>(name, sig)
    }

    fn fixed_length_bytes_from_obj_input<T, const N: usize>(
        &self,
        name: &'static str,
        obj: Object,
    ) -> Result<T, HostError>
    where
        T: From<[u8; N]>,
    {
        self.visit_obj(obj, |bytes: &Vec<u8>| {
            match <[u8; N]>::try_from(bytes.as_slice()) {
                Ok(arr) => {
                    self.charge_budget(CostType::BytesClone, N as u64)?;
                    Ok(arr.into())
                }
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
        self.visit_obj(k, |bytes: &Vec<u8>| {
            self.charge_budget(CostType::ComputeEd25519PubKey, bytes.len() as u64)?;
            PublicKey::from_bytes(bytes).map_err(|_| {
                self.err_status_msg(ScHostObjErrorCode::UnexpectedType, "invalid public key")
            })
        })
    }

    pub fn sha256_hash_from_bytes_input(&self, x: Object) -> Result<Vec<u8>, HostError> {
        self.visit_obj(x, |bytes: &Vec<u8>| {
            self.charge_budget(CostType::ComputeSha256Hash, bytes.len() as u64)?;
            let hash = Sha256::digest(bytes).as_slice().to_vec();
            if hash.len() != 32 {
                return Err(self.err_general("incorrect hash size"));
            }
            Ok(hash)
        })
    }

    /// Converts a [`RawVal`] to an [`ScVal`] and combines it with the currently-executing
    /// [`ContractID`] to produce a [`Key`], that can be used to access ledger [`Storage`].
    // Notes on metering: covered by components.
    pub fn storage_key_from_rawval(&self, k: RawVal) -> Result<LedgerKey, HostError> {
        Ok(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id: self.get_current_contract_id()?,
            key: self.from_host_val(k)?,
        }))
    }

    // Notes on metering: covered by components.
    pub fn contract_data_key_from_rawval(&self, k: RawVal) -> Result<LedgerKey, HostError> {
        if self.from_host_val(k)? == ScVal::Static(ScStatic::LedgerKeyContractCode) {
            return Err(self.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "cannot update contract code",
            ));
        }
        self.storage_key_from_rawval(k)
    }

    pub(crate) fn scobj_from_bigint(&self, bi: &MeteredBigInt) -> Result<ScObject, HostError> {
        let (sign, data) = bi.to_bytes_be()?;
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
        self.validate_event_topic(topic)?;
        self.from_host_val(topic)
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

    pub(crate) fn bigint_sign_from_rawval(&self, sign: RawVal) -> Result<Sign, HostError> {
        match i32::try_from(sign) {
            Ok(s) => match s {
                -1 => Ok(Sign::Minus),
                0 => Ok(Sign::NoSign),
                1 => Ok(Sign::Plus),
                _ => Err(self.err(
                    DebugError::new(ScHostFnErrorCode::InputArgsInvalid)
                        .msg("invalid sign {}, expect -1, 0 or 1")
                        .arg(sign),
                )),
            },
            Err(cvt) => Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
                    .msg("unexpected RawVal {} for sign, need I32")
                    .arg(sign),
            )),
        }
    }

    pub(crate) fn bigint_radix_from_rawval(&self, radix: RawVal) -> Result<u32, HostError> {
        match u32::try_from(radix) {
            Ok(r) => match r {
                2..=256 => Ok(r),
                _ => Err(self.err(
                    DebugError::new(ScHostFnErrorCode::InputArgsInvalid)
                        .msg("invalid radix {}, expected range 2..=256")
                        .arg(radix),
                )),
            },
            Err(cvt) => Err(self.err(
                DebugError::new(ScHostFnErrorCode::InputArgsWrongType)
                    .msg("unexpected RawVal {} for radix, need U32")
                    .arg(radix),
            )),
        }
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
                let v = self.usize_to_u32(u, "outside range")?;
                Ok(u64::from(v) | (1 << u32::BITS))
            }
            Err(u) => {
                let v = self.usize_to_u32(u, "outside range")?;
                Ok(u64::from(v))
            }
        }
    }

    pub(crate) fn call_args_from_obj(&self, args: Object) -> Result<Vec<RawVal>, HostError> {
        self.visit_obj(args, |hv: &HostVec| {
            // Metering: free
            Ok(hv.iter().map(|a| a.to_raw()).collect())
        })
    }
}
