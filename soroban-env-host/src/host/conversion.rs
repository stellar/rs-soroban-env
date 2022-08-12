use crate::events::DebugError;
use crate::xdr::{ScHostFnErrorCode, ScHostObjErrorCode, ScHostValErrorCode};
use crate::{Host, HostError, Object, RawVal};
use ed25519_dalek::{PublicKey, Signature, SIGNATURE_LENGTH};
use sha2::{Digest, Sha256};
use soroban_env_common::xdr::{Hash, LedgerKey, LedgerKeyContractData, ScStatic, ScVal, Uint256};

impl Host {
    pub(crate) fn usize_to_u32(&self, u: usize, msg: &'static str) -> Result<u32, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v),
            Err(_) => Err(self.err_general(msg)), // FIXME: need error status
        }
    }

    pub(crate) fn usize_to_rawval_u32(&self, u: usize) -> Result<RawVal, HostError> {
        match u32::try_from(u) {
            Ok(v) => Ok(v.into()),
            Err(_) => Err(self.err_status(ScHostValErrorCode::U32OutOfRange)),
        }
    }

    pub(crate) fn usize_from_rawval_u32_input(
        &self,
        name: &'static str,
        r: RawVal,
    ) -> Result<usize, HostError> {
        self.u32_from_rawval_input(name, r).map(|u| u as usize)
    }

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
}
