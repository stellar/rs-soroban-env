use crate::events::DebugError;
use crate::xdr::{ScHostFnErrorCode, ScHostObjErrorCode, ScHostValErrorCode};
use crate::{Host, HostError, Object, RawVal};
use stellar_contract_env_common::xdr::{Hash, Uint256};

impl Host {
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

    pub(crate) fn hash_from_rawval_input(
        &self,
        name: &'static str,
        hash: Object,
    ) -> Result<Hash, HostError> {
        self.visit_obj(hash, |bin: &Vec<u8>| {
            match <[u8; 32]>::try_from(bin.as_slice()) {
                Ok(arr) => Ok(Hash(arr)),
                Err(cvt) => Err(self.err(
                    DebugError::new(ScHostObjErrorCode::ContractHashWrongLength)
                        .msg("hash {} has wrong length for input {}")
                        .arg(hash.to_raw())
                        .arg(name),
                )),
            }
        })
    }
}
