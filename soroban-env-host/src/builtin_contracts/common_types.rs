use super::base_types::{Address, BytesN, String};
use crate::{
    host::metered_clone::MeteredClone, xdr, Host, HostError, StringObject, TryFromVal, TryIntoVal,
};
use soroban_builtin_sdk_macros::contracttype;

#[derive(Clone)]
#[contracttype]
pub(crate) struct ContractExecutableRef {
    pub(crate) owner: Address,
    pub(crate) tag: String,
}

impl ContractExecutableRef {
    fn from_xdr(
        host: &Host,
        external_ref: &xdr::ContractExecutableExternalRef,
    ) -> Result<Self, HostError> {
        let owner = Address::try_from_val(host, &external_ref.executable_owner)?;
        let tag_obj = host.add_host_object(external_ref.tag.metered_clone(host)?)?;
        let tag = String::try_from_val(host, &tag_obj)?;
        Ok(ContractExecutableRef { owner, tag })
    }

    pub(crate) fn to_xdr(
        &self,
        host: &Host,
    ) -> Result<xdr::ContractExecutableExternalRef, HostError> {
        let executable_owner = self.owner.to_sc_address()?;
        let tag_obj: StringObject = self.tag.clone().into();
        let tag = host.visit_obj(tag_obj, |s: &xdr::ScString| s.metered_clone(host))?;
        Ok(xdr::ContractExecutableExternalRef {
            executable_owner,
            tag,
        })
    }
}

#[derive(Clone)]
#[contracttype]
pub(crate) enum ContractExecutable {
    Wasm(BytesN<32>),
    StellarAsset,
    ExternalRef(ContractExecutableRef),
}

impl ContractExecutable {
    pub(crate) fn from_xdr(host: &Host, xdr: &xdr::ContractExecutable) -> Result<Self, HostError> {
        match xdr {
            xdr::ContractExecutable::Wasm(wasm_hash) => Ok(ContractExecutable::Wasm(
                BytesN::<32>::from_slice(host, &wasm_hash.0)?,
            )),
            xdr::ContractExecutable::StellarAsset => Ok(ContractExecutable::StellarAsset),
            xdr::ContractExecutable::ExternalRef(external_ref) => {
                Ok(ContractExecutable::ExternalRef(
                    ContractExecutableRef::from_xdr(host, external_ref)?,
                ))
            }
        }
    }

    pub(crate) fn to_xdr(&self, host: &Host) -> Result<xdr::ContractExecutable, HostError> {
        match self {
            ContractExecutable::Wasm(b) => Ok(xdr::ContractExecutable::Wasm(
                host.hash_from_bytesobj_input("wasm_ref", b.as_object())?,
            )),
            ContractExecutable::ExternalRef(external_ref) => Ok(
                xdr::ContractExecutable::ExternalRef(external_ref.to_xdr(host)?),
            ),
            ContractExecutable::StellarAsset => Ok(xdr::ContractExecutable::StellarAsset),
        }
    }
}

#[contracttype]
pub(crate) enum AddressExecutable {
    Wasm(BytesN<32>),
    StellarAsset,
    Account,
}

impl AddressExecutable {
    pub(crate) fn from_contract_executable_xdr(
        host: &Host,
        xdr: &xdr::ContractExecutable,
    ) -> Result<Self, HostError> {
        match xdr {
            xdr::ContractExecutable::Wasm(wasm_hash) => Ok(AddressExecutable::Wasm(
                BytesN::<32>::from_slice(host, &wasm_hash.0)?,
            )),
            xdr::ContractExecutable::StellarAsset => Ok(AddressExecutable::StellarAsset),
            xdr::ContractExecutable::ExternalRef(external_ref) => {
                let wasm_hash = host.resolve_external_ref_wasm_hash(external_ref)?;
                Ok(AddressExecutable::Wasm(BytesN::<32>::from_slice(
                    host,
                    &wasm_hash.0,
                )?))
            }
        }
    }
}
