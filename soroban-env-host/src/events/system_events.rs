use soroban_env_common::{xdr::ScContractExecutable, EnvBase, Symbol, TryFromVal, TryIntoVal};
use soroban_native_sdk_macros::contracttype;

use crate::{native_contract::base_types::BytesN, Host, HostError};

const CONTRACT_EXECUTABLE_UPDATE_TOPIC: &str = "executable_update";

#[contracttype]
enum ContractExecutable {
    WasmRef(BytesN<32>),
    Token,
}

impl ContractExecutable {
    fn from_xdr(host: &Host, xdr: &ScContractExecutable) -> Result<Self, HostError> {
        match xdr {
            ScContractExecutable::WasmRef(wasm_hash) => Ok(ContractExecutable::WasmRef(
                BytesN::<32>::from_slice(host, &wasm_hash.0)?,
            )),
            ScContractExecutable::Token => Ok(ContractExecutable::Token),
        }
    }
}

impl Host {
    // Emits a system event for updating the contract executable.
    // The only event topic is "executable_update" and the data contains
    // a vector of [old_executable, new_executable] encoded as contract types
    // (`ContractExecutable` above).
    pub(crate) fn emit_update_contract_event(
        &self,
        old_executable: &ScContractExecutable,
        new_executable: &ScContractExecutable,
    ) -> Result<(), HostError> {
        self.system_event(
            self.vec_new_from_slice(&[
                Symbol::try_from_val(self, &CONTRACT_EXECUTABLE_UPDATE_TOPIC)?.into(),
                ContractExecutable::from_xdr(self, old_executable)?.try_into_val(self)?,
                ContractExecutable::from_xdr(self, new_executable)?.try_into_val(self)?,
            ])?,
            self.vec_new_from_slice(&[])?.into(),
        )?;
        Ok(())
    }
}
