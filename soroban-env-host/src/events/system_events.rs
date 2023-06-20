use crate::{native_contract::common_types::ContractExecutable, Host, HostError};
use soroban_env_common::{xdr, EnvBase, Symbol, TryFromVal, TryIntoVal};

const CONTRACT_EXECUTABLE_UPDATE_TOPIC: &str = "executable_update";

impl Host {
    // Emits a system event for updating the contract executable.
    // The only event topic is "executable_update" and the data contains
    // a vector of [old_executable, new_executable] encoded as contract types
    // (`ContractExecutable` above).
    pub(crate) fn emit_update_contract_event(
        &self,
        old_executable: &xdr::ContractExecutable,
        new_executable: &xdr::ContractExecutable,
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
