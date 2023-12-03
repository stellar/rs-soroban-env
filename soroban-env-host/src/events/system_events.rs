use crate::{builtin_contracts::common_types::ContractExecutable, Host, HostError};
use soroban_env_common::{xdr, EnvBase, Symbol, TryFromVal, TryIntoVal, Val, VecObject};

const CONTRACT_EXECUTABLE_UPDATE_TOPIC: &str = "executable_update";

impl Host {
    pub(crate) fn system_event(&self, topics: VecObject, data: Val) -> Result<(), HostError> {
        self.record_contract_event(xdr::ContractEventType::System, topics, data)?;
        Ok(())
    }

    // Emits a system event for updating the contract executable. The topic
    // vector contains the symbol "executable_update" followed by the
    // old_executable and new_executable, encoded as contract types
    // (`ContractExecutable` above). The event data is empty.
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
