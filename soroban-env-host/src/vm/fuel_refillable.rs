use crate::{
    budget::AsBudget,
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Host, HostError,
};

use wasmi::{Caller, Store};

pub(crate) trait FuelRefillable {
    // Returns the amount of fuel consumed by the VM since it was last filled
    // with `last_fuel` units of fuel. wasmi keeps track of "how much fuel is
    // left" rather than "how much fuel was consumed", so it needs to be told
    // the amount the VM was last filled with in order to calculate this.
    fn fuel_consumed(&self, last_fuel: u64) -> Result<u64, HostError>;

    // Returns the amount of fuel remaining in the VM.
    fn fuel_total(&self) -> Result<u64, HostError>;

    fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError>;

    fn reset_fuel(&mut self) -> Result<(), HostError>;

    fn is_clean(&self) -> Result<bool, HostError> {
        Ok(self.fuel_total()? == 0)
    }

    // Asserts that the VM has no fuel in it, then calculates the current amount
    // of VM fuel the host's current CPU budget represents, adds that fuel to
    // the VM and records the amount added as the host's "last VM fuel".
    fn add_fuel_to_vm(&mut self, host: &Host) -> Result<(), HostError> {
        if !self.is_clean()? {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InternalError,
                "VM fuel must be clean before we supply to it",
                &[],
            ));
        }
        let fuel = host.as_budget().get_wasmi_fuel_remaining()?;
        self.add_fuel(fuel)?;
        host.set_last_vm_fuel(fuel)
    }

    // Computes the amount of fuel consumed by the VM since the last call to
    // `add_fuel_to_vm`, and charges it to the host's CPU budget, logically
    // accounting for a "return" of the remainder that was _not_ consumed to the
    // host for further use.
    //
    // The host's "last VM fuel" is cleared _before_ anything fallible happens:
    // once the fuel has been returned there is no outstanding supply, so a
    // later return (eg. by the outer `Vm::metered_func_call` after a host
    // function failed before re-supplying fuel) must not charge for it again.
    fn return_fuel_to_host(&mut self, host: &Host) -> Result<(), HostError> {
        let last_fuel = host.get_last_vm_fuel()?;
        host.set_last_vm_fuel(0)?;
        let fuel_consumed = self.fuel_consumed(last_fuel)?;
        host.as_budget()
            .bulk_charge(ContractCostType::WasmInsnExec, fuel_consumed, None)?;
        self.reset_fuel()
    }
}

macro_rules! impl_refillable_for_store {
    ($store: ty) => {
        impl<'a> FuelRefillable for $store {
            fn fuel_consumed(&self, last_fuel: u64) -> Result<u64, HostError> {
                Ok(last_fuel.saturating_sub(FuelRefillable::fuel_total(self)?))
            }

            fn fuel_total(&self) -> Result<u64, HostError> {
                self.get_fuel().map_err(HostError::from)
            }

            fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError> {
                let existing_fuel = FuelRefillable::fuel_total(self)?;
                self.set_fuel(existing_fuel.saturating_add(fuel))
                    .map_err(HostError::from)
            }

            fn reset_fuel(&mut self) -> Result<(), HostError> {
                self.set_fuel(0).map_err(HostError::from)
            }
        }
    };
}
impl_refillable_for_store!(Store<Host>);
impl_refillable_for_store!(Caller<'a, Host>);
