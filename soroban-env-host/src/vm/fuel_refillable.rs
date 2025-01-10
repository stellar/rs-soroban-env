use crate::{
    budget::AsBudget,
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Host, HostError,
};

use soroban_env_common::Error;
use wasmi::{errors::FuelError, Caller, Store};

pub(crate) trait FuelRefillable {
    // Returns the amount of fuel consumed by the VM since the last call to
    // `reset_fuel` / `add_fuel`. This is somewhat error-prone but the idea is
    // that some VMs keep track of "how much fuel is left" and others keep track
    // of "how much fuel was consumed". The former needs to be provided with the
    // actual last fuel amount that the VM was filled with, in order to
    // calculate the consumed amount.
    fn fuel_consumed(&self, last_fuel: u64) -> Result<u64, HostError>;

    fn fuel_total(&self) -> Result<u64, HostError>;

    fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError>;

    fn reset_fuel(&mut self) -> Result<(), HostError>;

    fn is_clean(&self) -> Result<bool, HostError> {
        Ok(self.fuel_consumed(self.fuel_total()?)? == 0 && self.fuel_total()? == 0)
    }

    // Asserts that the VM has no fuel in it, then calculates the current amount
    // of VM fuel the host's current CPU budget represents, and adds that fuel
    // to the VM. Returns the amount of fuel added.
    fn add_fuel_to_vm(&mut self, host: &Host) -> Result<u64, HostError> {
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
        Ok(fuel)
    }

    // Computes the amount of fuel consumed by the VM since the last call to
    // `add_fuel_to_vm`, and charges it to the host's CPU budget, logically
    // accounting for a "return" of the remainder that was _not_ consumed to the
    // host for further use. Takes the last fuel amount supplied to the VM
    // (which was returned from `add_fuel_to_vm`) as an argument, in case the VM
    // does not keep track of its fuel consumption, only remaining balance.
    fn return_fuel_to_host(&mut self, host: &Host, last_fuel: u64) -> Result<(), HostError> {
        let fuel_consumed = self.fuel_consumed(last_fuel)?;
        host.as_budget()
            .bulk_charge(ContractCostType::WasmInsnExec, fuel_consumed, None)?;
        self.reset_fuel()
    }
}

macro_rules! impl_refillable_for_store {
    ($store: ty) => {
        impl<'a> FuelRefillable for $store {
            fn fuel_consumed(&self, _initial_fuel: u64) -> Result<u64, HostError> {
                self.fuel_consumed().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })
            }

            fn fuel_total(&self) -> Result<u64, HostError> {
                self.fuel_total().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })
            }

            fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError> {
                self.add_fuel(fuel)
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))
            }

            fn reset_fuel(&mut self) -> Result<(), HostError> {
                self.reset_fuel()
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))
            }
        }
    };
}
impl_refillable_for_store!(Store<Host>);
impl_refillable_for_store!(Caller<'a, Host>);

const VM_INTERNAL_ERROR: Error =
    Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::InternalError);

const WASMTIME_FUEL_FACTOR: u64 = 1;

macro_rules! impl_refillable_for_wasmtime_store {
    ($store: ty) => {
        impl<'a> FuelRefillable for $store {
            fn fuel_consumed(&self, initial_fuel: u64) -> Result<u64, HostError> {
                let fuel = self.fuel_total()?;
                Ok(initial_fuel.saturating_sub(fuel))
            }

            fn fuel_total(&self) -> Result<u64, HostError> {
                self.get_fuel()
                    .map(|fuel| fuel.saturating_div(WASMTIME_FUEL_FACTOR))
                    .map_err(|_| HostError::from(VM_INTERNAL_ERROR))
            }

            fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError> {
                let existing_fuel = self.fuel_total()?;
                let new_fuel = existing_fuel
                    .saturating_add(fuel)
                    .saturating_mul(WASMTIME_FUEL_FACTOR);
                self.set_fuel(new_fuel)
                    .map_err(|_| HostError::from(VM_INTERNAL_ERROR))
            }

            fn reset_fuel(&mut self) -> Result<(), HostError> {
                self.set_fuel(0)
                    .map_err(|_| HostError::from(VM_INTERNAL_ERROR))
            }
        }
    };
}
impl_refillable_for_wasmtime_store!(wasmtime::Store<Host>);
impl_refillable_for_wasmtime_store!(wasmtime::Caller<'a, Host>);
