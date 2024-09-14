use crate::{
    budget::AsBudget,
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Host, HostError,
};

use wasmi::{errors::FuelError, Caller, Store};

pub(crate) trait FuelRefillable {
    fn fuel_consumed(&self) -> Result<u64, HostError>;

    fn fuel_total(&self) -> Result<u64, HostError>;

    fn add_fuel(&mut self, fuel: u64) -> Result<(), HostError>;

    fn reset_fuel(&mut self) -> Result<(), HostError>;

    fn is_clean(&self) -> Result<bool, HostError> {
        Ok(self.fuel_consumed()? == 0 && self.fuel_total()? == 0)
    }

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
        self.add_fuel(fuel)
    }

    fn return_fuel_to_host(&mut self, host: &Host) -> Result<(), HostError> {
        let fuel = self.fuel_consumed()?;
        host.as_budget()
            .bulk_charge(ContractCostType::WasmInsnExec, fuel, None)?;
        self.reset_fuel()
    }
}

macro_rules! impl_refillable_for_store {
    ($store: ty) => {
        impl<'a> FuelRefillable for $store {
            fn fuel_consumed(&self) -> Result<u64, HostError> {
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
