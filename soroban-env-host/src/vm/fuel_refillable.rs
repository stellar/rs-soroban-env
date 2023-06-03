use crate::{
    budget::AsBudget,
    xdr::{ScErrorCode, ScErrorType},
    Host, HostError,
};

use wasmi::{errors::FuelError, Caller, Store};

pub(crate) trait FuelRefillable {
    fn fuels_consumed(&self) -> Result<(u64, u64), HostError>;

    fn fuels_total(&self) -> Result<(u64, u64), HostError>;

    fn add_fuels(&mut self, cpu: u64, mem: u64) -> Result<(), HostError>;

    fn reset_fuels(&mut self) -> Result<(), HostError>;

    fn is_clean(&self) -> Result<bool, HostError> {
        Ok(self.fuels_consumed()? == (0, 0) && self.fuels_total()? == (0, 0))
    }

    fn fill_fuels(&mut self, host: &Host) -> Result<(), HostError> {
        if !self.is_clean()? {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InternalError,
                "VM fuel must be clean before we supply to it",
                &[],
            ));
        }
        let (cpu, mem) = host.as_budget().get_fuels_budget()?;
        self.add_fuels(cpu, mem)
    }

    fn return_fuels(&mut self, host: &Host) -> Result<(), HostError> {
        let (cpu, mem) = self.fuels_consumed()?;
        host.as_budget().apply_wasmi_fuels(cpu, mem)?;
        self.reset_fuels()
    }
}
//wasmi::Store<Host>
macro_rules! impl_refillable_for_store {
    ($store: ty) => {
        impl<'a> FuelRefillable for $store {
            fn fuels_consumed(&self) -> Result<(u64, u64), HostError> {
                let cpu = self.fuel_consumed().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })?;
                let mem = self.mem_fuel_consumed().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })?;
                Ok((cpu, mem))
            }

            fn fuels_total(&self) -> Result<(u64, u64), HostError> {
                let cpu = self.fuel_total().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })?;
                let mem = self.mem_fuel_total().ok_or_else(|| {
                    HostError::from(wasmi::Error::Store(FuelError::FuelMeteringDisabled))
                })?;
                Ok((cpu, mem))
            }

            fn add_fuels(&mut self, cpu: u64, mem: u64) -> Result<(), HostError> {
                self.add_fuel(cpu)
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))?;
                self.add_mem_fuel(mem)
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))?;
                Ok(())
            }

            fn reset_fuels(&mut self) -> Result<(), HostError> {
                self.reset_fuel()
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))?;
                self.reset_mem_fuel()
                    .map_err(|fe| HostError::from(wasmi::Error::Store(fe)))?;
                Ok(())
            }
        }
    };
}
impl_refillable_for_store!(Store<Host>);
impl_refillable_for_store!(Caller<'a, Host>);
