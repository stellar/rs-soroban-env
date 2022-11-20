use crate::{budget::CostType, Host, HostError, Vm, VmCaller};
use std::{ops::Range, rc::Rc};

impl Host {
    pub(crate) fn metered_vm_mem_write(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: Rc<Vm>,
        pos: u32,
        range: Range<usize>,
        buf: &Vec<u8>,
    ) -> Result<(), HostError> {
        self.charge_budget(CostType::VmMemWrite, range.len() as u64)?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.write(vmcaller.try_mut()?, pos as usize, &buf.as_slice()[range])
                .map_err(|me| wasmi::Error::Memory(me)),
        )
    }

    pub(crate) fn metered_vm_mem_read(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: Rc<Vm>,
        pos: u32,
        range: Range<usize>,
        buf: &mut Vec<u8>,
    ) -> Result<(), HostError> {
        self.charge_budget(CostType::VmMemRead, range.len() as u64)?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.read(
                vmcaller.try_mut()?,
                pos as usize,
                &mut buf.as_mut_slice()[range],
            )
            .map_err(|me| wasmi::Error::Memory(me)),
        )
    }
}
