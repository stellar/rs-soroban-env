use soroban_env_common::{
    xdr::{ScHostObjErrorCode, ScHostValErrorCode},
    U32Val,
};

use crate::{budget::CostType, host_object::MemHostObjectType, Host, HostError, VmCaller};

#[cfg(feature = "vm")]
use std::rc::Rc;

#[cfg(feature = "vm")]
use crate::{
    host::{Frame, VmSlice},
    Vm,
};

impl Host {
    // Notes on metering: free
    #[cfg(feature = "vm")]
    pub(crate) fn decode_vmslice(&self, pos: U32Val, len: U32Val) -> Result<VmSlice, HostError> {
        let pos: u32 = pos.into();
        let len: u32 = len.into();
        self.with_current_frame(|frame| match frame {
            Frame::ContractVM(vm, _, _) => {
                let vm = vm.clone();
                Ok(VmSlice { vm, pos, len })
            }
            _ => Err(self.err_general("attempt to access guest bytes in non-VM frame")),
        })
    }

    #[cfg(feature = "vm")]
    pub(crate) fn metered_vm_write_bytes_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &[u8],
    ) -> Result<(), HostError> {
        self.charge_budget(CostType::VmMemWrite, buf.len() as u64)?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.write(vmcaller.try_mut()?, mem_pos as usize, buf)
                .map_err(wasmi::Error::Memory),
        )
    }

    #[cfg(feature = "vm")]
    pub(crate) fn metered_vm_read_bytes_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &mut [u8],
    ) -> Result<(), HostError> {
        self.charge_budget(CostType::VmMemRead, buf.len() as u64)?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.read(vmcaller.try_mut()?, mem_pos as usize, buf)
                .map_err(wasmi::Error::Memory),
        )
    }

    #[cfg(feature = "vm")]
    pub(crate) fn metered_vm_write_vals_to_linear_memory<const VAL_SZ: usize, VAL>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &[VAL],
        to_le_bytes: impl Fn(&VAL) -> [u8; VAL_SZ],
    ) -> Result<(), HostError> {
        use soroban_env_common::xdr::ScVmErrorCode;

        let val_sz = self.usize_to_u32(VAL_SZ)?;
        let len = self.usize_to_u32(buf.len())?;
        let byte_len: u32 = len
            .checked_mul(val_sz)
            .ok_or(ScHostValErrorCode::U32OutOfRange)?;

        let mem_end = mem_pos
            .checked_add(byte_len)
            .ok_or(ScHostValErrorCode::U32OutOfRange)?;
        let mem_range = (mem_pos as usize)..(mem_end as usize);

        let mem_data = vm.get_memory(self)?.data_mut(vmcaller.try_mut()?);
        let mem_slice = mem_data.get_mut(mem_range).ok_or(ScVmErrorCode::Memory)?;

        self.charge_budget(CostType::VmMemWrite, byte_len as u64)?;
        for (src, dst) in buf.iter().zip(mem_slice.chunks_mut(VAL_SZ)) {
            if dst.len() != VAL_SZ {
                // This should be impossible unless there's an error above, but just in case.
                return Err(ScHostObjErrorCode::VecIndexOutOfBound.into());
            }
            let tmp: [u8; VAL_SZ] = to_le_bytes(src);
            dst.copy_from_slice(&tmp);
        }
        Ok(())
    }

    #[cfg(feature = "vm")]
    pub(crate) fn metered_vm_read_vals_from_linear_memory<const VAL_SZ: usize, VAL>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &mut [VAL],
        from_le_bytes: impl Fn(&[u8; VAL_SZ]) -> VAL,
    ) -> Result<(), HostError> {
        use soroban_env_common::xdr::ScVmErrorCode;

        let val_sz = self.usize_to_u32(VAL_SZ)?;
        let len = self.usize_to_u32(buf.len())?;
        let byte_len: u32 = len
            .checked_mul(val_sz)
            .ok_or(ScHostValErrorCode::U32OutOfRange)?;

        let mem_end = mem_pos
            .checked_add(byte_len)
            .ok_or(ScHostValErrorCode::U32OutOfRange)?;
        let mem_range = (mem_pos as usize)..(mem_end as usize);

        let mem_data = vm.get_memory(self)?.data(vmcaller.try_mut()?);
        let mem_slice = mem_data.get(mem_range).ok_or(ScVmErrorCode::Memory)?;

        self.charge_budget(CostType::VmMemRead, byte_len as u64)?;
        let mut tmp: [u8; VAL_SZ] = [0u8; VAL_SZ];
        for (dst, src) in buf.iter_mut().zip(mem_slice.chunks(VAL_SZ)) {
            if let Ok(src) = TryInto::<&[u8; VAL_SZ]>::try_into(src) {
                tmp.copy_from_slice(src);
                *dst = from_le_bytes(&tmp);
            } else {
                // This should be impossible unless there's an error above, but just in case.
                return Err(ScHostObjErrorCode::VecIndexOutOfBound.into());
            }
        }
        Ok(())
    }

    // This is the most complex one: it reads a sequence of slices _stored in
    // linear memory_ and then _follows_ each of them to read the slice of
    // linear memory they point at, and calls a callback with each of those
    // linear-memory slices. It's used strictly for comparing symbol &str-arrays
    // to the expected key-sets of UDT maps. Very special case.
    //
    // It assumes (as per the Rust unsafe code guidelines) that a slice in the
    // guest is represented as an 8 byte (ptr:u32,len:u32) pair. Note that if
    // this assumption is incorrect, this will not break the safety of this
    // function, only make it read junk memory in the guest and therefore likely
    // cause the callback to return an error.
    #[cfg(feature = "vm")]
    pub(crate) fn metered_vm_scan_slices_in_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mut mem_pos: u32,
        num_slices: usize,
        mut callback: impl FnMut(usize, &[u8]) -> Result<(), HostError>,
    ) -> Result<(), HostError> {
        use soroban_env_common::xdr::ScVmErrorCode;

        let mem_data = vm.get_memory(self)?.data(vmcaller.try_mut()?);
        self.charge_budget(CostType::VmMemRead, num_slices.saturating_mul(8) as u64)?;

        for i in 0..num_slices {
            // This is _very specific_ about what it's reading: 8 bytes
            // arranged as a 4 byte pointer followed by a 4 byte length.

            let next_pos = mem_pos
                .checked_add(8)
                .ok_or(ScHostValErrorCode::U32OutOfRange)?;
            let slice_ref_range = mem_pos as usize..next_pos as usize;
            let slice_ref_slice = mem_data.get(slice_ref_range).ok_or(ScVmErrorCode::Memory)?;
            mem_pos = next_pos;

            if let Ok(s) = TryInto::<&[u8; 8]>::try_into(slice_ref_slice) {
                let ptr_bytes: [u8; 4] = s[0..4].try_into().unwrap();
                let len_bytes: [u8; 4] = s[4..8].try_into().unwrap();
                let slice_ptr = u32::from_le_bytes(ptr_bytes);
                let slice_len = u32::from_le_bytes(len_bytes);
                let slice_end = slice_ptr
                    .checked_add(slice_len)
                    .ok_or(ScHostValErrorCode::U32OutOfRange)?;
                let slice_range = slice_ptr as usize..slice_end as usize;
                let slice = mem_data.get(slice_range).ok_or(ScVmErrorCode::Memory)?;
                callback(i, slice)?;
            } else {
                // This should be impossible unless there's an error above, but just in case.
                return Err(ScHostObjErrorCode::VecIndexOutOfBound.into());
            }
        }
        Ok(())
    }

    pub(crate) fn metered_scan_slice_of_slices(
        &self,
        slices: &[&str],
        mut callback: impl FnMut(usize, &str) -> Result<(), HostError>,
    ) -> Result<(), HostError> {
        // FIXME: this should be CostType::MemRead when we have that
        self.charge_budget(
            CostType::BytesCmp,
            slices
                .len()
                .saturating_mul(2)
                .saturating_mul(std::mem::size_of::<usize>()) as u64,
        )?;
        for (i, slice) in slices.iter().enumerate() {
            self.charge_budget(CostType::VmMemRead, slice.len() as u64)?;
            callback(i, slice)?;
        }
        Ok(())
    }

    fn metered_copy_byte_slice(&self, dst: &mut [u8], src: &[u8]) -> Result<(), HostError> {
        if src.len() != dst.len() {
            // This should be impossible on all caller codepaths, but we check just in case
            // since copy_from_slice below will panic if there's a size mismatch.
            return Err(ScHostObjErrorCode::VecIndexOutOfBound.into());
        }
        // FIXME: this should be CostType::HostMemCopy when we have that
        self.charge_budget(CostType::BytesClone, dst.len() as u64)?;
        dst.copy_from_slice(src);
        Ok(())
    }

    // Helper called by memobj_copy_to_slice and memobj_copy_to_linear_memory
    fn memobj_visit_and_copy_bytes_out<HOT: MemHostObjectType>(
        &self,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        len: u32,
        copy_bytes_out: impl FnOnce(&[u8]) -> Result<(), HostError>,
    ) -> Result<(), HostError> {
        let obj_pos: u32 = obj_pos.into();
        self.visit_obj(obj, move |hob: &HOT| {
            let obj_end = obj_pos
                .checked_add(len)
                .ok_or(ScHostValErrorCode::U32OutOfRange)?;
            let obj_range = obj_pos as usize..obj_end as usize;
            let obj_buf = hob
                .as_byte_slice()
                .get(obj_range)
                .ok_or(ScHostObjErrorCode::VecIndexOutOfBound)?;
            copy_bytes_out(obj_buf)
        })
    }

    pub(crate) fn memobj_copy_to_slice<HOT: MemHostObjectType>(
        &self,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        let len = self.usize_to_u32(slice.len())?;
        self.memobj_visit_and_copy_bytes_out::<HOT>(obj, obj_pos, len, |obj_buf| {
            self.metered_copy_byte_slice(slice, obj_buf)
        })
    }

    pub(crate) fn memobj_copy_to_linear_memory<HOT: MemHostObjectType>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<(), HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            self.memobj_visit_and_copy_bytes_out::<HOT>(obj, obj_pos, len, |obj_buf| {
                self.metered_vm_write_bytes_to_linear_memory(vmcaller, &vm, pos, obj_buf)
            })
        }
    }

    // Helper called by memobj_copy_from_slice and memobj_copy_from_linear_memory
    fn memobj_clone_resize_and_copy_bytes_in<HOT: MemHostObjectType>(
        &self,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        len: u32,
        copy_bytes_in: impl FnOnce(&mut [u8]) -> Result<(), HostError>,
    ) -> Result<HOT::Wrapper, HostError> {
        let obj_pos: u32 = obj_pos.into();
        let mut obj_new: Vec<u8> = self
            .visit_obj(obj, move |hv: &HOT| hv.metered_clone(&self.0.budget))?
            .into();
        let obj_end = obj_pos
            .checked_add(len)
            .ok_or(ScHostValErrorCode::U32OutOfRange)? as usize;
        // TODO: we currently grow the destination vec if it's not big enough,
        // make sure this is desirable behaviour.
        if obj_new.len() < obj_end {
            // FIXME: this should be CostType::HostMemAlloc when we have that
            self.charge_budget(CostType::BytesClone, (obj_end - obj_new.len()) as u64)?;
            obj_new.resize(obj_end, 0);
        }
        let obj_range = obj_pos as usize..obj_end;
        let obj_buf: &mut [u8] = obj_new
            .as_mut_slice()
            .get_mut(obj_range)
            .ok_or(ScHostObjErrorCode::VecIndexOutOfBound)?;
        copy_bytes_in(obj_buf)?;
        self.add_host_object(HOT::try_from(obj_new)?)
    }

    pub(crate) fn memobj_copy_from_slice<HOT: MemHostObjectType>(
        &self,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        slice: &[u8],
    ) -> Result<HOT::Wrapper, HostError> {
        let len = self.usize_to_u32(slice.len())?;
        self.memobj_clone_resize_and_copy_bytes_in::<HOT>(obj, obj_pos, len, |obj_buf| {
            self.metered_copy_byte_slice(obj_buf, slice)
        })
    }

    pub(crate) fn memobj_copy_from_linear_memory<HOT: MemHostObjectType>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        obj: HOT::Wrapper,
        obj_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<HOT::Wrapper, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            self.memobj_clone_resize_and_copy_bytes_in::<HOT>(obj, obj_pos, len, |obj_buf| {
                self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, obj_buf)
            })
        }
    }

    pub(crate) fn memobj_new_from_linear_memory<HOT: MemHostObjectType>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<HOT::Wrapper, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
            // FIXME: this should be CostType::HostMemAlloc when we have that
            self.charge_budget(CostType::BytesClone, len as u64)?;
            let mut vnew: Vec<u8> = vec![0; len as usize];
            self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, &mut vnew)?;
            self.add_host_object::<HOT>(vnew.try_into()?)
        }
    }
}
