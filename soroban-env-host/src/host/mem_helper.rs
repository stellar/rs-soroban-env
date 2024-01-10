use crate::{
    budget::AsBudget,
    host::Frame,
    host_object::MemHostObjectType,
    xdr::{ContractCostType, ScErrorCode, ScErrorType, ScSymbol},
    Compare, Host, HostError, Symbol, SymbolObject, SymbolSmall, SymbolStr, U32Val, Vm, VmCaller,
};

use std::{cmp::Ordering, rc::Rc};

/// Helper type for host functions that receive a position and length pair and
/// expect to operate on a VM. Pos and len are not validated and len may be a
/// count of bytes, Vals or slices depending on the host function.
pub(crate) struct MemFnArgs {
    pub(crate) vm: Rc<Vm>,
    pub(crate) pos: u32,
    pub(crate) len: u32,
}

impl Host {
    // Notes on metering: free
    // Helper for the first step in most functions that operate on linear
    // memory. Converts a pos and len from U32Vals to plain u32, and extracts
    // the current frame's vm. Does not validate anything about the pos and len
    // and does not even specify what len is a count of: some uses count bytes,
    // others count Vals or slices.
    pub(crate) fn get_mem_fn_args(&self, pos: U32Val, len: U32Val) -> Result<MemFnArgs, HostError> {
        let pos: u32 = pos.into();
        let len: u32 = len.into();
        self.with_current_frame(|frame| match frame {
            Frame::ContractVM { vm, .. } => {
                let vm = Rc::clone(&vm);
                Ok(MemFnArgs { vm, pos, len })
            }
            _ => Err(self.err(
                ScErrorType::WasmVm,
                ScErrorCode::InternalError,
                "attempt to access guest bytes in non-VM frame",
                &[],
            )),
        })
    }

    pub(crate) fn metered_vm_write_bytes_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &[u8],
    ) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::MemCpy, Some(buf.len() as u64))?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.write(vmcaller.try_mut()?, mem_pos as usize, buf)
                .map_err(|me| wasmi::Error::Memory(me)),
        )
    }

    pub(crate) fn metered_vm_read_bytes_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &mut [u8],
    ) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::MemCpy, Some(buf.len() as u64))?;
        let mem = vm.get_memory(self)?;
        self.map_err(
            mem.read(vmcaller.try_mut()?, mem_pos as usize, buf)
                .map_err(|me| wasmi::Error::Memory(me)),
        )
    }

    // Note on metering: covers the cost of memcpy from bytes into the linear memory.
    // It does not include the cost of getting those bytes, which is done by the
    // closure and must be metered in the closure at the caller side.
    pub(crate) fn metered_vm_write_vals_to_linear_memory<const VAL_SZ: usize, VAL>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &[VAL],
        to_le_bytes: impl Fn(&VAL) -> Result<[u8; VAL_SZ], HostError>,
    ) -> Result<(), HostError> {
        let val_sz = self.usize_to_u32(VAL_SZ)?;
        let len = self.usize_to_u32(buf.len())?;
        let byte_len: u32 = len
            .checked_mul(val_sz)
            .ok_or_else(|| self.err_arith_overflow())?;

        let mem_end = mem_pos
            .checked_add(byte_len)
            .ok_or_else(|| self.err_arith_overflow())?;
        let mem_range = (mem_pos as usize)..(mem_end as usize);

        let mem_data = vm.get_memory(self)?.data_mut(vmcaller.try_mut()?);
        let mem_slice = mem_data
            .get_mut(mem_range)
            .ok_or_else(|| self.err_oob_linear_memory())?;

        self.charge_budget(ContractCostType::MemCpy, Some(byte_len as u64))?;
        for (src, dst) in buf.iter().zip(mem_slice.chunks_mut(VAL_SZ)) {
            if dst.len() != VAL_SZ {
                // This should be impossible unless there's an error above, but just in case.
                return Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "chunks_mut produced chunk of unexpected length",
                    &[],
                ));
            }
            let tmp: [u8; VAL_SZ] = to_le_bytes(src)?;
            dst.copy_from_slice(&tmp);
        }
        Ok(())
    }

    // Notes on metering: covers the cost of memcpy from linear memory to local slices.
    // The cost of converting slices into `VAL`s are not covered and needs to be metered
    // by the closure at the caller side.
    pub(crate) fn metered_vm_read_vals_from_linear_memory<const VAL_SZ: usize, VAL>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mem_pos: u32,
        buf: &mut [VAL],
        from_le_bytes: impl Fn(&[u8; VAL_SZ]) -> Result<VAL, HostError>,
    ) -> Result<(), HostError> {
        let val_sz = self.usize_to_u32(VAL_SZ)?;
        let len = self.usize_to_u32(buf.len())?;
        let byte_len: u32 = len
            .checked_mul(val_sz)
            .ok_or_else(|| self.err_arith_overflow())?;

        let mem_end = mem_pos
            .checked_add(byte_len)
            .ok_or_else(|| self.err_arith_overflow())?;
        let mem_range = (mem_pos as usize)..(mem_end as usize);

        let mem_data = vm.get_memory(self)?.data(vmcaller.try_mut()?);
        let mem_slice = mem_data
            .get(mem_range)
            .ok_or_else(|| self.err_oob_linear_memory())?;

        self.charge_budget(ContractCostType::MemCpy, Some(byte_len as u64))?;
        let mut tmp: [u8; VAL_SZ] = [0u8; VAL_SZ];
        for (dst, src) in buf.iter_mut().zip(mem_slice.chunks(VAL_SZ)) {
            if let Ok(src) = TryInto::<&[u8; VAL_SZ]>::try_into(src) {
                tmp.copy_from_slice(src);
                *dst = from_le_bytes(&tmp)?;
            } else {
                // This should be impossible unless there's an error above, but just in case.
                return Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "chunks produced chunk of unexpected length",
                    &[],
                ));
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
    //
    // Notes on metering: metering for this function covers memcpy of the
    // `slices`, each of which is 8 bytes of memory (4 byte pos + 4 byte len).
    // The actual content pointed to by each slice is accessed and passed into
    // the closure as a reference so there is no cost to it. The actual cost of
    // work done on the slice needs to be metered in the closure by the caller.
    pub(crate) fn metered_vm_scan_slices_in_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vm: &Rc<Vm>,
        mut mem_pos: u32,
        num_slices: usize,
        mut callback: impl FnMut(usize, &[u8]) -> Result<(), HostError>,
    ) -> Result<(), HostError> {
        let mem_data = vm.get_memory(self)?.data(vmcaller.try_mut()?);
        // charge the cost of copying the slices (pointers to the content, not
        // the content themselves) upfront.
        self.charge_budget(
            ContractCostType::MemCpy,
            Some((num_slices as u64).saturating_mul(8)),
        )?;

        for i in 0..num_slices {
            // This is _very specific_ about what it's reading: 8 bytes
            // arranged as a 4 byte pointer followed by a 4 byte length.

            let next_pos = mem_pos
                .checked_add(8)
                .ok_or_else(|| self.err_arith_overflow())?;
            let slice_ref_range = mem_pos as usize..next_pos as usize;
            let slice_ref_slice = mem_data
                .get(slice_ref_range)
                .ok_or_else(|| self.err_oob_linear_memory())?;
            mem_pos = next_pos;

            if let Ok(s) = TryInto::<&[u8; 8]>::try_into(slice_ref_slice) {
                let ptr_bytes: [u8; 4] = s[0..4].try_into().unwrap();
                let len_bytes: [u8; 4] = s[4..8].try_into().unwrap();
                let slice_ptr = u32::from_le_bytes(ptr_bytes);
                let slice_len = u32::from_le_bytes(len_bytes);
                let slice_end = slice_ptr
                    .checked_add(slice_len)
                    .ok_or_else(|| self.err_arith_overflow())?;
                let slice_range = slice_ptr as usize..slice_end as usize;
                let slice = mem_data
                    .get(slice_range)
                    .ok_or_else(|| self.err_oob_linear_memory())?;
                callback(i, slice)?
            } else {
                // This should be impossible unless there's an error above, but just in case.
                return Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "slice-scan produced slice of unexpected length",
                    &[],
                ));
            }
        }
        Ok(())
    }

    fn metered_copy_byte_slice(&self, dst: &mut [u8], src: &[u8]) -> Result<(), HostError> {
        if src.len() != dst.len() {
            // This should be impossible on all caller codepaths, but we check just in case
            // since copy_from_slice below will panic if there's a size mismatch.
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "byte-slice copy src and dst lengths differ",
                &[],
            ));
        }
        self.charge_budget(ContractCostType::MemCpy, Some(dst.len() as u64))?;
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
                .ok_or_else(|| self.err_arith_overflow())?;
            let obj_range = obj_pos as usize..obj_end as usize;
            let obj_buf = hob.as_byte_slice().get(obj_range).ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::IndexBounds,
                    "out-of-bounds read from memory-like host object",
                    &[],
                )
            })?;
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
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(lm_pos, len)?;
        self.memobj_visit_and_copy_bytes_out::<HOT>(obj, obj_pos, len, |obj_buf| {
            self.metered_vm_write_bytes_to_linear_memory(vmcaller, &vm, pos, obj_buf)
        })
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
            .visit_obj(obj, move |hv: &HOT| hv.metered_clone(self))?
            .into();
        let obj_end = obj_pos
            .checked_add(len)
            .ok_or_else(|| self.err_arith_overflow())? as usize;
        if obj_new.len() < obj_end {
            self.charge_budget(
                ContractCostType::MemAlloc,
                Some((obj_end.saturating_sub(obj_new.len())) as u64),
            )?;
            obj_new.resize(obj_end, 0);
        }
        let obj_range = obj_pos as usize..obj_end;
        let obj_buf: &mut [u8] = obj_new.as_mut_slice().get_mut(obj_range).ok_or_else(|| {
            self.err(
                ScErrorType::Object,
                ScErrorCode::IndexBounds,
                "out-of-bounds write to memory-like host object",
                &[],
            )
        })?;
        copy_bytes_in(obj_buf)?;
        self.add_host_object(HOT::try_from_bytes(self, obj_new)?)
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
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(lm_pos, len)?;
        self.memobj_clone_resize_and_copy_bytes_in::<HOT>(obj, obj_pos, len, |obj_buf| {
            self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, obj_buf)
        })
    }

    pub(crate) fn memobj_new_from_linear_memory<HOT: MemHostObjectType>(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<HOT::Wrapper, HostError> {
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(lm_pos, len)?;
        self.charge_budget(ContractCostType::MemAlloc, Some(len as u64))?;
        let mut vnew: Vec<u8> = vec![0; len as usize];
        self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, &mut vnew)?;
        self.add_host_object::<HOT>(HOT::try_from_bytes(self, vnew)?)
    }

    pub(crate) fn symbol_matches(&self, s: &[u8], sym: Symbol) -> Result<bool, HostError> {
        if let Ok(ss) = SymbolSmall::try_from(sym) {
            let sstr: SymbolStr = ss.into();
            let slice: &[u8] = sstr.as_ref();
            self.as_budget()
                .compare(&slice, &s)
                .map(|c| c == Ordering::Equal)
        } else {
            let sobj: SymbolObject = sym.try_into()?;
            self.visit_obj(sobj, |scsym: &ScSymbol| {
                self.as_budget()
                    .compare(&scsym.as_slice(), &s)
                    .map(|c| c == Ordering::Equal)
            })
        }
    }

    pub(crate) fn check_symbol_matches(&self, s: &[u8], sym: Symbol) -> Result<(), HostError> {
        if self.symbol_matches(s, sym)? {
            Ok(())
        } else {
            Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "symbol mismatch",
                &[sym.to_val()],
            ))
        }
    }

    // Test function for calibration purpose. The caller needs to ensure `src` and `dest` has
    // the same length or else it panics.
    #[cfg(feature = "bench")]
    pub(crate) fn mem_copy_from_slice<T: Copy + super::declared_size::DeclaredSizeForMetering>(
        &self,
        src: &[T],
        dest: &mut [T],
    ) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::MemCpy, Some(src.len() as u64))?;
        for (s, d) in src.iter().zip(dest.iter_mut()) {
            *d = *s
        }
        Ok(())
    }
}
