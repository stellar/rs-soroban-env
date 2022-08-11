use crate::{
    host_object::{HostObj, HostVal},
    xdr::{ScObject, ScVal, ScVec},
    Host, HostError,
};

// Test utilities for the host, used in various tests in sub-modules.
pub(crate) trait AsScVal {
    fn as_scval(&self) -> ScVal;
}

impl AsScVal for u32 {
    fn as_scval(&self) -> ScVal {
        ScVal::U32(*self)
    }
}

impl AsScVal for i32 {
    fn as_scval(&self) -> ScVal {
        ScVal::I32(*self)
    }
}

impl Host {
    pub(crate) fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub(crate) fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<HostObj, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_obj(&ScObject::Vec(v))
    }

    pub(crate) fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<HostVal, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Object(Some(ScObject::Vec(v))))
    }

    pub(crate) fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScObject, HostError> {
        Ok(ScObject::Bytes(self.map_err(vals.try_into())?))
    }

    pub(crate) fn test_bin_obj(&self, vals: &[u8]) -> Result<HostObj, HostError> {
        self.to_host_obj(&self.test_bin_scobj(vals)?)
    }
}
