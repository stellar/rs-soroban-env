#[macro_export]
macro_rules! impl_wrapping_obj_from_num {
    ($host_fn: ident, $hot: ty, $obj: ty, $num: ty) => {
        fn $host_fn(&self, _vmcaller: &mut VmCaller<Host>, u: $num) -> Result<$obj, HostError> {
            self.add_host_object(<$hot>::from(u))
        }
    };
}

#[macro_export]
macro_rules! impl_wrapping_obj_to_num {
    ($host_fn: ident, $data: ty, $obj: ty, $num: ty) => {
        fn $host_fn(&self, _vmcaller: &mut VmCaller<Host>, obj: $obj) -> Result<$num, HostError> {
            self.visit_obj(obj, |t: &$data| Ok(t.metered_clone(self)?.into()))
        }
    };
}

#[macro_export]
macro_rules! impl_bignum_host_fns {
    ($host_fn: ident, $method: ident, $num: ty, $valty: ty, $cost: ident) => {
        fn $host_fn(
            &self,
            _vmcaller: &mut VmCaller<Self::VmUserState>,
            lhs_val: $valty,
            rhs_val: $valty,
        ) -> Result<$valty, Self::Error> {
            use soroban_env_common::TryIntoVal;
            self.charge_budget(ContractCostType::$cost, None)?;
            let lhs: $num = lhs_val.to_val().try_into_val(self)?;
            let rhs: $num = rhs_val.to_val().try_into_val(self)?;
            let res: $num = lhs.$method(rhs).ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::ArithDomain,
                    "overflow has occured",
                    &[lhs_val.to_val(), rhs_val.to_val()],
                )
            })?;
            Ok(res.try_into_val(self)?)
        }
    };
}

#[macro_export]
macro_rules! impl_bignum_host_fns_rhs_u32 {
    ($host_fn: ident, $method: ident, $num: ty, $valty: ty, $cost: ident) => {
        fn $host_fn(
            &self,
            _vmcaller: &mut VmCaller<Self::VmUserState>,
            lhs_val: $valty,
            rhs_val: U32Val,
        ) -> Result<$valty, Self::Error> {
            use soroban_env_common::TryIntoVal;
            self.charge_budget(ContractCostType::$cost, None)?;
            let lhs: $num = lhs_val.to_val().try_into_val(self)?;
            let res = lhs.$method(rhs_val.into()).ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::ArithDomain,
                    "overflow has occured",
                    &[lhs_val.to_val(), rhs_val.to_val()],
                )
            })?;
            Ok(res.try_into_val(self)?)
        }
    };
}
