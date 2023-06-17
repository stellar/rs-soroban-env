#[macro_export]
macro_rules! impl_bignum_host_fns {
    ($host_fn: ident, $method: ident, $num: ty, $cost: ident) => {
        fn $host_fn(
            &self,
            vmcaller: &mut VmCaller<Self::VmUserState>,
            lhs_obj: <$num as HostObjectType>::Wrapper,
            rhs_obj: <$num as HostObjectType>::Wrapper,
        ) -> Result<<$num as HostObjectType>::Wrapper, Self::Error> {
            self.charge_budget(ContractCostType::$cost, None)?;
            let res = self.visit_obj(lhs_obj, move |lhs: &$num| {
                self.visit_obj(rhs_obj, move |rhs: &$num| {
                    lhs.$method(*rhs).ok_or_else(|| {
                        self.err(
                            ScErrorType::Object,
                            ScErrorCode::ArithDomain,
                            "overflow has occured",
                            &[lhs_obj.to_val(), rhs_obj.to_val()],
                        )
                    })
                })
            })?;
            self.add_host_object(res)
        }
    };
}

#[macro_export]
macro_rules! impl_bignum_host_fns_rhs_u32 {
    ($host_fn: ident, $method: ident, $num: ty, $cost: ident) => {
        fn $host_fn(
            &self,
            vmcaller: &mut VmCaller<Self::VmUserState>,
            lhs_obj: <$num as HostObjectType>::Wrapper,
            rhs_val: U32Val,
        ) -> Result<<$num as HostObjectType>::Wrapper, Self::Error> {
            self.charge_budget(ContractCostType::$cost, None)?;
            let res = self.visit_obj(lhs_obj, move |lhs: &$num| {
                lhs.$method(rhs_val.into()).ok_or_else(|| {
                    self.err(
                        ScErrorType::Object,
                        ScErrorCode::ArithDomain,
                        "overflow has occured",
                        &[lhs_obj.to_val(), rhs_val.to_val()],
                    )
                })
            })?;
            self.add_host_object(res)
        }
    };
}
