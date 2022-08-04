use crate::{
    budget::CostType,
    xdr::{ScMap, ScMapEntry, ScObject, ScVal},
    Host, HostError,
};

#[test]
fn xdr_object_conversion() -> Result<(), HostError> {
    let host = Host::default();
    host.get_budget_mut(|budget| {
        budget.reset_limits(100, 100);
        budget.reset_models();
        budget
            .cpu_insns
            .get_cost_model_mut(CostType::ValXdrConv)
            .const_param = 1;
        budget
            .mem_bytes
            .get_cost_model_mut(CostType::ValXdrConv)
            .const_param = 2;
    });

    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(4),
            },
        ]
        .try_into(),
    )?;
    let scobj = ScObject::Map(scmap);
    host.to_host_obj(&scobj)?;

    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValXdrConv), 5);
        assert_eq!(budget.cpu_insns.get_count(), 5);
        assert_eq!(budget.mem_bytes.get_count(), 10);
    });
    Ok(())
}
