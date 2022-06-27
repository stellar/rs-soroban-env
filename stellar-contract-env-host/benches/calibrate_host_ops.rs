// Run this with
// $ cargo bench calibrate_host_ops -- --nocapture

mod common;
use common::*;
use stellar_contract_env_host::{
    budget::CostType,
    xdr::{ScObject, ScVal, ScVec},
    Host,
};

struct VecAllocRun {
    val: ScVal,
}

impl HostCostMeasurement for VecAllocRun {
    const COST_TYPE: CostType = CostType::HostVecAlloc;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let scvec: ScVec = ScVec(
            (0..size)
                .map(|i| ScVal::U32(i as u32))
                .collect::<Vec<ScVal>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { val }
    }

    fn run(&mut self, host: &Host) {
        host.intern(&self.val).unwrap();
    }
}

#[cfg(all(test, target_os = "linux"))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    let mut measurements = measure_costs::<VecAllocRun>(0..20)?;
    measurements.subtract_baseline();
    measurements.report();
    if std::env::var("FIT_MODELS").is_ok() {
        measurements.fit_model_to_cpu();
        measurements.fit_model_to_mem();
    }
    Ok(())
}
