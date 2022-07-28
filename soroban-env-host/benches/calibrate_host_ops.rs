// Run this with
// $ cargo bench calibrate_host_ops -- --nocapture

mod common;
use common::*;
use soroban_env_host::{
    budget::CostType,
    xdr::{ScMap, ScMapEntry, ScObject, ScVal, ScVec},
    Host,
};

struct VecAllocVariableSizeRun {
    val: ScVal,
}

struct EmptyVecAllocVariableCountRun {
    count: u64,
    val: ScVal,
}

struct MapAllocVariableSizeRun {
    val: ScVal,
}

struct EmptyMapAllocVariableCountRun {
    count: u64,
    val: ScVal,
}

/// Measures the costs of allocating vectors of varying sizes.
impl HostCostMeasurement for VecAllocVariableSizeRun {
    const COST_TYPE: CostType = CostType::HostVecAllocCell;

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
        host.inject_val(&self.val).unwrap();
    }
}

/// Measures the costs of allocating large numbers of 0-sized vectors.
impl HostCostMeasurement for EmptyVecAllocVariableCountRun {
    const COST_TYPE: CostType = CostType::HostVecAllocVec;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 1000;
        let scvec: ScVec = ScVec(vec![].try_into().unwrap());
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { count: size, val }
    }

    fn run(&mut self, host: &Host) {
        for _ in 0..self.count {
            host.inject_val(&self.val).unwrap();
        }
    }
}

/// Measures the costs of allocating maps of varying sizes.
impl HostCostMeasurement for MapAllocVariableSizeRun {
    const COST_TYPE: CostType = CostType::HostMapAllocCell;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let scmap: ScMap = ScMap(
            (0..size)
                .map(|i| ScMapEntry {
                    key: ScVal::U32(i as u32),
                    val: ScVal::U32(i as u32),
                })
                .collect::<Vec<ScMapEntry>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Map(scmap)));
        Self { val }
    }

    fn run(&mut self, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}

/// Measures the costs of allocating large numbers of 0-sized maps.
impl HostCostMeasurement for EmptyMapAllocVariableCountRun {
    const COST_TYPE: CostType = CostType::HostMapAllocMap;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 1000;
        let scmap: ScMap = ScMap(vec![].try_into().unwrap());
        let val = ScVal::Object(Some(ScObject::Map(scmap)));
        Self { count: size, val }
    }

    fn run(&mut self, host: &Host) {
        for _ in 0..self.count {
            host.inject_val(&self.val).unwrap();
        }
    }
}

fn measure_one<M: HostCostMeasurement>() -> std::io::Result<()> {
    let mut measurements = measure_costs::<M>(0..20)?;
    measurements.subtract_baseline();
    measurements.report();

    if std::env::var("FIT_MODELS").is_ok() {
        measurements.fit_model_to_cpu();
        measurements.fit_model_to_mem();
    }
    Ok(())
}

#[cfg(all(test, target_os = "linux"))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    measure_one::<VecAllocVariableSizeRun>()?;
    measure_one::<EmptyVecAllocVariableCountRun>()?;
    measure_one::<MapAllocVariableSizeRun>()?;
    measure_one::<EmptyMapAllocVariableCountRun>()?;
    Ok(())
}
