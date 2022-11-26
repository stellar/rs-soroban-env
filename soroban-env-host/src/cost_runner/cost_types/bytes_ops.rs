use soroban_env_common::Compare;

use crate::{
    budget::{AsBudget, CostType},
    cost_runner::CostRunner,
    host::metered_clone::MeteredClone,
};

pub struct BytesCloneRun;
impl CostRunner for BytesCloneRun {
    const COST_TYPE: CostType = CostType::BytesClone;
    type SampleType = Vec<u8>;

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        sample.metered_clone(&host.budget_cloned()).unwrap();
    }
}

pub struct BytesDelRun;
impl CostRunner for BytesDelRun {
    const COST_TYPE: CostType = CostType::BytesDel;
    type SampleType = (Vec<u8>, usize);

    fn run_iter(_host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        sample.0.remove(sample.1);
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.0.len() as u64) * Self::RUN_ITERATIONS
    }
}

pub struct BytesPushRun;
impl CostRunner for BytesPushRun {
    const COST_TYPE: CostType = CostType::BytesPush;
    type SampleType = Vec<u8>;

    fn run_iter(_host: &crate::Host, iter: u64, mut sample: Self::SampleType) {
        sample.push(iter as u8);
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.len() as u64) * Self::RUN_ITERATIONS
    }
}

pub struct BytesPopRun;
impl CostRunner for BytesPopRun {
    const COST_TYPE: CostType = CostType::BytesPop;
    type SampleType = Vec<u8>;

    fn run_iter(_host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        sample.pop();
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.len() as u64) * Self::RUN_ITERATIONS
    }
}

pub struct BytesInsertRun;
impl CostRunner for BytesInsertRun {
    const COST_TYPE: CostType = CostType::BytesInsert;
    type SampleType = (Vec<u8>, usize);

    fn run_iter(_host: &crate::Host, iter: u64, mut sample: Self::SampleType) {
        sample.0.insert(sample.1, iter as u8);
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        (sample.0.len() as u64) * Self::RUN_ITERATIONS
    }
}

pub struct BytesAppendRun;
impl CostRunner for BytesAppendRun {
    const COST_TYPE: CostType = CostType::BytesAppend;
    type SampleType = (Vec<u8>, Vec<u8>);

    fn run_iter(_host: &crate::Host, _iter: u64, mut sample: Self::SampleType) {
        sample.0.append(&mut sample.1);
    }

    fn get_total_input(_host: &crate::Host, sample: &Self::SampleType) -> u64 {
        ((sample.0.len() + sample.1.len()) as u64) * Self::RUN_ITERATIONS
    }
}

pub struct BytesCmpRun;
impl CostRunner for BytesCmpRun {
    const COST_TYPE: CostType = CostType::BytesCmp;
    type SampleType = (Vec<u8>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) {
        host.as_budget()
            .compare(&sample.0.as_slice(), &sample.1.as_slice())
            .unwrap();
    }
}
