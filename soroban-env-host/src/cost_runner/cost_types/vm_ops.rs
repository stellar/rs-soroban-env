use crate::{
    cost_runner::{CostRunner, CostType},
    xdr::Hash,
    xdr::{
        ContractCodeCostInputs,
        ContractCostType::{
            VmInstantiateDataSegments, VmInstantiateElemSegments, VmInstantiateExports,
            VmInstantiateFunctions, VmInstantiateGlobals, VmInstantiateImports,
            VmInstantiateInstructions, VmInstantiateTableEntries, VmInstantiateTypes,
            VmInstantiation,
        },
    },
    Vm,
};
use std::{hint::black_box, rc::Rc};

pub struct VmInstantiationRun;
pub struct VmInstantiationInstructionsRun;
pub struct VmInstantiationFunctionsRun;
pub struct VmInstantiationGlobalsRun;
pub struct VmInstantiationTableEntriesRun;
pub struct VmInstantiationTypesRun;
pub struct VmInstantiationDataSegmentsRun;
pub struct VmInstantiationElemSegmentsRun;
pub struct VmInstantiationImportsRun;
pub struct VmInstantiationExportsRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Option<Hash>,
    pub wasm: Vec<u8>,
    pub cost_inputs: Option<ContractCodeCostInputs>,
}

macro_rules! impl_costrunner_for_instantiation_cost_type {
    ($RUNNER:ty, $COST:ident) => {
        impl CostRunner for $RUNNER {
            const COST_TYPE: CostType = CostType::Contract($COST);

            const RUN_ITERATIONS: u64 = 10;

            type SampleType = VmInstantiationSample;

            type RecycledType = (Option<Rc<Vm>>, Vec<u8>);

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                let vm = black_box(
                    Vm::new(
                        host,
                        sample.id.unwrap(),
                        &sample.wasm[..],
                        sample.cost_inputs.clone(),
                    )
                    .unwrap(),
                );
                (Some(vm), sample.wasm)
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(host.charge_budget($COST, Some(0)).unwrap());
                black_box((None, sample.wasm))
            }
        }
    };
}

impl_costrunner_for_instantiation_cost_type!(VmInstantiationRun, VmInstantiation);
impl_costrunner_for_instantiation_cost_type!(
    VmInstantiationInstructionsRun,
    VmInstantiateInstructions
);
impl_costrunner_for_instantiation_cost_type!(VmInstantiationFunctionsRun, VmInstantiateFunctions);
impl_costrunner_for_instantiation_cost_type!(VmInstantiationGlobalsRun, VmInstantiateGlobals);
impl_costrunner_for_instantiation_cost_type!(
    VmInstantiationTableEntriesRun,
    VmInstantiateTableEntries
);
impl_costrunner_for_instantiation_cost_type!(VmInstantiationTypesRun, VmInstantiateTypes);
impl_costrunner_for_instantiation_cost_type!(
    VmInstantiationDataSegmentsRun,
    VmInstantiateDataSegments
);
impl_costrunner_for_instantiation_cost_type!(
    VmInstantiationElemSegmentsRun,
    VmInstantiateElemSegments
);
impl_costrunner_for_instantiation_cost_type!(VmInstantiationImportsRun, VmInstantiateImports);
impl_costrunner_for_instantiation_cost_type!(VmInstantiationExportsRun, VmInstantiateExports);
