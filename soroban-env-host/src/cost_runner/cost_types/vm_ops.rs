use crate::{
    cost_runner::{CostRunner, CostType},
    vm::{ModuleParseCostMode, ParsedModule},
    xdr::{ContractCostType::VmInstantiation, Hash},
    Vm,
};
use std::{hint::black_box, rc::Rc};

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Option<Hash>,
    pub wasm: Vec<u8>,
    pub module: Rc<ParsedModule>,
}

// Protocol 20 coarse and unified cost model
pub struct VmInstantiationRun;

impl CostRunner for VmInstantiationRun {
    const COST_TYPE: CostType = CostType::Contract(VmInstantiation);

    const RUN_ITERATIONS: u64 = 10;

    type SampleType = VmInstantiationSample;

    type RecycledType = (Option<Rc<Vm>>, Vec<u8>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let vm = black_box(
            Vm::new_with_cost_inputs(
                host,
                sample.id.unwrap(),
                &sample.wasm[..],
                sample.module.cost_inputs.clone(),
                ModuleParseCostMode::Normal,
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
        black_box(host.charge_budget(VmInstantiation, Some(0)).unwrap());
        black_box((None, sample.wasm))
    }
}

// Protocol 21 refined and split/caching cost model.
pub use v21::*;
mod v21 {
    use super::*;
    use crate::vm::ParsedModule;
    use crate::xdr::ContractCostType::{
        InstantiateWasmDataSegmentBytes, InstantiateWasmDataSegments, InstantiateWasmElemSegments,
        InstantiateWasmExports, InstantiateWasmFunctions, InstantiateWasmGlobals,
        InstantiateWasmImports, InstantiateWasmInstructions, InstantiateWasmTableEntries,
        InstantiateWasmTypes, ParseWasmDataSegmentBytes, ParseWasmDataSegments,
        ParseWasmElemSegments, ParseWasmExports, ParseWasmFunctions, ParseWasmGlobals,
        ParseWasmImports, ParseWasmInstructions, ParseWasmTableEntries, ParseWasmTypes,
        VmCachedInstantiation,
    };

    macro_rules! impl_costrunner_for_parse_cost_type {
        ($RUNNER:ty, $COST:ident) => {
            impl CostRunner for $RUNNER {
                const COST_TYPE: CostType = CostType::Contract($COST);

                const RUN_ITERATIONS: u64 = 10;

                type SampleType = VmInstantiationSample;

                type RecycledType = (Option<Rc<ParsedModule>>, Vec<u8>);

                fn run_iter(
                    host: &crate::Host,
                    _iter: u64,
                    sample: Self::SampleType,
                ) -> Self::RecycledType {
                    let module = black_box(
                        ParsedModule::new(
                            host,
                            sample.module.module.engine(),
                            &sample.wasm[..],
                            sample.module.cost_inputs.clone(),
                        )
                        .unwrap(),
                    );
                    (Some(module), sample.wasm)
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

    macro_rules! impl_costrunner_for_instantiation_cost_type {
        ($RUNNER:ty, $COST:ident, $IS_CONST:expr) => {
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
                        Vm::from_parsed_module(host, sample.id.unwrap(), sample.module).unwrap(),
                    );
                    (Some(vm), sample.wasm)
                }

                fn run_baseline_iter(
                    host: &crate::Host,
                    _iter: u64,
                    sample: Self::SampleType,
                ) -> Self::RecycledType {
                    if $IS_CONST {
                        black_box(host.charge_budget($COST, None).unwrap());
                    } else {
                        black_box(host.charge_budget($COST, Some(0)).unwrap());
                    }
                    black_box((None, sample.wasm))
                }
            }
        };
    }

    // This cost-type is recycled as unrefined-model, parse-only phase.
    pub struct VmInstantiationRun;
    // This cost-type is recycled as unrefined-model, instantiate-only phase.
    pub struct VmCachedInstantiationRun;

    pub struct ParseWasmInstructionsRun;
    pub struct ParseWasmFunctionsRun;
    pub struct ParseWasmGlobalsRun;
    pub struct ParseWasmTableEntriesRun;
    pub struct ParseWasmTypesRun;
    pub struct ParseWasmDataSegmentsRun;
    pub struct ParseWasmElemSegmentsRun;
    pub struct ParseWasmImportsRun;
    pub struct ParseWasmExportsRun;
    pub struct ParseWasmDataSegmentBytesRun;

    pub struct InstantiateWasmInstructionsRun;
    pub struct InstantiateWasmFunctionsRun;
    pub struct InstantiateWasmGlobalsRun;
    pub struct InstantiateWasmTableEntriesRun;
    pub struct InstantiateWasmTypesRun;
    pub struct InstantiateWasmDataSegmentsRun;
    pub struct InstantiateWasmElemSegmentsRun;
    pub struct InstantiateWasmImportsRun;
    pub struct InstantiateWasmExportsRun;
    pub struct InstantiateWasmDataSegmentBytesRun;

    impl_costrunner_for_parse_cost_type!(VmInstantiationRun, VmInstantiation);
    impl_costrunner_for_parse_cost_type!(ParseWasmInstructionsRun, ParseWasmInstructions);
    impl_costrunner_for_parse_cost_type!(ParseWasmFunctionsRun, ParseWasmFunctions);
    impl_costrunner_for_parse_cost_type!(ParseWasmGlobalsRun, ParseWasmGlobals);
    impl_costrunner_for_parse_cost_type!(ParseWasmTableEntriesRun, ParseWasmTableEntries);
    impl_costrunner_for_parse_cost_type!(ParseWasmTypesRun, ParseWasmTypes);
    impl_costrunner_for_parse_cost_type!(ParseWasmDataSegmentsRun, ParseWasmDataSegments);
    impl_costrunner_for_parse_cost_type!(ParseWasmElemSegmentsRun, ParseWasmElemSegments);
    impl_costrunner_for_parse_cost_type!(ParseWasmImportsRun, ParseWasmImports);
    impl_costrunner_for_parse_cost_type!(ParseWasmExportsRun, ParseWasmExports);
    impl_costrunner_for_parse_cost_type!(ParseWasmDataSegmentBytesRun, ParseWasmDataSegmentBytes);

    impl_costrunner_for_instantiation_cost_type!(
        VmCachedInstantiationRun,
        VmCachedInstantiation,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmInstructionsRun,
        InstantiateWasmInstructions,
        true
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmFunctionsRun,
        InstantiateWasmFunctions,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmGlobalsRun,
        InstantiateWasmGlobals,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmTableEntriesRun,
        InstantiateWasmTableEntries,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmTypesRun,
        InstantiateWasmTypes,
        true
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmDataSegmentsRun,
        InstantiateWasmDataSegments,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmElemSegmentsRun,
        InstantiateWasmElemSegments,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmImportsRun,
        InstantiateWasmImports,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmExportsRun,
        InstantiateWasmExports,
        false
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmDataSegmentBytesRun,
        InstantiateWasmDataSegmentBytes,
        false
    );
}
