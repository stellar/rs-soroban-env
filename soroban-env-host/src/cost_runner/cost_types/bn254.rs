use ark_bn254::{Bn254, Fq, Fr, G1Affine, G1Projective, G2Affine};
use ark_ec::pairing::PairingOutput;

use crate::{
    cost_runner::{CostRunner, CostType},
    crypto::bn254::BN254_FP_SERIALIZED_SIZE,
    impl_const_cost_runner_for_bls_consume_sample, impl_const_cost_runner_for_bls_deref_sample,
    impl_lin_cost_runner_for_bls_deref_sample,
    xdr::ContractCostType::{
        self, Bn254DecodeFp, Bn254EncodeFp, Bn254FrAddSub, Bn254FrFromU256, Bn254FrInv, Bn254FrMul,
        Bn254FrPow, Bn254FrToU256, Bn254G1Add, Bn254G1CheckPointOnCurve, Bn254G1Mul,
        Bn254G1ProjectiveToAffine, Bn254G2CheckPointInSubgroup, Bn254G2CheckPointOnCurve,
        Bn254Pairing,
    },
    Host, U256Val,
};
use std::hint::black_box;

pub struct Bn254EncodeFpRun;
pub struct Bn254DecodeFpRun;
pub struct Bn254G1CheckPointOnCurveRun;
pub struct Bn254G2CheckPointOnCurveRun;
pub struct Bn254G2CheckPointInSubgroupRun;
pub struct Bn254G1ProjectiveToAffineRun;
pub struct Bn254G1AddRun;
pub struct Bn254G1MulRun;
pub struct Bn254PairingRun;
pub struct Bn254FrFromU256Run;
pub struct Bn254FrToU256Run;
pub struct Bn254FrAddRun;
pub struct Bn254FrSubRun;
pub struct Bn254FrMulRun;
pub struct Bn254FrPowRun;
pub struct Bn254FrInvRun;

#[derive(Clone)]
pub struct Bn254G1ProjectiveToAffineSample(pub G1Projective);
#[derive(Clone)]
pub struct Bn254G1AddSample(pub G1Affine, pub G1Affine);
#[derive(Clone)]
pub struct Bn254G1MulSample(pub G1Affine, pub Fr);
#[derive(Clone)]
pub struct Bn254PairingSample(pub Vec<G1Affine>, pub Vec<G2Affine>);
#[derive(Clone)]
pub struct Bn254EncodeFpSample(pub Vec<u8>, pub Fq);
#[derive(Clone)]
pub struct Bn254DecodeFpSample(pub Vec<u8>);
#[derive(Clone)]
pub struct Bn254G1CheckPointOnCurveSample(pub G1Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bn254G2CheckPointOnCurveSample(pub G2Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bn254G2CheckPointInSubgroupSample(pub G2Affine);
#[derive(Clone)]
pub struct Bn254FrFromU256Sample(pub U256Val);
#[derive(Clone)]
pub struct Bn254FrToU256Sample(pub Fr);
#[derive(Clone)]
pub struct Bn254FrAddSubMulSample(pub Fr, pub Fr);
#[derive(Clone)]
pub struct Bn254FrPowSample(pub Fr, pub u64);
#[derive(Clone)]
pub struct Bn254FrInvSample(pub Fr);

impl_const_cost_runner_for_bls_consume_sample!(
    Bn254G1ProjectiveToAffineRun,
    Bn254G1ProjectiveToAffine,
    bn254_g1_projective_into_affine,
    Bn254G1ProjectiveToAffineSample,
    G1Affine,
    p0
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bn254G1AddRun,
    Bn254G1Add,
    bn254_g1_add_internal,
    Bn254G1AddSample,
    G1Projective,
    p0,
    p1
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bn254G1MulRun,
    Bn254G1Mul,
    bn254_g1_mul_internal,
    Bn254G1MulSample,
    G1Projective,
    p0,
    scalar
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bn254FrFromU256Run,
    Bn254FrFromU256,
    bn254_fr_from_u256val,
    Bn254FrFromU256Sample,
    Fr,
    sv
);

type InternalPairingOutput = PairingOutput<Bn254>;
impl_lin_cost_runner_for_bls_deref_sample!(
    Bn254PairingRun,
    Bn254Pairing,
    bn254_pairing_internal,
    Bn254PairingSample,
    InternalPairingOutput,
    vp1,
    vp2
);

// ser/deser

impl CostRunner for Bn254EncodeFpRun {
    const COST_TYPE: CostType = CostType::Contract(Bn254EncodeFp);

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Bn254EncodeFpSample;

    type RecycledType = Option<Bn254EncodeFpSample>;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Bn254EncodeFpSample,
    ) -> Self::RecycledType {
        let Bn254EncodeFpSample(buf, fp) = &mut sample;
        let _ = host
            .bn254_field_element_serialize::<BN254_FP_SERIALIZED_SIZE, _>(*fp, buf, "test")
            .unwrap();
        black_box(Some(sample))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Bn254EncodeFpSample,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Bn254EncodeFp, None).unwrap());
        black_box(Some(sample))
    }
}

impl CostRunner for Bn254DecodeFpRun {
    const COST_TYPE: CostType = CostType::Contract(Bn254DecodeFp);

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Bn254DecodeFpSample;

    type RecycledType = (Option<Self::SampleType>, Option<Fq>);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Bn254DecodeFpSample) -> Self::RecycledType {
        let Bn254DecodeFpSample(buf) = &sample;
        let res = host
            .bn254_field_element_deserialize::<BN254_FP_SERIALIZED_SIZE, _>(buf, "test")
            .unwrap();
        black_box((Some(sample), Some(res)))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Bn254DecodeFp, None).unwrap());
        black_box((Some(sample), None))
    }
}

impl_const_cost_runner_for_bls_deref_sample!(
    Bn254G1CheckPointOnCurveRun,
    Bn254G1CheckPointOnCurve,
    bn254_check_point_is_on_curve,
    Bn254G1CheckPointOnCurveSample,
    bool,
    pt,
    ty
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bn254G2CheckPointOnCurveRun,
    Bn254G2CheckPointOnCurve,
    bn254_check_point_is_on_curve,
    Bn254G2CheckPointOnCurveSample,
    bool,
    pt,
    ty
);

impl_const_cost_runner_for_bls_deref_sample!(
    Bn254G2CheckPointInSubgroupRun,
    Bn254G2CheckPointInSubgroup,
    bn254_check_g2_point_is_in_subgroup,
    Bn254G2CheckPointInSubgroupSample,
    bool,
    pt
);

// fr arith

impl_const_cost_runner_for_bls_consume_sample!(
    Bn254FrToU256Run,
    Bn254FrToU256,
    bn254_fr_to_u256val,
    Bn254FrToU256Sample,
    U256Val,
    fr
);

impl_const_cost_runner_for_bls_deref_sample!(
    Bn254FrAddRun,
    Bn254FrAddSub,
    bn254_fr_add_internal,
    Bn254FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bn254FrSubRun,
    Bn254FrAddSub,
    bn254_fr_add_internal,
    Bn254FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bn254FrMulRun,
    Bn254FrMul,
    bn254_fr_mul_internal,
    Bn254FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bn254FrInvRun,
    Bn254FrInv,
    bn254_fr_inv_internal,
    Bn254FrInvSample,
    Fr,
    lhs
);
impl_lin_cost_runner_for_bls_deref_sample!(
    Bn254FrPowRun,
    Bn254FrPow,
    bn254_fr_pow_internal,
    Bn254FrPowSample,
    Fr,
    lhs,
    rhs
);
