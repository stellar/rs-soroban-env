use ark_bls12_381::{Bls12_381, Fq, Fq2, Fr, G1Affine, G1Projective, G2Affine, G2Projective};
use ark_ec::pairing::PairingOutput;

use crate::{
    cost_runner::{CostRunner, CostType},
    crypto::bls12_381::FP_SERIALIZED_SIZE,
    impl_const_cost_runner_for_bls_consume_sample, impl_const_cost_runner_for_bls_deref_sample,
    impl_lin_cost_runner_for_bls_deref_sample,
    xdr::ContractCostType::{
        self, Bls12381DecodeFp, Bls12381EncodeFp, Bls12381FrAddSub, Bls12381FrFromU256,
        Bls12381FrInv, Bls12381FrMul, Bls12381FrPow, Bls12381FrToU256, Bls12381G1Add,
        Bls12381G1CheckPointInSubgroup, Bls12381G1CheckPointOnCurve, Bls12381G1Msm, Bls12381G1Mul,
        Bls12381G1ProjectiveToAffine, Bls12381G2Add, Bls12381G2CheckPointInSubgroup,
        Bls12381G2CheckPointOnCurve, Bls12381G2Msm, Bls12381G2Mul, Bls12381G2ProjectiveToAffine,
        Bls12381HashToG1, Bls12381HashToG2, Bls12381MapFp2ToG2, Bls12381MapFpToG1, Bls12381Pairing,
    },
    Host, U256Val,
};
use std::hint::black_box;

pub struct Bls12381EncodeFpRun;
pub struct Bls12381DecodeFpRun;
pub struct Bls12381G1CheckPointOnCurveRun;
pub struct Bls12381G1CheckPointInSubgroupRun;
pub struct Bls12381G2CheckPointOnCurveRun;
pub struct Bls12381G2CheckPointInSubgroupRun;
pub struct Bls12381G1ProjectiveToAffineRun;
pub struct Bls12381G2ProjectiveToAffineRun;
pub struct Bls12381G1AddRun;
pub struct Bls12381G1MulRun;
pub struct Bls12381G1MsmRun;
pub struct Bls12381MapFpToG1Run;
pub struct Bls12381HashToG1Run;
pub struct Bls12381G2AddRun;
pub struct Bls12381G2MulRun;
pub struct Bls12381G2MsmRun;
pub struct Bls12381MapFp2ToG2Run;
pub struct Bls12381HashToG2Run;
pub struct Bls12381PairingRun;
pub struct Bls12381FrFromU256Run;
pub struct Bls12381FrToU256Run;
pub struct Bls12381FrAddRun;
pub struct Bls12381FrSubRun;
pub struct Bls12381FrMulRun;
pub struct Bls12381FrPowRun;
pub struct Bls12381FrInvRun;

#[derive(Clone)]
pub struct Bls12381G1ProjectiveToAffineSample(pub G1Projective);
#[derive(Clone)]
pub struct Bls12381G1AddSample(pub G1Affine, pub G1Affine);
#[derive(Clone)]
pub struct Bls12381G1MulSample(pub G1Affine, pub Fr);
#[derive(Clone)]
pub struct Bls12381G1MsmSample(
    pub Vec<G1Affine>,
    pub Vec<Fr>,
    pub ContractCostType,
    pub String,
);
#[derive(Clone)]
pub struct Bls12381MapFpToG1Sample(pub Fq, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381HashToG1Sample(pub Vec<u8>, pub Vec<u8>, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381G2ProjectiveToAffineSample(pub G2Projective);
#[derive(Clone)]
pub struct Bls12381G2AddSample(pub G2Affine, pub G2Affine);
#[derive(Clone)]
pub struct Bls12381G2MulSample(pub G2Affine, pub Fr);
#[derive(Clone)]
pub struct Bls12381G2MsmSample(
    pub Vec<G2Affine>,
    pub Vec<Fr>,
    pub ContractCostType,
    pub String,
);
#[derive(Clone)]
pub struct Bls12381MapFp2ToG2Sample(pub Fq2, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381HashToG2Sample(pub Vec<u8>, pub Vec<u8>, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381PairingSample(pub Vec<G1Affine>, pub Vec<G2Affine>);
#[derive(Clone)]
pub struct Bls12381EncodeFpSample(pub Vec<u8>, pub Fq);
#[derive(Clone)]
pub struct Bls12381DecodeFpSample(pub Vec<u8>);
#[derive(Clone)]
pub struct Bls12381G1CheckPointOnCurveSample(pub G1Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381G1CheckPointInSubgroupSample(pub G1Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381G2CheckPointOnCurveSample(pub G2Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381G2CheckPointInSubgroupSample(pub G2Affine, pub ContractCostType);
#[derive(Clone)]
pub struct Bls12381FrToU256Sample(pub Fr);
#[derive(Clone)]
pub struct Bls12381FrFromU256Sample(pub U256Val);
#[derive(Clone)]
pub struct Bls12381FrAddSubMulSample(pub Fr, pub Fr);
#[derive(Clone)]
pub struct Bls12381FrPowSample(pub Fr, pub u64);
#[derive(Clone)]
pub struct Bls12381FrInvSample(pub Fr);

impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G1ProjectiveToAffineRun,
    Bls12381G1ProjectiveToAffine,
    g1_projective_into_affine,
    Bls12381G1ProjectiveToAffineSample,
    G1Affine,
    p0
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G1AddRun,
    Bls12381G1Add,
    g1_add_internal,
    Bls12381G1AddSample,
    G1Projective,
    p0,
    p1
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G1MulRun,
    Bls12381G1Mul,
    g1_mul_internal,
    Bls12381G1MulSample,
    G1Projective,
    p0,
    scalar
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381MapFpToG1Run,
    Bls12381MapFpToG1,
    map_to_curve,
    Bls12381MapFpToG1Sample,
    G1Affine,
    fq,
    ty
);

impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G2ProjectiveToAffineRun,
    Bls12381G2ProjectiveToAffine,
    g2_projective_into_affine,
    Bls12381G2ProjectiveToAffineSample,
    G2Affine,
    p0
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G2AddRun,
    Bls12381G2Add,
    g2_add_internal,
    Bls12381G2AddSample,
    G2Projective,
    p0,
    p1
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381G2MulRun,
    Bls12381G2Mul,
    g2_mul_internal,
    Bls12381G2MulSample,
    G2Projective,
    p0,
    scalar
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381MapFp2ToG2Run,
    Bls12381MapFp2ToG2,
    map_to_curve,
    Bls12381MapFp2ToG2Sample,
    G2Affine,
    fq2,
    ty
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381FrFromU256Run,
    Bls12381FrFromU256,
    fr_from_u256val,
    Bls12381FrFromU256Sample,
    Fr,
    sv
);
impl_const_cost_runner_for_bls_consume_sample!(
    Bls12381FrToU256Run,
    Bls12381FrToU256,
    fr_to_u256val,
    Bls12381FrToU256Sample,
    U256Val,
    fr
);

impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381HashToG1Run,
    Bls12381HashToG1,
    hash_to_curve,
    Bls12381HashToG1Sample,
    G1Affine,
    domain,
    msg,
    ty
);
impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381HashToG2Run,
    Bls12381HashToG2,
    hash_to_curve,
    Bls12381HashToG2Sample,
    G2Affine,
    domain,
    msg,
    ty
);

impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381G1MsmRun,
    Bls12381G1Msm,
    msm_internal,
    Bls12381G1MsmSample,
    G1Projective,
    vp,
    vs,
    ty,
    tag
);

impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381G2MsmRun,
    Bls12381G2Msm,
    msm_internal,
    Bls12381G2MsmSample,
    G2Projective,
    vp,
    vs,
    ty,
    tag
);

type InternalPairingOutput = PairingOutput<Bls12_381>;
impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381PairingRun,
    Bls12381Pairing,
    pairing_internal,
    Bls12381PairingSample,
    InternalPairingOutput,
    vp1,
    vp2
);

// ser/deser

impl CostRunner for Bls12381EncodeFpRun {
    const COST_TYPE: CostType = CostType::Contract(Bls12381EncodeFp);

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Bls12381EncodeFpSample;

    type RecycledType = Option<Bls12381EncodeFpSample>;

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        mut sample: Bls12381EncodeFpSample,
    ) -> Self::RecycledType {
        let Bls12381EncodeFpSample(buf, fp) = &mut sample;
        let _ = host
            .serialize_uncompressed_into_slice::<FP_SERIALIZED_SIZE, _>(fp, buf, "test")
            .unwrap();
        black_box(Some(sample))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Bls12381EncodeFpSample,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Bls12381EncodeFp, None).unwrap());
        black_box(Some(sample))
    }
}

impl CostRunner for Bls12381DecodeFpRun {
    const COST_TYPE: CostType = CostType::Contract(Bls12381DecodeFp);

    const RUN_ITERATIONS: u64 = 1;

    type SampleType = Bls12381DecodeFpSample;

    type RecycledType = (Option<Self::SampleType>, Option<Fq>);

    fn run_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Bls12381DecodeFpSample,
    ) -> Self::RecycledType {
        let Bls12381DecodeFpSample(buf) = &sample;
        let res = host
            .deserialize_uncompressed_no_validate::<FP_SERIALIZED_SIZE, _>(buf, "test")
            .unwrap();
        black_box((Some(sample), Some(res)))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(Bls12381DecodeFp, None).unwrap());
        black_box((Some(sample), None))
    }
}

// fr arith

impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381FrAddRun,
    Bls12381FrAddSub,
    fr_add_internal,
    Bls12381FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381FrSubRun,
    Bls12381FrAddSub,
    fr_sub_internal,
    Bls12381FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381FrMulRun,
    Bls12381FrMul,
    fr_mul_internal,
    Bls12381FrAddSubMulSample,
    (),
    lhs,
    rhs
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381FrInvRun,
    Bls12381FrInv,
    fr_inv_internal,
    Bls12381FrInvSample,
    Fr,
    lhs
);
impl_lin_cost_runner_for_bls_deref_sample!(
    Bls12381FrPowRun,
    Bls12381FrPow,
    fr_pow_internal,
    Bls12381FrPowSample,
    Fr,
    lhs,
    rhs
);

impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381G1CheckPointOnCurveRun,
    Bls12381G1CheckPointOnCurve,
    check_point_is_on_curve,
    Bls12381G1CheckPointOnCurveSample,
    bool,
    pt,
    ty
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381G1CheckPointInSubgroupRun,
    Bls12381G1CheckPointInSubgroup,
    check_point_is_in_subgroup,
    Bls12381G1CheckPointInSubgroupSample,
    bool,
    pt,
    ty
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381G2CheckPointOnCurveRun,
    Bls12381G2CheckPointOnCurve,
    check_point_is_on_curve,
    Bls12381G2CheckPointOnCurveSample,
    bool,
    pt,
    ty
);
impl_const_cost_runner_for_bls_deref_sample!(
    Bls12381G2CheckPointInSubgroupRun,
    Bls12381G2CheckPointInSubgroup,
    check_point_is_in_subgroup,
    Bls12381G2CheckPointInSubgroupSample,
    bool,
    pt,
    ty
);
