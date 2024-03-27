use std::hint::black_box;
use std::marker::PhantomData;

use ecdsa::{PrimeCurve, Signature, SignatureSize};
use elliptic_curve::CurveArithmetic;
use generic_array::ArrayLength;
// use k256::{ecdsa::Signature, Secp256k1};

use crate::cost_runner::{CostRunner, CostType};
use crate::xdr::ContractCostType::DecodeEcdsaCurve256Sig;

#[derive(Clone)]
pub struct DecodeEcdsaCurve256SigSample {
    pub bytes: Vec<u8>,
}

pub struct DecodeEcdsaCurve256SigRun<C>
where
    C: PrimeCurve + CurveArithmetic,
    SignatureSize<C>: ArrayLength<u8>,
{
    phantom: PhantomData<C>,
}

impl<C> CostRunner for DecodeEcdsaCurve256SigRun<C>
where
    C: PrimeCurve + CurveArithmetic,
    SignatureSize<C>: ArrayLength<u8>,
{
    const COST_TYPE: CostType = CostType::Contract(DecodeEcdsaCurve256Sig);

    type SampleType = DecodeEcdsaCurve256SigSample;

    type RecycledType = (Option<Signature<C>>, DecodeEcdsaCurve256SigSample);

    fn run_iter(host: &crate::Host, _iter: u64, sample: Self::SampleType) -> Self::RecycledType {
        let pk = black_box(host.ecdsa_signature_from_bytes::<C>(&sample.bytes).unwrap());
        black_box((Some(pk), sample))
    }

    fn run_baseline_iter(
        host: &crate::Host,
        _iter: u64,
        sample: Self::SampleType,
    ) -> Self::RecycledType {
        black_box(host.charge_budget(DecodeEcdsaCurve256Sig, None).unwrap());
        black_box((None, sample))
    }
}
