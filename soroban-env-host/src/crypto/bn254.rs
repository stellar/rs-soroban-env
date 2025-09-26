use std::cmp::Ordering;
use std::ops::{Add, Mul};

use ark_bn254::{
    g1::Config as G1Config, g2::Config as G2Config, Bn254, Fq12, Fr, G1Affine, G1Projective,
    G2Affine,
};
use ark_ec::{
    pairing::{Pairing, PairingOutput},
    short_weierstrass::{Affine, SWCurveConfig},
    CurveGroup,
};
use ark_ff::{Field, PrimeField};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Validate};

use crate::{
    budget::AsBudget,
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    Bool, BytesObject, Env, Host, HostError, TryFromVal, U256Object, U256Small, U256Val, Val,
    VecObject, U256,
};

pub(crate) const BN254_FP_SERIALIZED_SIZE: usize = 32;
pub(crate) const BN254_FP2_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G1_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G2_SERIALIZED_SIZE: usize = BN254_FP2_SERIALIZED_SIZE * 2;

#[inline(always)]
fn units_of_fp<const EXPECTED_SIZE: usize>() -> u64 {
    EXPECTED_SIZE.div_ceil(BN254_FP_SERIALIZED_SIZE) as u64
}

impl Host {
    fn bn254_err_invalid_input(&self, msg: &str) -> HostError {
        self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, msg, &[])
    }

    // This is the internal routine performing deserialization on various
    // element types, which can be conceptually decomposed into units of Fp
    // (the base field element), and will be charged accordingly.
    // Validation of the deserialized entity must be performed outside of this
    // function, to keep budget charging isolated.
    fn bn254_deserialize_uncompressed_no_validate<
        const EXPECTED_SIZE: usize,
        T: CanonicalDeserialize,
    >(
        &self,
        slice: &[u8],
        tag: &str,
    ) -> Result<T, HostError> {
        if EXPECTED_SIZE == 0 || slice.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bn254 {tag}: invalid input length to deserialize").as_str(),
                &[
                    Val::from_u32(slice.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }

        // FIXME: BN not bls
        self.as_budget().bulk_charge(
            ContractCostType::Bls12381DecodeFp,
            units_of_fp::<EXPECTED_SIZE>(),
            None,
        )?;

        // validation turned off here to isolate the cost of serialization.
        // proper validation has to be performed outside of this function
        T::deserialize_with_mode(slice, Compress::No, Validate::No).map_err(|_e| {
            self.bn254_err_invalid_input(format!("bn254: unable to deserialize {tag}").as_str())
        })
    }

    pub(crate) fn bn254_affine_deserialize<const EXPECTED_SIZE: usize, P: SWCurveConfig>(
        &self,
        bo: BytesObject,
        subgroup_check: bool,
        tag: &str,
    ) -> Result<Affine<P>, HostError> {
        let pt: Affine<P> = self.visit_obj(bo, |bytes: &ScBytes| {
            self.bn254_deserialize_uncompressed_no_validate::<EXPECTED_SIZE, _>(
                bytes.as_slice(),
                tag,
            )
        })?;

        if !self.bn254_check_point_is_on_curve(&pt)? {
            return Err(
                self.bn254_err_invalid_input(format!("bn254 {}: point not on curve", tag).as_str())
            );
        }
        if subgroup_check && !self.bn254_check_point_is_in_subgroup(&pt)? {
            self.bn254_err_invalid_input(
                format!("bn254 {}: point not in the correct subgroup", tag).as_str(),
            );
        }
        Ok(pt)
    }

    pub(crate) fn bn254_affine_vec_from_vecobj<const EXPECTED_SIZE: usize, P: SWCurveConfig>(
        &self,
        vp: VecObject,
        subgroup_check: bool,
        tag: &str,
    ) -> Result<Vec<Affine<P>>, HostError> {
        let len: u32 = self.vec_len(vp)?.into();
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * EXPECTED_SIZE as u64),
        )?;

        let mut points: Vec<Affine<P>> = Vec::with_capacity(len as usize);

        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp = self.bn254_affine_deserialize::<EXPECTED_SIZE, P>(
                    BytesObject::try_from_val(self, p)?,
                    subgroup_check,
                    tag,
                )?;
                points.push(pp);
            }
            Ok(())
        })?;
        Ok(points)
    }

    pub(crate) fn bn254_check_point_is_on_curve<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
    ) -> Result<bool, HostError> {
        // FIXME: Probably incorrect charge
        self.charge_budget(ContractCostType::MemCmp, None)?;
        Ok(pt.is_on_curve())
    }

    pub(crate) fn bn254_check_point_is_in_subgroup<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
    ) -> Result<bool, HostError> {
        // FIXME: Probably incorrect charge
        self.charge_budget(ContractCostType::MemCmp, None)?;
        Ok(pt.is_in_correct_subgroup_assuming_on_curve())
    }

    pub(crate) fn bn254_g1_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
        subgroup_check: bool,
    ) -> Result<G1Affine, HostError> {
        self.bn254_affine_deserialize::<BN254_G1_SERIALIZED_SIZE, G1Config>(
            bo,
            subgroup_check,
            "G1",
        )
    }

    pub(crate) fn bn254_g1_projective_into_affine(
        &self,
        g1: G1Projective,
    ) -> Result<G1Affine, HostError> {
        self.charge_budget(ContractCostType::MemCmp, None)?;
        Ok(g1.into_affine())
    }

    pub(crate) fn bn254_g1_affine_serialize_uncompressed(
        &self,
        g1: &G1Affine,
    ) -> Result<BytesObject, HostError> {
        let mut buf = [0u8; BN254_G1_SERIALIZED_SIZE];
        // FIXME: Use Bn type instead of BLS
        self.as_budget().bulk_charge(
            ContractCostType::Bls12381EncodeFp,
            units_of_fp::<BN254_G1_SERIALIZED_SIZE>(),
            None,
        )?;
        g1.serialize_uncompressed(buf.as_mut_slice())
            .map_err(|_e| self.bn254_err_invalid_input("bn254: unable to serialize G1"))?;
        self.add_host_object(self.scbytes_from_slice(&buf)?)
    }

    pub(crate) fn bn254_g1_projective_serialize_uncompressed(
        &self,
        g1: G1Projective,
    ) -> Result<BytesObject, HostError> {
        let g1_affine = self.bn254_g1_projective_into_affine(g1)?;
        self.bn254_g1_affine_serialize_uncompressed(&g1_affine)
    }

    pub(crate) fn bn254_g1_add_internal(
        &self,
        p0: G1Affine,
        p1: G1Affine,
    ) -> Result<G1Projective, HostError> {
        self.charge_budget(ContractCostType::MemCmp, None)?;
        Ok(p0.add(p1))
    }

    pub(crate) fn bn254_g1_mul_internal(
        &self,
        p0: G1Affine,
        scalar: Fr,
    ) -> Result<G1Projective, HostError> {
        self.charge_budget(ContractCostType::MemCmp, None)?;
        Ok(p0.mul(scalar))
    }

    pub(crate) fn bn254_checked_g1_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G1Affine>, HostError> {
        self.bn254_affine_vec_from_vecobj::<BN254_G1_SERIALIZED_SIZE, G1Config>(vp, true, "G1")
    }

    pub(crate) fn bn254_checked_g2_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G2Affine>, HostError> {
        self.bn254_affine_vec_from_vecobj::<BN254_G2_SERIALIZED_SIZE, G2Config>(vp, true, "G2")
    }

    pub(crate) fn bn254_fr_from_u256val(&self, sv: U256Val) -> Result<Fr, HostError> {
        self.charge_budget(ContractCostType::MemCpy, None)?;
        let fr = if let Ok(small) = U256Small::try_from(sv) {
            Fr::from_le_bytes_mod_order(&u64::from(small).to_le_bytes())
        } else {
            let obj: U256Object = sv.try_into()?;
            self.visit_obj(obj, |u: &U256| {
                Ok(Fr::from_le_bytes_mod_order(&u.to_le_bytes()))
            })?
        };
        Ok(fr)
    }

    pub(crate) fn bn254_pairing_internal(
        &self,
        vp1: &Vec<G1Affine>,
        vp2: &Vec<G2Affine>,
    ) -> Result<PairingOutput<Bn254>, HostError> {
        self.charge_budget(ContractCostType::MemCmp, Some(vp1.len() as u64))?;
        if vp1.len() != vp2.len() || vp1.is_empty() {
            return Err(self.bn254_err_invalid_input(
                format!(
                    "pairing: invalid input vector lengths ({}, {})",
                    vp1.len(),
                    vp2.len()
                )
                .as_str(),
            ));
        }
        // This calls into `bn254::multi_miller_loop`, which delegates to
        // `ark_ec::models::bn::BnConfig::multi_miller_loop` with the
        // parameters defined in `ark_bn254`.
        //
        // Panic analysis:
        //
        // The following potential panic conditions could exist:
        // 1. if two input vector lengths are not equal. There is a `zip_eq`
        // which panics if the length of the two vectors are not equal. This is
        // weeded out up front.
        //
        // 2. `coeffs.next().unwrap()`. This occurs when the algorithm Loops
        // over pairs of `(a: G1Affine, b: G2Affine)`, converting them into
        // `Vec<(G1Prepared, G2Preared::EllCoeff<Config>)>`, the latter contains
        // three elements of Fp2. For each pair, the coeffs.next() can at most
        // be called twice, when the bit being looped over in `Config::X` is
        // set. So this panic cannot happen.
        //
        // 3. if any of the G1Affine point is infinity. The ell() function which
        // calls p.xy().unwrap(), which is when the point is infinity. This
        // condition also cannot happen because when the pairs are generated,
        // any pair containing a zero point is filtered.
        //
        // The above analysis is best effort to weed out panics from the source,
        // however the algorithm is quite involved. So we cannot be 100% certain
        // every panic condition has been excluded.
        let mlo = Bn254::multi_miller_loop(vp1, vp2);
        // final_exponentiation returning None means the `mlo.0.is_zero()`
        Bn254::final_exponentiation(mlo).ok_or_else(|| {
            self.bn254_err_invalid_input(
                "final_exponentiation has failed, most likely multi_miller_loop produced infinity",
            )
        })
    }

    pub(crate) fn bn254_check_pairing_output(
        &self,
        output: &PairingOutput<Bn254>,
    ) -> Result<Bool, HostError> {
        self.charge_budget(
            ContractCostType::MemCmp,
            Some(12 * BN254_FP_SERIALIZED_SIZE as u64),
        )?;
        match output.0.cmp(&Fq12::ONE) {
            Ordering::Equal => Ok(true.into()),
            _ => Ok(false.into()),
        }
    }
}
