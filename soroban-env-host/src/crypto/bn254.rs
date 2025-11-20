use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign};

use ark_bn254::{
    Bn254, Fq12, Fr, G1Affine, G1Projective,
    G2Affine, Fq as Fp, Fq2 as Fp2
};
use ark_ec::AffineRepr;
use ark_ec::{
    pairing::{Pairing, PairingOutput},
    short_weierstrass::{Affine, SWCurveConfig},
    CurveGroup,
};
use ark_ff::Field;
use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use soroban_env_common::EnvBase;

use crate::crypto::metered_scalar::MeteredScalar;
use crate::{
    budget::AsBudget,
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    Bool, BytesObject, Env, Host, HostError, TryFromVal, U256Val, Val, VecObject,
};

pub(crate) const BN254_FP_SERIALIZED_SIZE: usize = 32;
pub(crate) const BN254_FP2_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G1_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G2_SERIALIZED_SIZE: usize = BN254_FP2_SERIALIZED_SIZE * 2;

// The expected size in terms of number of `Fp`, this is a helper for budget
// charging E.g. `Fp2` will be charged as 2 units of `Fp` for encoding/decoding.
#[inline(always)]
fn units_of_fp<const EXPECTED_SIZE: usize>() -> u64 {
    EXPECTED_SIZE.div_ceil(BN254_FP_SERIALIZED_SIZE) as u64
}

impl Host {
    fn bn254_err_invalid_input(&self, msg: &str) -> HostError {
        self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, msg, &[])
    }

    fn validate_flags(&self, msb: u8) -> Result<(), HostError> {
        let flags = 0b1100_0000 & msb;
        match flags {
            0b0000_0000 => Ok(()),
            _ => Err(self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "bn254 deserialize: the two flag bits must be unset", &[]))
        }
    }

    pub(crate) fn bn254_field_element_deserialize<const EXPECTED_SIZE: usize, T: CanonicalDeserialize>(
        &self,
        input: &[u8],
        tag: &str,
    ) -> Result<T, HostError> {
        if EXPECTED_SIZE == 0 || input.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!(
                    "field element {}: invalid input length to deserialize",
                    tag
                )
                .as_str(),
                &[
                    Val::from_u32(input.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }
        self.charge_budget(ContractCostType::MemCpy, Some(EXPECTED_SIZE as u64))?;
        let mut buf = [0u8; EXPECTED_SIZE];
        buf.copy_from_slice(input);
        buf.reverse();

        self.as_budget().bulk_charge(
            ContractCostType::Bn254DecodeFp,
            units_of_fp::<EXPECTED_SIZE>(),
            None,
        )?;

        // validation turned off here to isolate the cost of serialization.
        // proper validation has to be performed outside of this function
        T::deserialize_uncompressed_unchecked(buf.as_slice()).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bn254: unable to deserialize {tag}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn bn254_field_element_serialize<const EXPECTED_SIZE: usize, T: CanonicalSerialize>(
        &self,
        elem: T,
        buf: &mut [u8],
        tag: &str,
    ) -> Result<(), HostError> {
        if EXPECTED_SIZE == 0 || buf.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("field element {tag}: invalid buffer length to serialize into").as_str(),
                &[
                    Val::from_u32(buf.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }

        self.as_budget().bulk_charge(
            ContractCostType::Bn254EncodeFp,
            units_of_fp::<EXPECTED_SIZE>(),
            None,
        )?;        

        elem.serialize_uncompressed(&mut *buf).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("field element {tag}: unable to serialize").as_str(),
                &[],
            )
        })?;

        // the reverse here works for the same reason explained above
        buf.reverse();
        Ok(())
    }

    pub(crate) fn bn254_g1_affine_deserialize(
        &self,
        bo: BytesObject,
    ) -> Result<G1Affine, HostError> {
        self.visit_obj(bo, |bytes: &ScBytes| {  
            if bytes.len() != BN254_G1_SERIALIZED_SIZE {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    format!("bn254 G1: invalid input length to deserialize").as_str(),
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(BN254_G1_SERIALIZED_SIZE as u32).into(),
                    ],
                ));
            }
            self.validate_flags(bytes[0])?;
            if bytes.as_slice() == [0u8; BN254_G1_SERIALIZED_SIZE] {
                return Ok(G1Affine::zero())
            }

            let mut x = [0u8; BN254_FP_SERIALIZED_SIZE];
            let mut y = [0u8; BN254_FP_SERIALIZED_SIZE];
            x.copy_from_slice(&bytes[0..BN254_FP_SERIALIZED_SIZE]);
            y.copy_from_slice(&bytes[BN254_FP_SERIALIZED_SIZE..]);
            let fp_x = self.bn254_field_element_deserialize::<BN254_FP_SERIALIZED_SIZE, Fp>(&x,  "bn254 Fp")?;
            let fp_y = self.bn254_field_element_deserialize::<BN254_FP_SERIALIZED_SIZE, Fp>(&y,  "bn254 Fp")?;
            let pt = G1Affine::new_unchecked(fp_x, fp_y);
            // check point is on curve
            if !self.check_point_is_on_curve(&pt, &ContractCostType::Bn254G1CheckPointOnCurve)? {
                return Err(self.bn254_err_invalid_input("bn254 G1: point not on curve"));
            }
            // G1 point does not require subgroup check, if it is on the curve
            Ok(pt)
        })
    }

    pub(crate) fn bn254_g2_affine_deserialize(
        &self,
        bo: BytesObject,
    ) -> Result<G2Affine, HostError> {
        self.visit_obj(bo, |bytes: &ScBytes| {    
            if bytes.len() != BN254_G2_SERIALIZED_SIZE {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    format!("bn254 G2: invalid input length to deserialize").as_str(),
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(BN254_G2_SERIALIZED_SIZE as u32).into(),
                    ],
                ));
            }
            self.validate_flags(bytes[0])?;
            if bytes.as_slice() == [0u8; BN254_G2_SERIALIZED_SIZE] {
                return Ok(G2Affine::zero())
            }

            let mut x = [0u8; BN254_FP2_SERIALIZED_SIZE];
            let mut y = [0u8; BN254_FP2_SERIALIZED_SIZE];
            x.copy_from_slice(&bytes[0..BN254_FP2_SERIALIZED_SIZE]);
            y.copy_from_slice(&bytes[BN254_FP2_SERIALIZED_SIZE..]);
            let fp2_x = self.bn254_field_element_deserialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(&x,  "bn254 Fp2")?;
            let fp2_y = self.bn254_field_element_deserialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(&y,  "bn254 Fp2")?;
            let pt = G2Affine::new_unchecked(fp2_x, fp2_y);
            // check point is on curve
            if !self.check_point_is_on_curve(&pt, &ContractCostType::Bn254G2CheckPointOnCurve)? {
                return Err(self.bn254_err_invalid_input("bn254 G2: point not on curve"));
            }    
            // G2 point needs subgroup check
            if !self.bn254_check_g2_point_is_in_subgroup(&pt)? {
                return Err(self.bn254_err_invalid_input(
                    "bn254 G2: point not in the correct subgroup",
                ));
            }
            Ok(pt)
        })
    }

    pub(crate) fn bn254_check_point_is_on_curve<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
        ty: &ContractCostType,
    ) -> Result<bool, HostError> {
        // passing ty by reference in order to make it more template friendly for cost_runner code
        self.charge_budget(*ty, None)?;
        Ok(pt.is_on_curve())
    }

    pub(crate) fn bn254_check_g2_point_is_in_subgroup<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
    ) -> Result<bool, HostError> {
        // passing ty by reference in order to make it more template friendly for cost_runner code
        // The check is free for G1

        self.charge_budget(ContractCostType::Bn254G2CheckPointInSubgroup, None)?;
        Ok(pt.is_in_correct_subgroup_assuming_on_curve())
    }

    pub(crate) fn bn254_g1_projective_into_affine(
        &self,
        g1: G1Projective,
    ) -> Result<G1Affine, HostError> {
        self.charge_budget(ContractCostType::Bn254G1ProjectiveToAffine, None)?;
        Ok(g1.into_affine())
    }

    pub(crate) fn bn254_g1_affine_serialize_uncompressed(
        &self,
        g1: &G1Affine,
    ) -> Result<BytesObject, HostError> {
        // handle infinity point
        if g1.infinity {
            return self.bytes_new_from_slice(&[0u8; BN254_G1_SERIALIZED_SIZE]);
        }
        let mut buf = [0u8; BN254_G1_SERIALIZED_SIZE];
        self.bn254_field_element_serialize::<BN254_FP_SERIALIZED_SIZE, Fp>(g1.x, &mut buf[0..BN254_FP_SERIALIZED_SIZE], "BN254 Fp")?;
        self.bn254_field_element_serialize::<BN254_FP_SERIALIZED_SIZE, Fp>(g1.y, &mut buf[BN254_FP_SERIALIZED_SIZE..], "BN254 Fp")?;
        self.bytes_new_from_slice(&buf)
    }

    #[cfg(test)]
    pub(crate) fn bn254_g2_affine_serialize_uncompressed(
        &self,
        g2: &G2Affine,
    ) -> Result<BytesObject, HostError> {
        // handle infinity point
        if g2.infinity {
            return self.bytes_new_from_slice(&[0u8; BN254_G2_SERIALIZED_SIZE]);
        }
        let mut buf = [0u8; BN254_G2_SERIALIZED_SIZE];
        self.bn254_field_element_serialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(g2.x, &mut buf[0..BN254_FP2_SERIALIZED_SIZE], "BN254 Fp2")?;
        self.bn254_field_element_serialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(g2.y, &mut buf[BN254_FP2_SERIALIZED_SIZE..], "BN254 Fp2")?;
        self.bytes_new_from_slice(&buf)
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
        self.charge_budget(ContractCostType::Bn254G1Add, None)?;
        Ok(p0.add(p1))
    }

    pub(crate) fn bn254_g1_mul_internal(
        &self,
        p0: G1Affine,
        scalar: Fr,
    ) -> Result<G1Projective, HostError> {
        self.charge_budget(ContractCostType::Bn254G1Mul, None)?;
        Ok(p0.mul(scalar))
    }

    pub(crate) fn bn254_checked_g1_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G1Affine>, HostError> {
        let len: u32 = self.vec_len(vp)?.into();
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * BN254_G1_SERIALIZED_SIZE as u64),
        )?;        

        let mut points: Vec<G1Affine> = Vec::with_capacity(len as usize);
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp = self.bn254_g1_affine_deserialize(BytesObject::try_from_val(self, p)?)?;
                points.push(pp);
            }
            Ok(())
        })?;
        Ok(points)
    }

    pub(crate) fn bn254_checked_g2_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G2Affine>, HostError> {
        let len: u32 = self.vec_len(vp)?.into();
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * BN254_G2_SERIALIZED_SIZE as u64),
        )?;        

        let mut points: Vec<G2Affine> = Vec::with_capacity(len as usize);
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp = self.bn254_g2_affine_deserialize(BytesObject::try_from_val(self, p)?)?;
                points.push(pp);
            }
            Ok(())
        })?;
        Ok(points)
    }

    pub(crate) fn bn254_fr_from_u256val(&self, sv: U256Val) -> Result<Fr, HostError> {
        <Fr as MeteredScalar>::from_u256val(self, sv)
    }

    #[cfg(feature = "bench")]
    pub(crate) fn bn254_fr_to_u256val(&self, fr: Fr) -> Result<U256Val, HostError> {
        fr.into_u256val(self)
    }

    pub(crate) fn bn254_fr_add_internal(&self, lhs: &mut Fr, rhs: &Fr) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::Bn254FrAddSub, None)?;
        lhs.add_assign(rhs);
        Ok(())
    }

    pub(crate) fn bn254_fr_mul_internal(&self, lhs: &mut Fr, rhs: &Fr) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::Bn254FrMul, None)?;
        lhs.mul_assign(rhs);
        Ok(())
    }

    pub(crate) fn bn254_fr_pow_internal(&self, lhs: &Fr, rhs: &u64) -> Result<Fr, HostError> {
        self.charge_budget(
            ContractCostType::Bn254FrPow,
            Some(64 - rhs.leading_zeros() as u64),
        )?;
        Ok(lhs.pow(&[*rhs]))
    }

    #[cfg(feature = "bench")]
    pub(crate) fn bn254_fr_inv_internal(&self, lhs: &Fr) -> Result<Fr, HostError> {
        self.charge_budget(ContractCostType::Bn254FrInv, None)?;
        lhs.inverse().ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "bn254 fr_inv: field element has no inverse",
                &[],
            )
        })
    }

    pub(crate) fn bn254_pairing_internal(
        &self,
        vp1: &Vec<G1Affine>,
        vp2: &Vec<G2Affine>,
    ) -> Result<PairingOutput<Bn254>, HostError> {
        self.charge_budget(ContractCostType::Bn254Pairing, Some(vp1.len() as u64))?;
        // check length requirements
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
        // `Vec<(G1Prepared, G2Prepared::EllCoeff<Config>)>`, the latter contains
        // three elements of Fp2. For each pair, the coeffs.next() can at most
        // be called twice, when the bit being looped over in `Config::X` is
        // set. So this panic cannot happen.
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
