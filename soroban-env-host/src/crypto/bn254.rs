use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign};

use ark_bn254::{Bn254, Fq as Fp, Fq12, Fq2 as Fp2, Fr, G1Affine, G1Projective, G2Affine};
use ark_ec::AffineRepr;
use ark_ec::{
    pairing::{Pairing, PairingOutput},
    short_weierstrass::{Affine, SWCurveConfig},
    CurveGroup,
};
use ark_ff::Field;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
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

    // Arkworks and ethereum differ in terms of how flags are handled.
    //
    // - In arkworks, the two free bits are reserved for flags: the
    //   most-significant-bit is the Y-sign flag, and the 2nd
    //   most-significant-bit is the infinity flag
    // - In ethereum, the two free bits are always unset.
    //
    // Since encoding/decoding is in uncompressed mode (Y is included), there is
    // no disambiguity. But we need to make sure to stick to the our
    // specification (which is modeled after Ethereum). That means during
    // decoding, we make sure the incoming bytes have the two "flag bits" remain
    // unset.
    fn bn254_validate_flags_and_check_infinity<const EXPECTED_SIZE: usize>(
        &self,
        bytes: &[u8],
        tag: &str,
    ) -> Result<bool, HostError> {
        if bytes.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bn254 {}: invalid input length to deserialize", tag).as_str(),
                &[
                    Val::from_u32(bytes.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }

        // The incoming bytes is in big-endian, the sign bits are contained in
        // the first byte (for G1Affine that's the MSB of the y-coordinate (Fp),
        // for G2Affine that's the MSB of the c1 (Fp) part of the Y-coordinate
        // (Fp2)). The highest bit (0x80) is the y-sign flag, the 2nd highest
        // bit (0x40) is the infinity flag. We ensure both are unset.
        let flags = 0b1100_0000 & bytes[0];
        if flags != 0b0000_0000 {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bn254 {} deserialize: the two flag bits must be unset", tag).as_str(),
                &[],
            ));
        }
        let inf = bytes.iter().all(|b| *b == 0u8);
        return Ok(inf);
    }

    // The encoding/decoding of field elements (Fp and Fp2) differs from
    // arkworks and ethereum
    //
    // (Notation: An Fp2 is an element in the quadratic extension field that can
    // be written as `c0 + c1 * i` `c0` is the real component, `c1` is the
    // imaginary component, both are of `Fp` type. and we use `||` to denote
    // concatenation)
    //
    // - In arkworks, `Fp` is serialized in little-endian, and `Fp2` is
    // serialized as `c0 || c1`, where each component is in little-endian
    // - In ethereum, `Fp` is serialized in big-endian, and `Fp2` is `c1 || c0`
    // where each component is big-endian encoded
    //
    // Therefore, on the decoding path, from an ethereum compatible bytes buffer
    // to arkwork's Fp/Fp2, reversing the buffer will do the trick (for both
    // `Fp` and `Fp2`).
    pub(crate) fn bn254_field_element_deserialize<
        const EXPECTED_SIZE: usize,
        T: CanonicalDeserialize,
    >(
        &self,
        input: &[u8],
        tag: &str,
    ) -> Result<T, HostError> {
        if EXPECTED_SIZE == 0 || input.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("field element {}: invalid input length to deserialize", tag).as_str(),
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

    pub(crate) fn bn254_field_element_serialize<
        const EXPECTED_SIZE: usize,
        T: CanonicalSerialize,
    >(
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

    // Arkworks and Ethereum both encode/decode the point coordinates in the
    // same order, first X, then Y. However, how they decode the element types
    // (Fp/Fp2) and how they assume the two free bits differ in important and
    // subtle ways. See `bn254_validate_flags_and_check_infinity` for flags
    // explanation and see `bn254_field_element_deserialize` for the element
    // explanation.
    //
    // Due to these differences, we cannot rely on the type's own
    // `CanonicalDeserialization`. Instead we break it down into:
    // - handle flags and point-at-infinity special case
    // - divide the buffer into two equal halves: X and Y, decode them
    //   individually
    // - construct the point back from (X, Y), perform additional checks
    pub(crate) fn bn254_g1_affine_deserialize(
        &self,
        bo: BytesObject,
    ) -> Result<G1Affine, HostError> {
        self.visit_obj(bo, |bytes: &ScBytes| {
            if self
                .bn254_validate_flags_and_check_infinity::<BN254_G1_SERIALIZED_SIZE>(bytes, "G1")?
            {
                return Ok(G1Affine::zero());
            }
            let mut x = [0u8; BN254_FP_SERIALIZED_SIZE];
            let mut y = [0u8; BN254_FP_SERIALIZED_SIZE];
            // Size already validated by bn254_validate_flags_and_check_infinity
            // These slices are guaranteed to be in bounds due to the size check,
            // but we use explicit checks for defense in depth
            x.copy_from_slice(
                bytes
                    .get(0..BN254_FP_SERIALIZED_SIZE)
                    .ok_or_else(|| self.bn254_err_invalid_input("G1 X coordinate out of bounds"))?,
            );
            y.copy_from_slice(
                bytes
                    .get(BN254_FP_SERIALIZED_SIZE..BN254_G1_SERIALIZED_SIZE)
                    .ok_or_else(|| self.bn254_err_invalid_input("G1 Y coordinate out of bounds"))?,
            );
            let fp_x = self
                .bn254_field_element_deserialize::<BN254_FP_SERIALIZED_SIZE, Fp>(&x, "bn254 Fp")?;
            let fp_y = self
                .bn254_field_element_deserialize::<BN254_FP_SERIALIZED_SIZE, Fp>(&y, "bn254 Fp")?;
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
            if self
                .bn254_validate_flags_and_check_infinity::<BN254_G2_SERIALIZED_SIZE>(bytes, "G2")?
            {
                return Ok(G2Affine::zero());
            }
            let mut x = [0u8; BN254_FP2_SERIALIZED_SIZE];
            let mut y = [0u8; BN254_FP2_SERIALIZED_SIZE];
            // Size already validated by bn254_validate_flags_and_check_infinity
            // These slices are guaranteed to be in bounds due to the size check,
            // but we use explicit checks for defense in depth
            x.copy_from_slice(
                bytes
                    .get(0..BN254_FP2_SERIALIZED_SIZE)
                    .ok_or_else(|| self.bn254_err_invalid_input("G2 X coordinate out of bounds"))?,
            );
            y.copy_from_slice(
                bytes
                    .get(BN254_FP2_SERIALIZED_SIZE..BN254_G2_SERIALIZED_SIZE)
                    .ok_or_else(|| self.bn254_err_invalid_input("G2 Y coordinate out of bounds"))?,
            );
            let fp2_x = self.bn254_field_element_deserialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(
                &x,
                "bn254 Fp2",
            )?;
            let fp2_y = self.bn254_field_element_deserialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(
                &y,
                "bn254 Fp2",
            )?;
            let pt = G2Affine::new_unchecked(fp2_x, fp2_y);
            // check point is on curve
            if !self
                .bn254_check_point_is_on_curve(&pt, &ContractCostType::Bn254G2CheckPointOnCurve)?
            {
                return Err(self.bn254_err_invalid_input("bn254 G2: point not on curve"));
            }
            // G2 point needs subgroup check
            if !self.bn254_check_g2_point_is_in_subgroup(&pt)? {
                return Err(
                    self.bn254_err_invalid_input("bn254 G2: point not in the correct subgroup")
                );
            }
            Ok(pt)
        })
    }

    pub(crate) fn bn254_g1_affine_serialize_uncompressed(
        &self,
        g1: &G1Affine,
    ) -> Result<BytesObject, HostError> {
        // we do not need to special handle flags on the serialization path,
        // since we are serializing each element directly which does not carry
        // flags, i.e. `Fp<P, N>::serialize_with_mode` just uses `EmptyFlags`.
        let mut buf = [0u8; BN254_G1_SERIALIZED_SIZE];
        self.bn254_field_element_serialize::<BN254_FP_SERIALIZED_SIZE, Fp>(
            g1.x,
            &mut buf[0..BN254_FP_SERIALIZED_SIZE],
            "BN254 Fp",
        )?;
        self.bn254_field_element_serialize::<BN254_FP_SERIALIZED_SIZE, Fp>(
            g1.y,
            &mut buf[BN254_FP_SERIALIZED_SIZE..],
            "BN254 Fp",
        )?;
        self.bytes_new_from_slice(&buf)
    }

    #[cfg(test)]
    pub(crate) fn bn254_g2_affine_serialize_uncompressed(
        &self,
        g2: &G2Affine,
    ) -> Result<BytesObject, HostError> {
        // we do not need to special handle flags on the serialization path,
        // since we are serializing each element directly which does not carry
        // flags, i.e. `Fp<P, N>::serialize_with_mode` just uses `EmptyFlags`.
        let mut buf = [0u8; BN254_G2_SERIALIZED_SIZE];
        self.bn254_field_element_serialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(
            g2.x,
            &mut buf[0..BN254_FP2_SERIALIZED_SIZE],
            "BN254 Fp2",
        )?;
        self.bn254_field_element_serialize::<BN254_FP2_SERIALIZED_SIZE, Fp2>(
            g2.y,
            &mut buf[BN254_FP2_SERIALIZED_SIZE..],
            "BN254 Fp2",
        )?;
        self.bytes_new_from_slice(&buf)
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
