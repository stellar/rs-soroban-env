use std::cmp::Ordering;
use std::ops::{Add, Mul};

use ark_bn254::{Bn254, Fq, Fq12, Fq2, Fr, G1Affine, G1Projective, G2Affine, G2Projective};
use ark_ec::{
    pairing::{Pairing, PairingOutput},
    short_weierstrass::{Affine, SWCurveConfig},
    AffineRepr, CurveGroup,
};
use ark_ff::{BigInteger, Field, PrimeField};
use ark_serialize::{CanonicalDeserialize, Compress, Validate};
use num_traits::Zero;

use crate::{
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    Bool, BytesObject, Host, HostError, TryFromVal, U256Object, U256Small, U256Val, Val, VecObject,
    U256,
};

pub(crate) const BN254_FP_SERIALIZED_SIZE: usize = 32;
pub(crate) const BN254_FP2_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G1_SERIALIZED_SIZE: usize = BN254_FP_SERIALIZED_SIZE * 2;
pub(crate) const BN254_G2_SERIALIZED_SIZE: usize = BN254_FP2_SERIALIZED_SIZE * 2;

impl Host {
    fn bn254_err_invalid_input(&self, msg: &str) -> HostError {
        self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, msg, &[])
    }

    fn bn254_deserialize_unchecked<T: CanonicalDeserialize>(
        &self,
        le_bytes: &[u8],
        tag: &str,
    ) -> Result<T, HostError> {
        T::deserialize_with_mode(le_bytes, Compress::No, Validate::No).map_err(|_| {
            self.bn254_err_invalid_input(format!("bn254: unable to deserialize {tag}").as_str())
        })
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
        let pt = self.visit_obj(bo, |bytes: &ScBytes| {
            if bytes.len() != BN254_G1_SERIALIZED_SIZE {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "bn254 G1: invalid input length to deserialize",
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(BN254_G1_SERIALIZED_SIZE as u32).into(),
                    ],
                ));
            }
            if bytes.iter().all(|b| b.is_zero()) {
                return Ok(G1Projective::zero().into_affine());
            }
            // Split the 64-byte big-endian encoding into 32-byte X and Y limbs.
            let mut x_be = [0u8; BN254_FP_SERIALIZED_SIZE];
            let mut y_be = [0u8; BN254_FP_SERIALIZED_SIZE];
            x_be.copy_from_slice(&bytes[0..32]);
            y_be.copy_from_slice(&bytes[32..64]);

            // Convert from big-endian to little-endian, which the field deserializer expects.
            let mut x_le = x_be;
            let mut y_le = y_be;
            x_le.reverse();
            y_le.reverse();
            // Deserialize coordinates into field elements without validating curve/subgroup membership.
            let x: Fq = self.bn254_deserialize_unchecked(&x_le, "G1.x")?;
            let y: Fq = self.bn254_deserialize_unchecked(&y_le, "G1.y")?;
            // Construct the affine point without checks; on-curve and subgroup checks follow below.
            Ok(G1Affine::new_unchecked(x, y))
        })?;

        if !self.bn254_check_point_is_on_curve(&pt)? {
            return Err(self.bn254_err_invalid_input("bn254 G1: point not on curve"));
        }

        if subgroup_check && !self.bn254_check_point_is_in_subgroup(&pt)? {
            return Err(self.bn254_err_invalid_input("bn254 G1: point not in the correct subgroup"));
        }
        Ok(pt)
    }

    pub(crate) fn bn254_g2_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
        subgroup_check: bool,
    ) -> Result<G2Affine, HostError> {
        let pt = self.visit_obj(bo, |bytes: &ScBytes| {
            if bytes.len() != BN254_G2_SERIALIZED_SIZE {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "bn254 G2: invalid input length to deserialize",
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(BN254_G2_SERIALIZED_SIZE as u32).into(),
                    ],
                ));
            }
            if bytes.iter().all(|b| b.is_zero()) {
                return Ok(G2Projective::zero().into_affine());
            }
            let mut x_c1_be = [0u8; 32];
            let mut x_c0_be = [0u8; 32];
            let mut y_c1_be = [0u8; 32];
            let mut y_c0_be = [0u8; 32];

            x_c1_be.copy_from_slice(&bytes[0..32]);
            x_c0_be.copy_from_slice(&bytes[32..64]);
            y_c1_be.copy_from_slice(&bytes[64..96]);
            y_c0_be.copy_from_slice(&bytes[96..128]);

            let mut x_c1_le = x_c1_be;
            let mut x_c0_le = x_c0_be;
            let mut y_c1_le = y_c1_be;
            let mut y_c0_le = y_c0_be;
            x_c1_le.reverse();
            x_c0_le.reverse();
            y_c1_le.reverse();
            y_c0_le.reverse();

            let x_c1: Fq = self.bn254_deserialize_unchecked(&x_c1_le, "G2.x.c1")?;
            let x_c0: Fq = self.bn254_deserialize_unchecked(&x_c0_le, "G2.x.c0")?;
            let y_c1: Fq = self.bn254_deserialize_unchecked(&y_c1_le, "G2.y.c1")?;
            let y_c0: Fq = self.bn254_deserialize_unchecked(&y_c0_le, "G2.y.c0")?;
            let x = Fq2::new(x_c0, x_c1);
            let y = Fq2::new(y_c0, y_c1);
            Ok(G2Affine::new_unchecked(x, y))
        })?;

        if !self.bn254_check_point_is_on_curve(&pt)? {
            return Err(self.bn254_err_invalid_input("bn254 G2: point not on curve"));
        }

        if subgroup_check && !self.bn254_check_point_is_in_subgroup(&pt)? {
            return Err(self.bn254_err_invalid_input("bn254 G2: point not in the correct subgroup"));
        }

        Ok(pt)
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
        if g1.is_zero() {
            return self.add_host_object(self.scbytes_from_slice(&buf)?);
        }
        let (x, y) = g1
            .xy()
            .ok_or_else(|| self.bn254_err_invalid_input("bn254 G1: infinity"))?;

        let x_be: [u8; 32] = x
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| self.bn254_err_invalid_input("bn254 G1: invalid x"))?;
        let y_be: [u8; 32] = y
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| self.bn254_err_invalid_input("bn254 G1: invalid y"))?;
        buf[0..32].copy_from_slice(&x_be);
        buf[32..64].copy_from_slice(&y_be);
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
        let mut points: Vec<G1Affine> = Vec::new();
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp = self.bn254_g1_affine_deserialize_from_bytesobj(
                    BytesObject::try_from_val(self, p)?,
                    true,
                )?;
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
        let mut points: Vec<G2Affine> = Vec::new();
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp = self.bn254_g2_affine_deserialize_from_bytesobj(
                    BytesObject::try_from_val(self, p)?,
                    true,
                )?;
                points.push(pp);
            }
            Ok(())
        })?;
        Ok(points)
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
        let mlo = Bn254::multi_miller_loop(vp1, vp2);
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
