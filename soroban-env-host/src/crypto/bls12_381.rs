use crate::{
    budget::AsBudget,
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    Bool, BytesObject, ConversionError, Env, Error, Host, HostError, TryFromVal, U256Object,
    U256Small, U256Val, Val, VecObject, U256,
};
use ark_bls12_381::{
    g1, g2, Bls12_381, Fq, Fq12, Fq2, Fr, G1Affine, G1Projective, G2Affine, G2Projective,
};
use ark_ec::{
    hashing::{
        curve_maps::wb::WBMap,
        map_to_curve_hasher::{MapToCurve, MapToCurveBasedHasher},
        HashToCurve,
    },
    pairing::{Pairing, PairingOutput},
    scalar_mul::variable_base::VariableBaseMSM,
    short_weierstrass::{Affine, Projective, SWCurveConfig},
    CurveGroup,
};
use ark_ff::{field_hashers::DefaultFieldHasher, BigInteger, Field, PrimeField};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Valid, Validate};
use num_traits::Zero;
use sha2::Sha256;
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign, SubAssign};

pub(crate) const FP_SERIALIZED_SIZE: usize = 48;
pub(crate) const FP2_SERIALIZED_SIZE: usize = FP_SERIALIZED_SIZE * 2;
pub(crate) const G1_SERIALIZED_SIZE: usize = FP_SERIALIZED_SIZE * 2;
pub(crate) const G2_SERIALIZED_SIZE: usize = FP2_SERIALIZED_SIZE * 2;
pub(crate) const FR_SERIALIZED_SIZE: usize = 32;

impl Host {
    // This is the internal routine performing deserialization on various
    // element types, which can be conceptually decomposed into units of Fp
    // (the base field element), and will be charged accordingly.
    // Validation of the deserialized entity must be performed outside of this
    // function, to keep budget charging isolated.
    pub(crate) fn deserialize_uncompessed_no_validate<T: CanonicalDeserialize>(
        &self,
        slice: &[u8],
        units_of_fp: u64,
        msg: &str,
    ) -> Result<T, HostError> {
        self.as_budget()
            .bulk_charge(ContractCostType::Bls12381DecodeFp, units_of_fp, None)?;
        // validation turned off here to isolate the cost of serialization.
        // proper validation has to be performed outside of this function
        T::deserialize_with_mode(slice, Compress::No, Validate::No).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381: unable to deserialize {msg}").as_str(),
                &[],
            )
        })
    }

    // This is the internal routine performing serialization on various
    // element types, which can be conceptually decomposed into units of Fp
    // (the base field element), and will be charged accordingly.
    pub(crate) fn serialize_uncompressed_into_slice<T: CanonicalSerialize>(
        &self,
        element: &T,
        buf: &mut [u8],
        units_of_fp: u64,
        msg: &str,
    ) -> Result<(), HostError> {
        self.as_budget()
            .bulk_charge(ContractCostType::Bls12381EncodeFp, units_of_fp, None)?;
        element.serialize_uncompressed(buf).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("bls12-381: unable to serialize {msg}").as_str(),
                &[],
            )
        })?;
        Ok(())
    }

    fn validate_point_encoding<const EXPECTED_SIZE: usize>(
        &self,
        bytes: &[u8],
        msg: &str,
    ) -> Result<(), HostError> {
        // validate input bytes length
        if EXPECTED_SIZE == 0 || bytes.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381 {msg}: invalid input length to deserialize").as_str(),
                &[
                    Val::from_u32(bytes.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }
        // validated encoded flags. The most significant three bits encode the flags,
        // i.e. `byte[0] == [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_7]`
        // - the compression_flag should be unset
        // - the infinity_flag should be set **only if** rest of bits are all zero
        // - the sort_flag should be unset
        let flags = 0b1110_0000 & bytes[0];
        match flags {
            0b0100_0000 => {
                // infinite bit is set, check all other bits are zero
                let mut expected_bytes = [0; EXPECTED_SIZE];
                expected_bytes[0] = flags;
                if bytes != expected_bytes {
                    Err(self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, format!("bls12-381 {msg} deserialize: infinity flag (bit 1) is set while remaining bits are not all zero").as_str(), &[]))
                } else {
                    Ok(())
                }
            },
            0b0000_0000 => Ok(()), // infinite bit is unset
            _ => Err(self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, format!("bls12-381 {msg} deserialize: either compression flag (bit 0) or the sort flag (bit 2) is set, while the input should be encoded uncompressed").as_str(), &[]))
        }
    }

    pub(crate) fn metered_check_point<P: SWCurveConfig>(
        &self,
        pt: Affine<P>,
        ty: ContractCostType,
    ) -> Result<Affine<P>, HostError> {
        self.charge_budget(ty, None)?;
        // performs following checks 1. point belongs to the curve 2. point
        // belongs to the correct subgroup
        pt.check().map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "bls12-381 G1 affine deserialize: invalid point",
                &[],
            )
        })?;
        Ok(pt)
    }

    pub(crate) fn g1_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
    ) -> Result<G1Affine, HostError> {
        let msg: &str = "G1";
        let pt: G1Affine = self.visit_obj(bo, |bytes: &ScBytes| {
            self.validate_point_encoding::<G1_SERIALIZED_SIZE>(&bytes, msg)?;
            // `CanonicalDeserialize` of `Affine<P>` calls into
            // `P::deserialize_with_mode`, where `P` is `arc_bls12_381::g1::Config`, the
            // core logic is in `arc_bls12_381::curves::util::read_g1_uncompressed`.
            //
            // The `arc_bls12_381` lib already expects the input to be serialized in
            // big-endian order (aligning with the common standard and contrary
            // to ark::serialize's convention),
            //
            // i.e. `input = be_bytes(X) || be_bytes(Y)` and the
            // most-significant three bits of X are flags:
            //
            // `bits(X) = [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]`
            //
            // internally when deserializing `Fp`, the flag bits are masked off
            // to get `X: Fp`. The Y however, does not have the top bits masked off
            // so it is possible for Y to exceed 381 bits. I've checked all over and
            // didn't find that being an invalid condition, so we will leave them as is.
            self.deserialize_uncompessed_no_validate(&bytes, 2, msg)
        })?;
        self.metered_check_point::<ark_bls12_381::g1::Config>(
            pt,
            ContractCostType::Bls12381G1Validate,
        )
    }

    pub(crate) fn g1_projective_into_affine(
        &self,
        g1: G1Projective,
    ) -> Result<G1Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381G1ProjectiveToAffine, None)?;
        Ok(g1.into_affine())
    }

    pub(crate) fn g1_affine_serialize_uncompressed(
        &self,
        g1: G1Affine,
    ) -> Result<BytesObject, HostError> {
        let mut buf = vec![0; G1_SERIALIZED_SIZE];
        // `CanonicalSerialize of Affine<P>` calls into
        // `P::serialize_with_mode`, where `P` is `ark_bls12_381::g1::Config`. The
        // output bytes will be in following format: `be_bytes(X) || be_bytes(Y)`
        // , where the most-significant three bits of X encodes the flags, i.e.
        //
        // bits(X) =  [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]
        //
        // This aligns with our standard (which is same as the ZCash standard
        // https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization)
        self.serialize_uncompressed_into_slice(&g1, &mut buf, 2, "G1")?;
        self.add_host_object(self.scbytes_from_vec(buf)?)
    }

    pub(crate) fn g1_projective_serialize_uncompressed(
        &self,
        g1: G1Projective,
    ) -> Result<BytesObject, HostError> {
        let g1_affine = self.g1_projective_into_affine(g1)?;
        self.g1_affine_serialize_uncompressed(g1_affine)
    }

    pub(crate) fn g2_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
    ) -> Result<G2Affine, HostError> {
        let msg: &str = "G2";
        let pt: G2Affine = self.visit_obj(bo, |bytes: &ScBytes| {
            self.validate_point_encoding::<G2_SERIALIZED_SIZE>(&bytes, msg)?;
            // `CanonicalDeserialize` of `Affine<P>` calls into
            // `P::deserialize_with_mode`, where `P` is `arc_bls12_381::g2::Config`, the
            // core logic is in `arc_bls12_381::curves::util::read_g2_uncompressed`.
            //
            // The `arc_bls12_381` lib already expects the input to be serialized in
            // big-endian order (aligning with the common standard and contrary
            // to ark::serialize's convention),
            //
            // i.e. `input = be_bytes(X) || be_bytes(Y)` and the
            // most-significant three bits of X are flags:
            //
            // `bits(X) = [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]`
            //
            // internally when deserializing `Fp`, the flag bits are masked off
            // to get `X: Fp`. The Y however, does not have the top bits masked off
            // so it is possible for Y to exceed 381 bits. I've checked all over and
            // didn't find that being an invalid condition, so we will leave them as is.
            self.deserialize_uncompessed_no_validate(&bytes, 4, msg)
        })?;
        self.metered_check_point::<ark_bls12_381::g2::Config>(
            pt,
            ContractCostType::Bls12381G2Validate,
        )
    }

    pub(crate) fn g2_projective_into_affine(
        &self,
        g2: G2Projective,
    ) -> Result<G2Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381G2ProjectiveToAffine, None)?;
        Ok(g2.into_affine())
    }

    pub(crate) fn g2_affine_serialize_uncompressed(
        &self,
        g2: G2Affine,
    ) -> Result<BytesObject, HostError> {
        let mut buf = vec![0; G2_SERIALIZED_SIZE];
        // `CanonicalSerialization of Affine<P>` where `P` is `ark_bls12_381::curves::g2::Config`,
        // calls into `P::serialize_with_mode`.
        //
        // The output is in the following format:
        // `be_bytes(X_c1) || be_bytes(X_c0) || be_bytes(Y_c1) || be_bytes(Y_c0)`
        //
        // The most significant three bits of `X_c1` encodes the flags, i.e.
        // `bits(X_c1) = [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]`
        //
        // This format conforms to the zcash standard https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization
        // and is the one we picked.
        self.serialize_uncompressed_into_slice(&g2, &mut buf, 4, "G2")?;
        self.add_host_object(self.scbytes_from_vec(buf)?)
    }

    pub(crate) fn g2_projective_serialize_uncompressed(
        &self,
        g2: G2Projective,
    ) -> Result<BytesObject, HostError> {
        let g2_affine = self.g2_projective_into_affine(g2)?;
        self.g2_affine_serialize_uncompressed(g2_affine)
    }

    pub(crate) fn fr_from_u256val(&self, sv: U256Val) -> Result<Fr, HostError> {
        self.charge_budget(ContractCostType::Bls12381FrFromU256, None)?;
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

    pub(crate) fn fr_to_u256val(&self, scalar: Fr) -> Result<U256Val, HostError> {
        self.charge_budget(ContractCostType::Bls12381FrToU256, None)?;
        let bytes: [u8; 32] = scalar
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| HostError::from(ConversionError))?;
        let u = U256::from_be_bytes(bytes);
        self.map_err(U256Val::try_from_val(self, &u))
    }

    pub(crate) fn fp_deserialize_from_bytesobj(&self, bo: BytesObject) -> Result<Fq, HostError> {
        let expected_size = FP_SERIALIZED_SIZE;
        self.visit_obj(bo, |bytes: &ScBytes| {
            if bytes.len() != expected_size {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "bls12-381 field element (Fp): invalid input length to deserialize",
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(expected_size as u32).into(),
                    ],
                ));
            }
            // `CanonicalDeserialize for Fp<P, N>` assumes input bytes in
            // little-endian order, with the highest bits being empty flags.
            // thus we must first reverse the bytes before passing them in.
            // there is no other check for Fp besides the length check.
            self.charge_budget(ContractCostType::MemCpy, Some(FP_SERIALIZED_SIZE as u64))?;
            let mut buf = [0u8; FP_SERIALIZED_SIZE];
            buf.copy_from_slice(bytes);
            buf.reverse();
            self.deserialize_uncompessed_no_validate(&buf, 1, "Fp")
        })
    }

    pub(crate) fn fp2_deserialize_from_bytesobj(&self, bo: BytesObject) -> Result<Fq2, HostError> {
        let expected_size = FP2_SERIALIZED_SIZE;
        self.visit_obj(bo, |bytes: &ScBytes| {
            if bytes.len() != expected_size {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "bls12-381 quadradic extention field element (Fp2): invalid input length to deserialize",
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(expected_size as u32).into(),
                    ],
                ));
            }
            // `CanonicalDeserialize for QuadExtField<P>` reads the first chunk,
            // deserialize it into `Fp` as c0. Then repeat for c1. The
            // deserialization for `Fp` follows same rules as above, where the
            // bytes are expected in little-endian, with the highest bits being
            // empty flags. There is no check involved.
            //
            // This is entirely reversed from the [zcash standard](https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization)
            // the one we have adopted. This is the input format we provide:
            // 
            // `input = be_bytes(c1) || be_bytes(c0)`
            // 
            // So we just need to reverse our input.
            let mut buf = [0u8; FP2_SERIALIZED_SIZE];
            buf.copy_from_slice(&bytes);
            buf.reverse();
            self.deserialize_uncompessed_no_validate(&buf, 2, "Fp2")
        })
    }

    pub(crate) fn g1_vec_from_vecobj(&self, vp: VecObject) -> Result<Vec<G1Affine>, HostError> {
        let len: u32 = self.vec_len(vp)?.into();
        let mut points: Vec<G1Affine> = vec![];
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * G1_SERIALIZED_SIZE as u64),
        )?;
        points.reserve(len as usize);
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp =
                    self.g1_affine_deserialize_from_bytesobj(BytesObject::try_from_val(self, p)?)?;
                points.push(pp);
            }
            Ok(())
        });
        Ok(points)
    }

    pub(crate) fn scalar_vec_from_vecobj(&self, vs: VecObject) -> Result<Vec<Fr>, HostError> {
        let len: u32 = self.vec_len(vs)?.into();
        let mut scalars: Vec<Fr> = vec![];
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * FR_SERIALIZED_SIZE as u64),
        )?;
        scalars.reserve(len as usize);
        let _ = self.visit_obj(vs, |vs: &HostVec| {
            for s in vs.iter() {
                let ss = self.fr_from_u256val(U256Val::try_from_val(self, s)?)?;
                scalars.push(ss);
            }
            Ok(())
        });
        Ok(scalars)
    }

    pub(crate) fn g1_add_internal(
        &self,
        p0: G1Affine,
        p1: G1Affine,
    ) -> Result<G1Projective, HostError> {
        self.charge_budget(ContractCostType::Bls12381G1Add, None)?;
        Ok(p0.add(p1))
    }

    pub(crate) fn g1_mul_internal(
        &self,
        p0: G1Affine,
        scalar: Fr,
    ) -> Result<G1Projective, HostError> {
        self.charge_budget(ContractCostType::Bls12381G1Mul, None)?;
        Ok(p0.mul(scalar))
    }

    pub(crate) fn g1_msm_internal(
        &self,
        points: &[G1Affine],
        scalars: &[Fr],
    ) -> Result<G1Projective, HostError> {
        // this check should've been done outside, here is just extra caution
        if points.len() != scalars.len() || points.len() == 0 {
            return Err(
                Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput).into(),
            );
        }
        // The actual logic happens inside msm_bigint_wnaf (ark_ec/variable_base/mod.rs)
        // under branch negation is cheap.
        // the unchecked version just skips the length equal check
        self.charge_budget(ContractCostType::Bls12381G1Msm, Some(points.len() as u64))?;
        Ok(G1Projective::msm_unchecked(points, scalars))
    }

    pub(crate) fn map_fp_to_g1_internal(&self, fp: Fq) -> Result<G1Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381MapFpToG1, None)?;
        let mapper = WBMap::<g1::Config>::new().map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })?;
        mapper.map_to_curve(fp).map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn hash_to_g1_internal(
        &self,
        domain: &[u8],
        msg: &[u8],
    ) -> Result<G1Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381HashToG1, Some(msg.len() as u64))?;
        let g1_mapper = MapToCurveBasedHasher::<
            Projective<g1::Config>,
            DefaultFieldHasher<Sha256, 128>,
            WBMap<g1::Config>,
        >::new(domain)
        .map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })?;
        g1_mapper.hash(msg.as_ref()).map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn g2_vec_from_vecobj(&self, vp: VecObject) -> Result<Vec<G2Affine>, HostError> {
        let len: u32 = self.vec_len(vp)?.into();
        self.charge_budget(
            ContractCostType::MemAlloc,
            Some(len as u64 * G2_SERIALIZED_SIZE as u64),
        )?;
        let mut points: Vec<G2Affine> = Vec::with_capacity(len as usize);
        let _ = self.visit_obj(vp, |vp: &HostVec| {
            for p in vp.iter() {
                let pp =
                    self.g2_affine_deserialize_from_bytesobj(BytesObject::try_from_val(self, p)?)?;
                points.push(pp);
            }
            Ok(())
        });
        Ok(points)
    }

    pub(crate) fn g2_add_internal(
        &self,
        p0: G2Affine,
        p1: G2Affine,
    ) -> Result<G2Projective, HostError> {
        self.charge_budget(ContractCostType::Bls12381G2Add, None)?;
        Ok(p0.add(p1))
    }

    pub(crate) fn g2_mul_internal(
        &self,
        p0: G2Affine,
        scalar: Fr,
    ) -> Result<G2Projective, HostError> {
        self.charge_budget(ContractCostType::Bls12381G2Mul, None)?;
        Ok(p0.mul(scalar))
    }

    pub(crate) fn g2_msm_internal(
        &self,
        points: &[G2Affine],
        scalars: &[Fr],
    ) -> Result<G2Projective, HostError> {
        // this check should've been done outside, here is just extra caution
        if points.len() != scalars.len() || points.len() == 0 {
            return Err(
                Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput).into(),
            );
        }
        // The actual logic happens inside msm_bigint_wnaf (ark_ec/variable_base/mod.rs)
        // under branch negation is cheap.
        // the unchecked version just skips the length equal check
        self.charge_budget(ContractCostType::Bls12381G2Msm, Some(points.len() as u64))?;
        Ok(G2Projective::msm_unchecked(points, scalars))
    }

    pub(crate) fn map_fp2_to_g2_internal(&self, fp: Fq2) -> Result<G2Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381MapFp2ToG2, None)?;
        let mapper = WBMap::<g2::Config>::new().map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })?;
        mapper.map_to_curve(fp).map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn hash_to_g2_internal(
        &self,
        domain: &[u8],
        msg: &[u8],
    ) -> Result<G2Affine, HostError> {
        self.charge_budget(ContractCostType::Bls12381HashToG2, Some(msg.len() as u64))?;
        let mapper = MapToCurveBasedHasher::<
            Projective<g2::Config>,
            DefaultFieldHasher<Sha256, 128>,
            WBMap<g2::Config>,
        >::new(domain)
        .map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })?;
        mapper.hash(msg.as_ref()).map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn pairing_internal(
        &self,
        vp1: &Vec<G1Affine>,
        vp2: &Vec<G2Affine>,
    ) -> Result<PairingOutput<Bls12_381>, HostError> {
        // this check should've been done outside, here is just extra caution
        if vp1.len() != vp2.len() || vp1.len() == 0 {
            return Err(
                Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput).into(),
            );
        }
        self.charge_budget(ContractCostType::Bls12381Pairing, Some(vp1.len() as u64))?;
        // This is doing exact same as `Bls12_381::pairing(p, q)`, but avoids
        // the `unwrap`
        let mlo = Bls12_381::multi_miller_loop(vp1, vp2);
        Bls12_381::final_exponentiation(mlo).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                "fail to perform final exponentiation",
                &[],
            )
        })
    }

    pub(crate) fn check_pairing_output(
        &self,
        output: &PairingOutput<Bls12_381>,
    ) -> Result<Bool, HostError> {
        self.charge_budget(ContractCostType::MemCmp, Some(576))?;
        match output.0.cmp(&Fq12::ONE) {
            Ordering::Equal => Ok(true.into()),
            _ => Ok(false.into()),
        }
    }

    pub(crate) fn fr_add_internal(&self, lhs: &mut Fr, rhs: &Fr) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::Bls12381FrAddSub, None)?;
        lhs.add_assign(rhs);
        Ok(())
    }

    pub(crate) fn fr_sub_internal(&self, lhs: &mut Fr, rhs: &Fr) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::Bls12381FrAddSub, None)?;
        lhs.sub_assign(rhs);
        Ok(())
    }

    pub(crate) fn fr_mul_internal(&self, lhs: &mut Fr, rhs: &Fr) -> Result<(), HostError> {
        self.charge_budget(ContractCostType::Bls12381FrMul, None)?;
        lhs.mul_assign(rhs);
        Ok(())
    }

    pub(crate) fn fr_pow_internal(&self, lhs: &Fr, rhs: &u64) -> Result<Fr, HostError> {
        self.charge_budget(
            ContractCostType::Bls12381FrPow,
            Some(64 - rhs.leading_zeros() as u64),
        )?;
        Ok(lhs.pow(&[*rhs]))
    }

    pub(crate) fn fr_inv_internal(&self, lhs: &Fr) -> Result<Fr, HostError> {
        if lhs.is_zero() {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "scalar inversion input is zero",
                &[],
            ));
        }
        self.charge_budget(ContractCostType::Bls12381FrInv, None)?;
        lhs.inverse().ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("scalar inversion {lhs} failed").as_str(),
                &[],
            )
        })
    }
}
