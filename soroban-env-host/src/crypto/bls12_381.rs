use crate::{
    budget::AsBudget,
    host_object::HostVec,
    xdr::{ContractCostType, ScBytes, ScErrorCode, ScErrorType},
    Bool, BytesObject, ConversionError, Env, Host, HostError, TryFromVal, U256Object, U256Small,
    U256Val, Val, VecObject, U256,
};
use ark_bls12_381::{
    g1::Config as G1Config, g2::Config as G2Config, Bls12_381, Fq, Fq12, Fq2, Fr, G1Affine,
    G1Projective, G2Affine, G2Projective,
};
use ark_ec::{
    hashing::{
        curve_maps::wb::{WBConfig, WBMap},
        map_to_curve_hasher::{MapToCurve, MapToCurveBasedHasher},
        HashToCurve,
    },
    pairing::{Pairing, PairingOutput},
    scalar_mul::variable_base::VariableBaseMSM,
    short_weierstrass::{Affine, Projective, SWCurveConfig},
    AffineRepr, CurveConfig, CurveGroup,
};
use ark_ff::{field_hashers::DefaultFieldHasher, BigInteger, Field, PrimeField};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Validate};
use num_traits::Zero;
use sha2::Sha256;
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, MulAssign, SubAssign};

pub(crate) const FP_SERIALIZED_SIZE: usize = 48;
pub(crate) const FP2_SERIALIZED_SIZE: usize = FP_SERIALIZED_SIZE * 2;
pub(crate) const FP12_SERIALIZED_SIZE: usize = FP_SERIALIZED_SIZE * 12;
pub(crate) const G1_SERIALIZED_SIZE: usize = FP_SERIALIZED_SIZE * 2;
pub(crate) const G2_SERIALIZED_SIZE: usize = FP2_SERIALIZED_SIZE * 2;
pub(crate) const FR_SERIALIZED_SIZE: usize = 32;

#[inline(always)]
fn units_of_fp<const EXPECTED_SIZE: usize>() -> u64 {
    EXPECTED_SIZE.div_ceil(FP_SERIALIZED_SIZE) as u64
}

impl Host {
    // This is the internal routine performing deserialization on various
    // element types, which can be conceptually decomposed into units of Fp
    // (the base field element), and will be charged accordingly.
    // Validation of the deserialized entity must be performed outside of this
    // function, to keep budget charging isolated.
    pub(crate) fn deserialize_uncompressed_no_validate<
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
                format!("bls12-381 {tag}: invalid input length to deserialize").as_str(),
                &[
                    Val::from_u32(slice.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }
        self.as_budget().bulk_charge(
            ContractCostType::Bls12381DecodeFp,
            units_of_fp::<EXPECTED_SIZE>(),
            None,
        )?;
        // validation turned off here to isolate the cost of serialization.
        // proper validation has to be performed outside of this function
        T::deserialize_with_mode(slice, Compress::No, Validate::No).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381: unable to deserialize {tag}").as_str(),
                &[],
            )
        })
    }

    // This is the internal routine performing serialization on various
    // element types, which can be conceptually decomposed into units of Fp
    // (the base field element), and will be charged accordingly.
    pub(crate) fn serialize_uncompressed_into_slice<
        const EXPECTED_SIZE: usize,
        T: CanonicalSerialize,
    >(
        &self,
        element: &T,
        buf: &mut [u8],
        tag: &str,
    ) -> Result<(), HostError> {
        if EXPECTED_SIZE == 0 || buf.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381 {tag}: invalid buffer length to serialize into").as_str(),
                &[
                    Val::from_u32(buf.len() as u32).into(),
                    Val::from_u32(EXPECTED_SIZE as u32).into(),
                ],
            ));
        }
        self.as_budget().bulk_charge(
            ContractCostType::Bls12381EncodeFp,
            units_of_fp::<EXPECTED_SIZE>(),
            None,
        )?;
        element.serialize_uncompressed(buf).map_err(|_e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("bls12-381: unable to serialize {tag}").as_str(),
                &[],
            )
        })?;
        Ok(())
    }

    fn validate_point_encoding<const EXPECTED_SIZE: usize>(
        &self,
        bytes: &[u8],
        tag: &str,
    ) -> Result<(), HostError> {
        // validate input bytes length
        if EXPECTED_SIZE == 0 || bytes.len() != EXPECTED_SIZE {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381 {tag}: invalid input length to deserialize").as_str(),
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
                let is_valid = bytes[0] == 0b0100_0000 && bytes[1..].iter().all(|x| x.is_zero());
                if !is_valid {
                    Err(self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, format!("bls12-381 {tag} deserialize: infinity flag (bit 1) is set while remaining bits are not all zero").as_str(), &[]))
                } else {
                    Ok(())
                }
            },
            0b0000_0000 => Ok(()), // infinite bit is unset
            _ => Err(self.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, format!("bls12-381 {tag} deserialize: either compression flag (bit 0) or the sort flag (bit 2) is set, while the input should be encoded uncompressed").as_str(), &[]))
        }
    }

    pub(crate) fn check_point_is_on_curve<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
        ty: &ContractCostType,
    ) -> Result<bool, HostError> {
        // passing ty by reference in order to make it more template friendly for cost_runner code
        self.charge_budget(*ty, None)?;
        Ok(pt.is_on_curve())
    }

    pub(crate) fn check_point_is_in_subgroup<P: SWCurveConfig>(
        &self,
        pt: &Affine<P>,
        ty: &ContractCostType,
    ) -> Result<bool, HostError> {
        // passing ty by reference in order to make it more template friendly for cost_runner code
        self.charge_budget(*ty, None)?;
        Ok(pt.is_in_correct_subgroup_assuming_on_curve())
    }

    pub(crate) fn affine_deserialize<const EXPECTED_SIZE: usize, P: SWCurveConfig>(
        &self,
        bo: BytesObject,
        ct_curve: ContractCostType,
        subgroup_check: bool,
        ct_subgroup: ContractCostType,
        tag: &str,
    ) -> Result<Affine<P>, HostError> {
        let pt: Affine<P> = self.visit_obj(bo, |bytes: &ScBytes| {
            self.validate_point_encoding::<EXPECTED_SIZE>(&bytes, tag)?;
            // `CanonicalDeserialize` of `Affine<P>` calls into
            // `P::deserialize_with_mode`, where `P` is `arc_bls12_381::{g1,g2}::Config`, the
            // core logic is in `arc_bls12_381::curves::util::read_{g1,g2}_uncompressed`.
            //
            // The `arc_bls12_381` lib already expects the input to be serialized in
            // big-endian order (aligning with the common standard and contrary
            // to ark::serialize's convention),
            //
            // i.e. `input = be_bytes(X) || be_bytes(Y)` and the
            // most-significant three bits of X are flags:
            //
            // `bits(Affine) = [compression_flag, infinity_flag, sort_flag, ..remaining X_bits.., ..Y_bits..]`
            //
            // For `G1Affine`, each coordinate is an `Fp` that is 48 bytes.
            //
            // For `G2Affine`, each coordinate is an `Fp2` which contains two `Fp`,
            // i.e. `(c1: Fp, c0: Fp)` see `field_element_deserialize` for more details.
            //
            // Internally when deserializing `Fp`, the flag bits are masked off
            // to get `X: Fp`. The Y however, does not have the top bits masked off
            // so it is possible for Y to exceed 381 bits. Internally `Fp` deserialization
            // makes sure any value >= prime modulus results in an error.
            self.deserialize_uncompressed_no_validate::<EXPECTED_SIZE, _>(bytes.as_slice(), tag)
        })?;
        if !self.check_point_is_on_curve(&pt, &ct_curve)? {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381 {}: point not on curve", tag).as_str(),
                &[],
            ));
        }
        if subgroup_check && !self.check_point_is_in_subgroup(&pt, &ct_subgroup)? {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("bls12-381 {}: point not in the correct subgroup", tag).as_str(),
                &[],
            ));
        }
        Ok(pt)
    }

    pub(crate) fn g1_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
        subgroup_check: bool,
    ) -> Result<G1Affine, HostError> {
        self.affine_deserialize::<G1_SERIALIZED_SIZE, G1Config>(
            bo,
            ContractCostType::Bls12381G1CheckPointOnCurve,
            subgroup_check,
            ContractCostType::Bls12381G1CheckPointInSubgroup,
            "G1",
        )
    }

    pub(crate) fn g2_affine_deserialize_from_bytesobj(
        &self,
        bo: BytesObject,
        subgroup_check: bool,
    ) -> Result<G2Affine, HostError> {
        self.affine_deserialize::<G2_SERIALIZED_SIZE, G2Config>(
            bo,
            ContractCostType::Bls12381G2CheckPointOnCurve,
            subgroup_check,
            ContractCostType::Bls12381G2CheckPointInSubgroup,
            "G2",
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
        g1: &G1Affine,
    ) -> Result<BytesObject, HostError> {
        let mut buf = [0; G1_SERIALIZED_SIZE];
        // `CanonicalSerialize of Affine<P>` calls into
        // `P::serialize_with_mode`, where `P` is `ark_bls12_381::g1::Config`. The
        // output bytes will be in following format: `be_bytes(X) || be_bytes(Y)`
        // , where the most-significant three bits of X encodes the flags, i.e.
        //
        // bits(X) =  [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]
        //
        // This aligns with our chosen standard
        // (https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization)
        self.serialize_uncompressed_into_slice::<G1_SERIALIZED_SIZE, _>(g1, &mut buf, "G1")?;
        self.add_host_object(self.scbytes_from_slice(&buf)?)
    }

    pub(crate) fn g1_projective_serialize_uncompressed(
        &self,
        g1: G1Projective,
    ) -> Result<BytesObject, HostError> {
        let g1_affine = self.g1_projective_into_affine(g1)?;
        self.g1_affine_serialize_uncompressed(&g1_affine)
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
        g2: &G2Affine,
    ) -> Result<BytesObject, HostError> {
        let mut buf = [0; G2_SERIALIZED_SIZE];
        // `CanonicalSerialization of Affine<P>` where `P` is `ark_bls12_381::curves::g2::Config`,
        // calls into `P::serialize_with_mode`.
        //
        // The output is in the following format:
        // `be_bytes(X_c1) || be_bytes(X_c0) || be_bytes(Y_c1) || be_bytes(Y_c0)`
        //
        // The most significant three bits of `X_c1` encodes the flags, i.e.
        // `bits(X_c1) = [compression_flag, infinity_flag, sort_flag, bit_3, .. bit_383]`
        //
        // This aligns with the standard we've picked https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization
        self.serialize_uncompressed_into_slice::<G2_SERIALIZED_SIZE, _>(g2, &mut buf, "G2")?;
        self.add_host_object(self.scbytes_from_slice(&buf)?)
    }

    pub(crate) fn g2_projective_serialize_uncompressed(
        &self,
        g2: G2Projective,
    ) -> Result<BytesObject, HostError> {
        let g2_affine = self.g2_projective_into_affine(g2)?;
        self.g2_affine_serialize_uncompressed(&g2_affine)
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
        // The `into_bigint` carries the majority of the cost. It performs the
        // Montgomery reduction on the internal representation, which is doing a
        // number of wrapping arithmetics on each u64 word (`Fr` contains 4
        // words). The core routine is in `ark_ff::MontConfig::into_bigint`,
        // this cannot panic.
        let bytes: [u8; 32] = scalar
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| HostError::from(ConversionError))?;
        let u = U256::from_be_bytes(bytes);
        self.map_err(U256Val::try_from_val(self, &u))
    }

    pub(crate) fn field_element_deserialize<const EXPECTED_SIZE: usize, T: CanonicalDeserialize>(
        &self,
        bo: BytesObject,
        tag: &str,
    ) -> Result<T, HostError> {
        self.visit_obj(bo, |bytes: &ScBytes| {
            if bytes.len() != EXPECTED_SIZE {
                return Err(self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    format!(
                        "bls12-381 field element {}: invalid input length to deserialize",
                        tag
                    )
                    .as_str(),
                    &[
                        Val::from_u32(bytes.len() as u32).into(),
                        Val::from_u32(EXPECTED_SIZE as u32).into(),
                    ],
                ));
            }
            self.charge_budget(ContractCostType::MemCpy, Some(EXPECTED_SIZE as u64))?;
            let mut buf = [0u8; EXPECTED_SIZE];
            buf.copy_from_slice(bytes);
            buf.reverse();

            // The field element here an either be a Fp<P, N=6> (base field
            // element) or QuadExtField<P> (quadratic extension)
            //
            // - `CanonicalDeserialize for Fp<P, N>` assumes input bytes in
            // little-endian order, with the highest (right-most) bits being
            // empty flags. This is reverse of our rule, which assumes
            // big-endian order with the highest (left-most) bits for flags.
            //
            // - `CanonicalDeserialize for QuadExtField<P>` reads the first
            // chunk, deserialize it into `Fp` as `c0`. Then repeat for `c1`. The
            // deserialization for `Fp` follows same rules as above, where the
            // bytes are expected in little-endian, with the highest bits being
            // empty flags. There is no check involved. This is entirely
            // reversed from our input format: `be_bytes(c1) || be_bytes(c0)` from
            // [standard](https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#serialization)
            //
            // In either case, we just need to reverse the input bytes before
            // passing them in. There is no other check for `Fp` besides the
            // length check, internally it makes sure `Fp` is valid integer
            // modulo `q` (the prime modulus)
            self.deserialize_uncompressed_no_validate::<EXPECTED_SIZE, _>(&buf, tag)
        })
    }

    pub(crate) fn fp_deserialize_from_bytesobj(&self, bo: BytesObject) -> Result<Fq, HostError> {
        self.field_element_deserialize::<FP_SERIALIZED_SIZE, Fq>(bo, "Fp")
    }

    pub(crate) fn fp2_deserialize_from_bytesobj(&self, bo: BytesObject) -> Result<Fq2, HostError> {
        self.field_element_deserialize::<FP2_SERIALIZED_SIZE, Fq2>(bo, "Fp2")
    }

    pub(crate) fn fr_vec_from_vecobj(&self, vs: VecObject) -> Result<Vec<Fr>, HostError> {
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
        })?;
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

    pub(crate) fn affine_vec_from_vecobj<const EXPECTED_SIZE: usize, P: SWCurveConfig>(
        &self,
        vp: VecObject,
        ct_curve: ContractCostType,
        subgroup_check: bool,
        ct_subgroup: ContractCostType,
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
                let pp = self.affine_deserialize::<EXPECTED_SIZE, P>(
                    BytesObject::try_from_val(self, p)?,
                    ct_curve,
                    subgroup_check,
                    ct_subgroup,
                    tag,
                )?;
                points.push(pp);
            }
            Ok(())
        })?;
        Ok(points)
    }

    pub(crate) fn checked_g1_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G1Affine>, HostError> {
        self.affine_vec_from_vecobj::<G1_SERIALIZED_SIZE, G1Config>(
            vp,
            ContractCostType::Bls12381G1CheckPointOnCurve,
            true,
            ContractCostType::Bls12381G1CheckPointInSubgroup,
            "G1",
        )
    }

    pub(crate) fn checked_g2_vec_from_vecobj(
        &self,
        vp: VecObject,
    ) -> Result<Vec<G2Affine>, HostError> {
        self.affine_vec_from_vecobj::<G2_SERIALIZED_SIZE, G2Config>(
            vp,
            ContractCostType::Bls12381G2CheckPointOnCurve,
            true,
            ContractCostType::Bls12381G2CheckPointInSubgroup,
            "G2",
        )
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

    pub(crate) fn msm_internal<P: SWCurveConfig>(
        &self,
        points: &[Affine<P>],
        scalars: &[<P as CurveConfig>::ScalarField],
        ty: &ContractCostType,
        tag: &str,
    ) -> Result<Projective<P>, HostError> {
        self.charge_budget(*ty, Some(points.len() as u64))?;
        if points.len() != scalars.len() || points.len() == 0 {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!(
                    "{tag} msm: invalid input vector lengths ({}, {})",
                    points.len(),
                    scalars.len()
                )
                .as_str(),
                &[],
            ));
        }
        // The actual logic happens inside msm_bigint_wnaf (ark_ec/variable_base/mod.rs)
        // under branch negation is cheap.
        // the unchecked version just skips the length equal check
        Ok(Projective::<P>::msm_unchecked(points, scalars))
    }

    pub(crate) fn map_to_curve<P: WBConfig>(
        &self,
        fp: <Affine<P> as AffineRepr>::BaseField,
        ty: ContractCostType,
    ) -> Result<Affine<P>, HostError> {
        self.charge_budget(ty, None)?;

        // The `WBMap<g2::Config>::new()` first calls
        // `P::ISOGENY_MAP.apply(GENERATOR)` which returns error if the result
        // point is not on curve. This should not happen if the map constants
        // have been correctly defined. otherwise it would be an internal error
        // since it's a bug in the library implementation.
        //
        // Then it returns `WBMap`, which wraps a `SWUMap<P>` where P is the
        // `ark_bls12_381::curves::g2_swu_iso::SwuIsoConfig`.
        //
        // Potential panic condition: `SWUMap::new().unwrap()`
        //
        // The `SWUMap::new()` function performs some validation on the static
        // parameters `ZETA`, `COEFF_A`, `COEFF_B`, all of which are statically
        // defined in `ark_bls12_381::curves::g1_swu_iso` and `g2_swu_iso`.
        // Realistically this panic cannot occur, otherwise it will panic every
        // time including during tests
        let mapper = WBMap::<P>::new().map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })?;

        // The `SWUMap::map_to_curve` function contains several panic conditions
        // 1. assert!(!div3.is_zero())
        // 2. gx1.sqrt().expect()
        // 3. zeta_gx1.sqrt().expect()
        // 4. assert!(point_on_curve.is_on_curve())
        //
        // While all of these should theoretically just be debug assertions that
        // can't happen if the map parameters are correctly defined (several of
        // these have recently been downgraded to debug_assert, e.g see
        // https://github.com/arkworks-rs/algebra/pull/659#discussion_r1450808159),
        // we cannot guaruantee with 100% confidence these panics will never
        // happen.
        //
        // Otherwise, this function should never Err.
        mapper.map_to_curve(fp).map_err(|e| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                format!("hash-to-curve error {e}").as_str(),
                &[],
            )
        })
    }

    pub(crate) fn hash_to_curve<P: WBConfig>(
        &self,
        domain: &[u8],
        msg: &[u8],
        ty: &ContractCostType,
    ) -> Result<Affine<P>, HostError> {
        self.charge_budget(*ty, Some(msg.len() as u64))?;
        // check dst requirements
        let dst_len = domain.len();
        if dst_len == 0 || dst_len > 255 {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!("hash_to_curve: invalid input dst length {dst_len}, must be > 0 and < 256")
                    .as_str(),
                &[],
            ));
        }

        // The `new` function here constructs a DefaultFieldHasher and a WBMap.
        // - The DefaultFieldHasher::new() function creates an ExpanderXmd with
        // Sha256. This cannot fail or panic.
        // - Construction of WBMap follows the exact same analysis as map_to_curve
        // function earlier.
        // This function cannot realistically produce an error or panic.
        let mapper =
            MapToCurveBasedHasher::<Projective<P>, DefaultFieldHasher<Sha256, 128>, WBMap<P>>::new(
                domain,
            )
            .map_err(|e| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InternalError,
                    format!("hash-to-curve error {e}").as_str(),
                    &[],
                )
            })?;

        // `ark_ec::hashing::map_to_curve_hasher::MapToCurveBasedHasher::hash`
        // contains the following calls
        // - `DefaultFieldHasher::hash_to_field`
        // - `SWUMap::map_to_curve`
        // - `clear_cofactor`. This cannot fail or panic.
        //
        // `hash_to_field` calls the ExpanderXmd::expand function, there are two
        // assertions on the length of bytes produced by the hash function. Both
        // of these cannot happen because the output size can be computed
        // analytically. Let's use G2:
        // - `block_size = 384 (Fp bit size) + 128 (security padding) / 8 = 64`
        // - `len_in_bytes = 2 (number of elements to produce) *  2 (extention
        //   degree of Fp2) * 64 (block_size) = 256`
        // - `ell = 256 (len_in_bytes) / 32 (sha256 output size) = 8`
        //
        // # Assertion #1. ell <= 255, which is saying the expander cannot expand
        // up to a certain length. in our case ell == 8.
        // # Assertion #2. len_in_bytes < 2^16, which is clearly true as well.
        //
        // The rest is just hashing, dividing bytes into element size, and
        // producing field elements from bytes. None of these can panic or
        // error.
        //
        // The only panic conditions we cannot 100% exclude comes from
        // `map_to_curve`, see previous analysis.
        //
        // This function should not Err.
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
        self.charge_budget(ContractCostType::Bls12381Pairing, Some(vp1.len() as u64))?;
        // check length requirements
        if vp1.len() != vp2.len() || vp1.len() == 0 {
            return Err(self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                format!(
                    "pairing: invalid input vector lengths ({}, {})",
                    vp1.len(),
                    vp2.len()
                )
                .as_str(),
                &[],
            ));
        }

        // This calls into `Bls12<Config>::multi_miller_loop`, which just calls
        // `ark_ec::models::bls12::Bls12Config::multi_miller_loop` with specific
        // parameters defined in `ark_bls12_381::curves`.
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
        let mlo = Bls12_381::multi_miller_loop(vp1, vp2);
        // final_exponentiation returning None means the `mlo.0.is_zero()`
        Bls12_381::final_exponentiation(mlo).ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "final_exponentiation has failed, most likely multi_miller_loop produced infinity",
                &[],
            )
        })
    }

    pub(crate) fn check_pairing_output(
        &self,
        output: &PairingOutput<Bls12_381>,
    ) -> Result<Bool, HostError> {
        self.charge_budget(ContractCostType::MemCmp, Some(FP12_SERIALIZED_SIZE as u64))?;
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
        // `inverse()` returns `None` only if the rhs is zero, which we have
        // checked upfront, so this cannot fail.
        lhs.inverse().ok_or_else(|| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InternalError,
                "scalar inversion failed",
                &[],
            )
        })
    }
}
