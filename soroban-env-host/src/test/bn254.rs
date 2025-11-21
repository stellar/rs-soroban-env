use crate::{
    crypto::bn254::{BN254_G1_SERIALIZED_SIZE, BN254_G2_SERIALIZED_SIZE},
    xdr::{ScErrorCode, ScErrorType},
    BytesObject, Compare, Env, EnvBase, ErrorHandler, Host, HostError, U256Val, U32Val,
    {ConversionError, TryFromVal, U256},
};
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G2Affine};
use ark_ec::{AffineRepr, CurveGroup};
use ark_ff::{BigInteger, PrimeField, UniformRand};
use ark_serialize::CanonicalSerialize;
use core::panic;
use rand::{rngs::StdRng, SeedableRng};
use std::{cmp::Ordering, ops::Add};

const MODULUS: &str = "0x2523648240000001BA344D80000000086121000000000013A700000000000013";

enum InvalidPointTypes {
    TooManyBytes,
    TooFewBytes,
    PointNotOnCurve,
    PointNotInSubgroup,
    OutOfRange,
}

fn sample_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::rand(rng))
}

fn sample_g1_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq::rand(rng);
        let y = Fq::rand(rng);
        let p = G1Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.bn254_g1_affine_serialize_uncompressed(&p);
        }
    }
}

fn invalid_g1(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G1Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.bn254_g1_affine_serialize_uncompressed(&affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(
                bo,
                U32Val::from(BN254_G1_SERIALIZED_SIZE as u32),
                U32Val::from(0),
            )
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(BN254_G1_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g1_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => {
            panic!("BN254 G1 points on curve are always in subgroup")
        }
        InvalidPointTypes::OutOfRange => sample_g1_out_of_range(host, rng),
    }
}

fn compressed_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let mut buf: Vec<u8> = Vec::with_capacity(BN254_G1_SERIALIZED_SIZE / 2);
    let g1 = G1Affine::rand(rng);
    g1.serialize_compressed(&mut buf).unwrap();
    host.add_host_object(host.scbytes_from_slice(&buf)?)
}

fn sample_g1_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g1 = sample_g1(host, rng)?;
    host.bytes_copy_from_slice(g1, U32Val::from(0), MODULUS.as_bytes())
}

fn g1_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::zero())
}

fn neg_g1(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g1 = host.bn254_g1_affine_deserialize(bo)?;
    host.bn254_g1_affine_serialize_uncompressed(&-g1)
}

fn sample_g2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.bn254_g2_affine_serialize_uncompressed(&G2Affine::rand(rng))
}

fn sample_g2_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        let y = Fq2::rand(rng);
        let p = G2Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.bn254_g2_affine_serialize_uncompressed(&p);
        }
    }
}

fn sample_g2_not_in_subgroup(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        if let Some(p) = G2Affine::get_point_from_x_unchecked(x, true) {
            assert!(p.is_on_curve());
            if !p.is_in_correct_subgroup_assuming_on_curve() {
                return host.bn254_g2_affine_serialize_uncompressed(&p);
            }
        }
    }
}

fn sample_g2_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g2 = sample_g2(host, rng)?;
    host.bytes_copy_from_slice(g2, U32Val::from(0), MODULUS.as_bytes())
}

fn neg_g2(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g2 = host.bn254_g2_affine_deserialize(bo)?;
    host.bn254_g2_affine_serialize_uncompressed(&-g2)
}

fn g2_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g2_affine_serialize_uncompressed(&G2Affine::zero())
}

fn invalid_g2(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G2Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.bn254_g2_affine_serialize_uncompressed(&affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(
                bo,
                U32Val::from(BN254_G2_SERIALIZED_SIZE as u32),
                U32Val::from(0),
            )
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(BN254_G2_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g2_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => sample_g2_not_in_subgroup(host, rng),
        InvalidPointTypes::OutOfRange => sample_g2_out_of_range(host, rng),
    }
}

fn fr_to_u256val(host: &Host, fr: Fr) -> Result<U256Val, HostError> {
    let bytes: [u8; 32] = fr
        .into_bigint()
        .to_bytes_be()
        .try_into()
        .map_err(|_| HostError::from(ConversionError))?;
    let u = U256::from_be_bytes(bytes);
    host.map_err(U256Val::try_from_val(host, &u))
}

fn sample_fr(host: &Host, rng: &mut StdRng) -> Result<U256Val, HostError> {
    fr_to_u256val(host, Fr::rand(rng))
}

#[test]
fn test_bn254_g1_add() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5b; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // Add a compressed point - should fail
    {
        let p1 = compressed_g1(&host, &mut rng)?;
        let p2 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(p1, p2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // invalid p1 - test only length validation (which BN254 supports)
    {
        let p2 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::OutOfRange, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // invalid p2
    {
        let p1 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::OutOfRange, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 3. lhs.add(zero) = lhs
    {
        let p1 = sample_g1(&host, &mut rng)?;
        let res = host.bn254_g1_add(p1, g1_zero(&host)?)?;
        assert_eq!(host.obj_cmp(p1.into(), res.into())?, Ordering::Equal as i64);
    }

    // 4. zero.add(rhs) = rhs
    {
        let p2 = sample_g1(&host, &mut rng)?;
        let res = host.bn254_g1_add(g1_zero(&host)?, p2)?;
        assert_eq!(host.obj_cmp(p2.into(), res.into())?, Ordering::Equal as i64);
    }

    // 5. commutative a + b = b + a
    {
        let a = sample_g1(&host, &mut rng)?;
        let b = sample_g1(&host, &mut rng)?;
        let a_plus_b = host.bn254_g1_add(a, b)?;
        let b_plus_a = host.bn254_g1_add(b, a)?;
        assert_eq!(
            host.obj_cmp(a_plus_b.into(), b_plus_a.into())?,
            Ordering::Equal as i64
        );
    }

    // 6. associative (a + b) + c = a + (b + c)
    {
        let a = sample_g1(&host, &mut rng)?;
        let b = sample_g1(&host, &mut rng)?;
        let c = sample_g1(&host, &mut rng)?;
        let aplusb = host.bn254_g1_add(a, b)?;
        let aplusb_plus_c = host.bn254_g1_add(aplusb, c)?;
        let bplusc = host.bn254_g1_add(b, c)?;
        let a_plus_bplusc = host.bn254_g1_add(a, bplusc)?;
        assert_eq!(
            host.obj_cmp(aplusb_plus_c.into(), a_plus_bplusc.into())?,
            Ordering::Equal as i64
        );
    }

    // 7. a - a = zero
    {
        let a = sample_g1(&host, &mut rng)?;
        let neg_a = neg_g1(a.clone(), &host)?;
        let res = host.bn254_g1_add(a, neg_a)?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

#[test]
fn test_bn254_g1_mul() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5c; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // 1. lhs * 0 = 0
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = host.obj_from_u256_pieces(0, 0, 0, 0)?;
        let res = host.bn254_g1_mul(lhs, rhs.into())?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }

    // 2. lhs * 1 = lhs
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = U256Val::from_u32(1);
        let res = host.bn254_g1_mul(lhs, rhs)?;
        assert_eq!(
            host.obj_cmp(res.into(), lhs.into())?,
            Ordering::Equal as i64
        );
    }

    // 3. associative P * a * b = P * b * a
    {
        let p = sample_g1(&host, &mut rng)?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let pa = host.bn254_g1_mul(p, a)?;
        let pab = host.bn254_g1_mul(pa, b)?;
        let pb = host.bn254_g1_mul(p, b)?;
        let pba = host.bn254_g1_mul(pb, a)?;
        assert_eq!(
            host.obj_cmp(pab.into(), pba.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

#[test]
fn test_bn254_multi_pairing_check() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // 1. vector lengths don't match
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 2. vector length is 0
    {
        let vp1 = host.vec_new()?;
        let vp2 = host.vec_new()?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 3. any G1 point is invalid
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 4. any G2 point is invalid
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            invalid_g2(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 5. e(P, Q+R) = e(P, Q)*e(P, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = neg_g1(p, &host)?;
        let q = G2Affine::rand(&mut rng);
        let r = G2Affine::rand(&mut rng);
        let q_plus_r = host.bn254_g2_affine_serialize_uncompressed(&q.add(&r).into_affine())?;

        let q_bytes = host.bn254_g2_affine_serialize_uncompressed(&q)?;
        let r_bytes = host.bn254_g2_affine_serialize_uncompressed(&r)?;

        //check e(-P, Q+R)*e(P, Q)*e(P, R) == 1
        let g1_vec = host.vec_new_from_slice(&[neg_p.to_val(), p.to_val(), p.to_val()])?;
        let g2_vec =
            host.vec_new_from_slice(&[q_plus_r.to_val(), q_bytes.to_val(), r_bytes.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 6. e(P+S, R) = e(P, R)*e(S, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let s = sample_g1(&host, &mut rng)?;
        let r = sample_g2(&host, &mut rng)?;
        let neg_r = neg_g2(r, &host)?;
        let p_plus_s = host.bn254_g1_add(p, s)?;
        // check e(P+S, -R) * e(P, R)*e(S, R) == 1
        let g1_vec = host.vec_new_from_slice(&[p_plus_s.to_val(), p.to_val(), s.to_val()])?;
        let g2_vec = host.vec_new_from_slice(&[neg_r.to_val(), r.to_val(), r.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 7. e([a]P, [b]Q) = e([b]P, [a]Q) = e([ab]P, Q)= e(P, [ab]Q)
    {
        host.budget_ref().reset_default()?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = neg_g1(p, &host)?;
        let q_affine = G2Affine::rand(&mut rng);
        let neg_q = host.bn254_g2_affine_serialize_uncompressed(&-q_affine)?;

        // Use bn254_g1_mul for G1 scalar multiplications
        let a_p = host.bn254_g1_mul(p, a)?;
        let b_p = host.bn254_g1_mul(p, b)?;

        // Compute G2 scalar multiplications and ab using ark
        let a_q = host.bn254_g2_affine_serialize_uncompressed(
            &(q_affine * host.bn254_fr_from_u256val(a)?).into(),
        )?;
        let b_q = host.bn254_g2_affine_serialize_uncompressed(
            &(q_affine * host.bn254_fr_from_u256val(b)?).into(),
        )?;
        let ab_fr = host.bn254_fr_from_u256val(a)? * host.bn254_fr_from_u256val(b)?;
        let ab_p = host.bn254_g1_mul(p, fr_to_u256val(&host, ab_fr)?)?;
        let ab_q = host.bn254_g2_affine_serialize_uncompressed(&(q_affine * ab_fr).into())?;

        // check e([a]P, [b]Q) * e([b]P, [a]Q) * e([ab]P, -Q) * e(-P, [ab]Q) == 1
        let g1_vec =
            host.vec_new_from_slice(&[a_p.to_val(), b_p.to_val(), ab_p.to_val(), neg_p.to_val()])?;
        let g2_vec =
            host.vec_new_from_slice(&[b_q.to_val(), a_q.to_val(), neg_q.to_val(), ab_q.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 8. any of g1 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            g1_zero(&host)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // 9. any of g2 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            g2_zero(&host)?.to_val(),
        ])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // 10. entire vector is zero
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[g1_zero(&host)?.to_val(), g1_zero(&host)?.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[g2_zero(&host)?.to_val(), g2_zero(&host)?.to_val()])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    Ok(())
}

#[test]
fn test_serialization_roundtrip() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // g1
    {
        let g1_roundtrip_check = |g1: &G1Affine| -> Result<bool, HostError> {
            let bo = host.bn254_g1_affine_serialize_uncompressed(&g1)?;
            let g1_back = host.bn254_g1_affine_deserialize(bo)?;
            Ok(g1.eq(&g1_back))
        };
        assert!(g1_roundtrip_check(&G1Affine::zero())?);
        assert!(g1_roundtrip_check(&G1Affine::generator())?);
        for _ in 0..20 {
            // on curve (and always in subgroup for BN254 G1)
            let g1 = G1Affine::rand(&mut rng);
            assert!(g1_roundtrip_check(&g1)?)
        }
        for _ in 0..10 {
            // not on curve
            let g1 = G1Affine::new_unchecked(Fq::rand(&mut rng), Fq::rand(&mut rng));
            if g1.is_on_curve() {
                continue;
            }
            assert!(HostError::result_matches_err(
                g1_roundtrip_check(&g1),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }
    // g2
    {
        let g2_roundtrip_check = |g2: &G2Affine| -> Result<bool, HostError> {
            let bo = host.bn254_g2_affine_serialize_uncompressed(&g2)?;
            let g2_back = host.bn254_g2_affine_deserialize(bo)?;
            Ok(g2.eq(&g2_back))
        };
        assert!(g2_roundtrip_check(&G2Affine::zero())?);
        assert!(g2_roundtrip_check(&G2Affine::generator())?);
        for _ in 0..20 {
            // on curve and in subgroup
            let g2 = G2Affine::rand(&mut rng);
            assert!(g2_roundtrip_check(&g2)?)
        }
        for i in 0..10 {
            // on curve and not in subgroup
            let g2 = G2Affine::get_point_from_x_unchecked(Fq2::rand(&mut rng), (i % 2) != 0)
                .unwrap_or(G2Affine::zero());
            if !g2.is_in_correct_subgroup_assuming_on_curve() {
                assert!(HostError::result_matches_err(
                    g2_roundtrip_check(&g2),
                    (ScErrorType::Crypto, ScErrorCode::InvalidInput)
                ));
            }
        }
        for _ in 0..10 {
            // not on curve
            let g2 = G2Affine::new_unchecked(Fq2::rand(&mut rng), Fq2::rand(&mut rng));
            if g2.is_on_curve() {
                continue;
            }
            assert!(HostError::result_matches_err(
                g2_roundtrip_check(&g2),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }

    // fr
    {
        let fr_roundtrip_check = |fr: Fr| -> Result<bool, HostError> {
            let uv = fr_to_u256val(&host, fr.clone())?;
            let fr_back = host.bn254_fr_from_u256val(uv)?;
            Ok(fr == fr_back)
        };
        for _ in 0..20 {
            assert!(fr_roundtrip_check(Fr::rand(&mut rng))?)
        }
    }
    Ok(())
}

#[test]
fn hardcoded_serialization() -> Result<(), HostError> {
    /*
    This is a test vector from Matteo Lisotto with 260 bytes containing:
        4 bytes as selector that can be ignored
        64 bytes is a G1 point
        128 bytes is a G2 point
        64 bytes G1 point
        They are in big endian.
     */
    let test_data = "73c457ba01698c70b2f3dd6b19a450205e703fe33be07c41caec66a6f03384788aac1c0121a63c107b0e9d223e3ccb9343ac4244d077b481d94ebf1b6e6fb6365e5818c729b12498230d06e179d6858d9ce69c1ad1cd892bcbd9a89d2df0ba329bde93971f05a1b0e7babcfb772f3f4c2088b16f45feb2aa0c58f18f78a5861fdc5cc02007e1f86e21d516b42a6ad7bad5bbcc4b8b3fbaefd56804748828cd3dece00f531cc3ea0c1d713852e2916bf95e5e883e4ebdc2085a9652967ac6c1d164761bdb1f181e7635aad462677a0d5d24d1d7757fc074d17ba32d2aa2637b71d62c426505f71b3f87954f7b27ba60325520b87701e95d457cf3bd7d663e9f1af453fb4d";

    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // Decode hex string to bytes
    let bytes = hex::decode(test_data).unwrap();

    // Skip the first 4 bytes (selector)
    let offset = 4;

    // Extract first G1 point (64 bytes)
    let g1_1_bytes = &bytes[offset..offset + BN254_G1_SERIALIZED_SIZE];
    let g1_1 = host.add_host_object(host.scbytes_from_slice(g1_1_bytes)?)?;

    // Extract G2 point (128 bytes)
    let g2_offset = offset + BN254_G1_SERIALIZED_SIZE;
    let g2_bytes = &bytes[g2_offset..g2_offset + BN254_G2_SERIALIZED_SIZE];
    let g2 = host.add_host_object(host.scbytes_from_slice(g2_bytes)?)?;

    // Extract second G1 point (64 bytes)
    let g1_2_offset = g2_offset + BN254_G2_SERIALIZED_SIZE;
    let g1_2_bytes = &bytes[g1_2_offset..g1_2_offset + BN254_G1_SERIALIZED_SIZE];
    let g1_2 = host.add_host_object(host.scbytes_from_slice(g1_2_bytes)?)?;

    {
        let double = U256Val::from_u32(2);
        let res1 = host.bn254_g1_mul(g1_1, double)?;
        let res2 = host.bn254_g1_add(g1_1, g1_1)?;

        assert_eq!(
            host.obj_cmp(res1.into(), res2.into())?,
            Ordering::Equal as i64
        );
    }

    {
        let a_plus_b = host.bn254_g1_add(g1_1, g1_2)?;
        let b_plus_a = host.bn254_g1_add(g1_2, g1_1)?;
        assert_eq!(
            host.obj_cmp(a_plus_b.into(), b_plus_a.into())?,
            Ordering::Equal as i64
        );
    }

    // any of g2 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[g1_1.to_val(), g1_2.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[g2.to_val(), g2_zero(&host)?.to_val()])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // Pass in raw infinity point
    {
        let zero = hex::decode("0".repeat(128)).unwrap();
        let g1_zero = host.add_host_object(host.scbytes_from_slice(zero.as_slice())?)?;
        let g1_plus_zero = host.bn254_g1_add(g1_1, g1_zero)?;

        assert_eq!(
            host.obj_cmp(g1_plus_zero.into(), g1_1.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

// Tests from https://raw.githubusercontent.com/zcash-hackworks/bn/refs/heads/master/tests/serialization.rs
#[test]
fn g1_vectors() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let expected = vec![
        "0400000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002",
        "040e97c669de7c670d734ca98a3bc5176c8f82aa5e44f9a8780998c22dfa5fb5ea19305ad020d76d8529f57dbdc43f7e77a637f17b2d5db5b06a2554c1151b1255",
        "04070eef7bbfe7c9fa76338ed6d6a12b84f61a1a1b5b4d54c009b3993e2262c5fb1f4118919fac6678cb671ccfe5a3ab7ffc42dae56e8f4382ea3e6582ea7fdd15",
        "041ce66de558c33edf36b637bd39478e2f317f674e0559025d1e428f37dda84c342e916be0e1e8ab1ba6a9ef642a2f1de44c1f6f91aca23991cb636dbb997cbc01",
        "042e8d7e2ab119b53b2e2e5f8ddda2116aa821949ae0bd00a6ee6ebbe130397c0709e07974bfeae5c973c098c6dff65e864ee6ee05e9ccd1d61c8ca72e0c49232a",
        "040460253e7e4e9804bdc30e1d76f98d7f7fcb4a6845fd6f9df0f06a18e1a2ebb3066fddd38227281acd80b6b6253f5d89e401918586254edad1a56fbb8544dbbd",
        "04130f5f5f20846ffcd6dbfded402bebb7bb07e0a4cb2024bc84262de67498dd7e18dfc445b80fd0a1843d6ddb5ffc16cb8872c373b7adb9fd1b98ccc922318966",
        "042849878760819716f11108e902836a72f4bba88eadbe26b5b9f53e75c1f8714f0732eeeaacbbde8dce07be752d413d58538fc11f82a1c434afac5c27760e8688",
        "041846eb3215c26d8be94d8cb688f19f59db6c771bc8155d1fff826440bdaf8ce720553790d70aeccfb178e6c33f4331b8cc24e02c8ae8b5a0bb9b57a9c1132001",
        "0421f8b0a60f933351788799a7271fce8eb3349b98b1e0dae6a1de4eebba06daed2f966958654ace4bad972296bfd8d3f1568954bec25b665c0fc8f64f1a0c9856",
        "04163ddafd9f4677caa2b8feb510a6b0ef09191a00adfd9f09ae3e59d687af8ccb1a47d8362079b22c9064718f02948da7832a8e123b8c6e3730a9e7e137f84da7",
        "041ac98fe325a2faf01816da51b3733edb8a13935dc0b82a6fc2667773d09461ce258861533cd79b9ca801a366e40dbf61431296ca6431b51e61adb41e389328ea",
        "040dfcd832e70b1ebfc5f07d66ef71e88f1903eb69e25a43e17fcb119305f936a101f6022efdd7bb49bb2f9882d1edb54a618117ac9a213776ee09661dc813cb90",
        "041c11d08dd28ed037fb7800988bf9ed0ffe5dea2ee321ba36725bbc2fbda12df82151ba8776a883a4d2fdb5daefb681aed75636db330956b36a795031b238494a",
        "0407592e35e3444f61a5cd4bd3b62765ab5d67ff57b88aa781bfc0a14017746f9f2ff61c2caf223a33ea998503491389ec7b5ef2797f9d7bf5b63640cff086bc11",
        "040a5e52f3b28b3d8a4eae2332ddcfaeb3300432f9c11409c4388607dfe2a0dd171781fd5e8fffab898a916ee4436f487b7ff9a0a7ffdae2e0f98d6fc1075f60fe",
        "042db55c23fcdffc4000befcc40b612d1bc38d8e34ba4c1eb68f7a1446f851630d200c8cc5cf724a332ac366160836f91c086df5c8e023e0131eac51f1c18431c3",
        "04283f5279f2bc83c2c0829d1814f066d1828c267c95edc39ad4853c5cb35c02dd1f0f1bfd04a340863a34d8638c0b7c254473da29ec0c0a95fae6a7ccde78c740",
        "0401cf54e2dd9c2c97a96b809781114b82aee7d0b9f648d91fe01533062f99fe590037123d18acef099417572bdb041f77ef044b9c3fbef0ef0f9894f9a4061313",
        "040b0115940193b2c17fb561c317ed88d5b5bac173f4eacd887fc8de03b9915ce62be17e169ae45579e2f0982b686a611309a8435d1e2f0de9a49c97f72679c795",
        "04194e71fb26deed74c5681a0e616ed67135add9f3eb97fba96202539876fe0a642b27a2f7a125e849eca00af763e532d5c86ab650bcded04d47d26d8c6c62c139",
        "04001082425143d275f44e16040d7f8922ccdcd77892cd80796a86051f4ff5b2de272e4a7dbfccbd7ef79cae19d0de3a0697adc6cc44d99f014187a99b6c26d7cc",
        "041e6b87d81c1af93bacf529258f36145b434b84e843e0963674c529133b7ccbdc09384b95b6c343d8012f4a8b3a1bd6f34142d7688963df85c38369638b1c3117",
        "041e34c8d7c0ba039b67537c75939a14fd61598babc9a448cd103601253b93692c1eb61a4444ac063eb1ae75e77df30f69cda5dadc08912727ce2426276fca10ad",
        "041cd5b925c2174a380cf78f14a57388711b91c90010c55c0bea173cf77443869f1f7fdb3f0414e61d12589a46d8ae49f95ef9b1bece46a85d2d14d2deed1cbdaa",
        "042826a17e76e78d1883003a32e8cabf44ea62087a2ec95ef68d72ed31f0dc0eab11d1fcb397c18539c7ed620a502fba6f4ee19adc17aaf9501b801e72a21c7b9d",
        "0425b7a1770cc9e63055969af6b201dc7b6b5c8ac819b6ef82f39d3dec427a41c914b4f4c56c3fd44b34430f2a5cb7ae272c1a6fbcd86a22255db728fcc91d14b3",
        "04150f39cb53bcf80d39c77a2a2d52ed7f3e0ce5b576a89981b9d9d6aee05110131ad181c717291b23eb881845ba88903859be7f65fc6e795507574452a03d4e25",
        "041555ccb4555301cd5e3beaa6d0b8778a4505b72135567322f9a5a540e3b8a4fa127f16e181d7e3a75a2d944091c78caf9849eb6c3f3b24242c514e42fd30c0b3",
        "042349519b9c99cf376534801d42d098b2541903f5fd07e0fa81d71ea68c806f89086bad59e91f4341cef634582513b9359c751e3640c0fa9a205a6bde1fdaf697",
        "041033e55052e5ad95302c5dbd98a7be3387d605956d0eec74576a4251eb58f4da2534b51e49ce5768a4bef802e159fe1ffa37dba8437669d4c31292a2ecd24e4d",
        "04070ca0593d04d87f769bb59c63b76cc1478fcb5eb5528b2d32ec6294eaa155351aea25fbfd40948c8fd8d202e31c4a1374869cc9eabd991ac2e00f5b2baf4e78",
        "041d61b6c3dfba531cea1f35f652f6f94dbdbd1f62c43396dc378f70b6b0990e5b23b161cbce3acc6673a44939abc55b4a86f683e1e8ad4e0aec9e0e991c4ffd09",
        "04131d5925f21a50af1da2367abadf38f2dfd9e7257523dc4662d0bacbc0808f130dbf763b7934a0a6224c34de1b4d5a791d88729df242434e0145bb713a38288f",
        "042bfe71e528ee56f4c1c6f1c1de11864fe3d67e56b87dfb97cf608a83c4c2f18a2c2383632ed6378da76655cf8a5e151fc692ef5e07bfceb339c3d9504555a47f",
        "04058210b550fc16609877b5fc0fb3722a44e77de300ea96de918d913e952f0c401a03daecc90ce916f495fac309acf7c33694a2f77bbbc770384460c2c98fd13c",
        "0428ae164d26d0987c3c875e691d78b346d8c32a0aeb6fefe1510e5528e7fe05232a52f102a41d3dda80ec1eac66c6f759b405257bb836eec514b298747129b6ca",
        "0422b56318bed36b21b46827b3d39a6a63fd327d84ad09efdd6fd9b7d5bd71c22505fc11eb76fd14c4b5b692723e28c454f7db0a36eace914b2f2c87f9b81e52ff",
        "042a3e5a4c98a171667d6caaa396b0ccd03aee91724c0f3349214523861e5fcfa81ff385ef6803e70830ded0e23c2e76fa4d94ec16043290b16ac805dc2ca51153",
        "0420be7bffa5976e062a75eae9a4dca83c27207152d8e7301c92bf990e2eddec35166a2963ed3c15a5350c685cf5f9c6580b9b96de49afcd5842344350a9d4d03f",
        "0422e11534dc3c78583b6be6a70f21bd3f91b2d65f0776b88e871be1444c5323c21fb10a5e7f3f001a0476a99ed6eef5988d8899916f2e89c1d813a59d19bbdfcc",
        "040da396124abe233d7227258003fabf15f50e21fea5c48fd98c805104240a084113368c4132a31a97f20762b7c0fcdda013936649d410ed33575d9fd727fdd22e",
        "04278100c505a2dcdb395665dce5b91afb7d458deb9b2e148f63671f8bad4f67910271a00bb8db2441412cbe445219773f4be7b7d628a2123d667f2fca8a05055e",
        "042ccfdef921ed73c7c5bfc7e77f630048631e0e1d9dac267a2cc55fd15d9648ed30380375c926c49823159426aaa804e7aae72012d0a10373466c6c09c27ee0dc",
        "04016b16f7158ea78ed57cddf9eefee37f652faf139225b23fbbe72fdbeaaa1c5900deae79748f9742b576cfc8936fb1b24c86401e5756457791925764e4088fc3",
        "042e27be22937dac068c73fd21b5d70df22a95a7c5fc1902f28ee36f1c1211c46421657c8e8d11664c0dc1d0ea02dc0126e0430448cf415e1d7d2449dca37a06a6",
        "040d3b6892d20ddf0eb053e3fb40b71461a35f89212d25b8e6ff4c7b3e4272d5a61e8bb4cce229d8035ca37cf6a9fabf103299ce19b05a2cef38cb0948dee5b799",
        "0411d389c11fdc323af24c9e3488abb5064b1b0b312f641c2041572777922525ef26f06bda6216d6310b1d2d9997b1d721b34f8b13c5c9c06fa19badf43da68e72",
        "0425f96f8440bbe27602bcbec4ef5ae8d1bddcf2933a915438320c8d652823c9902508cabfce8c45cf54c9598cdf75371b7d55dc73cabf16e0199e83b5d06d203c",
        "040d6229b56cc2c4005612bb667790d1e5ef7d7737ed62c31b97b84d6af2e160ec03190f4f4bb3fcb136b89a99f82f4941b1c54a2b3d7224e56e04f9273c0b8d79",
        "042fcc98351a49c7137bbcf3644b6961e6b40adc959207dfa5894705da09d3b898028570506c9076b553b9399a488060c7b80c8279fc461ae9e150fcf55d4a42f0",
        "0400662d7bf0d66e45ce36e706f3c762933ed4ab09e664a58c557c3323e1eaab67118a6509dfc87dd195a650ab7781c410ebe0fbcaf1f462ab4f7966cfc705463d",
        "0413aa3bc49295213ae2369d60baacce80e2468b482a5e2a36233d9cac99c2d3f00a22280c564152ef48f9310f91a58658b39624b5d0f44b77f2d7388ed63392a9",
        "0418c6fc464fc1098293f4bebdeda8914e3eac056daa6c6fb9f0178750810cbf8d22f99251d98ea9e8b737863c65534759143af10ed2c83067b6e9b4a3dd713672",
        "040edf342ebb44b3f2f265e649376826fa04ba65e7d3800201066ed794f30b6ddd14c36e6cbb4125b46131f589e32d766f57fd36c34359f58b104e96a81473e2fe",
        "040d668b047386cb0acab1550b0c1afbeff2536de23a689bf1e129bc2ac61f6fd51f69419fa97ec0382f147232ed59c682df6fda581e2d2009522264fa67e213dc",
        "04283c1e0f0f38a5728c609b5e1ed6192ff14130c1d47245e1c26762566a4e93500553b00c7ad4aa30ccb8c2123cedb190c3d26630430071914b51800b06e88597",
        "0418799821c0ff5eabbad869f09253e0a7fd4b3e7a55aaf549b3ee84a9a198af4e2aab3995cbf05b1f4bd826d04f2f51052b620cccc2a3c22b83bcef70a1e64c73",
        "041258ee1a09c598dbaeb9955644fd23f4fc6328f1675c0ad6c847e338e72d086f16e0e91972ec2d221d9c08269298ab19fbb0cf7007e56936fb8b665af0c936fd",
        "0410bca800784c7b06518136ac6593c81187a36283a4164d2c827ad02f49610fd9137495b1e03a5c3e7d7d7980b16d6fd99baedce9cee07389066f0c68fc68d444",
        "0429e65fad8bdbb365d8001660da7e46c40e6b15c38e69066e1d7343e6a1b2cc9d248e0bb624695732a765f71e95022aad7b2961a8108e258b4da9273983450c93",
        "040a9bd379f4bf47075c922d7b4ead1f508c34d6ca473f599bd552c5858db58f8a05d0415d73bf2de7449651e777c3a06c73755a6a8881561e9c11e9c030fb97ef",
    ];

    let mut acc = host.bn254_g1_affine_serialize_uncompressed(&G1Affine::generator())?;

    let scalar = U256Val::from_u32(23938123);

    for i in 0..expected.len() {
        let hex: &str = &expected[i][2..]; // Remove first two characters
                                           // Compare acc (BytesObject) with expected hex string by converting hex to BytesObject
        let expected_bytes = hex::decode(hex).unwrap();
        let expected_bo = host.test_bin_obj(&expected_bytes)?;

        assert_eq!(
            (*host).compare(&acc.to_val(), &expected_bo.to_val())?,
            core::cmp::Ordering::Equal
        );

        let res = host.bn254_g1_mul(acc, scalar)?;
        acc = host.bn254_g1_add(res, acc)?;
    }

    Ok(())
}

// From https://www.evm.codes/precompiled
#[test]
fn g1_add_mul_hardcoded() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let expected_sum = host.test_bin_obj(&hex::decode("030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd315ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4").unwrap())?;

    let g_1_2 = "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002";

    let bytes = hex::decode(g_1_2).unwrap();
    let g1_bytes = host.test_bin_obj(&bytes)?;

    // Test g1_add: g1_bytes + g1_bytes
    let res_add = host.bn254_g1_add(g1_bytes, g1_bytes)?;

    assert_eq!(
        (*host).compare(&res_add.to_val(), &expected_sum.to_val())?,
        core::cmp::Ordering::Equal
    );

    // Test g1_mul: g1_bytes * 2
    let scalar_2 = U256Val::from_u32(2);
    let res_mul = host.bn254_g1_mul(g1_bytes, scalar_2)?;

    assert_eq!(
        (*host).compare(&res_mul.to_val(), &expected_sum.to_val())?,
        core::cmp::Ordering::Equal
    );

    Ok(())
}

// From https://www.evm.codes/precompiled
#[test]
fn g1_pairing_hardcoded() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // From Solidity pairing example
    // First pairing: e(G1_1, G2_1)
    let g1_1_x = "2cf44499d5d27bb186308b7af7af02ac5bc9eeb6a3d147c186b21fb1b76e18da";
    let g1_1_y = "2c0f001f52110ccfe69108924926e45f0b0c868df0e7bde1fe16d3242dc715f6";
    let g1_1_bytes = hex::decode(format!("{}{}", g1_1_x, g1_1_y)).unwrap();
    let g1_1 = host.test_bin_obj(&g1_1_bytes)?;

    let g2_1_x1 = "1fb19bb476f6b9e44e2a32234da8212f61cd63919354bc06aef31e3cfaff3ebc";
    let g2_1_x0 = "22606845ff186793914e03e21df544c34ffe2f2f3504de8a79d9159eca2d98d9";
    let g2_1_y1 = "2bd368e28381e8eccb5fa81fc26cf3f048eea9abfdd85d7ed3ab3698d63e4f90";
    let g2_1_y0 = "2fe02e47887507adf0ff1743cbac6ba291e66f59be6bd763950bb16041a0a85e";
    let g2_1_bytes = hex::decode(format!("{}{}{}{}", g2_1_x1, g2_1_x0, g2_1_y1, g2_1_y0)).unwrap();
    let g2_1 = host.test_bin_obj(&g2_1_bytes)?;

    // Second pairing: e(G1_2, G2_2)
    let g1_2_x = "0000000000000000000000000000000000000000000000000000000000000001";
    let g1_2_y = "30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45";
    let g1_2_bytes = hex::decode(format!("{}{}", g1_2_x, g1_2_y)).unwrap();
    let g1_2 = host.test_bin_obj(&g1_2_bytes)?;

    let g2_2_x1 = "1971ff0471b09fa93caaf13cbf443c1aede09cc4328f5a62aad45f40ec133eb4";
    let g2_2_x0 = "091058a3141822985733cbdddfed0fd8d6c104e9e9eff40bf5abfef9ab163bc7";
    let g2_2_y1 = "2a23af9a5ce2ba2796c1f4e453a370eb0af8c212d9dc9acd8fc02c2e907baea2";
    let g2_2_y0 = "23a8eb0b0996252cb548a4487da97b02422ebc0e834613f954de6c7e0afdc1fc";
    let g2_2_bytes = hex::decode(format!("{}{}{}{}", g2_2_x1, g2_2_x0, g2_2_y1, g2_2_y0)).unwrap();
    let g2_2 = host.test_bin_obj(&g2_2_bytes)?;

    // Test multi pairing check: e(G1_1, G2_1) * e(G1_2, G2_2) == 1
    host.budget_ref().reset_default()?;
    let g1_vec = host.vec_new_from_slice(&[g1_1.to_val(), g1_2.to_val()])?;
    let g2_vec = host.vec_new_from_slice(&[g2_1.to_val(), g2_2.to_val()])?;
    let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;

    assert!(res.as_val().is_true());

    Ok(())
}
