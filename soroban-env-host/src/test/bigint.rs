use crate::{xdr::ScUnknownErrorCode, CheckedEnv, Host, HostError};

#[test]
fn bigint_tests() -> Result<(), HostError> {
    let host = Host::default();
    let a: u64 = 2374340;
    let b: i64 = -438730;
    let obj_0 = host.bigint_from_i64(0)?;
    let obj_a = host.bigint_from_u64(a)?;
    let obj_b = host.bigint_from_i64(b)?;
    // add
    {
        let obj_res = host.bigint_add(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 + b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // sub
    {
        let obj_res = host.bigint_sub(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 - b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // mul
    {
        let obj_res = host.bigint_mul(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 * b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // div
    {
        let obj_res = host.bigint_div(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 / b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        // div by 0
        let res = host.bigint_div(obj_a, obj_0);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // rem
    {
        let obj_res = host.bigint_rem(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 % b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        // div by 0
        let res = host.bigint_rem(obj_a, obj_0);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // and
    {
        let obj_res = host.bigint_and(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 & b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // or
    {
        let obj_res = host.bigint_or(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 | b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // xor
    {
        let obj_res = host.bigint_xor(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(a as i64 ^ b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
    }
    // shl
    {
        let obj_res = host.bigint_shl(obj_a, host.bigint_from_i64(5)?)?;
        let obj_ref = host.bigint_from_u64(a << 5)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let res = host.bigint_shl(obj_a, host.bigint_from_i64(-5)?);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
        // a 65-bit integer
        let obj_c = host.bigint_shl(host.bigint_from_u64(u64::MAX)?, host.bigint_from_i64(1)?)?;
        let res = host.bigint_shl(obj_a, obj_c);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // shr
    {
        let obj_res = host.bigint_shr(obj_a, host.bigint_from_i64(5)?)?;
        let obj_ref = host.bigint_from_u64(a >> 5)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let res = host.bigint_shr(obj_a, host.bigint_from_i64(-5)?);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
        // a 65-bit integer
        let obj_c = host.bigint_shl(host.bigint_from_u64(u64::MAX)?, host.bigint_from_i64(1)?)?;
        let res = host.bigint_shr(obj_a, obj_c);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // cmp
    {
        use std::cmp::Ordering;
        let ord_greater = host.obj_cmp(obj_a, obj_b)?;
        let ord_less = host.obj_cmp(obj_b, obj_a)?;
        let obj3 = host.bigint_from_u64(a)?;
        let ord_equal = host.obj_cmp(obj_a, obj3)?;
        assert_eq!(ord_greater, Ordering::Greater as i64);
        assert_eq!(ord_less, Ordering::Less as i64);
        assert_eq!(ord_equal, Ordering::Equal as i64);
    }
    // is zero
    {
        assert!(host.bigint_is_zero(obj_a)?.is_false());
        assert!(host.bigint_is_zero(obj_b)?.is_false());
        assert!(host.bigint_is_zero(obj_0)?.is_true());
    }
    // neg
    {
        let obj_res = host.bigint_neg(obj_b)?;
        let obj_ref = host.bigint_from_i64(-b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        assert_eq!(
            host.obj_cmp(host.bigint_neg(obj_res)?.into(), obj_b.into())?,
            0
        );
    }
    // not
    {
        let obj_res = host.bigint_not(obj_b)?;
        let obj_ref = host.bigint_from_i64(!b)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        assert_eq!(
            host.obj_cmp(host.bigint_not(obj_res)?.into(), obj_b.into())?,
            0
        );
    }
    // gcd
    {
        let obj_res = host.bigint_gcd(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(10)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        // gcd by 0 is self
        let obj_res = host.bigint_gcd(obj_a, obj_0)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_a.into())?, 0);
        // gcd of (0, 0) is 0
        let obj_res = host.bigint_gcd(obj_0, obj_0)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_0.into())?, 0);
    }
    // lcm
    {
        let obj_res = host.bigint_lcm(obj_a, obj_b)?;
        let obj_ref = host.bigint_from_i64(104169418820)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        // lcm by 0 is 0
        let obj_res = host.bigint_lcm(obj_a, obj_0)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_0.into())?, 0);
        // lcm of (0, 0) is 0
        let obj_res = host.bigint_lcm(obj_0, obj_0)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_0.into())?, 0);
    }
    // pow
    {
        let obj_res = host.bigint_pow(obj_b, host.bigint_from_u64(2_u32.into())?)?;
        let obj_ref = host.bigint_from_i64(192484012900)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let obj_res = host.bigint_pow(obj_b, host.bigint_from_u64(0_u32.into())?)?;
        let obj_ref = host.bigint_from_i64(1)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let res = host.bigint_pow(obj_b, host.bigint_from_i64(-1)?);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));

        // a 65-bit integer
        let obj_c = host.bigint_shl(host.bigint_from_u64(u64::MAX)?, host.bigint_from_i64(1)?)?;
        let res = host.bigint_pow(obj_b, obj_c);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // pow_mod
    {
        let obj_2 = host.bigint_from_i64(2)?;
        let obj_res = host.bigint_pow_mod(obj_a, obj_2, obj_b)?;
        let obj_ref = host.bigint_from_i64(-94310)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let res = host.bigint_pow_mod(obj_a, obj_b, obj_2);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
        let res = host.bigint_pow_mod(obj_a, obj_2, obj_0);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // sqrt
    {
        let obj_res = host.bigint_sqrt(obj_a)?;
        let obj_ref = host.bigint_from_i64(1540)?;
        assert_eq!(host.obj_cmp(obj_res.into(), obj_ref.into())?, 0);
        let res = host.bigint_sqrt(obj_b);
        let code = ScUnknownErrorCode::General;
        assert!(HostError::result_matches_err_status(res, code));
    }
    // bits
    {
        assert_eq!(host.bigint_bits(obj_a)?, 22);
        assert_eq!(host.bigint_bits(obj_b)?, 19);
        assert_eq!(host.bigint_bits(obj_0)?, 0);
    }
    // convert to digits
    {
        let bytes = host.bigint_to_radix_be(obj_a, 10_u32.into())?;
        let digits = host.test_bin_obj(&[2, 3, 7, 4, 3, 4, 0])?;
        assert_eq!(host.obj_cmp(bytes.into(), digits.into())?, 0);

        let bin2 = host.bigint_to_radix_be(obj_b, 10_u32.into())?;
        let digits2 = host.test_bin_obj(&[4, 3, 8, 7, 3, 0])?;
        assert_eq!(host.obj_cmp(bin2.into(), digits2.into())?, 0);
    }
    // convert from digits
    {
        let digits = host.test_bin_obj(&[2, 3, 7, 4, 3, 4, 0])?;
        let bi_a = host.bigint_from_radix_be(1i32.into(), digits.into(), 10u32.into())?;
        assert_eq!(host.obj_cmp(bi_a.into(), obj_a.into())?, 0);

        let digits2 = host.test_bin_obj(&[4, 3, 8, 7, 3, 0])?;
        let sign: i32 = -1;
        let bi_b = host.bigint_from_radix_be(sign.into(), digits2.into(), 10u32.into())?;
        assert_eq!(host.obj_cmp(bi_b.into(), obj_b.into())?, 0);
    }
    // convert to and from bytes roundtrip
    {
        let bytes = host.bigint_to_bytes_be(obj_a)?;
        let a_back = host.bigint_from_bytes_be(1i32.into(), bytes)?;
        assert_eq!(host.obj_cmp(a_back.into(), obj_a.into())?, 0);

        let bytes2 = host.bigint_to_bytes_be(obj_b)?;
        let sign: i32 = -1;
        let b_back = host.bigint_from_bytes_be(sign.into(), bytes2)?;
        assert_eq!(host.obj_cmp(b_back.into(), obj_b.into())?, 0);
    }
    Ok(())
}
