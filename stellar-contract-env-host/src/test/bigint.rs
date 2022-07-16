use crate::{xdr::ScUnknownErrorCode, CheckedEnv, Host, HostError, RawVal};

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
        let ord_greater: i32 = host.bigint_cmp(obj_a, obj_b)?.try_into()?;
        let ord_less: i32 = host.bigint_cmp(obj_b, obj_a)?.try_into()?;
        let obj3 = host.bigint_from_u64(a)?;
        let ord_equal: i32 = host.bigint_cmp(obj_a, obj3)?.try_into()?;
        assert_eq!(ord_greater, Ordering::Greater as i32);
        assert_eq!(ord_less, Ordering::Less as i32);
        assert_eq!(ord_equal, Ordering::Equal as i32);
    }
    // is zero
    {
        let f = RawVal::from_bool(false);
        let t = RawVal::from_bool(true);
        assert_eq!(host.bigint_is_zero(obj_a)?.get_payload(), f.get_payload());
        assert_eq!(host.bigint_is_zero(obj_b)?.get_payload(), f.get_payload());
        assert_eq!(host.bigint_is_zero(obj_0)?.get_payload(), t.get_payload());
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

    Ok(())
}
