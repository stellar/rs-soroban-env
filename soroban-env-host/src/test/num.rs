use soroban_env_common::{
    xdr::{ScVal, Uint256},
    TryFromVal, TryIntoVal, I256,
};

use crate::{Host, HostError, RawVal};
use core::fmt::Debug;

#[test]
fn test_i64_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i64 = -1;
    let rv: RawVal = i.try_into_val(&host)?;
    let iv: i64 = rv.try_into_val(&host)?;
    assert_eq!(i, iv);
    Ok(())
}

#[test]
fn test_i128_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let i: i128 = -1;
    let rv: RawVal = i.try_into_val(&host)?;
    let iv: i128 = rv.try_into_val(&host)?;
    assert_eq!(i, iv);
    Ok(())
}

#[test]
fn test_256_roundtrip() -> Result<(), HostError> {
    let host = Host::default();

    let i: I256 = I256::new(-1234_i128);
    let rv = host.add_host_object(i)?.to_raw();
    let scv_back = host.from_host_val(rv)?;

    let scv_ref = ScVal::I256(Uint256(i.to_be_bytes()));
    assert_eq!(scv_back, scv_ref);

    Ok(())
}

#[test]
fn test_num_scval_roundtrips() {
    #[allow(clippy::wildcard_imports)]
    use soroban_env_common::num::*;

    fn check_roundtrip_ok<
        T: Eq + Debug + Copy + TryIntoVal<Host, RawVal> + TryFromVal<Host, RawVal>,
    >(
        h: &Host,
        input: T,
        expect_object: bool,
    ) {
        let raw1: RawVal = input.try_into_val(h).unwrap();
        let sc1: ScVal = raw1.try_into_val(h).unwrap();
        let raw2: RawVal = sc1.try_into_val(h).unwrap();
        let output: T = raw2.try_into_val(h).unwrap();
        assert_eq!(input, output);
        if expect_object {
            assert!(raw1.is_object());
            assert!(raw2.is_object());
        } else {
            assert!(!raw1.is_object());
            assert!(!raw2.is_object());
            assert_eq!(raw1.get_payload(), raw2.get_payload());
        }
    }

    let host = Host::default();

    check_roundtrip_ok::<i64>(&host, 0_i64, false);
    check_roundtrip_ok::<i64>(&host, 1_i64, false);
    check_roundtrip_ok::<i64>(&host, -1_i64, false);
    check_roundtrip_ok::<i64>(&host, 12345_i64, false);
    check_roundtrip_ok::<i64>(&host, -12345_i64, false);
    check_roundtrip_ok::<i64>(&host, MIN_SMALL_I64, false);
    check_roundtrip_ok::<i64>(&host, MAX_SMALL_I64, false);
    check_roundtrip_ok::<i64>(&host, MIN_SMALL_I64 - 1, true);
    check_roundtrip_ok::<i64>(&host, MAX_SMALL_I64 + 1, true);
    check_roundtrip_ok::<i64>(&host, i64::MIN, true);
    check_roundtrip_ok::<i64>(&host, i64::MAX, true);

    check_roundtrip_ok::<u64>(&host, 0_u64, false);
    check_roundtrip_ok::<u64>(&host, 1_u64, false);
    check_roundtrip_ok::<u64>(&host, 12345_u64, false);
    check_roundtrip_ok::<u64>(&host, MAX_SMALL_U64, false);
    check_roundtrip_ok::<u64>(&host, MAX_SMALL_U64 + 1, true);
    check_roundtrip_ok::<u64>(&host, u64::MAX, true);

    check_roundtrip_ok::<i128>(&host, 0_i128, false);
    check_roundtrip_ok::<i128>(&host, 1_i128, false);
    check_roundtrip_ok::<i128>(&host, -1_i128, false);
    check_roundtrip_ok::<i128>(&host, 12345_i128, false);
    check_roundtrip_ok::<i128>(&host, -12345_i128, false);
    check_roundtrip_ok::<i128>(&host, MIN_SMALL_I128, false);
    check_roundtrip_ok::<i128>(&host, MAX_SMALL_I128, false);
    check_roundtrip_ok::<i128>(&host, MIN_SMALL_I128 - 1, true);
    check_roundtrip_ok::<i128>(&host, MAX_SMALL_I128 + 1, true);
    check_roundtrip_ok::<i128>(&host, i128::MIN, true);
    check_roundtrip_ok::<i128>(&host, i128::MAX, true);

    check_roundtrip_ok::<u128>(&host, 0_u128, false);
    check_roundtrip_ok::<u128>(&host, 1_u128, false);
    check_roundtrip_ok::<u128>(&host, 12345_u128, false);
    check_roundtrip_ok::<u128>(&host, MAX_SMALL_U128, false);
    check_roundtrip_ok::<u128>(&host, MAX_SMALL_U128 + 1, true);
    check_roundtrip_ok::<u128>(&host, u128::MAX, true);

    // TODO: add roundtrips for {iu}256 when conversions exist.
}
