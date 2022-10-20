use crate::{host::Frame, xdr::Hash, Host, HostError};
use soroban_env_common::{CheckedEnv, EnvBase, RawVal, Symbol};

#[test]
fn test_prng() -> Result<(), HostError> {
    let host = Host::default();
    let buf0 = [0; 256];
    let mut buf_a1 = [0; 256];
    let mut buf_a2 = [0; 256];
    let mut buf_b1 = [0; 256];
    let frame_a = Frame::TestContract(Hash([0; 32]), Symbol::from_str("foo"));
    let frame_b = Frame::TestContract(Hash([1; 32]), Symbol::from_str("foo"));
    let void = || Ok(RawVal::from_void());

    host.with_frame(frame_a.clone(), || {
        host.prng_fill_slice(&mut buf_a1, false)?;
        void()
    })?;
    host.with_frame(frame_a.clone(), || {
        host.prng_fill_slice(&mut buf_a2, true)?;
        void()
    })?;
    host.with_frame(frame_b.clone(), || {
        host.prng_fill_slice(&mut buf_b1, false)?;
        void()
    })?;

    // all 3 buffers should have become nonzero
    assert_ne!(buf0, buf_a1);
    assert_ne!(buf0, buf_a2);
    assert_ne!(buf0, buf_b1);

    // a1 and a2 should differ since they're sequential calls from the same context
    assert_ne!(buf_a1, buf_a2);

    // a1 and b1 should differ since they're separate contexts
    assert_ne!(buf_a1, buf_b1);

    // a third call to frame_a should fail since we finalized it in the previous call
    let e = host.with_frame(frame_a, || {
        host.prng_fill_slice(&mut buf_b1, true)?;
        void()
    });
    assert!(e.is_err());

    // check that two calls to next_u32 differ
    let x32: u32 = host
        .with_frame(frame_b.clone(), || {
            host.prng_next_u32(RawVal::from_bool(false))
        })?
        .try_into()
        .expect("u32");
    let y32: u32 = host
        .with_frame(frame_b.clone(), || {
            host.prng_next_u32(RawVal::from_bool(false))
        })?
        .try_into()
        .expect("u32");
    assert_ne!(0, y32);
    assert_ne!(x32, 0);
    assert_ne!(x32, y32);

    // check that two calls to next_u63 differ
    let xraw = host.with_frame(frame_b.clone(), || {
        host.prng_next_u63(RawVal::from_bool(false))
    })?;
    let yraw = host.with_frame(frame_b, || host.prng_next_u63(RawVal::from_bool(false)))?;
    assert!(xraw.is_u63());
    assert!(yraw.is_u63());
    let x63 = unsafe { xraw.unchecked_as_u63() };
    let y63 = unsafe { yraw.unchecked_as_u63() };
    assert_ne!(0, y63);
    assert_ne!(x63, 0);
    assert_ne!(x63, y63);

    // TODO: add a test that fills guest linear memory

    Ok(())
}
