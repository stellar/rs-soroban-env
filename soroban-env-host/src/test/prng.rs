use hex_literal::hex;
use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;

use crate::{
    test::observe::ObservedHost,
    xdr::{Hash, ScAddress, ScVal, ScVec},
    AddressObject, BytesObject, ContractFunctionSet, Env, EnvBase, Host, HostError, Symbol,
    SymbolSmall, TryFromVal, TryIntoVal, U32Val, U64Object, U64Val, Val, VecObject,
};

/// prng tests

// Copy of SymbolSmall::from_str, but not protected by feature="testutils".
#[cfg(test)]
const fn ss_from_str(s: &str) -> SymbolSmall {
    match SymbolSmall::try_from_str(s) {
        Ok(sym) => sym,
        _ => panic!("bad symbol"),
    }
}

const BYTES_NEW: SymbolSmall = ss_from_str("bytes_new");
const U64_RANGE: SymbolSmall = ss_from_str("u64_range");
const SHUFFLE: SymbolSmall = ss_from_str("shuffle");
const RESEED: SymbolSmall = ss_from_str("reseed");

const SEED_LEN: u32 = 32;
const LO: u64 = 12345;
const HI: u64 = 78910;

pub struct PRNGUsingTest;

impl ContractFunctionSet for PRNGUsingTest {
    fn call(&self, func: &Symbol, host: &Host, args: &[Val]) -> Option<Val> {
        let Ok(func) = SymbolSmall::try_from(func.to_val()) else {
            return None;
        };
        let val = if func == BYTES_NEW {
            host.prng_bytes_new(U32Val::from(SEED_LEN))
                .unwrap()
                .to_val()
        } else if func == U64_RANGE {
            let ulo: u64 = U64Val::try_from(args[0])
                .unwrap()
                .try_into_val(host)
                .unwrap();
            let uhi: u64 = U64Val::try_from(args[1])
                .unwrap()
                .try_into_val(host)
                .unwrap();
            host.obj_from_u64(host.prng_u64_in_inclusive_range(ulo, uhi).unwrap())
                .unwrap()
                .to_val()
        } else if func == SHUFFLE {
            host.prng_vec_shuffle(args[0].try_into().unwrap())
                .unwrap()
                .to_val()
        } else if func == RESEED {
            // The reseed method reseeds and then returns the result of bytes_new
            let _ = host.prng_reseed(args[0].try_into().unwrap()).unwrap();
            host.prng_bytes_new(U32Val::from(SEED_LEN))
                .unwrap()
                .to_val()
        } else {
            return None;
        };
        Some(val)
    }
}

#[test]
fn prng_test() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    host.enable_debug()?;
    host.set_base_prng_seed([0; 32])?;

    let dummy_id = [0; 32];
    let dummy_address = ScAddress::Contract(Hash(dummy_id));
    let id = host.add_host_object(dummy_address)?;

    host.register_test_contract(id, std::rc::Rc::new(PRNGUsingTest))?;
    let args = host.test_vec_obj::<i32>(&[1, 2])?;

    let bytes0: BytesObject = host.call(id, BYTES_NEW.into(), args)?.try_into()?;
    let bytes1: BytesObject = host.call(id, BYTES_NEW.into(), args)?.try_into()?;
    assert_ne!(0, host.obj_cmp(bytes0.to_val(), bytes1.to_val())?);

    // prng_bytes_new
    let mut buf0 = [0u8; SEED_LEN as usize];
    let mut buf1 = [0u8; SEED_LEN as usize];
    host.bytes_copy_to_slice(bytes0, U32Val::from(0), &mut buf0)?;
    host.bytes_copy_to_slice(bytes1, U32Val::from(0), &mut buf1)?;
    eprintln!("buf0: {:?}", buf0);
    eprintln!("buf1: {:?}", buf1);
    assert_ne!(buf0, buf1);

    // prng_u64_in_inclusive_range
    let args = host.test_vec_obj::<u64>(&[LO, HI])?;
    let u0: U64Object = host.call(id, U64_RANGE.into(), args)?.try_into()?;
    let u0 = host.obj_to_u64(u0)?;
    let u1: U64Object = host.call(id, U64_RANGE.into(), args)?.try_into()?;
    let u1 = host.obj_to_u64(u1)?;
    eprintln!("u0: {:?}", u0);
    eprintln!("u1: {:?}", u1);
    assert_ne!(u0, u1);
    assert!(LO <= u0);
    assert!(u0 <= HI);
    assert!(LO <= u1);
    assert!(u1 <= HI);

    // prng_vec_shuffle
    let scv0: ScVec = host.test_scvec::<i32>(&[1, 2, 3, 4, 5, 6, 7, 8])?;
    let args = host.test_vec_obj::<ScVec>(&[scv0.clone()])?;
    let res: VecObject = host.call(id, SHUFFLE.into(), args)?.try_into()?;
    let ScVal::Vec(Some(scv1)) = ScVal::from(host.from_host_obj(res)?) else {
        panic!("from_host_obj(VecObject) produced non-Some(ScVec(...))");
    };
    eprintln!("scv0: {:?}", &scv0);
    eprintln!("scv1: {:?}", &scv1);
    assert_ne!(&scv0, &scv1);
    for x in scv0.0.iter() {
        assert!(scv1.0.contains(x))
    }
    for y in scv1.0.iter() {
        assert!(scv0.0.contains(y))
    }

    // prng_reseed -- here we're running 2 calls to
    // reseed-and-return-a-random-bytes-object that should, since they're
    // provided with the same seed input, produce the same output -- even though
    // they're in different frames, and _unlike_ previous repeated calls to
    // prngs in different frames.
    let args = host.vec_new_from_slice(&[bytes0.to_val()])?;
    let res0: BytesObject = host.call(id, RESEED.into(), args)?.try_into()?;
    let res1: BytesObject = host.call(id, RESEED.into(), args)?.try_into()?;
    assert_eq!(0, host.obj_cmp(res0.to_val(), res1.to_val())?);

    Ok(())
}

// This test checks that setting the base seed to two different values
// produces _frame_ PRNG behaviour that differs; and that setting it
// to the same value twice produces the same behaviour both times.
#[test]
fn base_prng_seed() -> Result<(), HostError> {
    fn hostand_contract_with_seed(
        seed: [u8; 32],
        testname: &'static str,
    ) -> Result<(ObservedHost, AddressObject), HostError> {
        let host = ObservedHost::new(testname, Host::test_host_with_recording_footprint());
        host.enable_debug()?;
        host.set_base_prng_seed(seed)?;

        let dummy_id = [0; 32];
        let dummy_address = ScAddress::Contract(Hash(dummy_id));
        let id = host.add_host_object(dummy_address)?;
        host.register_test_contract(id, std::rc::Rc::new(PRNGUsingTest))?;
        Ok((host, id))
    }

    let seed0 = [0; 32];
    let seed1 = [0; 32];
    let seed2 = [2; 32];

    let (host0, id0) =
        hostand_contract_with_seed(seed0, "soroban-end-host::test::prng::base_prng_seed_0")?;
    let (host1, id1) =
        hostand_contract_with_seed(seed1, "soroban-end-host::test::prng::base_prng_seed_1")?;
    let (host2, id2) =
        hostand_contract_with_seed(seed2, "soroban-end-host::test::prng::base_prng_seed_2")?;

    let args0 = host0.test_vec_obj::<u64>(&[0, 90])?;
    let args1 = host1.test_vec_obj::<u64>(&[0, 90])?;
    let args2 = host2.test_vec_obj::<u64>(&[0, 90])?;
    let u64_0: U64Val = host0.call(id0, U64_RANGE.into(), args0)?.try_into()?;
    let u64_1: U64Val = host1.call(id1, U64_RANGE.into(), args1)?.try_into()?;
    let u64_2: U64Val = host2.call(id2, U64_RANGE.into(), args2)?.try_into()?;

    let u64_0 = u64::try_from_val(&*host0, &u64_0)?;
    let u64_1 = u64::try_from_val(&*host1, &u64_1)?;
    let u64_2 = u64::try_from_val(&*host2, &u64_2)?;

    eprintln!("u64_0: {}", u64_0);
    eprintln!("u64_1: {}", u64_1);
    eprintln!("u64_2: {}", u64_2);

    eprintln!("u64_0 bits: {:>064b}", u64_0);
    eprintln!("u64_1 bits: {:>064b}", u64_1);
    eprintln!("u64_2 bits: {:>064b}", u64_2);
    assert_eq!(u64_0, u64_1);
    assert_ne!(u64_0, u64_2);

    Ok(())
}

// This is a variant of the above test, but using the bytes_new method.
#[test]
fn base_prng_seed_bytes() -> Result<(), HostError> {
    fn hostand_contract_with_seed(
        seed: [u8; 32],
        testname: &'static str,
    ) -> Result<(ObservedHost, AddressObject), HostError> {
        let host = ObservedHost::new(testname, Host::test_host_with_recording_footprint());
        host.enable_debug()?;
        host.set_base_prng_seed(seed)?;

        let dummy_id = [0; 32];
        let dummy_address = ScAddress::Contract(Hash(dummy_id));
        let id = host.add_host_object(dummy_address)?;
        host.register_test_contract(id, std::rc::Rc::new(PRNGUsingTest))?;
        Ok((host, id))
    }

    let seed0 = [0; 32];
    let seed1 = [0; 32];
    let seed2 = [2; 32];

    let (host0, id0) = hostand_contract_with_seed(
        seed0,
        "soroban-end-host::test::prng::base_prng_seed_bytes_0",
    )?;
    let (host1, id1) = hostand_contract_with_seed(
        seed1,
        "soroban-end-host::test::prng::base_prng_seed_bytes_1",
    )?;
    let (host2, id2) = hostand_contract_with_seed(
        seed2,
        "soroban-end-host::test::prng::base_prng_seed_bytes_2",
    )?;

    let args0 = host0.test_vec_obj::<u64>(&[0])?;
    let args1 = host1.test_vec_obj::<u64>(&[0])?;
    let args2 = host2.test_vec_obj::<u64>(&[0])?;
    let bytes0: BytesObject = host0.call(id0, BYTES_NEW.into(), args0)?.try_into()?;
    let bytes1: BytesObject = host1.call(id1, BYTES_NEW.into(), args1)?.try_into()?;
    let bytes2: BytesObject = host2.call(id2, BYTES_NEW.into(), args2)?.try_into()?;

    let mut buf0 = [0u8; SEED_LEN as usize];
    let mut buf1 = [0u8; SEED_LEN as usize];
    let mut buf2 = [0u8; SEED_LEN as usize];
    host0.bytes_copy_to_slice(bytes0, U32Val::from(0), &mut buf0)?;
    host1.bytes_copy_to_slice(bytes1, U32Val::from(0), &mut buf1)?;
    host2.bytes_copy_to_slice(bytes2, U32Val::from(0), &mut buf2)?;

    eprintln!("buf0: {:?}", buf0);
    eprintln!("buf1: {:?}", buf1);
    eprintln!("buf2: {:?}", buf2);
    let u64_0 = u64::from_le_bytes(buf0[0..8].try_into().unwrap());
    let u64_1 = u64::from_le_bytes(buf1[0..8].try_into().unwrap());
    let u64_2 = u64::from_le_bytes(buf2[0..8].try_into().unwrap());
    eprintln!("first 8 bytes as u64 LE: {}", u64_0);
    eprintln!("first 8 bytes as u64 LE: {}", u64_1);
    eprintln!("first 8 bytes as u64 LE: {}", u64_2);
    eprintln!("first 8 bytes as u64 LE bits: {:>064b}", u64_0);
    eprintln!("first 8 bytes as u64 LE bits: {:>064b}", u64_1);
    eprintln!("first 8 bytes as u64 LE bits: {:>064b}", u64_2);

    assert_eq!(buf0, buf1);
    assert_ne!(buf0, buf2);

    Ok(())
}

#[test]
fn chacha_test_vectors() {
    // This just checks that the ChaCha library we're using conforms to
    // test vectors found in RFC 7539.
    let mut seed = [0; 32];
    let mut chacha_0 = ChaCha20Rng::from_seed(seed.into());
    // Test vectors 1 and 2 use a 0-seeded ChaCha
    let ref_out1 = hex!("76b8e0ada0f13d90405d6ae55386bd28bdd219b8a08ded1aa836efcc8b770dc7da41597c5157488d7724e03fb8d84a376a43b8f41518a11cc387b669b2ee6586");
    let ref_out2 = hex!("9f07e7be5551387a98ba977c732d080dcb0f29a048e3656912c6533e32ee7aed29b721769ce64e43d57133b074d839d531ed1f28510afb45ace10a1f4b794d6f");
    let mut out = [0u8; 64];
    chacha_0.fill_bytes(&mut out);
    assert_eq!(ref_out1, out);
    chacha_0.fill_bytes(&mut out);
    assert_eq!(ref_out2, out);

    // Test vector 3 uses a 1-seeded ChaCha
    seed[31] = 1;
    let mut chacha_1 = ChaCha20Rng::from_seed(seed.into());
    let ref_out3 = hex!("3aeb5224ecf849929b9d828db1ced4dd832025e8018b8160b82284f3c949aa5a8eca00bbb4a73bdad192b5c42f73f2fd4e273644c8b36125a64addeb006c13a0");
    chacha_1.fill_bytes(&mut out); // advance from block 0 to block 1
    chacha_1.fill_bytes(&mut out);
    assert_eq!(ref_out3, out);

    // Test vector 4 uses a high-ff-seeded ChaCha
    seed[31] = 0;
    seed[1] = 0xff;
    let mut chacha_ff = ChaCha20Rng::from_seed(seed.into());
    let ref_out4 = hex!("72d54dfbf12ec44b362692df94137f328fea8da73990265ec1bbbea1ae9af0ca13b25aa26cb4a648cb9b9d1be65b2c0924a66c54d545ec1b7374f4872e99f096");
    chacha_ff.fill_bytes(&mut out); // advance from block 0 to block 2
    chacha_ff.fill_bytes(&mut out);
    chacha_ff.fill_bytes(&mut out);
    assert_eq!(ref_out4, out);

    // Test vector 5 uses a different "nonce" (a.k.a. "stream number")
    seed[1] = 0;
    let mut chacha_stream_2 = ChaCha20Rng::from_seed(seed.into());
    chacha_stream_2.set_stream(0x0200_0000_0000_0000u64);
    let ref_out5 = hex!("c2c64d378cd536374ae204b9ef933fcd1a8b2288b3dfa49672ab765b54ee27c78a970e0e955c14f3a88e741b97c286f75f8fc299e8148362fa198a39531bed6d");
    chacha_stream_2.fill_bytes(&mut out);
    assert_eq!(ref_out5, out);
}
