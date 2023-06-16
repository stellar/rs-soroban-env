use crate::{
    xdr::{Hash, ScAddress, ScVal, ScVec},
    BytesObject, ContractFunctionSet, Env, EnvBase, Host, HostError, Symbol, SymbolSmall, U32Val,
    U64Object, Val, VecObject,
};

/// prng tests

// Copy of SymbolSmall::from_str, but not protected by feature="testutils".
// We know our uses here are test-only.
pub const fn ss_from_str(s: &str) -> SymbolSmall {
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
        let Ok(func) = SymbolSmall::try_from(func.to_raw()) else {
            return None
        };
        let val = if func == BYTES_NEW {
            host.prng_bytes_new(U32Val::from(SEED_LEN))
                .unwrap()
                .to_raw()
        } else if func == U64_RANGE {
            host.obj_from_u64(host.prng_u64_in_inclusive_range(LO, HI).unwrap())
                .unwrap()
                .to_raw()
        } else if func == SHUFFLE {
            host.prng_vec_shuffle(args[0].try_into().unwrap())
                .unwrap()
                .to_raw()
        } else if func == RESEED {
            // The reseed method reseeds and then returns the result of bytes_new
            let _ = host.prng_reseed(args[0].try_into().unwrap()).unwrap();
            host.prng_bytes_new(U32Val::from(SEED_LEN))
                .unwrap()
                .to_raw()
        } else {
            return None;
        };
        Some(val)
    }
}

#[test]
fn prng_test() -> Result<(), HostError> {
    let host = Host::default();

    host.enable_debug();
    host.set_base_prng_seed([0; 32]);

    let dummy_id = [0; 32];
    let dummy_address = ScAddress::Contract(Hash(dummy_id.clone()));
    let id = host.add_host_object(dummy_address)?;

    host.register_test_contract(id, std::rc::Rc::new(PRNGUsingTest))?;
    let args = host.test_vec_obj::<i32>(&[1, 2])?;

    let bytes0: BytesObject = host.call(id, BYTES_NEW.into(), args)?.try_into()?;
    let bytes1: BytesObject = host.call(id, BYTES_NEW.into(), args)?.try_into()?;
    assert_ne!(0, host.obj_cmp(bytes0.to_raw(), bytes1.to_raw())?);

    // prng_bytes_new
    let mut buf0 = [0u8; SEED_LEN as usize];
    let mut buf1 = [0u8; SEED_LEN as usize];
    host.bytes_copy_to_slice(bytes0, U32Val::from(0), &mut buf0)?;
    host.bytes_copy_to_slice(bytes1, U32Val::from(0), &mut buf1)?;
    eprintln!("buf0: {:?}", buf0);
    eprintln!("buf1: {:?}", buf1);
    assert_ne!(buf0, buf1);

    // prng_u64_in_inclusive_range
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
    let args = host.vec_new_from_slice(&[bytes0.to_raw()])?;
    let res0: BytesObject = host.call(id, RESEED.into(), args)?.try_into()?;
    let res1: BytesObject = host.call(id, RESEED.into(), args)?.try_into()?;
    assert_eq!(0, host.obj_cmp(res0.to_raw(), res1.to_raw())?);

    Ok(())
}
