use soroban_env_common::xdr::{Limits, ReadXdr, WriteXdr};

use crate::{
    budget::AsBudget,
    host::metered_clone::MeteredClone,
    xdr::{ScErrorCode, ScErrorType, ScVal, ScVec},
    Env, Host, HostError, DEFAULT_XDR_RW_LIMITS,
};

#[test]
fn deep_scval_to_host_val() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.as_budget().reset_unlimited()?;

    let mut v = ScVec::default();
    // Having a large value here, e.g. 1000, will still cause a stack overflow,
    // but this should be fine as long as this creation path is not in the host.
    // I.e. the guest can create whatever structure they like, as long as the host
    // limits catch it.
    for _ in 0..300 {
        let vv = ScVec::try_from(vec![ScVal::from(v)])?;
        v = vv;
    }

    let res = host.to_host_val(&ScVal::from(v));
    // NB: this error code is not great, it's a consequence of
    // bug https://github.com/stellar/rs-soroban-env/issues/1046
    // where the TryIntoVal impl in common eats HostErrors
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_val_to_scval() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.as_budget().reset_unlimited()?;

    let mut hv = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv = host.vec_push_back(vv, hv.to_val())?;
    }
    let res = host.from_host_obj(hv);
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_obj_clone() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.as_budget().reset_unlimited()?;

    let mut v = ScVec::default();
    for _ in 0..1000 {
        let vv = ScVec::try_from(vec![ScVal::from(v)])?;
        v = vv;
    }

    let res = v.metered_clone(host.as_budget());
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn deep_host_obj_cmp() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.as_budget().reset_unlimited()?;

    let mut hv = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv = host.vec_push_back(vv, hv.to_val())?;
    }

    let mut hv2 = host.test_vec_obj::<u32>(&[])?;
    for _ in 0..1000 {
        let vv = host.test_vec_obj::<u32>(&[])?;
        hv2 = host.vec_push_back(vv, hv2.to_val())?;
    }

    let res = host.obj_cmp(hv.to_val(), hv2.to_val());
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn depth_limited_scval_xdr_serialization() -> Result<(), HostError> {
    let mut v = ScVal::from(ScVec::default());
    for _ in 0..200 {
        let vv = ScVec::try_from(vec![v])?;
        v = ScVal::from(vv);
    }
    // default depth limit will cause serialization to fail
    let res = v
        .to_xdr(DEFAULT_XDR_RW_LIMITS)
        .map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));

    // remove the Limits to make serialization pass
    let bytes = v.to_xdr(Limits::none())?;
    // deserialization from the input with limits will fail
    let res = ScVal::from_xdr(bytes, DEFAULT_XDR_RW_LIMITS).map_err(|e| HostError::from(e));
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn length_limited_scval_xdr_conversion() -> Result<(), HostError> {
    let buf = vec![0; 400000];
    let scv_bytes = ScVal::Bytes(buf.try_into().unwrap());
    let mut scv_vec = ScVal::Vec(Some(ScVec(vec![scv_bytes.clone(); 1].try_into().unwrap())));
    // roughly consumes 40MiB (> 32 MiB the limit)
    for _i in 1..100 {
        let mut v = vec![scv_vec; 1];
        v.push(scv_bytes.clone());
        scv_vec = ScVal::Vec(Some(v.try_into().unwrap()));
    }
    // default length limit will cause serialization to fail
    let res = scv_vec
        .to_xdr(DEFAULT_XDR_RW_LIMITS)
        .map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));

    // remove the Limits to make serialization pass
    let bytes = scv_vec
        .to_xdr(Limits::none())
        .map_err(|e| HostError::from(e))?;
    // deserialization from the input with limits will fail
    let res = ScVal::from_xdr(bytes, DEFAULT_XDR_RW_LIMITS).map_err(|e| HostError::from(e));
    let code = (ScErrorType::Context, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

mod instance_storage_depth {
    use super::*;
    use crate::{
        testutils::generate_account_id,
        xdr::{
            InvokeContractArgs, ScAddress, SorobanAddressCredentials, SorobanAuthorizationEntry,
            SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials,
        },
        AddressObject, BytesObject, EnvBase, Symbol, TryFromVal, Val, DEFAULT_HOST_DEPTH_LIMIT,
    };
    use soroban_test_wasms::INSTANCE_STORAGE_DEPTH_TEST_CONTRACT;

    struct InstanceStorageDepthTest {
        host: Host,
        wasm_hash: BytesObject,
        deployer: AddressObject,
        // Contract instantiated with an empty instance storage.
        contract: AddressObject,
    }

    impl InstanceStorageDepthTest {
        fn new() -> Self {
            let host = Host::test_host_with_recording_footprint();
            host.switch_to_recording_auth(true).unwrap();
            let wasm_hash = host
                .upload_wasm(
                    host.bytes_new_from_slice(INSTANCE_STORAGE_DEPTH_TEST_CONTRACT)
                        .unwrap(),
                )
                .unwrap();
            let deployer = host
                .add_host_object(ScAddress::Account(generate_account_id(&host)))
                .unwrap();
            let contract =
                Self::create_contract_internal(&host, wasm_hash, deployer, 0, 0).unwrap();
            Self {
                host,
                wasm_hash,
                deployer,
                contract,
            }
        }

        fn create_contract_internal(
            host: &Host,
            wasm_hash: BytesObject,
            deployer: AddressObject,
            salt: u8,
            constructor_depth: u32,
        ) -> Result<AddressObject, HostError> {
            host.create_contract_with_constructor(
                deployer,
                wasm_hash,
                host.bytes_new_from_slice(&[salt; 32]).unwrap(),
                host.vec_new_from_slice(&[Val::from_u32(constructor_depth).into()])
                    .unwrap(),
            )
        }

        fn create_contract_with_deep_value_in_constructor(
            &self,
            salt: u8,
            constructor_depth: u32,
        ) -> Result<AddressObject, HostError> {
            Self::create_contract_internal(
                &self.host,
                self.wasm_hash,
                self.deployer,
                salt,
                constructor_depth,
            )
        }

        fn call(
            &self,
            contract: AddressObject,
            func: &str,
            args: &[u32],
        ) -> Result<Val, HostError> {
            let args: Vec<Val> = args.iter().map(|a| Val::from_u32(*a).into()).collect();
            self.host.call(
                contract,
                Symbol::try_from_val(&self.host, &func).unwrap(),
                self.host.vec_new_from_slice(&args).unwrap(),
            )
        }

        // Writes a value of the given depth into instance storage, then requires
        // auth for the current contract, which persists the instance storage before
        // the self-reentrant `__check_auth` call. The value is removed before
        // returning, so that the instance storage persisted at the end of the
        // call doesn't contain it.
        fn put_val_and_self_auth(&self, depth: u32, nonce: i64) -> Result<Val, HostError> {
            let contract_address = self.host.scaddress_from_address(self.contract).unwrap();
            self.host
                .set_authorization_entries(vec![SorobanAuthorizationEntry {
                    credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                        address: contract_address.clone(),
                        nonce,
                        signature: ScVal::Void,
                        signature_expiration_ledger: 1000,
                    }),
                    root_invocation: SorobanAuthorizedInvocation {
                        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                            contract_address,
                            function_name: "put_val_and_self_auth".try_into().unwrap(),
                            args: vec![ScVal::U32(depth)].try_into().unwrap(),
                        }),
                        sub_invocations: Default::default(),
                    },
                }])
                .unwrap();
            self.call(self.contract, "put_val_and_self_auth", &[depth])
        }
    }

    #[test]
    fn max_depth_instance_storage_value() {
        let test = InstanceStorageDepthTest::new();

        assert!(test
            .call(test.contract, "put_val", &[DEFAULT_HOST_DEPTH_LIMIT - 1])
            .is_ok());
        assert!(test.call(test.contract, "put_small", &[]).is_ok());

        assert!(HostError::result_matches_err(
            test.call(test.contract, "put_val", &[DEFAULT_HOST_DEPTH_LIMIT]),
            (ScErrorType::Context, ScErrorCode::ExceededLimit)
        ));
        assert!(test.call(test.contract, "put_small", &[]).is_ok());
    }

    #[test]
    fn max_depth_instance_storage_key() {
        let test = InstanceStorageDepthTest::new();

        assert!(test
            .call(test.contract, "put_key", &[DEFAULT_HOST_DEPTH_LIMIT - 1])
            .is_ok());
        assert!(test.call(test.contract, "put_small", &[]).is_ok());

        assert!(HostError::result_matches_err(
            test.call(test.contract, "put_key", &[DEFAULT_HOST_DEPTH_LIMIT]),
            (ScErrorType::Context, ScErrorCode::ExceededLimit)
        ));
        assert!(test.call(test.contract, "put_small", &[]).is_ok());
    }

    #[test]
    fn max_depth_instance_storage_in_constructor() {
        let test = InstanceStorageDepthTest::new();

        let Ok(contract) =
            test.create_contract_with_deep_value_in_constructor(1, DEFAULT_HOST_DEPTH_LIMIT - 1)
        else {
            panic!("contract creation should succeed");
        };
        assert!(test.call(contract, "put_small", &[]).is_ok());

        // Constructor failures are converted to a generic `InvalidAction`
        // error.
        assert!(HostError::result_matches_err(
            test.create_contract_with_deep_value_in_constructor(2, DEFAULT_HOST_DEPTH_LIMIT),
            (ScErrorType::Context, ScErrorCode::InvalidAction)
        ));
    }

    #[test]
    fn max_depth_instance_storage_on_self_reentry() {
        let test = InstanceStorageDepthTest::new();

        assert!(test
            .put_val_and_self_auth(DEFAULT_HOST_DEPTH_LIMIT - 1, 1)
            .is_ok());
        assert!(test.call(test.contract, "put_small", &[]).is_ok());

        // The failure happens while invoking `__check_auth`, so it's reported
        // as an auth failure.
        assert!(HostError::result_matches_err(
            test.put_val_and_self_auth(DEFAULT_HOST_DEPTH_LIMIT, 2),
            (ScErrorType::Auth, ScErrorCode::InvalidAction)
        ));
        assert!(test.call(test.contract, "put_small", &[]).is_ok());
    }
}
