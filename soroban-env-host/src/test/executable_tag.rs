use crate::{
    xdr::{ScErrorCode, ScErrorType, ScVal},
    Compare, Env, EnvBase, Host, HostError, StorageType, Symbol, Tag, Val,
};
use soroban_test_wasms::{ADD_I32, CONTRACT_STORAGE, SUM_I32};

#[test]
fn test_executable_tag_conversion_roundtrip() {
    let host = observe_host!(Host::test_host());
    let tag = "executable tag";
    let string_obj = host.string_new_from_slice(tag.as_bytes()).unwrap();

    let tag_obj = host.create_executable_tag(string_obj).unwrap();
    assert_eq!(tag_obj.to_val().get_tag(), Tag::ExecutableTagObject);
    let tag_scval = host.from_host_val(tag_obj.to_val()).unwrap();

    if let ScVal::ExecutableTag(s) = &tag_scval {
        assert_eq!(s.as_slice(), tag.as_bytes());
    } else {
        panic!("expected ScVal::ExecutableTag, got {tag_scval:?}");
    }

    let tag_val = host.to_host_val(&tag_scval).unwrap();
    assert!((*host)
        .compare(&tag_obj.to_val(), &tag_val)
        .unwrap()
        .is_eq());
}

#[test]
fn test_executable_tag_storage_invariants() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let owner = host.register_test_contract_wasm(ADD_I32);
    let owner_id = host.contract_id_from_address(owner).unwrap();
    let wasm_hash_1_obj = host.upload_contract_wasm(SUM_I32.to_vec()).unwrap();
    let wasm_hash_1 = wasm_hash_1_obj.to_val();
    let wasm_hash_2 = host
        .upload_contract_wasm(CONTRACT_STORAGE.to_vec())
        .unwrap()
        .to_val();
    let owner_wasm_hash = host
        .upload_contract_wasm(ADD_I32.to_vec())
        .unwrap()
        .to_val();

    host.with_test_contract_frame(owner_id, Symbol::try_from_small_str("foo").unwrap(), || {
        let tag = host
            .create_executable_tag(host.string_new_from_slice(b"exec tag").unwrap())
            .unwrap()
            .to_val();

        // Persistent put with a valid, existing Wasm hash succeeds.
        assert!(host
            .put_contract_data(tag, wasm_hash_1, StorageType::Persistent)
            .is_ok());
        // Owner's own Wasm hash is not special in any way.
        assert!(host
            .put_contract_data(tag, owner_wasm_hash, StorageType::Persistent)
            .is_ok());
        assert!(host
            .put_contract_data(tag, wasm_hash_2, StorageType::Persistent)
            .is_ok());

        // The entry can be used in all the read-operations (has, get,
        // extend ttl).
        assert!(bool::from(
            host.has_contract_data(tag, StorageType::Persistent)
                .unwrap()
        ));
        let v = host.get_contract_data(tag, StorageType::Persistent)?;
        assert!((*host).compare(&v, &wasm_hash_2).unwrap().is_eq());
        assert!(host
            .extend_contract_data_ttl(tag, StorageType::Persistent, 10000.into(), 10000.into())
            .is_ok());

        // Temporary and Instance storage are rejected.
        assert!(HostError::result_matches_err(
            host.put_contract_data(tag, wasm_hash_1, StorageType::Temporary),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.put_contract_data(tag, wasm_hash_1, StorageType::Instance),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));

        // A non-BytesObject values are rejected.
        let wasm_hash_bytes: [u8; 32] = host
            .fixed_length_bytes_from_bytesobj_input("hash", wasm_hash_1_obj)
            .unwrap();
        let str_hash = host.string_new_from_slice(&wasm_hash_bytes).unwrap();
        assert!(HostError::result_matches_err(
            host.put_contract_data(tag, str_hash.to_val(), StorageType::Persistent),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.put_contract_data(tag, 5_u32.into(), StorageType::Persistent),
            (ScErrorType::Storage, ScErrorCode::InvalidInput)
        ));

        // A 32-byte value that is not pointing to the hash of an existing Wasm
        // is rejected.
        let non_existent = host.bytes_new_from_slice(&[0u8; 32])?;
        assert!(HostError::result_matches_err(
            host.put_contract_data(tag, non_existent.to_val(), StorageType::Persistent),
            (ScErrorType::Storage, ScErrorCode::MissingValue)
        ));

        // The entry can never be deleted.
        assert!(HostError::result_matches_err(
            host.del_contract_data(tag, StorageType::Persistent),
            (ScErrorType::Storage, ScErrorCode::InvalidAction)
        ));

        // Ensure entry indeed still exists.
        assert!(bool::from(
            host.has_contract_data(tag, StorageType::Persistent)
                .unwrap()
        ));

        Ok(Val::VOID.into())
    })
    .unwrap();
}
