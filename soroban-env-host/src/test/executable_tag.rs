use std::cmp::Ordering;

use crate::{
    budget::AsBudget,
    host::metered_clone::MeteredClone,
    xdr::{
        AccountId, ContractCostType, ContractExecutable, ContractExecutableExternalRef, ContractId,
        ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgs,
        CreateContractArgsV2, Hash, PublicKey, ScAddress, ScErrorCode, ScErrorType, ScString,
        ScVal, Uint256,
    },
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

// region: ExternalRef metering

const DECLARED_EXECUTABLE_SIZE: u64 = 72;

fn external_ref(owner: u8, tag: &[u8]) -> ContractExecutable {
    ContractExecutable::ExternalRef(ContractExecutableExternalRef {
        executable_owner: ScAddress::Contract(ContractId(Hash([owner; 32]))),
        tag: ScString(tag.to_vec().try_into().unwrap()),
    })
}

fn tag_of(len: usize, last: u8) -> Vec<u8> {
    let mut tag = vec![b'A'; len];
    if let Some(last_byte) = tag.last_mut() {
        *last_byte = last;
    }
    tag
}

#[test]
fn test_external_ref_compare_charges_tag_bytes() {
    let len = 64;
    let a = external_ref(7, &tag_of(len, b'x'));
    let b = external_ref(7, &tag_of(len, b'y'));
    let host = Host::test_host();

    assert_eq!(host.as_budget().compare(&a, &b).unwrap(), Ordering::Less);
    assert_eq!(
        host.as_budget()
            .get_tracker(ContractCostType::MemCmp)
            .unwrap()
            .inputs,
        Some(DECLARED_EXECUTABLE_SIZE + len as u64)
    );
}

#[test]
fn test_external_ref_clone_charges_tag_bytes() {
    let short_len = 8;
    let long_len = 64;
    let short = external_ref(7, &tag_of(short_len, b'x'));
    let long = external_ref(7, &tag_of(long_len, b'x'));
    let host = Host::test_host();

    short.metered_clone(host.as_budget()).unwrap();
    let short_mem_alloc = host
        .as_budget()
        .get_tracker(ContractCostType::MemAlloc)
        .unwrap()
        .inputs
        .unwrap();

    host.as_budget().reset_tracker().unwrap();
    long.metered_clone(host.as_budget()).unwrap();
    let long_mem_alloc = host
        .as_budget()
        .get_tracker(ContractCostType::MemAlloc)
        .unwrap()
        .inputs
        .unwrap();

    assert_eq!(
        long_mem_alloc,
        short_mem_alloc + (long_len - short_len) as u64
    );
}

fn create_args(executable: ContractExecutable) -> CreateContractArgs {
    CreateContractArgs {
        contract_id_preimage: ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: ScAddress::Contract(ContractId(Hash([1; 32]))),
            salt: Uint256([2; 32]),
        }),
        executable,
    }
}

fn create_args_v2(executable: ContractExecutable) -> CreateContractArgsV2 {
    let args = create_args(executable);
    CreateContractArgsV2 {
        contract_id_preimage: args.contract_id_preimage,
        executable: args.executable,
        constructor_args: Default::default(),
    }
}

// The metered `Compare` impls for executables and create-contract args are
// written by hand, so check they order every pair exactly like the derived
// `Ord`: auth matching depends on that order.
#[test]
fn test_executable_compare_matches_derived_ord() {
    let executables = vec![
        ContractExecutable::Wasm(Hash([0; 32])),
        ContractExecutable::Wasm(Hash([9; 32])),
        ContractExecutable::StellarAsset,
        external_ref(7, b""),
        external_ref(7, b"a"),
        external_ref(7, b"ab"),
        external_ref(7, b"abc"),
        external_ref(7, b"abd"),
        external_ref(7, b"b"),
        external_ref(8, b""),
        external_ref(8, b"abc"),
        ContractExecutable::ExternalRef(ContractExecutableExternalRef {
            executable_owner: ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(
                Uint256([7; 32]),
            ))),
            tag: ScString(b"abc".to_vec().try_into().unwrap()),
        }),
    ];
    let host = Host::test_host();
    let budget = host.as_budget();
    for a in &executables {
        for b in &executables {
            assert_eq!(budget.compare(a, b).unwrap(), a.cmp(b), "{a:?} vs {b:?}");
            let (va, vb) = (create_args(a.clone()), create_args(b.clone()));
            assert_eq!(budget.compare(&va, &vb).unwrap(), va.cmp(&vb));
            let (va, vb) = (create_args_v2(a.clone()), create_args_v2(b.clone()));
            assert_eq!(budget.compare(&va, &vb).unwrap(), va.cmp(&vb));
        }
    }
}

// endregion: ExternalRef metering
