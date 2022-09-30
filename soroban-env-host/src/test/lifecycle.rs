use crate::{
    budget::Budget,
    host::metered_map::MeteredOrdMap,
    host::Frame,
    storage::{AccessType, Footprint, Storage},
    xdr::{
        self, AccountId, HostFunction, LedgerEntryData, LedgerKey, LedgerKeyContractData,
        ScContractCode, ScObject, ScStatic, ScVal, ScVec,
    },
    CheckedEnv, Host, HostError, RawVal, Symbol,
};
use hex::FromHex;
use soroban_test_wasms::CREATE_CONTRACT;

use im_rc::OrdMap;
use sha2::{Digest, Sha256};

pub(crate) fn check_new_code(host: &Host, storage_key: LedgerKey, code: ScVal) {
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key)?);

        match s.get(&storage_key)?.data {
            LedgerEntryData::ContractData(cde) => assert_eq!(cde.val, code),
            _ => panic!("expected contract data"),
        };
        Ok(())
    })
    .unwrap();
}

/// create contract tests
fn create_contract_test_helper(salt: &[u8], code: &[u8]) -> Result<Host, HostError> {
    let source_account = AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256([0; 32])));
    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();

    // Make contractID so we can include it in the footprint
    let pre_image = xdr::HashIdPreimage::ContractIdFromSourceAccount(
        xdr::HashIdPreimageSourceAccountContractId {
            source_account: source_account.clone(),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        },
    );

    let hash = sha256_hash_id_preimage(pre_image);

    let hash_copy = hash.clone();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCode);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: hash,
        key,
    });

    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadWrite)?;

    // Initialize storage and host
    let budget = Budget::default();
    let storage = Storage::with_enforcing_footprint_and_map(
        footprint,
        MeteredOrdMap {
            map: OrdMap::new(),
            budget: budget.clone(),
        },
    );
    let host = Host::with_storage_and_budget(storage, budget);

    host.set_source_account(source_account);

    // Create contract
    let obj_code = host.test_bin_obj(&code)?;
    let obj_salt = host.test_bin_obj(&salt_bytes)?;
    let contract_id = host
        .with_frame(
            Frame::HostFunction(HostFunction::CreateContractWithSourceAccount),
            || {
                host.create_contract_from_source_account(obj_code.to_object(), obj_salt.to_object())
                    .map(|o| o.to_raw())
            },
        )
        .unwrap();

    let v = host.from_host_val(contract_id)?;
    let bytes = match v {
        ScVal::Object(Some(scobj)) => match scobj {
            ScObject::Bytes(bytes) => bytes,
            _ => panic!("Wrong type"),
        },
        _ => panic!("Wrong type"),
    };

    if bytes.as_slice() != hash_copy.0.as_slice() {
        panic!("return value doesn't match")
    }

    check_new_code(
        &host,
        storage_key,
        ScVal::Object(Some(ScObject::ContractCode(ScContractCode::Wasm(
            code.try_into().unwrap(),
        )))),
    );

    Ok(host)
}

/// VM tests
#[test]
fn create_contract_using_parent_id_test() {
    let salt: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();

    let host = create_contract_test_helper(salt, CREATE_CONTRACT).unwrap();

    // Get parent contractID
    let pre_image = xdr::HashIdPreimage::ContractIdFromSourceAccount(
        xdr::HashIdPreimageSourceAccountContractId {
            source_account: host.source_account().unwrap(),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        },
    );

    let parent_id = sha256_hash_id_preimage(pre_image);

    //Put child contract that will be created into the footprint
    //Use the same salt
    let child_pre_image =
        xdr::HashIdPreimage::ContractIdFromContract(xdr::HashIdPreimageContractId {
            contract_id: parent_id.clone(),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        });

    let child_id = sha256_hash_id_preimage(child_pre_image);

    let child_storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: child_id,
        key: ScVal::Static(ScStatic::LedgerKeyContractCode),
    });

    host.with_mut_storage(|s: &mut Storage| {
        s.footprint
            .record_access(&child_storage_key, AccessType::ReadWrite)
            .unwrap();
        Ok(())
    })
    .unwrap();

    // prepare arguments
    let child_code: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";
    let code_val = ScVal::Object(Some(ScObject::Bytes(child_code.try_into().unwrap())));

    let sym = Symbol::from_str("create");
    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();
    let scvec0: ScVec = vec![
        code_val.clone(),
        ScVal::Object(Some(ScObject::Bytes(salt_bytes.try_into().unwrap()))),
    ]
    .try_into()
    .unwrap();
    let args = host.to_host_obj(&ScObject::Vec(scvec0)).unwrap();

    let p_id_sobj = ScObject::Bytes(parent_id.0.try_into().unwrap());
    let p_id_obj = host.to_host_obj(&p_id_sobj).unwrap();
    host.call(p_id_obj.to_object(), sym.into(), args.into())
        .unwrap();

    //Validate child contract exists and code is what we expected
    check_new_code(
        &host,
        child_storage_key,
        ScVal::Object(Some(ScObject::ContractCode(ScContractCode::Wasm(
            child_code.try_into().unwrap(),
        )))),
    );
}

#[test]
fn create_contract_from_source_account() -> Result<(), HostError> {
    let code: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";
    let public_key: &[u8] = b"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a";
    let salt: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();
    let source_account = AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256(
        pub_bytes.as_slice().try_into().unwrap(),
    )));

    let pre_image = xdr::HashIdPreimage::ContractIdFromSourceAccount(
        xdr::HashIdPreimageSourceAccountContractId {
            source_account: source_account.clone(),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        },
    );

    let hash = sha256_hash_id_preimage(pre_image);

    let hash_copy = hash.clone();
    let key = ScVal::Static(ScStatic::LedgerKeyContractCode);
    let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
        contract_id: hash,
        key,
    });

    let mut footprint = Footprint::default();
    footprint.record_access(&storage_key, AccessType::ReadWrite)?;

    // Initialize storage and host
    let budget = Budget::default();
    let storage = Storage::with_enforcing_footprint_and_map(
        footprint,
        MeteredOrdMap {
            map: OrdMap::new(),
            budget: budget.clone(),
        },
    );
    let host = Host::with_storage_and_budget(storage, budget);
    host.set_source_account(source_account);

    let obj_code = host.test_bin_obj(&code)?;
    let obj_salt = host.test_bin_obj(&salt_bytes)?;

    host.with_frame(
        Frame::HostFunction(HostFunction::CreateContractWithSourceAccount),
        || {
            let contract_id = host
                .create_contract_from_source_account(obj_code.to_object(), obj_salt.to_object())?;

            let v = host.from_host_val(contract_id.to_raw())?;
            let bytes = match v {
                ScVal::Object(Some(scobj)) => match scobj {
                    ScObject::Bytes(bytes) => bytes,
                    _ => panic!("Wrong type"),
                },
                _ => panic!("Wrong type"),
            };

            if bytes.as_slice() != hash_copy.0.as_slice() {
                panic!("return value doesn't match")
            }

            check_new_code(
                &host,
                storage_key,
                ScVal::Object(Some(ScObject::ContractCode(ScContractCode::Wasm(
                    code.try_into().unwrap(),
                )))),
            );
            Ok(RawVal::from_void())
        },
    )?;
    Ok(())
}

pub(crate) fn sha256_hash_id_preimage(pre_image: xdr::HashIdPreimage) -> xdr::Hash {
    use xdr::WriteXdr;

    let mut buf = Vec::new();
    pre_image
        .write_xdr(&mut buf)
        .expect("preimage write failed");

    xdr::Hash(Sha256::digest(buf).try_into().expect("invalid hash"))
}
