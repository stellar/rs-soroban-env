use crate::{
    budget::Budget,
    host::metered_map::MeteredOrdMap,
    host::Frame,
    storage::{AccessType, Footprint, Storage},
    xdr::{
        self, LedgerEntryData, LedgerKey, LedgerKeyContractData, ScContractCode, ScHostFnErrorCode,
        ScObject, ScStatic, ScVal, ScVec,
    },
    CheckedEnv, Host, HostError, Symbol,
};
use hex::FromHex;
use soroban_test_wasms::CREATE_CONTRACT;

use ed25519_dalek::{
    Keypair, PublicKey, SecretKey, Signature, Signer, PUBLIC_KEY_LENGTH, SECRET_KEY_LENGTH,
};
use im_rc::OrdMap;
use sha2::{Digest, Sha256};

pub(crate) fn check_new_code(host: &Host, storage_key: LedgerKey, code: ScVal) {
    host.visit_storage(|s: &mut Storage| {
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
fn create_contract_test_helper(
    secret: &[u8],
    public: &[u8],
    salt: &[u8],
    code: &[u8],
    sig_preimage: &Vec<u8>,
) -> Result<Host, HostError> {
    let sec_bytes: Vec<u8> = FromHex::from_hex(secret).unwrap();
    let pub_bytes: Vec<u8> = FromHex::from_hex(public).unwrap();
    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();

    let secret: SecretKey = SecretKey::from_bytes(&sec_bytes[..SECRET_KEY_LENGTH]).unwrap();
    let public: PublicKey = PublicKey::from_bytes(&pub_bytes[..PUBLIC_KEY_LENGTH]).unwrap();
    let keypair: Keypair = Keypair {
        secret: secret,
        public: public,
    };

    // Create signature
    let signature: Signature = keypair.sign(Sha256::digest(sig_preimage).as_slice());

    // Make contractID so we can include it in the footprint
    let pre_image =
        xdr::HashIdPreimage::ContractIdFromEd25519(xdr::HashIdPreimageEd25519ContractId {
            ed25519: xdr::Uint256(pub_bytes.as_slice().try_into().unwrap()),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        });

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

    // Create contract
    let obj_code = host.test_bin_obj(&code)?;
    let obj_pub = host.test_bin_obj(&pub_bytes)?;
    let obj_salt = host.test_bin_obj(&salt_bytes)?;
    let obj_sig = host.test_bin_obj(&signature.to_bytes())?;

    let contract_id = host.create_contract_from_ed25519(
        obj_code.to_object(),
        obj_salt.to_object(),
        obj_pub.to_object(),
        obj_sig.to_object(),
    )?;

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

    Ok(host)
}

#[test]
fn create_contract_test() -> Result<(), HostError> {
    let secret_key: &[u8] = b"9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60";
    let public_key: &[u8] = b"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a";
    let salt: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    let code: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";

    let separator =
        "create_contract_from_ed25519(contract: Vec<u8>, salt: u256, key: u256, sig: Vec<u8>)";

    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();
    let sig_preimage = [separator.as_bytes(), salt_bytes.as_slice(), code].concat();

    // Incorrect separator
    let bad_sep = ["d".as_bytes(), salt_bytes.as_slice(), code].concat();
    assert!(create_contract_test_helper(secret_key, public_key, salt, code, &bad_sep).is_err());

    // wrong public key
    let bad_pub: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    assert!(create_contract_test_helper(secret_key, bad_pub, salt, code, &sig_preimage).is_err());

    // Create a valid contract, and then try to update and remove it
    let host = create_contract_test_helper(secret_key, public_key, salt, code, &sig_preimage)?;

    let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
    let id_pre_image =
        xdr::HashIdPreimage::ContractIdFromEd25519(xdr::HashIdPreimageEd25519ContractId {
            ed25519: xdr::Uint256(pub_bytes.as_slice().try_into().unwrap()),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        });

    let hash = sha256_hash_id_preimage(id_pre_image);

    //Push the contract id onto the stack to simulate a contract call
    host.with_frame(Frame::TestContract(hash), || {
        let key = ScVal::Static(ScStatic::LedgerKeyContractCode);

        // update
        let put_res = host.put_contract_data(host.to_host_val(&key)?.to_raw(), ().into());
        let code = ScHostFnErrorCode::InputArgsInvalid;
        assert!(HostError::result_matches_err_status(put_res, code));

        // delete
        let del_res = host.del_contract_data(host.to_host_val(&key)?.to_raw());
        let code = ScHostFnErrorCode::InputArgsInvalid;
        assert!(HostError::result_matches_err_status(del_res, code));
        Ok(())
    })
}

/// VM tests
#[test]
fn create_contract_using_parent_id_test() {
    let secret_key: &[u8] = b"9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60";
    let public_key: &[u8] = b"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a";
    let salt: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";

    let separator =
        "create_contract_from_ed25519(contract: Vec<u8>, salt: u256, key: u256, sig: Vec<u8>)";

    let salt_bytes: Vec<u8> = FromHex::from_hex(salt).unwrap();
    let sig_preimage = [separator.as_bytes(), salt_bytes.as_slice(), CREATE_CONTRACT].concat();

    let host =
        create_contract_test_helper(secret_key, public_key, salt, CREATE_CONTRACT, &sig_preimage)
            .unwrap();

    // Get parent contractID
    let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
    let pre_image =
        xdr::HashIdPreimage::ContractIdFromEd25519(xdr::HashIdPreimageEd25519ContractId {
            ed25519: xdr::Uint256(pub_bytes.as_slice().try_into().unwrap()),
            salt: xdr::Uint256(salt_bytes.as_slice().try_into().unwrap()),
        });

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

    host.visit_storage(|s: &mut Storage| {
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

pub(crate) fn sha256_hash_id_preimage(pre_image: xdr::HashIdPreimage) -> xdr::Hash {
    use xdr::WriteXdr;

    let mut buf = Vec::new();
    pre_image
        .write_xdr(&mut buf)
        .expect("preimage write failed");

    xdr::Hash(Sha256::digest(buf).try_into().expect("invalid hash"))
}
