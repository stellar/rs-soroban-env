use crate::{
    xdr::{ScObject, ScVal},
    CheckedEnv, Host, HostError,
};
use hex::FromHex;

/// crypto tests
#[test]
fn sha256_test() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_bin_obj(&[1])?;
    let hash_obj = host.compute_hash_sha256(obj0)?;

    let v = host.from_host_val(hash_obj.to_raw())?;
    let bytes = match v {
        ScVal::Object(Some(scobj)) => match scobj {
            ScObject::Bytes(bytes) => bytes,
            _ => panic!("Wrong type"),
        },
        _ => panic!("Wrong type"),
    };

    /*
    We took the sha256 of [1], which is 4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a
    The exp array contains the decimal representation of each hex value
    */
    let exp: Vec<u8> = vec![
        75, 245, 18, 47, 52, 69, 84, 197, 59, 222, 46, 187, 140, 210, 183, 227, 209, 96, 10, 214,
        49, 195, 133, 165, 215, 204, 226, 60, 119, 133, 69, 154,
    ];
    assert_eq!(bytes.as_vec().clone(), exp);
    Ok(())
}

#[test]
fn ed25519_verify_test() -> Result<(), HostError> {
    let host = Host::default();

    // From https://datatracker.ietf.org/doc/html/rfc8032#section-7.1

    // First verify successfully
    let public_key: &[u8] = b"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";
    let message: &[u8] = b"72";
    let signature: &[u8] = b"92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00";

    let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
    let msg_bytes: Vec<u8> = FromHex::from_hex(message).unwrap();
    let sig_bytes: Vec<u8> = FromHex::from_hex(signature).unwrap();

    let obj_pub = host.test_bin_obj(&pub_bytes)?;
    let obj_msg = host.test_bin_obj(&msg_bytes)?;
    let obj_sig = host.test_bin_obj(&sig_bytes)?;

    let res = host.verify_sig_ed25519(obj_msg, obj_pub, obj_sig);

    res.expect("verification failed");

    // Now verify with wrong message
    let message2: &[u8] = b"73";
    let msg_bytes2: Vec<u8> = FromHex::from_hex(message2).unwrap();
    let obj_msg2 = host.test_bin_obj(&msg_bytes2)?;

    let res_failed = host.verify_sig_ed25519(obj_msg2, obj_pub, obj_sig);

    match res_failed {
        Ok(_) => panic!("verification test failed"),
        _ => (),
    };
    Ok(())
}
