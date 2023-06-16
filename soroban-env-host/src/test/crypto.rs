use crate::{xdr::ScVal, Env, Host, HostError};
use hex::FromHex;
use soroban_env_common::{EnvBase, U32Val};

/// crypto tests
#[test]
fn sha256_test() -> Result<(), HostError> {
    let host = Host::default();
    let obj0 = host.test_bin_obj(&[1])?;
    let hash_obj = host.compute_hash_sha256(obj0)?;

    let v = host.from_host_val(hash_obj.to_raw())?;
    let ScVal::Bytes(bytes) = v else {
        panic!("Wrong type")
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
fn keccak256_test() -> Result<(), HostError> {
    // From https://paulmillr.com/noble/

    let host = Host::default();
    let obj0 = host.test_bin_obj(b"test vector for soroban")?;
    let hash_obj = host.compute_hash_keccak256(obj0)?;

    let v = host.from_host_val(hash_obj.to_raw())?;
    let ScVal::Bytes(bytes) = v else {
        panic!("Wrong type")
    };

    let exp: Vec<u8> =
        FromHex::from_hex(b"352fe2eaddf44eb02eb3eab1f8d6ff4ba426df4f1734b1e3f210d621ee8853d9")
            .unwrap();
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

    let res = host.verify_sig_ed25519(obj_pub, obj_msg, obj_sig);

    res.expect("verification failed");

    // Now verify with wrong message
    let message2: &[u8] = b"73";
    let msg_bytes2: Vec<u8> = FromHex::from_hex(message2).unwrap();
    let obj_msg2 = host.test_bin_obj(&msg_bytes2)?;

    let res_failed = host.verify_sig_ed25519(obj_pub, obj_msg2, obj_sig);

    match res_failed {
        Ok(_) => panic!("verification test failed"),
        _ => (),
    };
    Ok(())
}

#[test]
fn recover_ecdsa_secp256k1_key_test() -> Result<(), HostError> {
    let host = Host::default();

    // From ethereum: https://github.com/ethereum/go-ethereum/blob/master/crypto/secp256k1/secp256_test.go

    let msg_digest: Vec<u8> =
        FromHex::from_hex(b"ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008")
            .unwrap();
    let sig: Vec<u8> = FromHex::from_hex(b"90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap();
    let pk: Vec<u8> = FromHex::from_hex(b"04e32df42865e97135acfb65f3bae71bdc86f4d49150ad6a440b6f15878109880a0a2b2667f7e725ceea70c673093bf67663e0312623c8e091b13cf2c0f11ef652").unwrap();
    let msg_digest_obj = host.test_bin_obj(msg_digest.as_slice())?;
    let sig_obj = host.test_bin_obj(sig.as_slice())?;
    let pk_obj = host.test_bin_obj(pk.as_slice())?;
    let pk_obj_2 = host.recover_key_ecdsa_secp256k1(msg_digest_obj, sig_obj, U32Val::from(1))?;
    let mut buf = [0u8; 65];
    assert_eq!(u32::from(host.bytes_len(pk_obj_2)?), 65);
    host.bytes_copy_to_slice(pk_obj_2, U32Val::from(0), &mut buf)?;
    assert_eq!(pk.as_slice(), buf.as_slice());
    assert_eq!(host.obj_cmp(pk_obj.to_raw(), pk_obj_2.to_raw())?, 0);
    Ok(())
}
