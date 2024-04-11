use crate::xdr::{ScErrorCode, ScErrorType};
use crate::{Env, EnvBase, Host, HostError, U32Val};
use hex::ToHex;

fn is_budget_exceeded(err: HostError) -> bool {
    err.error.is_type(ScErrorType::Budget) && err.error.is_code(ScErrorCode::ExceededLimit)
}

fn is_crypto_error(err: HostError) -> bool {
    err.error.is_type(ScErrorType::Crypto)
}

fn is_object_error(err: HostError) -> bool {
    err.error.is_type(ScErrorType::Object)
}

/// crypto tests
#[test]
fn sha256_test() {
    let host = observe_host!(Host::test_host());
    let compute_hash = |input_bytes: &[u8]| -> Result<Vec<u8>, HostError> {
        let bytes_obj = host.bytes_new_from_slice(input_bytes).unwrap();
        let hash_obj = host.compute_hash_sha256(bytes_obj)?;
        Ok(host
            .hash_from_bytesobj_input("hash", hash_obj)
            .unwrap()
            .0
            .to_vec())
    };
    assert_eq!(
        compute_hash(&[]).unwrap().encode_hex::<String>(),
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string()
    );
    assert_eq!(
        compute_hash(&[1]).unwrap().encode_hex::<String>(),
        "4bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459a".to_string()
    );
    assert_eq!(
        compute_hash(b"test vector for soroban")
            .unwrap()
            .encode_hex::<String>(),
        "91a8e0fbbf626bf0c24d3cb7adbbef332e42339f56dd943cf272be28978dc294".to_string()
    );
    let long_vec = vec![1u8; 1_000_000];
    assert_eq!(
        compute_hash(long_vec.as_slice())
            .unwrap()
            .encode_hex::<String>(),
        "1fb6a051d8996888485d47fea0007a88e1e78ea273fa5fb60e1ab00608dbb764".to_string()
    );

    host.budget_ref().reset_default().unwrap();
    let too_long_vec = vec![1u8; 10_000_000];
    assert!(is_budget_exceeded(
        compute_hash(too_long_vec.as_slice()).err().unwrap()
    ));
}

#[test]
fn keccak256_test() {
    let host = observe_host!(Host::test_host());
    let compute_hash = |input_bytes: &[u8]| -> Result<Vec<u8>, HostError> {
        let bytes_obj = host.bytes_new_from_slice(input_bytes).unwrap();
        let hash_obj = host.compute_hash_keccak256(bytes_obj)?;
        Ok(host
            .hash_from_bytesobj_input("hash", hash_obj)
            .unwrap()
            .0
            .to_vec())
    };
    assert_eq!(
        compute_hash(&[]).unwrap().encode_hex::<String>(),
        "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470".to_string()
    );
    assert_eq!(
        compute_hash(&[1]).unwrap().encode_hex::<String>(),
        "5fe7f977e71dba2ea1a68e21057beebb9be2ac30c6410aa38d4f3fbe41dcffd2".to_string()
    );
    assert_eq!(
        compute_hash(b"test vector for soroban")
            .unwrap()
            .encode_hex::<String>(),
        "352fe2eaddf44eb02eb3eab1f8d6ff4ba426df4f1734b1e3f210d621ee8853d9".to_string()
    );

    let long_vec = vec![1u8; 1_000_000];
    assert_eq!(
        compute_hash(long_vec.as_slice())
            .unwrap()
            .encode_hex::<String>(),
        "eb8c4805c2569851fe8a82ed3bf5a95f61090aad0489058aaa99d9b98019aad3".to_string()
    );
    host.budget_ref().reset_default().unwrap();
    let too_long_vec = vec![1u8; 10_000_000];
    assert!(is_budget_exceeded(
        compute_hash(too_long_vec.as_slice()).err().unwrap()
    ));
}

#[test]
fn ed25519_verify_test() {
    let host = observe_host!(Host::test_host());

    let verify_sig =
        |public_key: Vec<u8>, message: Vec<u8>, signature: Vec<u8>| -> Result<(), HostError> {
            let public_key_obj = host.bytes_new_from_slice(public_key.as_slice()).unwrap();
            let message_obj = host.bytes_new_from_slice(message.as_slice()).unwrap();
            let signature_obj = host.bytes_new_from_slice(signature.as_slice()).unwrap();
            host.verify_sig_ed25519(public_key_obj, message_obj, signature_obj)
                .map(|_| ())
        };

    // Successful scenarios from (https://datatracker.ietf.org/doc/html/rfc8032#section-7.1
    assert!(verify_sig(
        hex::decode(
            "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        )
        .unwrap(),
        hex::decode("").unwrap(),
        hex::decode(
            "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        )
        .unwrap(),
    )
    .is_ok());

    assert!(verify_sig(hex::decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c")
            .unwrap(),
        hex::decode("72")
            .unwrap(),
        hex::decode("92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")
            .unwrap(),
    ).is_ok());

    // Valid signature for a long message
    assert!(verify_sig(hex::decode("25ddcf6f16fe15f846553ceee99faad07b4c12e063cd817b7df66310d7338fda")
                                              .unwrap(),
                                          vec![b'a'; 100_000],
                                          hex::decode("1c2e91002652bf75c61d35055b674b7aab7a868a9feffd70b5f07abd8d95b09fb3d2e55b4a4b48f80bb92d1ed6b9d1cfe1388be443a2b37f95ee744756e1f702")
                                              .unwrap(),
    ).is_ok());

    // Failing scenarios
    // Incorrect payload for signature.
    assert!(is_crypto_error(verify_sig(hex::decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c")
                           .unwrap(),
                       hex::decode("73")
                           .unwrap(),
                       hex::decode("92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")
                           .unwrap(),
    ).err().unwrap()));

    // Malformed public key (missing one byte)
    assert!(is_crypto_error(verify_sig(hex::decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af466")
                           .unwrap(),
                       hex::decode("72")
                           .unwrap(),
                       hex::decode("92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")
                           .unwrap(),
    ).err().unwrap()));

    // Malformed signature (one extra byte)
    // This is a bit inconsistent with key error and returns an object error
    // (and not a crypto error).
    assert!(is_object_error(verify_sig(hex::decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c")
                                          .unwrap(),
                                      hex::decode("72")
                                          .unwrap(),
                                      hex::decode("92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c0000")
                                          .unwrap(),
    ).err().unwrap()));

    // Too long message with correct signature, run out of budget
    assert!(is_budget_exceeded(verify_sig(hex::decode("25ddcf6f16fe15f846553ceee99faad07b4c12e063cd817b7df66310d7338fda")
                           .unwrap(),
                       vec![b'a'; 10_000_000],
                       hex::decode("87f78aa58df29e330f2dea33a0c668a9953a918552ba1c6d44e8bdeb41acbdf4e8c1202fd32652ebf0fecc82e2569c500faeb7b1c33ece63eb6d7381b0910a0b")
                           .unwrap(),
    ).err().unwrap()));
}

#[test]
fn recover_ecdsa_secp256k1_key_test() {
    let host = observe_host!(Host::test_host());

    let recover_sig =
        |msg_digest: Vec<u8>, signature: Vec<u8>, recovery_id: u32| -> Result<String, HostError> {
            let msg_digest_obj = host.bytes_new_from_slice(msg_digest.as_slice()).unwrap();
            let signature_obj = host.bytes_new_from_slice(signature.as_slice()).unwrap();
            let recovery_id = U32Val::from(recovery_id);
            // Make sure we always verify with the fresh budget to make large payload tests
            // independent of each other.
            host.budget_ref().reset_default().unwrap();
            host.recover_key_ecdsa_secp256k1(msg_digest_obj, signature_obj, recovery_id)
                .map(|pk_obj| {
                    let bytes = host
                        .fixed_length_bytes_from_bytesobj_input::<Vec<u8>, 65>("pk", pk_obj)
                        .unwrap();
                    bytes.encode_hex()
                })
        };

    // Successful scenario
    // From ethereum: https://github.com/ethereum/go-ethereum/blob/master/crypto/secp256k1/secp256_test.go
    assert_eq!(recover_sig(
        hex::decode(
            "ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008"
        )
            .unwrap(),
        hex::decode("90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap(),
        1,
    )
        .unwrap(), "04e32df42865e97135acfb65f3bae71bdc86f4d49150ad6a440b6f15878109880a0a2b2667f7e725ceea70c673093bf67663e0312623c8e091b13cf2c0f11ef652");

    // Failing scenarios
    // Bad recovery id.
    assert!(is_crypto_error(recover_sig(
        hex::decode(
            "ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008"
        )
            .unwrap(),
        hex::decode("90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap(),
        5,
    )
                   .err().unwrap()));
    // Another bad recovery id.
    assert!(is_crypto_error(recover_sig(
        hex::decode(
            "ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008"
        )
            .unwrap(),
        hex::decode("90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap(),
        u32::MAX,
    )
        .err().unwrap()));
    // Malformed digest (missing one byte)
    assert!(is_object_error(recover_sig(
        hex::decode(
            "ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce9710"
        )
            .unwrap(),
        hex::decode("90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap(),
        1,
    )
        .err().unwrap()));
    // Malformed signature (one extra byte)
    assert!(is_crypto_error(recover_sig(
        hex::decode(
            "ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008"
        )
            .unwrap(),
        hex::decode("90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc9300").unwrap(),
        5,
    )
        .err().unwrap()));
}

const PROTOCOL_SUPPORT_FOR_SECP256R1: u32 = 21;

#[test]
fn test_secp256r1_signature_verification() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    if host.get_ledger_protocol_version()? < PROTOCOL_SUPPORT_FOR_SECP256R1 {
        return Ok(());
    }

    let verify_sig = |public_key: Vec<u8>,
                      msg_digest: Vec<u8>,
                      signature: Vec<u8>|
     -> Result<crate::Void, HostError> {
        let public_key_obj = host.bytes_new_from_slice(&public_key)?;
        let msg_digest_obj = host.bytes_new_from_slice(&msg_digest)?;
        let signature_obj = host.bytes_new_from_slice(&signature)?;
        // Make sure we always verify with the fresh budget to make large payload tests
        // independent of each other.
        host.budget_ref().reset_default().unwrap();
        host.verify_sig_ecdsa_secp256r1(public_key_obj, msg_digest_obj, signature_obj)
    };

    // 0. Valid
    assert!(verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest, computed from sha256(e1130af../* see NIST for full message */) */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        ).is_ok());

    // 1. invalid message digest
    // a) delete one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a").unwrap() /* msg digest, deleted one byte */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(
        res,
        (ScErrorType::Object, ScErrorCode::UnexpectedSize), /* this error comes from `xdr::Hash::try_from` */
    );
    // b) append one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a9400").unwrap() /* msg digest, padded one extra byte */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Object, ScErrorCode::UnexpectedSize));
    // c) modify one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a95").unwrap() /* msg digest, padded one extra byte */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));

    // 2. invalid public key
    // a) delete one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee9").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // b) append one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee92700").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // c) modify one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee928").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // d) wrong compression format: compressed, y is even (tag = 0x02)
    let res = verify_sig(
            hex::decode("02e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // e) wrong compression format: compressed, y is odd (tag = 0x03)
    let res = verify_sig(
            hex::decode("03e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // f) wrong compression format: y is zero element (tag = 0x00)
    let res = verify_sig(
            hex::decode("00").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // g) invalid tag
    let res = verify_sig(
            hex::decode("09e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // h) correct compression, invalid point (flip y and x coordinates)
    let res = verify_sig(
            hex::decode("04970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));

    // 3. signature
    // a) delete one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec87" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // b) append one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c00" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // c) modify one byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871d" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // d) r is 0
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("000000000000000000000000000000000000000000000000000000000000000017c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871c" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // e) s is 0
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f0000000000000000000000000000000000000000000000000000000000000000" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // f) s is in the upper half
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            /* this signature is produced by inverting the `s` of the signature
             above, thus it is still a mathematically correct one, but since it's not
             normalized it will get rejected by us */
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4fe83aaf697e6f763e1fc4632bea5420ed7898c87d313e0f5361ae2cb3a4769e35" ).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));
    // g) invalid signature, change a byte
    let res = verify_sig(
            hex::decode("04e424dc61d4bb3cb7ef4344a7f8957a0c5134e16f7a67c074f82e6e12f49abf3c970eed7aa2bc48651545949de1dddaf0127e5965ac85d1243d6f60e7dfaee927").unwrap() /* public key */, 
            hex::decode("d1b8ef21eb4182ee270638061063a3f3c16c114e33937f69fb232cc833965a94").unwrap() /* msg digest */, 
            hex::decode("bf96b99aa49c705c910be33142017c642ff540c76349b9dab72f981fd9347f4f17c55095819089c2e03b9cd415abdf12444e323075d98f31920b9e0f57ec871d" /* signature */).unwrap()
        );
    HostError::result_matches_err(res, (ScErrorType::Crypto, ScErrorCode::InvalidInput));

    Ok(())
}
