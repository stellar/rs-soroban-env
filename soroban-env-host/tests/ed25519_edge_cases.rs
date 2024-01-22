//! Ed25519 was originally provided in a reference implementation, and later
//! multiply implemented in other libraries, and finally specified in RFC-8032
//! (https://!datatracker.ietf.org/doc/html/rfc8032).
//!
//! All these implementations, as well as RFC-8032, have a certain degree of
//! wiggle room in how they handle some edge cases. Specifically:
//!
//!   - "Small-order" and/or "mixed-order" group elements (curve points), which
//!     are group elements that have a nonzero torsion-subgroup component of
//!     order 1..=8, may or may not be considered equal depending on the
//!     equality check used:
//!
//!     - One type of equality check (the "batched" or "cofactored" check)
//!       multiplies all points by 8 before checking equality, which eliminates
//!       torsion-subgroup component variation of such group elements (the
//!       torsion-subgroup component is zeroed by the multiplication).
//!
//!     - The other type of equality check (the "unbatched" or "cofactor-free"
//!       check) does not do this, instead considering elements with differing
//!       torsion-subgroup components unequal.
//!
//!   - Group elements (curve points) may also be encoded in different ways. The
//!     "canonical" encoding involves reducing the y-coordinate modulo 2^255-19,
//!     the "non-canonical" encoding does not. There are 19 possible
//!     non-canonical point encodings and of which 2 have small order, 10 have
//!     mixed order (i.e. have nonzero torsion-subgroup components _and_ nonzero
//!     prime-subgroup components), and 7 decode to invalid curve points.
//!
//!   - It is also possible to encode the scalar used in the signature equation
//!     in different ways -- again reducing modulo the group order L, or not --
//!     but in practice all libraries reject the non-reduced ("non-canonical")
//!     scalar encodings.
//!
//! If you want to read a treatment of these issues in more detail or by people
//! who are better at math, read the blog post
//! https://hdevalence.ca/blog/2020-10-04-its-25519am as well as the paper
//! https://eprint.iacr.org/2020/1244.pdf
//!
//! The Zcash project noticed the remaining variation, and published ZIP-215 to
//! specify what to do. They chose to _accept_ all the non-canonical encodings
//! and mandate the cofactor in the verification equation (i.e. multiplying by
//! 8). They made these choices for the sake of (a) supporting batch
//! verification and (b) supporting existing signatures they have in the field
//! under existing code, some of which might have nonzero torsion-subgroup
//! components or non-canonical encodings.
//!
//! Since Soroban is a new implementation based on the dalek libraries, and
//! because we wish to also interoperate with stellar-core which is based on the
//! libsodium library, we actually take the opposite position from ZIP-215:
//!
//!   - reject all small-order points (though accept mixed-order)
//!   - reject all non-canonical encodings
//!   - verify without the cofactor in the verification equation
//!
//! This is the default behaviour in libsodium (at least after version 1.0.16-18
//! which changed the rules, which we use in stellar-core) as well as dalek's
//! `verify_strict` function. We test dalek in this file and have a
//! corresponding set of tests with the same test vectors in stellar-core to
//! confirm identical behaviour with libsodium.

use ed25519_dalek::{Signature, VerifyingKey, PUBLIC_KEY_LENGTH, SIGNATURE_LENGTH};
use soroban_env_common::{Env, EnvBase};
use soroban_env_host::{
    xdr::{ScErrorCode, ScErrorType},
    Host,
};

// The Zcash work produced 14 x 14 = 196 test vectors expressing combinations
// of boundary cases for public keys and signatures (14 here being the 8
// canonical encodings of the 8 small-order points, plus 6 non-canonical
// encodings of some of these points; and then there's a test vector for such a
// small-order point occurring in either the public key _or_ the signature, so
// 14 x 14). We use these test vectors to verify that our implementation
// _rejects_ them all.
#[test]
fn check_zcash_test_vectors_are_rejected() {
    let host = Host::default();
    const ZIP215_TEST_MESSAGE: &[u8] = b"Zcash";
    let msg = host.bytes_new_from_slice(&ZIP215_TEST_MESSAGE).unwrap();
    for (i, test_vector) in ZCASH_TEST_VECTORS.iter().enumerate() {
        println!("Zcash Test vector {}", i);
        let public_key: [u8; PUBLIC_KEY_LENGTH] = hex::decode(test_vector.public_key)
            .unwrap()
            .try_into()
            .unwrap();
        let signature: [u8; SIGNATURE_LENGTH] = hex::decode(test_vector.signature)
            .unwrap()
            .try_into()
            .unwrap();
        let vk = VerifyingKey::from_bytes(&public_key).unwrap();
        let sig = Signature::from_bytes(&signature);
        // The ZIP-215 test vectors are all valid signatures of the message "Zcash",
        // but we want to reject them because they're all small-order points.
        assert!(vk.verify_strict(&ZIP215_TEST_MESSAGE, &sig).is_err());

        // Also run the same test through the host interface to confirm it there.
        let pk = host.bytes_new_from_slice(&public_key).unwrap();
        let sig = host.bytes_new_from_slice(&signature).unwrap();
        match host.verify_sig_ed25519(pk, msg, sig) {
            Ok(_) => panic!("accepted Zcash testcase {} that should be rejected", i),
            Err(e) => assert!(
                e.error.is_type(ScErrorType::Crypto) && e.error.is_code(ScErrorCode::InvalidInput),
            ),
        }
    }
}

// The IACR 2020/1244 paper (https://eprint.iacr.org/2020/1244.pdf) includes a
// set of test vectors in section 5 of the paper that we check here as well.
#[test]
fn check_iacr_2020_1244_test_vectors() {
    let host = Host::default();
    for (i, test_vector) in IACR_2020_1244_TEST_VECTORS.iter().enumerate() {
        println!("IACR 2020/1244 Test vector {}", i);
        let public_key: [u8; PUBLIC_KEY_LENGTH] = hex::decode(test_vector.pub_key)
            .unwrap()
            .try_into()
            .unwrap();
        let signature: [u8; SIGNATURE_LENGTH] = hex::decode(test_vector.signature)
            .unwrap()
            .try_into()
            .unwrap();
        let vk = VerifyingKey::from_bytes(&public_key).unwrap();
        let sig = Signature::from_bytes(&signature);
        let message: [u8; 32] = hex::decode(test_vector.message)
            .unwrap()
            .try_into()
            .unwrap();
        let res = vk.verify_strict(&message, &sig).is_err();
        assert_eq!(res, test_vector.should_fail);

        // Test through the host interface as well.
        let pk = host.bytes_new_from_slice(&public_key).unwrap();
        let sig = host.bytes_new_from_slice(&signature).unwrap();
        let msg = host.bytes_new_from_slice(&message).unwrap();
        match host.verify_sig_ed25519(pk, msg, sig) {
            Ok(_) => assert!(!test_vector.should_fail),
            Err(e) => assert!(
                test_vector.should_fail
                    && e.error.is_type(ScErrorType::Crypto)
                    && e.error.is_code(ScErrorCode::InvalidInput),
            ),
        }
    }
}

struct ZcashTestVector {
    public_key: &'static str,
    signature: &'static str,
}

struct Iacr20201244TestVector {
    message: &'static str,
    pub_key: &'static str,
    signature: &'static str,
    should_fail: bool,
}

const IACR_2020_1244_TEST_VECTORS: &[Iacr20201244TestVector; 12] = &[
    // Case 0: Small-order A and R components (should be rejected) but verifies
    // under either equality check.
    Iacr20201244TestVector {
        message: "8c93255d71dcab10e8f379c26200f3c7bd5f09d9bc3068d3ef4edeb4853022b6" ,
        pub_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
        should_fail: true,
    },
    // Case 1: Small-order A component (should be rejected) but verifies under
    // either equality check.
    Iacr20201244TestVector {
        message: "9bd9f44f4dcc75bd531b56b2cd280b0bb38fc1cd6d1230e14861d861de092e79",
        pub_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "f7badec5b8abeaf699583992219b7b223f1df3fbbea919844e3f7c554a43dd43a5bb704786be79fc476f91d3f3f89b03984d8068dcf1bb7dfc6637b45450ac04",
        should_fail: true,
    },
    // Case 2: Small-order R component (should be rejected) but verifies under
    // either equality check.
    Iacr20201244TestVector {
        message: "aebf3f2601a0c8c5d39cc7d8911642f740b78168218da8471772b35f9d35b9ab",
        pub_key: "f7badec5b8abeaf699583992219b7b223f1df3fbbea919844e3f7c554a43dd43",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa8c4bd45aecaca5b24fb97bc10ac27ac8751a7dfe1baff8b953ec9f5833ca260e",
        should_fail: true,
    },
    // Case 3: Mixed-order A and R, verifies under either equality check, should
    // be accepted.
    Iacr20201244TestVector {
        message: "9bd9f44f4dcc75bd531b56b2cd280b0bb38fc1cd6d1230e14861d861de092e79",
        pub_key: "cdb267ce40c5cd45306fa5d2f29731459387dbf9eb933b7bd5aed9a765b88d4d",
        signature: "9046a64750444938de19f227bb80485e92b83fdb4b6506c160484c016cc1852f87909e14428a7a1d62e9f22f3d3ad7802db02eb2e688b6c52fcd6648a98bd009",
        should_fail: false,
    },
    // Case 4: Mixed-order A and R, only verifies under cofactor equality check,
    // should be rejected.
    Iacr20201244TestVector {
        message: "e47d62c63f830dc7a6851a0b1f33ae4bb2f507fb6cffec4011eaccd55b53f56c",
        pub_key: "cdb267ce40c5cd45306fa5d2f29731459387dbf9eb933b7bd5aed9a765b88d4d",
        signature: "160a1cb0dc9c0258cd0a7d23e94d8fa878bcb1925f2c64246b2dee1796bed5125ec6bc982a269b723e0668e540911a9a6a58921d6925e434ab10aa7940551a09",
        should_fail: true,
    },
    // Case 5: Mixed-order A, order-L R, only verifies under cofactor equality
    // check, should be rejected.
    Iacr20201244TestVector {
        message: "e47d62c63f830dc7a6851a0b1f33ae4bb2f507fb6cffec4011eaccd55b53f56c",
        pub_key: "cdb267ce40c5cd45306fa5d2f29731459387dbf9eb933b7bd5aed9a765b88d4d",
        signature: "21122a84e0b5fca4052f5b1235c80a537878b38f3142356b2c2384ebad4668b7e40bc836dac0f71076f9abe3a53f9c03c1ceeeddb658d0030494ace586687405",
        should_fail: true,
    },
    // Case 6: Order-L A and R, non-canonical S (> L), should be rejected.
    Iacr20201244TestVector {
        message: "85e241a07d148b41e47d62c63f830dc7a6851a0b1f33ae4bb2f507fb6cffec40",
        pub_key: "442aad9f089ad9e14647b1ef9099a1ff4798d78589e66f28eca69c11f582a623",
        signature: "e96f66be976d82e60150baecff9906684aebb1ef181f67a7189ac78ea23b6c0e547f7690a0e2ddcd04d87dbc3490dc19b3b3052f7ff0538cb68afb369ba3a514",
        should_fail: true,
    },
    // Case 7: Order-L A and R, non-canonical S (>> L) in a way that fails
    // bitwise canonicity tests, should be rejected.
    //
    // NB: There's a typo (an extra 'e') in the middle of test vector 7's
    // signature in the appendix of the paper itself, but this is corrected in
    // Novi's formal / machine-generated testcases in
    // https://github.com/novifinancial/ed25519-speccheck/blob/main/cases.txt
    Iacr20201244TestVector {
        message: "85e241a07d148b41e47d62c63f830dc7a6851a0b1f33ae4bb2f507fb6cffec40",
        pub_key: "442aad9f089ad9e14647b1ef9099a1ff4798d78589e66f28eca69c11f582a623",
        signature: "8ce5b96c8f26d0ab6c47958c9e68b937104cd36e13c33566acd2fe8d38aa19427e71f98a473474f2f13f06f97c20d58cc3f54b8bd0d272f42b695dd7e89a8c22",
        should_fail: true,
    },
    // Case 8: Non-canonical R, should fail.
    Iacr20201244TestVector {
        message: "9bedc267423725d473888631ebf45988bad3db83851ee85c85e241a07d148b41",
        pub_key: "f7badec5b8abeaf699583992219b7b223f1df3fbbea919844e3f7c554a43dd43",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff03be9678ac102edcd92b0210bb34d7428d12ffc5df5f37e359941266a4e35f0f",
        should_fail: true
    },
    // Case 9: Non-canonical R noticed at a different phase of checking in some
    // implementations, should also fail.
    Iacr20201244TestVector {
        message: "9bedc267423725d473888631ebf45988bad3db83851ee85c85e241a07d148b41",
        pub_key: "f7badec5b8abeaf699583992219b7b223f1df3fbbea919844e3f7c554a43dd43",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffca8c5b64cd208982aa38d4936621a4775aa233aa0505711d8fdcfdaa943d4908",
        should_fail: true
    },
    // Case 10: Non-canonical A
    Iacr20201244TestVector {
        message: "e96b7021eb39c1a163b6da4e3093dcd3f21387da4cc4572be588fafae23c155b",
        pub_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "a9d55260f765261eb9b84e106f665e00b867287a761990d7135963ee0a7d59dca5bb704786be79fc476f91d3f3f89b03984d8068dcf1bb7dfc6637b45450ac04",
        should_fail: true
    },
    // Case 11: Non-canonical A noticed at a different phase of checking in some
    // implementations, should also fail.
    Iacr20201244TestVector {
        message: "39a591f5321bbe07fd5a23dc2f39d025d74526615746727ceefd6e82ae65c06f",
        pub_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "a9d55260f765261eb9b84e106f665e00b867287a761990d7135963ee0a7d59dca5bb704786be79fc476f91d3f3f89b03984d8068dcf1bb7dfc6637b45450ac04",
        should_fail: true
    }
];

const ZCASH_TEST_VECTORS: &[ZcashTestVector; 196] = &
[
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000000",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000080",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0000000000000000000000000000000000000000000000000000000000000000",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "0100000000000000000000000000000000000000000000000000000000000080",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc050000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc850000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "01000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f0000000000000000000000000000000000000000000000000000000000000000",
    },
    ZcashTestVector {
        public_key: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        signature: "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000",
    }
];
