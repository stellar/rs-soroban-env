use blst::{
    blst_fp, blst_fp2, blst_fr, blst_keygen, blst_p1, blst_p1_affine, blst_p2, blst_p2_affine,
    blst_scalar,
};
use ed25519_dalek::ed25519::signature::rand_core::OsRng;
use rand::RngCore;

use crate::host::bls::{
    bls_fr_to_bytes, decode_p1, decode_p1_affine, decode_p2, decode_p2_affine, g1_one, g1_zero,
    g2_one, g2_zero, scalar_fr_from_bytes, BLS_FP_SIZE, BLS_G1_UNCOMPRESSED_SIZE,
    BLS_G2_UNCOMPRESSED_SIZE, BLS_SCALAR_SIZE,
};
use crate::Host;

#[test]
fn test_bls_fr_to_bytes() {
    let scalar_fr = blst_fr::default();
    let bytes = bls_fr_to_bytes(scalar_fr).unwrap();
    assert_eq!(bytes.len(), BLS_SCALAR_SIZE);
}

#[test]
fn test_scalar_fr_from_bytes() {
    let bytes = [0u8; BLS_SCALAR_SIZE];
    let scalar_fr = scalar_fr_from_bytes(&bytes).unwrap();
    assert_eq!(scalar_fr, blst_fr::default());
}

#[test]
fn test_decode_p1_affine() {
    // additive identity
    let mut bytes = [0u8; BLS_G1_UNCOMPRESSED_SIZE];
    bytes[0] = 0x40;
    let p1_affine = decode_p1_affine(&bytes).unwrap();
    assert_eq!(p1_affine, blst_p1_affine::default());
}

#[test]
fn test_p1_zero() {
    // additive identity
    let p1_affine = g1_zero();
    assert_eq!(hex::encode(p1_affine), "400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
}

#[test]
fn test_p1_one() {
    // multiplicative identity
    let p1_affine = g1_one();
    assert_eq!(hex::encode(p1_affine), "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1");
}

#[test]
fn test_p2_zero() {
    // additive identity
    let p2_affine = g2_zero();
    assert_eq!(hex::encode(p2_affine), "400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
}

#[test]
fn test_p2_one() {
    // multiplicative identity
    let p2_affine = g2_one();
    assert_eq!(hex::encode(p2_affine), "13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb80606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801");
}

#[test]
fn test_decode_p1() {
    let s = "026fcea34d1a4c5125142dfa3b616086309cab49e60e548d95de658af4d9329c269dc132bd5d884617e8767600daeee90c6f5d25f3d63540f3b799d291e5df4a90244346ed780d5c9d3afa8f3c9a196e089fa4edc4a9806592e8561d626579e3";
    let bytes = hex::decode(s).unwrap();
    let p1 = decode_p1(bytes.as_slice().try_into().unwrap()).unwrap();
    assert_eq!(
        p1,
        blst_p1 {
            x: blst_fp {
                l: [
                    9522032212070104096,
                    14704008314964026780,
                    6240597254928821153,
                    5889904107358641280,
                    17135096366920513157,
                    1083974352369730169
                ]
            },
            y: blst_fp {
                l: [
                    9803444800102823628,
                    5059586347318398602,
                    591178676386880618,
                    4712874644763535345,
                    6778859501939582959,
                    1645574050539793690
                ]
            },
            z: blst_fp {
                l: [
                    8505329371266088957,
                    17002214543764226050,
                    6865905132761471162,
                    8632934651105793861,
                    6631298214892334189,
                    1582556514881692819
                ]
            },
        }
    );
}

#[test]
fn test_decode_p2_affine() {
    // additive identity
    let mut bytes = [0u8; BLS_G2_UNCOMPRESSED_SIZE];
    bytes[0] = 0x40;
    let p2_affine = decode_p2_affine(&bytes).unwrap();
    assert_eq!(p2_affine, blst_p2_affine::default());
}

#[test]
fn test_decode_p2() {
    let bytes = hex::decode("14e9b22683a66543ec447b7aa76e4404424709728507581d0b3f60a8062c3f7c7d3365197c59f7c961fa9731084f5be60d0a936e93d556bdef2032cdcae2fa9902dcbe105e01d7ab7126d83486d882c4efd2fc1ac55044157333be19acf0cb7a10bc41c8081c9babd8d5b41b645badd4a679b3d4e1b3ea2c0e1f53b39c00b3889a40306c9b9ee2da5831e90148334d91016474d07e0f4e36d2d51b5ca11b633b9a940b9c126aebf4a2537c18fdc6967fb677824bfa902157e53cb499a021e57b").unwrap();
    let p2 = decode_p2(bytes.as_slice().try_into().unwrap()).unwrap();
    assert_eq!(
        p2,
        blst_p2 {
            x: blst_fp2 {
                fp: [
                    blst_fp {
                        l: [
                            5910463804193348924,
                            12106263024642462352,
                            4583498720154949598,
                            11556036403352332129,
                            10086399433537606111,
                            814493755602011239
                        ]
                    },
                    blst_fp {
                        l: [
                            11671710108490681288,
                            14831228016838450683,
                            7848753107382297332,
                            3730035743707207515,
                            2990542987574555629,
                            1652844757846900206
                        ]
                    }
                ]
            },
            y: blst_fp2 {
                fp: [
                    blst_fp {
                        l: [
                            6636923444040220808,
                            6431740067118963969,
                            1060006293027338967,
                            13238938954656170417,
                            7085560437947721606,
                            53942804531120882
                        ]
                    },
                    blst_fp {
                        l: [
                            10240908403147073127,
                            4495815664776461195,
                            9095410607741535746,
                            11386040834773514749,
                            7488306074634164353,
                            893202691648791915
                        ]
                    }
                ]
            },
            z: blst_fp2 {
                fp: [
                    blst_fp {
                        l: [
                            8505329371266088957,
                            17002214543764226050,
                            6865905132761471162,
                            8632934651105793861,
                            6631298214892334189,
                            1582556514881692819
                        ]
                    },
                    blst_fp {
                        l: [0, 0, 0, 0, 0, 0]
                    }
                ]
            },
        }
    );
}

#[test]
fn test_bls_g1_add() {
    let g1_zero = g1_zero();
    let g1_one = g1_one();
    let host = Host::default();
    let result = host.bls_g1_add_raw_internal(&g1_zero, &g1_zero);
    assert_eq!(result.unwrap(), g1_zero);
    let result = host.bls_g1_add_raw_internal(&g1_zero, &g1_one);
    assert_eq!(result.unwrap(), g1_one);
}

#[test]
fn test_bls_g2_add() {
    let g2_zero = g2_zero();
    let g2_one = g2_one();
    let host = Host::default();
    let result = host.bls_g2_add_raw_internal(&g2_zero, &g2_zero);
    assert_eq!(result.unwrap(), g2_zero);
    let result = host.bls_g2_add_raw_internal(&g2_zero, &g2_one);
    assert_eq!(result.unwrap(), g2_one);
}

#[test]
fn test_bls_g1_mul() {
    let scalar_zero = [0; BLS_SCALAR_SIZE];
    let mut scalar_one = [0; BLS_SCALAR_SIZE];
    scalar_one[0] = 1;
    let host = Host::default();
    let result = host.bls_g1_mul_raw_internal(&scalar_zero, &g1_zero());
    assert_eq!(result.unwrap(), g1_zero());
    let result = host.bls_g1_mul_raw_internal(&scalar_one, &g1_one());
    assert_eq!(result.unwrap(), g1_one());
}

#[test]
fn test_bls_g2_mul() {
    let scalar_zero = [0; BLS_SCALAR_SIZE];
    let mut scalar_one = [0; BLS_SCALAR_SIZE];
    scalar_one[0] = 1;
    let host = Host::default();
    let result = host.bls_g2_mul_raw_internal(&scalar_zero, &g2_zero());
    assert_eq!(result.unwrap(), g2_zero());
    let result = host.bls_g2_mul_raw_internal(&scalar_one, &g2_one());
    assert_eq!(result.unwrap(), g2_one());
}

#[test]
fn test_bls_map_to_g1_internal() {
    let host = Host::default();
    let fp = [0u8; BLS_FP_SIZE * 2];
    let result = host.bls_map_to_g1_internal(&fp);
    assert!(
        result.is_ok(),
        "bls_map_to_g1_internal failed with valid input"
    );
    let expected: [u8;BLS_G1_UNCOMPRESSED_SIZE] = hex::decode("19b6652bc7e44b6ca66a7803d1dff1b2d0fd02a32fa1b09f43716e21fec0b508e688e87b2d7a03618c066409ad53665c10549370803d643dee27b367d4381b08e1655cc8887914917419eed52ad0472115c9fac1a14974ddea16ada22eb37ba7").unwrap().try_into().unwrap();
    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_bls_map_to_g2_internal() {
    let host = Host::default();
    let fp = [0u8; BLS_FP_SIZE * 4];
    let result = host.bls_map_to_g2_internal(&fp);
    assert!(
        result.is_ok(),
        "bls_map_to_g2_internal failed with valid input"
    );
    let expected: [u8;BLS_G2_UNCOMPRESSED_SIZE] = hex::decode("18426da25dadd359adfda64fbaddac4414da2a841cb467935289877db450fac424361efb2e7fb141b7b98e6b2f888aef19da1b4d47efeeb154f8968b43da2125376e0999ba722141419b03fd857490562fa42a5d0973956d1932dd20c1e0a28403257c3be77016e69b75905a97871008a6dfd2e324a6748c48d3304380156987bd0905991824936fcfe34ab25c3b6caa0c2f8d431770d9be9b087c36fc5b66bb83ce6372669f48294193ef646105e0f21d17b134e7d1ad9c18f54b81f6a3707b").unwrap().try_into().unwrap();
    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_bls_hash_to_g1_internal() {
    let host = Host::default();
    let msg = "";
    let result = host.bls_hash_to_g1_internal(msg.as_bytes());
    // test vector from https://github.com/cfrg/draft-irtf-cfrg-hash-to-curve/blob/664b13592116cecc9e52fb192dcde0ade36f904e/poc/vectors/BLS12381G1_XMD%3ASHA-256_SSWU_RO_.json#L18-L37
    let expected: [u8;BLS_G1_UNCOMPRESSED_SIZE] = hex::decode("052926add2207b76ca4fa57a8734416c8dc95e24501772c814278700eed6d1e4e8cf62d9c09db0fac349612b759e79a108ba738453bfed09cb546dbb0783dbb3a5f1f566ed67bb6be0e8c67e2e81a4cc68ee29813bb7994998f3eae0c9c6a265").unwrap().try_into().unwrap();
    assert_eq!(result, Ok(expected));

    let msg = "abc";
    let result = host.bls_hash_to_g1_internal(msg.as_bytes());
    // test vector from https://github.com/cfrg/draft-irtf-cfrg-hash-to-curve/blob/664b13592116cecc9e52fb192dcde0ade36f904e/poc/vectors/BLS12381G1_XMD%3ASHA-256_SSWU_RO_.json#L38-L56
    let expected: [u8;BLS_G1_UNCOMPRESSED_SIZE] = hex::decode("03567bc5ef9c690c2ab2ecdf6a96ef1c139cc0b2f284dca0a9a7943388a49a3aee664ba5379a7655d3c68900be2f69030b9c15f3fe6e5cf4211f346271d7b01c8f3b28be689c8429c85b67af215533311f0b8dfaaa154fa6b88176c229f2885d").unwrap().try_into().unwrap();
    assert_eq!(result, Ok(expected));
}

#[test]
fn test_bls_hash_to_g2_internal() {
    let host = Host::default();
    let msg = "";
    let result = host.bls_hash_to_g2_internal(msg.as_bytes());
    // test vector from https://github.com/cfrg/draft-irtf-cfrg-hash-to-curve/blob/664b13592116cecc9e52fb192dcde0ade36f904e/poc/vectors/BLS12381G2_XMD%3ASHA-256_SSWU_RO_.json#L18-L37
    let expected: [u8;BLS_G2_UNCOMPRESSED_SIZE] = hex::decode("05cb8437535e20ecffaef7752baddf98034139c38452458baeefab379ba13dff5bf5dd71b72418717047f5b0f37da03d0141ebfbdca40eb85b87142e130ab689c673cf60f1a3e98d69335266f30d9b8d4ac44c1038e9dcdd5393faf5c41fb78a12424ac32561493f3fe3c260708a12b7c620e7be00099a974e259ddc7d1f6395c3c811cdd19f1e8dbf3e9ecfdcbab8d60503921d7f6a12805e72940b963c0cf3471c7b2a524950ca195d11062ee75ec076daf2d4bc358c4b190c0c98064fdd92").unwrap().try_into().unwrap();
    assert_eq!(result, Ok(expected));

    let msg = "abc";
    let result = host.bls_hash_to_g2_internal(msg.as_bytes());
    // test vector from https://github.com/cfrg/draft-irtf-cfrg-hash-to-curve/blob/664b13592116cecc9e52fb192dcde0ade36f904e/poc/vectors/BLS12381G2_XMD%3ASHA-256_SSWU_RO_.json#L38-L56
    let expected: [u8;BLS_G2_UNCOMPRESSED_SIZE] = hex::decode("139cddbccdc5e91b9623efd38c49f81a6f83f175e80b06fc374de9eb4b41dfe4ca3a230ed250fbe3a2acf73a41177fd802c2d18e033b960562aae3cab37a27ce00d80ccd5ba4b7fe0e7a210245129dbec7780ccc7954725f4168aff2787776e600aa65dae3c8d732d10ecd2c50f8a1baf3001578f71c694e03866e9f3d49ac1e1ce70dd94a733534f106d4cec0eddd161787327b68159716a37440985269cf584bcb1e621d3a7202be6ea05c4cfe244aeb197642555a0645fb87bf7466b2ba48").unwrap().try_into().unwrap();
    assert_eq!(result, Ok(expected));
}

#[test]
fn test_bls_pk_g1() {
    let host = Host::default();
    let msg = random_msg();
    let sk = random_scalar();

    let pk = host.bls_g1_mul_raw_internal(&sk, &g1_one()).unwrap();
    let hashed_msg = host.bls_hash_to_g2_internal(&msg).unwrap();
    let signature = host.bls_g2_mul_raw_internal(&sk, &hashed_msg).unwrap();

    let pk_msg_pairing = host.bls_pairing_internal(&pk, &hashed_msg).unwrap();
    let g1_gen_sig_pairing = host.bls_pairing_internal(&g1_one(), &signature).unwrap();
    assert_eq!(pk_msg_pairing, g1_gen_sig_pairing);
}

#[test]
fn bls_pk_g2() {
    let host = Host::default();
    let msg = random_msg();
    let sk = random_scalar();

    let pk = host.bls_g2_mul_raw_internal(&sk, &g2_one()).unwrap();
    let hashed_msg = host.bls_hash_to_g1_internal(&msg).unwrap();
    let signature = host.bls_g1_mul_raw_internal(&sk, &hashed_msg).unwrap();

    let pk_msg_pairing = host.bls_pairing_internal(&hashed_msg, &pk).unwrap();
    let g2_gen_sig_pairing = host.bls_pairing_internal(&signature, &g2_one()).unwrap();

    assert_eq!(pk_msg_pairing, g2_gen_sig_pairing);
}

fn random_msg() -> [u8; 32] {
    let mut rng = OsRng;
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    msg
}
fn random_scalar() -> [u8; BLS_SCALAR_SIZE] {
    let mut rng = OsRng;
    let mut buffer = [0u8; 64];
    rng.fill_bytes(&mut buffer);

    let mut scalar = blst_scalar::default();

    unsafe {
        blst_keygen(
            &mut scalar,
            buffer.as_ptr(),
            buffer.len(),
            [0; 0].as_ptr(),
            0,
        );
    }

    scalar.b
}
