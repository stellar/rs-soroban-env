mod v22 {
    use ark_bls12_381::{G1Affine, G2Affine};
    use ark_ec::AffineRepr;
    use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Validate};
    use hex::FromHex;
    use serde::Deserialize;
    use soroban_env_host::{
        budget::AsBudget,
        xdr::{ScErrorCode, ScErrorType},
        BytesObject, Env, EnvBase, Error, Host, HostError, Val,
    };
    use std::cmp::Ordering;

    pub const DST_ETHEREUM: &str = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_";

    #[derive(Deserialize, Debug)]
    struct Aggregate {
        input: Vec<String>,
        output: String,
    }

    #[derive(Deserialize, Debug)]
    struct AggregateVerifyInput {
        pubkeys: Vec<String>,
        messages: Vec<String>,
        signature: String,
    }

    #[derive(Deserialize, Debug)]
    struct AggregateVerify {
        input: AggregateVerifyInput,
        output: bool,
    }

    #[derive(Deserialize, Debug)]
    struct BatchVerifyInput {
        pubkeys: Vec<String>,
        messages: Vec<String>,
        signatures: Vec<String>,
    }

    #[derive(Deserialize, Debug)]
    struct BatchVerify {
        input: BatchVerifyInput,
        output: bool,
    }

    #[derive(Debug, Deserialize)]
    struct FastAggregateVerifyInput {
        pubkeys: Vec<String>,
        message: String,
        signature: String,
    }

    #[derive(Debug, Deserialize)]
    struct FastAggregateVerify {
        input: FastAggregateVerifyInput,
        output: bool,
    }

    #[derive(Deserialize, Debug)]
    struct VerifyInput {
        pubkey: String,
        message: String,
        signature: String,
    }

    #[derive(Debug, Deserialize)]
    struct Verify {
        input: VerifyInput,
        output: bool,
    }

    #[derive(Deserialize, Debug)]
    struct SignInput {
        privkey: String,
        message: String,
    }

    #[derive(Deserialize, Debug)]
    struct Sign {
        input: SignInput,
        output: String,
    }

    fn parse_hex(s: &str) -> Vec<u8> {
        Vec::from_hex(s.trim_start_matches("0x")).unwrap()
    }

    fn adapt_g1_point(host: &Host, p: &String) -> Result<BytesObject, HostError> {
        let p = parse_hex(p.as_str());
        assert_eq!(p.len(), 48);
        let pt =
            G1Affine::deserialize_with_mode(p.as_slice(), Compress::Yes, Validate::No).unwrap();
        let mut buf = vec![0u8; 96];
        pt.serialize_with_mode(buf.as_mut_slice(), Compress::No)
            .unwrap();
        host.bytes_new_from_slice(&buf)
    }

    fn neg_g1(host: &Host) -> Result<BytesObject, HostError> {
        let mut buf = [0u8; 96];
        let neg_g1 = -G1Affine::generator();
        neg_g1.serialize_uncompressed(buf.as_mut_slice()).unwrap();
        host.bytes_new_from_slice(&buf)
    }

    fn adapt_g2_point(host: &Host, p: &String) -> Result<BytesObject, HostError> {
        let p = parse_hex(p.as_str());
        assert_eq!(p.len(), 96);
        let pt =
            G2Affine::deserialize_with_mode(p.as_slice(), Compress::Yes, Validate::No).unwrap();
        let mut buf = vec![0u8; 192];
        pt.serialize_with_mode(buf.as_mut_slice(), Compress::No)
            .unwrap();
        host.bytes_new_from_slice(&buf)
    }

    fn hash_msg_to_curve(
        host: &Host,
        msg: &String,
        dst: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let msg = parse_hex(msg.as_str());
        let msg = host.bytes_new_from_slice(&msg)?;
        host.bls12_381_hash_to_g2(msg, dst)
    }

    fn aggregate_g1(host: &Host, inputs: &Vec<String>) -> Result<BytesObject, HostError> {
        assert!(!inputs.is_empty());
        let mut agg = adapt_g1_point(&host, &inputs[0])?;
        for i in 1..inputs.len() {
            let pt = adapt_g1_point(&host, &inputs[i])?;
            agg = host.bls12_381_g1_add(agg, pt)?;
        }
        match host.bls12_381_check_g1_is_in_subgroup(agg)?.into() {
            true => Ok(agg),
            false => Err(HostError::from(Error::from_type_and_code(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
            ))),
        }
    }

    fn aggregate_g2(host: &Host, inputs: &Vec<String>) -> Result<BytesObject, HostError> {
        assert!(!inputs.is_empty());
        let mut agg = adapt_g2_point(&host, &inputs[0])?;
        for i in 1..inputs.len() {
            let pt = adapt_g2_point(&host, &inputs[i])?;
            agg = host.bls12_381_g2_add(agg, pt)?;
        }
        match host.bls12_381_check_g2_is_in_subgroup(agg)?.into() {
            true => Ok(agg),
            false => Err(HostError::from(Error::from_type_and_code(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
            ))),
        }
    }

    fn verify_single_signature(
        host: &Host,
        pubkey: &String,
        msg: &String,
        sig: &String,
    ) -> Result<bool, HostError> {
        let dst = host.bytes_new_from_slice(DST_ETHEREUM.as_bytes())?;
        let pk = adapt_g1_point(host, &pubkey).unwrap().to_val();
        let msg = hash_msg_to_curve(host, &msg, dst).unwrap().to_val();
        let sig = adapt_g2_point(host, &sig)?.to_val();
        let neg_g1 = neg_g1(host)?.to_val();
        let g1_vec = host.vec_new_from_slice(&[pk, neg_g1])?;
        let g2_vec = host.vec_new_from_slice(&[msg, sig])?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        if res.as_val().is_false() {
            return Ok(false);
        }
        Ok(true)
    }

    // implements the CoreAggregateVerify algorithm specified in
    // https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-05.html#section-2.9
    fn aggregate_verify(
        host: &Host,
        pubkeys: &Vec<String>,
        msgs: &Vec<String>,
        sig: &String,
    ) -> Result<bool, HostError> {
        assert_eq!(pubkeys.len(), msgs.len());
        let dst = host.bytes_new_from_slice(DST_ETHEREUM.as_bytes())?;
        let mut g1_vec: Vec<Val> = pubkeys
            .iter()
            .map(|pk| adapt_g1_point(host, pk).unwrap().to_val())
            .collect();
        let mut g2_vec: Vec<Val> = msgs
            .iter()
            .map(|msg| hash_msg_to_curve(host, msg, dst).unwrap().to_val())
            .collect();
        let neg_g1 = neg_g1(host)?.to_val();
        g1_vec.push(neg_g1);
        let sig = adapt_g2_point(host, sig)?.to_val();
        g2_vec.push(sig);

        let g1_vec = host.vec_new_from_slice(&g1_vec)?;
        let g2_vec = host.vec_new_from_slice(&g2_vec)?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        if res.as_val().is_false() {
            return Ok(false);
        }
        Ok(true)
    }

    // we won't implement the batch verify logic for this test, we'll just verify individual signature
    fn batch_verify(
        host: &Host,
        pubkeys: &Vec<String>,
        msgs: &Vec<String>,
        sigs: &Vec<String>,
    ) -> Result<bool, HostError> {
        assert_eq!(pubkeys.len(), msgs.len());
        assert_eq!(pubkeys.len(), sigs.len());
        for i in 0..pubkeys.len() {
            let res = verify_single_signature(host, &pubkeys[i], &msgs[i], &sigs[i])?;
            if !res {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn fast_aggregate_verify(
        host: &Host,
        pubkeys: &Vec<String>,
        msg: &String,
        sig: &String,
    ) -> Result<bool, HostError> {
        assert!(!pubkeys.is_empty());
        let dst = host.bytes_new_from_slice(DST_ETHEREUM.as_bytes())?;
        let agg_pk = aggregate_g1(host, &pubkeys)?.to_val();
        let msg = hash_msg_to_curve(host, msg, dst).unwrap().to_val();
        let sig = adapt_g2_point(host, &sig)?.to_val();
        let neg_g1 = neg_g1(host)?.to_val();
        let g1_vec = host.vec_new_from_slice(&[agg_pk, neg_g1])?;
        let g2_vec = host.vec_new_from_slice(&[msg, sig])?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        if res.as_val().is_false() {
            return Ok(false);
        }
        Ok(true)
    }

    fn sign(host: &Host, priv_key: &String, msg: &String) -> Result<BytesObject, HostError> {
        let dst = host.bytes_new_from_slice(DST_ETHEREUM.as_bytes())?;
        let key_bytes = host.bytes_new_from_slice(parse_hex(&priv_key).as_slice())?;
        let key_u256 = host.u256_val_from_be_bytes(key_bytes)?;
        let msg = host.bytes_new_from_slice(&parse_hex(msg).as_slice())?;
        let msg_g2 = host.bls12_381_hash_to_g2(msg, dst)?;
        host.bls12_381_g2_mul(msg_g2, key_u256)
    }

    #[test]
    fn test_aggregate() -> Result<(), HostError> {
        let test_files = [
            "aggregate_0x0000000000000000000000000000000000000000000000000000000000000000.json",
            "aggregate_0x5656565656565656565656565656565656565656565656565656565656565656.json",
            "aggregate_0xabababababababababababababababababababababababababababababababab.json",
            "aggregate_infinity_signature.json",
            "aggregate_single_signature.json",
        ];
        let host = Host::test_host();
        for filename in test_files {
            let test_suite: Aggregate = serde_json::from_slice(
                &std::fs::read(format!("./tests/data/ethereum-bls/aggregate/{}", filename))
                    .unwrap(),
            )
            .unwrap();
            let agg = aggregate_g2(&host, &test_suite.input)?;
            let res = adapt_g2_point(&host, &test_suite.output)?;
            assert_eq!(
                host.obj_cmp(agg.to_val(), res.to_val())?,
                Ordering::Equal as i64
            );
        }
        Ok(())
    }

    #[test]
    fn test_aggregate_verify() -> Result<(), HostError> {
        let test_files = ["aggregate_verify_valid.json"];

        let host = Host::test_host();
        host.enable_debug()?;
        for filename in test_files {
            let test_suite: AggregateVerify = serde_json::from_slice(
                &std::fs::read(format!(
                    "./tests/data/ethereum-bls/aggregate_verify/{}",
                    filename
                ))
                .unwrap(),
            )
            .unwrap();
            let res = aggregate_verify(
                &host,
                &test_suite.input.pubkeys,
                &test_suite.input.messages,
                &test_suite.input.signature,
            );
            assert_eq!(res?, test_suite.output)
        }
        Ok(())
    }

    #[test]
    fn test_batch_verify() -> Result<(), HostError> {
        let test_files = [
            "batch_verify_invalid_forged_signature_set.json",
            "batch_verify_valid_multiple_signature_set.json",
            "batch_verify_valid_simple_signature_set.json",
        ];

        let host = Host::test_host();
        host.enable_debug()?;
        for filename in test_files {
            host.as_budget().reset_default()?;
            let test_suite: BatchVerify = serde_json::from_slice(
                &std::fs::read(format!(
                    "./tests/data/ethereum-bls/batch_verify/{}",
                    filename
                ))
                .unwrap(),
            )
            .unwrap();
            let res = batch_verify(
                &host,
                &test_suite.input.pubkeys,
                &test_suite.input.messages,
                &test_suite.input.signatures,
            );
            assert_eq!(res?, test_suite.output)
        }
        Ok(())
    }

    #[test]
    fn test_fast_aggregate_verify() -> Result<(), HostError> {
        let test_files = [
            "fast_aggregate_verify_extra_pubkey_4f079f946446fabf.json",
            "fast_aggregate_verify_valid_3d7576f3c0e3570a.json",
            "fast_aggregate_verify_extra_pubkey_5a38e6b4017fe4dd.json",
            "fast_aggregate_verify_valid_5e745ad0c6199a6c.json",
            "fast_aggregate_verify_extra_pubkey_a698ea45b109f303.json",
            "fast_aggregate_verify_valid_652ce62f09290811.json",
        ];
        let host = Host::test_host();
        host.enable_debug()?;
        for filename in test_files {
            host.as_budget().reset_default()?;
            let test_suite: FastAggregateVerify = serde_json::from_slice(
                &std::fs::read(format!(
                    "./tests/data/ethereum-bls/fast_aggregate_verify/{}",
                    filename
                ))
                .unwrap(),
            )
            .unwrap();
            let res = fast_aggregate_verify(
                &host,
                &test_suite.input.pubkeys,
                &test_suite.input.message,
                &test_suite.input.signature,
            );
            assert_eq!(res?, test_suite.output)
        }
        Ok(())
    }

    #[test]
    fn test_verify() -> Result<(), HostError> {
        let test_files = [
            "verify_valid_case_195246ee3bd3b6ec.json",
            "verify_wrong_pubkey_case_2ea479adf8c40300.json",
            "verify_valid_case_2ea479adf8c40300.json",
            "verify_wrong_pubkey_case_2f09d443ab8a3ac2.json",
            "verify_valid_case_2f09d443ab8a3ac2.json",
            "verify_wrong_pubkey_case_3208262581c8fc09.json",
            "verify_valid_case_3208262581c8fc09.json",
            "verify_wrong_pubkey_case_6b3b17f6962a490c.json",
            "verify_valid_case_6b3b17f6962a490c.json",
            "verify_wrong_pubkey_case_6eeb7c52dfd9baf0.json",
            "verify_valid_case_6eeb7c52dfd9baf0.json",
            "verify_wrong_pubkey_case_8761a0b7e920c323.json",
            "verify_valid_case_8761a0b7e920c323.json",
            "verify_wrong_pubkey_case_d34885d766d5f705.json",
            "verify_valid_case_d34885d766d5f705.json",
            "verify_wrong_pubkey_case_e8a50c445c855360.json",
            "verify_valid_case_e8a50c445c855360.json",
            "verifycase_one_privkey_47117849458281be.json",
            "verify_wrong_pubkey_case_195246ee3bd3b6ec.json",
        ];
        let host = Host::test_host();
        host.enable_debug()?;
        for filename in test_files {
            host.as_budget().reset_default()?;
            let test_suite: Verify = serde_json::from_slice(
                &std::fs::read(format!("./tests/data/ethereum-bls/verify/{}", filename)).unwrap(),
            )
            .unwrap();
            let res = verify_single_signature(
                &host,
                &test_suite.input.pubkey,
                &test_suite.input.message,
                &test_suite.input.signature,
            );
            assert_eq!(res?, test_suite.output)
        }
        Ok(())
    }

    #[test]
    fn test_sign() -> Result<(), HostError> {
        let test_files = [
            "sign_case_11b8c7cad5238946.json",
            "sign_case_37286e1a6d1f6eb3.json",
            "sign_case_84d45c9c7cca6b92.json",
            "sign_case_c82df61aa3ee60fb.json",
            "sign_case_f2ae1097e7d0e18b.json",
            "sign_case_142f678a8d05fcd1.json",
            "sign_case_7055381f640f2c1d.json",
            "sign_case_8cd3d4d0d9a5b265.json",
            "sign_case_d0e28d7e76eb6e9c.json",
        ];
        let host = Host::test_host();
        host.enable_debug()?;
        for filename in test_files {
            host.as_budget().reset_default()?;
            let test_suite: Sign = serde_json::from_slice(
                &std::fs::read(format!("./tests/data/ethereum-bls/sign/{}", filename)).unwrap(),
            )
            .unwrap();
            let res = sign(&host, &test_suite.input.privkey, &test_suite.input.message)?;
            let output = adapt_g2_point(&host, &test_suite.output)?;
            assert_eq!(
                host.obj_cmp(res.to_val(), output.to_val())?,
                std::cmp::Ordering::Equal as i64
            );
        }
        Ok(())
    }
}
