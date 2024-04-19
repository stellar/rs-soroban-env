use crate::Host;
use soroban_env_common::{
    xdr::{AccountId, Hash, PublicKey, ScAddress, ScBytes, ScString, Uint256},
    Compare, Env, StringObject, Val,
};

fn extract_string(host: &Host, s: StringObject) -> String {
    host.visit_obj(s, |st: &ScString| Ok(st.0.to_string()))
        .unwrap()
}

fn string_to_object(host: &Host, s: &str) -> Val {
    host.add_host_object(ScString(s.try_into().unwrap()))
        .unwrap()
        .to_val()
}

fn string_to_bytes_object(host: &Host, s: &str) -> Val {
    host.add_host_object(ScBytes(s.try_into().unwrap()))
        .unwrap()
        .to_val()
}

// Example values are taken from
// https://github.com/stellar/stellar-protocol/blob/master/ecosystem/sep-0023.md

#[test]
fn test_account_address_conversions() {
    let host = observe_host!(Host::test_host());
    let account_pk = [
        0x3f, 0x0c, 0x34, 0xbf, 0x93, 0xad, 0x0d, 0x99, 0x71, 0xd0, 0x4c, 0xcc, 0x90, 0xf7, 0x05,
        0x51, 0x1c, 0x83, 0x8a, 0xad, 0x97, 0x34, 0xa4, 0xa2, 0xfb, 0x0d, 0x7a, 0x03, 0xfc, 0x7f,
        0xe8, 0x9a,
    ];
    let address_obj = host
        .add_host_object(ScAddress::Account(AccountId(
            PublicKey::PublicKeyTypeEd25519(Uint256(account_pk)),
        )))
        .unwrap();

    let strkey = host.address_to_strkey(address_obj).unwrap();
    assert_eq!(
        extract_string(&host, strkey),
        "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
    );

    let converted_address_object = host.strkey_to_address(strkey.to_val()).unwrap();
    assert!((*host)
        .compare(&address_obj, &converted_address_object)
        .unwrap()
        .is_eq());
    let converted_from_bytes_obj = host
        .strkey_to_address(string_to_bytes_object(
            &host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ",
        ))
        .unwrap();
    assert!((*host)
        .compare(&address_obj, &converted_from_bytes_obj)
        .unwrap()
        .is_eq());
}

#[test]
fn test_contract_address_conversions() {
    let host = observe_host!(Host::test_host());
    let contract_id = [
        0x3f, 0x0c, 0x34, 0xbf, 0x93, 0xad, 0x0d, 0x99, 0x71, 0xd0, 0x4c, 0xcc, 0x90, 0xf7, 0x05,
        0x51, 0x1c, 0x83, 0x8a, 0xad, 0x97, 0x34, 0xa4, 0xa2, 0xfb, 0x0d, 0x7a, 0x03, 0xfc, 0x7f,
        0xe8, 0x9a,
    ];
    let address_obj = host
        .add_host_object(ScAddress::Contract(Hash(contract_id)))
        .unwrap();

    let strkey = host.address_to_strkey(address_obj).unwrap();
    assert_eq!(
        extract_string(&host, strkey),
        "CA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA"
    );

    let converted_address_object = host.strkey_to_address(strkey.to_val()).unwrap();
    assert!((*host)
        .compare(&address_obj, &converted_address_object)
        .unwrap()
        .is_eq());

    let converted_from_bytes_obj = host
        .strkey_to_address(string_to_bytes_object(
            &host,
            "CA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA",
        ))
        .unwrap();
    assert!((*host)
        .compare(&address_obj, &converted_from_bytes_obj)
        .unwrap()
        .is_eq());
}

#[test]
fn invalid_strkey_to_address_conversion() {
    let host = observe_host!(Host::test_host());
    assert!(host.strkey_to_address(string_to_object(&host, "")).is_err());
    assert!(host
        .strkey_to_address(string_to_object(&host, "GAAAAAAAACGC6"))
        .is_err());
    // one symbol short
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSG"
        ))
        .is_err());
    // one symbol long
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZZ"
        ))
        .is_err());
    // Invalid bit length (congruent to 1 mod 8)
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZA"
        ))
        .is_err());
    // Incorrect version
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "DA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA"
        ))
        .is_err());
    // Valid multiplexed account
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "MA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAAAAAAAACJUQ"
        ))
        .is_err());
    // Valid private key
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "SBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWHOKR"
        ))
        .is_err());
    // Valid pre auth tx
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "TBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWHXL7"
        ))
        .is_err());
    // Valid hash x
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "XBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWGTOG"
        ))
        .is_err());
    // Valid signed payload
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "PA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAQACAQDAQCQMBYIBEFAWDANBYHRAEISCMKBKFQXDAMRUGY4DUPB6IBZGM"
        ))
        .is_err());
}
