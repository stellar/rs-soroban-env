use crate::{
    host_object::MuxedScAddress,
    xdr::{
        AccountId, ClaimableBalanceId, ContractId, Hash, MuxedEd25519Account, PoolId, PublicKey,
        ScAddress, ScBytes, ScErrorCode, ScErrorType, ScString, ScVal, Uint256,
    },
    AddressObject, Compare, Env, Host, HostError, MuxedAddressObject, StringObject, Val,
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

#[test]
fn test_muxed_address_to_components_conversion() {
    let host = observe_host!(Host::test_host());
    let muxed_address_obj = host
        .add_host_object(MuxedScAddress(ScAddress::MuxedAccount(
            MuxedEd25519Account {
                id: 123,
                ed25519: Uint256([10; 32]),
            },
        )))
        .unwrap();
    let address = host
        .get_address_from_muxed_address(muxed_address_obj)
        .unwrap();
    let mux_id = host.get_id_from_muxed_address(muxed_address_obj).unwrap();
    let address_val = host.from_host_val(address.into()).unwrap();
    let mux_id_val = host.from_host_val(mux_id.into()).unwrap();
    assert_eq!(
        address_val,
        ScVal::Address(ScAddress::Account(AccountId(
            PublicKey::PublicKeyTypeEd25519(Uint256([10; 32])),
        )))
    );
    assert_eq!(mux_id_val, ScVal::U64(123));
}

#[test]
fn test_invalid_muxed_address_object_conversions() {
    let host = observe_host!(Host::test_host());
    assert!(HostError::result_matches_err(
        host.add_host_object(MuxedScAddress(ScAddress::Account(AccountId(
            PublicKey::PublicKeyTypeEd25519(Uint256([0; 32])),
        )))),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    assert!(HostError::result_matches_err(
        host.add_host_object(MuxedScAddress(ScAddress::Contract(ContractId(Hash(
            [100; 32]
        ))))),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    assert!(HostError::result_matches_err(
        host.add_host_object(MuxedScAddress(ScAddress::LiquidityPool(PoolId(Hash(
            [66; 32],
        ))))),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    assert!(HostError::result_matches_err(
        host.add_host_object(MuxedScAddress(ScAddress::ClaimableBalance(
            ClaimableBalanceId::ClaimableBalanceIdTypeV0(Hash([5; 32]))
        ))),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));
}

#[test]
fn test_invalid_address_object_conversions() {
    let host = observe_host!(Host::test_host());

    assert!(HostError::result_matches_err(
        host.add_host_object(ScAddress::MuxedAccount(MuxedEd25519Account {
            id: 123,
            ed25519: Uint256([10; 32]),
        },)),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    assert!(HostError::result_matches_err(
        host.add_host_object(ScAddress::LiquidityPool(PoolId(Hash([66; 32],)))),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));

    assert!(HostError::result_matches_err(
        host.add_host_object(ScAddress::ClaimableBalance(
            ClaimableBalanceId::ClaimableBalanceIdTypeV0(Hash([5; 32]))
        )),
        (ScErrorType::Object, ScErrorCode::InvalidInput)
    ));
}

// Example values are taken from
// https://github.com/stellar/stellar-protocol/blob/master/ecosystem/sep-0023.md

fn run_test_account_address_conversions<F, G>(
    host: &Host,
    address_to_strkey: F,
    strkey_to_address: G,
) where
    F: Fn(&Host, AddressObject) -> StringObject,
    G: Fn(&Host, Val) -> AddressObject,
{
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

    let strkey = address_to_strkey(host, address_obj);
    assert_eq!(
        extract_string(host, strkey),
        "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
    );

    let converted_address_object = strkey_to_address(host, strkey.to_val());
    assert!(host
        .compare(&address_obj, &converted_address_object)
        .unwrap()
        .is_eq());

    let converted_from_bytes_obj = strkey_to_address(
        host,
        string_to_bytes_object(
            host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ",
        ),
    );
    assert!(host
        .compare(&address_obj, &converted_from_bytes_obj)
        .unwrap()
        .is_eq());
}

fn run_test_contract_address_conversions<F, G>(
    host: &Host,
    address_to_strkey: F,
    strkey_to_address: G,
) where
    F: Fn(&Host, AddressObject) -> StringObject,
    G: Fn(&Host, Val) -> AddressObject,
{
    let contract_id = [
        0x3f, 0x0c, 0x34, 0xbf, 0x93, 0xad, 0x0d, 0x99, 0x71, 0xd0, 0x4c, 0xcc, 0x90, 0xf7, 0x05,
        0x51, 0x1c, 0x83, 0x8a, 0xad, 0x97, 0x34, 0xa4, 0xa2, 0xfb, 0x0d, 0x7a, 0x03, 0xfc, 0x7f,
        0xe8, 0x9a,
    ];
    let address_obj = host
        .add_host_object(ScAddress::Contract(ContractId(Hash(contract_id))))
        .unwrap();

    let strkey = address_to_strkey(host, address_obj);
    assert_eq!(
        extract_string(host, strkey),
        "CA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA"
    );

    let converted_address_object = strkey_to_address(host, strkey.to_val());
    assert!(host
        .compare(&address_obj, &converted_address_object)
        .unwrap()
        .is_eq());

    let converted_from_bytes_obj = strkey_to_address(
        host,
        string_to_bytes_object(
            host,
            "CA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA",
        ),
    );
    assert!(host
        .compare(&address_obj, &converted_from_bytes_obj)
        .unwrap()
        .is_eq());
}

fn run_test_invalid_strkey_to_address<F, T>(host: &Host, strkey_to_address: F)
where
    F: Fn(&Host, Val) -> Result<T, HostError>,
{
    assert!(strkey_to_address(host, string_to_object(host, "")).is_err());
    assert!(strkey_to_address(host, string_to_object(host, "GAAAAAAAACGC6")).is_err());
    // one symbol short
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSG"
        )
    )
    .is_err());
    // one symbol long
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZZ"
        )
    )
    .is_err());
    // Invalid bit length (congruent to 1 mod 8)
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZA"
        )
    )
    .is_err());
    // Incorrect version
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "DA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA"
        )
    )
    .is_err());
    // Valid private key
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "SBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWHOKR"
        )
    )
    .is_err());
    // Valid pre auth tx
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "TBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWHXL7"
        )
    )
    .is_err());
    // Valid hash x
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "XBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWGTOG"
        )
    )
    .is_err());
    // Valid signed payload
    assert!(strkey_to_address(
        host,
        string_to_object(host, "PA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAQACAQDAQCQMBYIBEFAWDANBYHRAEISCMKBKFQXDAMRUGY4DUPB6IBZGM")
    )
    .is_err());
    // Valid liquidity pool
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "LA3D5KRYM6CB7OWQ6TWYRR3Z4T7GNZLKERYNZGGA5SOAOPIFY6YQGZ5J"
        )
    )
    .is_err());
    // Valid claimable balance
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "BAADMPVKHBTYIH522D2O3CGHPHSP4ZXFNISHBXEYYDWJYBZ5AXD3CA3GDE"
        )
    )
    .is_err());

    // Invalid base32 characters (0, 1, 8, 9 are not in base32 alphabet)
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA0QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA1QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA8QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA9QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());

    // Lowercase (stellar strkey uses uppercase base32)
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "ga7qynf7sowq3glr2bgmzehxavirza4kvwltjjfc7mgxua74p7ujvsgz"
        )
    )
    .is_err());

    // Special characters
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA!QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());
    assert!(strkey_to_address(
        host,
        string_to_object(
            host,
            "GA QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ"
        )
    )
    .is_err());

    // Invalid UTF-8 bytes (passed as BytesObject)
    // Lone continuation byte (0x80 is invalid as a start byte)
    let invalid_utf8_lone_continuation: [u8; 56] = [0x80; 56];
    assert!(strkey_to_address(
        host,
        host.add_host_object(ScBytes(invalid_utf8_lone_continuation.try_into().unwrap()))
            .unwrap()
            .to_val(),
    )
    .is_err());
    // Invalid start byte 0xC0 followed by valid continuation
    let mut invalid_utf8_overlong = [b'G'; 56];
    invalid_utf8_overlong[0] = 0xC0;
    invalid_utf8_overlong[1] = 0x80;
    assert!(strkey_to_address(
        host,
        host.add_host_object(ScBytes(invalid_utf8_overlong.try_into().unwrap()))
            .unwrap()
            .to_val(),
    )
    .is_err());
    // Truncated multi-byte sequence (0xE0 expects 2 more bytes)
    let mut invalid_utf8_truncated = [b'A'; 56];
    invalid_utf8_truncated[0] = 0xE0;
    assert!(strkey_to_address(
        host,
        host.add_host_object(ScBytes(invalid_utf8_truncated.try_into().unwrap()))
            .unwrap()
            .to_val(),
    )
    .is_err());
}

#[test]
fn test_account_address_conversions() {
    let host = observe_host!(Host::test_host());
    run_test_account_address_conversions(
        &host,
        |h, addr| h.address_to_strkey(addr).unwrap(),
        |h, strkey| h.strkey_to_address(strkey).unwrap(),
    );
}

#[test]
fn test_contract_address_conversions() {
    let host = observe_host!(Host::test_host());
    run_test_contract_address_conversions(
        &host,
        |h, addr| h.address_to_strkey(addr).unwrap(),
        |h, strkey| h.strkey_to_address(strkey).unwrap(),
    );
}

#[test]
fn invalid_strkey_to_address_conversion() {
    let host = observe_host!(Host::test_host());
    run_test_invalid_strkey_to_address(&host, |h, strkey| h.strkey_to_address(strkey));
    assert!(host
        .strkey_to_address(string_to_object(
            &host,
            "MA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAAAAAAAACJUQ"
        ))
        .is_err());
}

#[test]
fn test_account_address_conversions_with_muxed_address() {
    let host = observe_host!(Host::test_host());
    run_test_account_address_conversions(
        &host,
        |h, addr| h.muxed_address_to_strkey(addr.to_val()).unwrap(),
        |h, strkey| AddressObject::try_from(h.strkey_to_muxed_address(strkey).unwrap()).unwrap(),
    );
}

#[test]
fn test_contract_address_conversions_with_muxed_address() {
    let host = observe_host!(Host::test_host());
    run_test_contract_address_conversions(
        &host,
        |h, addr| h.muxed_address_to_strkey(addr.to_val()).unwrap(),
        |h, strkey| AddressObject::try_from(h.strkey_to_muxed_address(strkey).unwrap()).unwrap(),
    );
}

#[test]
fn test_muxed_address_strkey_conversions() {
    let host = observe_host!(Host::test_host());
    let account_pk = [
        0x3f, 0x0c, 0x34, 0xbf, 0x93, 0xad, 0x0d, 0x99, 0x71, 0xd0, 0x4c, 0xcc, 0x90, 0xf7, 0x05,
        0x51, 0x1c, 0x83, 0x8a, 0xad, 0x97, 0x34, 0xa4, 0xa2, 0xfb, 0x0d, 0x7a, 0x03, 0xfc, 0x7f,
        0xe8, 0x9a,
    ];
    let muxed_id: u64 = 0;
    let muxed_address_obj = host
        .add_host_object(MuxedScAddress(ScAddress::MuxedAccount(
            MuxedEd25519Account {
                id: muxed_id,
                ed25519: Uint256(account_pk),
            },
        )))
        .unwrap();

    let strkey = host
        .muxed_address_to_strkey(muxed_address_obj.to_val())
        .unwrap();
    assert_eq!(
        extract_string(&host, strkey),
        "MA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAAAAAAAACJUQ"
    );

    let converted_val = host.strkey_to_muxed_address(strkey.to_val()).unwrap();
    let converted_muxed_address_object = MuxedAddressObject::try_from(converted_val).unwrap();
    assert!((*host)
        .compare(&muxed_address_obj, &converted_muxed_address_object)
        .unwrap()
        .is_eq());

    // Test with a non-zero mux id
    let muxed_id_2: u64 = 12345678901234567890;
    let muxed_address_obj_2 = host
        .add_host_object(MuxedScAddress(ScAddress::MuxedAccount(
            MuxedEd25519Account {
                id: muxed_id_2,
                ed25519: Uint256(account_pk),
            },
        )))
        .unwrap();

    let strkey_2 = host
        .muxed_address_to_strkey(muxed_address_obj_2.to_val())
        .unwrap();
    let converted_val_2 = host.strkey_to_muxed_address(strkey_2.to_val()).unwrap();
    let converted_muxed_address_object_2 = MuxedAddressObject::try_from(converted_val_2).unwrap();
    assert!((*host)
        .compare(&muxed_address_obj_2, &converted_muxed_address_object_2)
        .unwrap()
        .is_eq());

    // Test conversion from bytes
    let converted_from_bytes_val = host
        .strkey_to_muxed_address(string_to_bytes_object(
            &host,
            "MA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAAAAAAAACJUQ",
        ))
        .unwrap();
    let converted_from_bytes_obj = MuxedAddressObject::try_from(converted_from_bytes_val).unwrap();
    assert!((*host)
        .compare(&muxed_address_obj, &converted_from_bytes_obj)
        .unwrap()
        .is_eq());
}

#[test]
fn invalid_strkey_to_muxed_address_conversion() {
    let host = observe_host!(Host::test_host());
    run_test_invalid_strkey_to_address(&host, |h, strkey| h.strkey_to_muxed_address(strkey));
}

#[test]
fn invalid_muxed_address_to_strkey_conversion() {
    let host = observe_host!(Host::test_host());

    // Test with non-object Val
    assert!(host.muxed_address_to_strkey(Val::VOID.into()).is_err());

    // Test with wrong object type (e.g., VecObject)
    let vec_obj = host.vec_new().unwrap();
    assert!(host.muxed_address_to_strkey(vec_obj.to_val()).is_err());
}
