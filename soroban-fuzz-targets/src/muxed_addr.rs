//! Fuzzes strkey/address conversions using valid host objects.

use crate::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use soroban_env_host::{
    xdr::{AccountId, ContractId, Hash, MuxedEd25519Account, PublicKey, ScAddress, ScVal, Uint256},
    AddressObject, Compare, Env, EnvBase, Host, MuxedAddressObject, TryIntoVal, Val,
};

#[derive(Arbitrary, Clone, Copy, Debug)]
enum AddressKind {
    Account,
    Contract,
    MuxedAccount,
}

#[derive(Arbitrary, Debug)]
enum Target {
    RoundTrip {
        address_kind: AddressKind,
        payload: [u8; 32],
        muxed_id: u64,
        use_bytes: bool,
    },
    Untrusted {
        use_bytes: bool,
        input: Vec<u8>,
    },
}

fn test_untrusted(host: &Host, input: &[u8], use_bytes: bool) -> FuzzResult {
    let strkey: Val = if use_bytes {
        host.bytes_new_from_slice(input).unwrap().into()
    } else {
        host.string_new_from_slice(input).unwrap().into()
    };
    let muxed_result = host.strkey_to_muxed_address(strkey);
    let address_result = host.strkey_to_address(strkey);

    let address_val = match muxed_result {
        Ok(address) => address,
        // `strkey_to_muxed_address` accepts every strkey accepted by
        // `strkey_to_address`, so the latter must not succeed on its own.
        Err(error) => {
            assert!(error.is_recoverable());
            assert!(address_result.unwrap_err().is_recoverable());
            return FuzzResult::Ok;
        }
    };

    let canonical = host.muxed_address_to_strkey(address_val).unwrap();

    // Any accepted input must produce a canonical strkey that is itself
    // accepted and stable under another conversion round trip.
    let reparsed = host.strkey_to_muxed_address(canonical.into()).unwrap();
    let reencoded = host.muxed_address_to_strkey(reparsed).unwrap();
    assert!(host.compare(&reencoded, &canonical).unwrap().is_eq());

    match address_result {
        Ok(address) => {
            let muxed_api_address = AddressObject::try_from(address_val).unwrap();
            assert!(host.compare(&muxed_api_address, &address).unwrap().is_eq());

            let address_strkey = host.address_to_strkey(address).unwrap();
            assert!(host.compare(&address_strkey, &canonical).unwrap().is_eq());
        }
        Err(error) => {
            assert!(error.is_recoverable());
            // M-addresses are the only successful inputs unique to the muxed API.
            MuxedAddressObject::try_from(address_val).unwrap();
        }
    }

    FuzzResult::Ok
}

fn test_round_trip(
    host: &Host,
    address_kind: AddressKind,
    payload: [u8; 32],
    muxed_id: u64,
    use_bytes: bool,
) -> FuzzResult {
    let expected_address = match address_kind {
        AddressKind::Account => {
            ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(payload))))
        }
        AddressKind::Contract => ScAddress::Contract(ContractId(Hash(payload))),
        AddressKind::MuxedAccount => ScAddress::MuxedAccount(MuxedEd25519Account {
            ed25519: Uint256(payload),
            id: muxed_id,
        }),
    };
    let expected_strkey = expected_address.to_string();
    let strkey_val: Val = if use_bytes {
        host.bytes_new_from_slice(expected_strkey.as_bytes())
            .unwrap()
            .into()
    } else {
        host.string_new_from_slice(expected_strkey.as_bytes())
            .unwrap()
            .into()
    };

    let address_val = host.strkey_to_muxed_address(strkey_val).unwrap();
    let roundtrip_strkey = host.muxed_address_to_strkey(address_val).unwrap();
    let roundtrip_string: String = roundtrip_strkey.try_into_val(host).unwrap();
    assert_eq!(roundtrip_string, expected_strkey);

    let decoded_address: ScVal = address_val.try_into_val(host).unwrap();
    assert_eq!(decoded_address, ScVal::Address(expected_address));

    match address_kind {
        AddressKind::Account | AddressKind::Contract => {
            let muxed_api_address = AddressObject::try_from(address_val).unwrap();
            let address = host.strkey_to_address(strkey_val).unwrap();
            let address_strkey = host.address_to_strkey(address).unwrap();
            assert!(host
                .compare(&address_strkey, &roundtrip_strkey)
                .unwrap()
                .is_eq());

            assert!(host.compare(&muxed_api_address, &address).unwrap().is_eq());
        }
        AddressKind::MuxedAccount => {
            MuxedAddressObject::try_from(address_val).unwrap();
        }
    }

    FuzzResult::Ok
}

pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let mut u = Unstructured::new(data);
    let target = match Target::arbitrary(&mut u) {
        Ok(target) => target,
        Err(_) => return FuzzResult::Reject,
    };
    let host = Host::test_host();

    match target {
        Target::RoundTrip {
            address_kind,
            payload,
            muxed_id,
            use_bytes,
        } => test_round_trip(&host, address_kind, payload, muxed_id, use_bytes),
        Target::Untrusted { input, use_bytes } => test_untrusted(&host, &input, use_bytes),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_all_address_types() {
        for kind in [
            AddressKind::Account,
            AddressKind::Contract,
            AddressKind::MuxedAccount,
        ] {
            for use_bytes in [false, true] {
                for muxed_id in [0, u64::MAX] {
                    assert_eq!(
                        test_round_trip(&Host::test_host(), kind, [42; 32], muxed_id, use_bytes),
                        FuzzResult::Ok
                    );
                }
            }
        }
    }

    #[test]
    fn untrusted_valid_strkeys() {
        for strkey in [
            "GA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJVSGZ",
            "CA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUWDA",
            "MA7QYNF7SOWQ3GLR2BGMZEHXAVIRZA4KVWLTJJFC7MGXUA74P7UJUAAAAAAAAAAAACJUQ",
        ] {
            for use_bytes in [false, true] {
                assert_eq!(
                    test_untrusted(&Host::test_host(), strkey.as_bytes(), use_bytes),
                    FuzzResult::Ok
                );
            }
        }
    }

    #[test]
    fn untrusted_invalid_strkeys() {
        let inputs: &[&[u8]] = &[
            b"",
            b"not an address",
            &[0xff; 56],
            &[0xff; 69],
            b"SBU2RRGLXH3E5CQHTD3ODLDF2BWDCYUSSBLLZ5GNW7JXHDIYKXZWHOKR",
        ];
        for input in inputs {
            for use_bytes in [false, true] {
                assert_eq!(
                    test_untrusted(&Host::test_host(), input, use_bytes),
                    FuzzResult::Ok
                );
            }
        }
    }
}
