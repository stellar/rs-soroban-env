use crate::xdr::{self, ScErrorCode, ScErrorType, ScSymbol, ScVal};
use crate::{Host, HostError};
use soroban_env_common::{Symbol, SymbolSmall, SymbolStr, TryFromVal, TryIntoVal, Val};

#[test]
fn invalid_chars() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let s = "#";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_err());

    Ok(())
}

#[test]
fn overlong() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let s = "123456789012345678901234567890___";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_err());

    Ok(())
}

#[test]
fn max_len() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let s = "123456789012345678901234567890__";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_ok());

    Ok(())
}

#[test]
fn zero_len() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());

    let s = "";
    let s = Symbol::try_from_val(&*host, &s);

    assert!(s.is_ok());

    Ok(())
}

#[test]
fn invalid_symbol_scval_to_val() {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    let string_m = xdr::StringM::try_from(vec![36 /*$*/]).unwrap();
    let sc_symbol = xdr::ScSymbol(string_m);

    let symbol = xdr::ScVal::Symbol(sc_symbol);

    assert!(Val::try_from_val(&*host, &symbol).is_err())
}

#[test]
fn valid_short_symbol_conversions() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let valid_symbol_strings = ["", "_", "5", "D", "y", "AaZz_09", "qwe_ASD_7"];
    for s in valid_symbol_strings {
        let val: Val = SymbolSmall::try_from_bytes(s.as_bytes()).unwrap().into();
        let val_payload = val.get_payload();
        assert_eq!(
            Val::from(SymbolSmall::try_from_str(s).unwrap()).get_payload(),
            val_payload
        );
        assert_eq!(
            Val::from(Symbol::try_from_small_bytes(s.as_bytes()).unwrap()).get_payload(),
            val_payload
        );
        assert_eq!(
            Val::from(Symbol::try_from_val(&*host, &s).unwrap()).get_payload(),
            val_payload
        );
        assert_eq!(
            Val::from(Symbol::try_from_val(&*host, &s.as_bytes()).unwrap()).get_payload(),
            val_payload
        );
        assert_eq!(
            Val::from(Symbol::try_from_val(&*host, &ScSymbol(s.try_into().unwrap())).unwrap())
                .get_payload(),
            val_payload
        );
        assert_eq!(
            Val::from(
                Symbol::try_from_val(&*host, &ScVal::Symbol(ScSymbol(s.try_into().unwrap())))
                    .unwrap()
            )
            .get_payload(),
            val_payload
        );
        assert_eq!(
            host.to_host_val(&ScVal::Symbol(ScSymbol(s.try_into().unwrap())))
                .unwrap()
                .get_payload(),
            val_payload
        );
    }
}

#[test]
fn valid_long_symbol_conversions() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let symbol_to_str = |s: Symbol| {
        let symbol_str: SymbolStr = s.try_into_val(&*host).unwrap();
        symbol_str.to_string()
    };
    let valid_symbol_strings = [
        "_abc_DEF_567_",
        "abcdefghijklmnopqrstuvwxyz123456",
        "098765ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "________________________________",
    ];

    for s in valid_symbol_strings {
        assert_eq!(symbol_to_str(Symbol::try_from_val(&*host, &s).unwrap()), s);
        assert_eq!(
            symbol_to_str(Symbol::try_from_val(&*host, &s.as_bytes()).unwrap()),
            s
        );
        assert_eq!(symbol_to_str(Symbol::try_from_val(&*host, &s).unwrap()), s);
        assert_eq!(
            symbol_to_str(Symbol::try_from_val(&*host, &s.as_bytes()).unwrap()),
            s
        );
        assert_eq!(
            symbol_to_str(Symbol::try_from_val(&*host, &ScSymbol(s.try_into().unwrap())).unwrap()),
            s
        );
        assert_eq!(
            symbol_to_str(
                Symbol::try_from_val(&*host, &ScVal::Symbol(ScSymbol(s.try_into().unwrap())))
                    .unwrap()
            ),
            s
        );
        assert_eq!(
            symbol_to_str(
                Symbol::try_from_val(
                    &*host,
                    &host
                        .to_host_val(&ScVal::Symbol(ScSymbol(s.try_into().unwrap())))
                        .unwrap()
                )
                .unwrap()
            ),
            s
        );
    }
}

#[test]
fn invalid_symbol_conversions() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let symbol_byte_strs: &[&[u8]] = &[
        b"12345678\xF4",
        b"\0",
        b"12345678\0",
        b"abc\xC2def",
        b"\xFFABC",
        b"_!",
        b"a b",
        b"abcCD12345678\xF4",
        b"aaaaaaaaaaaaaa\0",
        b"abcdfasdfdasf\xC2def",
        b"\xFFABCfsdfasdgfsadgsdf",
        b"35235325235235235131_!",
        b"asdgfASDFSDFSDGFSDGFa b",
    ];
    let invalid_input_err = |e: HostError| {
        assert!(e.error.is_type(ScErrorType::Value));
        assert!(e.error.is_code(ScErrorCode::InvalidInput));
    };
    for s in symbol_byte_strs {
        // Some of the test strings aren't valid UTF-8, but we should only interpret
        // them as bytes.
        let s_str = unsafe { core::str::from_utf8_unchecked(s) };

        assert!(SymbolSmall::try_from_bytes(s).is_err());
        assert!(SymbolSmall::try_from_str(s_str).is_err());
        assert!(Symbol::try_from_small_bytes(s).is_err());
        invalid_input_err(Symbol::try_from_val(&*host, s).err().unwrap().into());
        invalid_input_err(Symbol::try_from_val(&*host, &s_str).err().unwrap().into());

        invalid_input_err(
            Symbol::try_from_val(&*host, &ScSymbol(s_str.try_into().unwrap()))
                .err()
                .unwrap()
                .into(),
        );
        invalid_input_err(
            Symbol::try_from_val(&*host, &ScVal::Symbol(ScSymbol(s_str.try_into().unwrap())))
                .err()
                .unwrap()
                .into(),
        );
        invalid_input_err(
            host.to_host_val(&ScVal::Symbol(ScSymbol(s_str.try_into().unwrap())))
                .err()
                .unwrap(),
        );
    }
}
