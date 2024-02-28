use super::{asset_info::read_asset_info, public_types::AssetInfo};
use crate::{
    builtin_contracts::base_types::{BytesN, String},
    host::Host,
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, HostError, StorageType, SymbolSmall, TryFromVal, TryIntoVal,
};
use soroban_builtin_sdk_macros::contracttype;
use std::fmt::Write;
use stellar_strkey::ed25519;

const METADATA_KEY: &str = "METADATA";

#[derive(Clone)]
#[contracttype]
pub(crate) struct StellarAssetContractMetadata {
    pub decimal: u32,
    pub name: String,
    pub symbol: String,
}

pub const DECIMAL: u32 = 7;

// This does a specific and fairly unique escaping transformation as defined in
// TxRep / SEP-0011.
//
// Note: this seemed appropriate when it was initially added but is really
// overkill, since all asset codes admitted to the system are validated to be
// ASCII alphanumeric, with only trailing zeroes and only unambiguous
// zero-padded lengths, over in [`validate_asset`]; but we reproduce the
// escaping logic in SEP-0011 here anyway to add some defense in depth, in case
// validation was somehow missed.
fn render_sep0011_asset_code(buf: &[u8], out: &mut std::string::String) -> Result<(), HostError> {
    if buf.len() != 4 && buf.len() != 12 {
        return Err((ScErrorType::Value, ScErrorCode::InvalidInput).into());
    }
    for (i, x) in buf.iter().enumerate() {
        match *x {
            // When dealing with a 4-byte asset code we stop at the first NUL.
            0 if buf.len() == 4 => break,
            // When dealing with a 12-byte asset code we continue escaping NULs
            // as \x00 until past the 5th byte, so that the result is
            // unambiguously different than a 4-byte code.
            0 if buf.len() == 12 && i > 4 => break,
            b':' | b'\\' | 0..=0x20 | 0x7f..=0xff => write!(out, r"\x{:02x}", x)
                .map_err(|_| HostError::from((ScErrorType::Value, ScErrorCode::InvalidInput)))?,
            _ => out.push(*x as char),
        }
    }
    Ok(())
}

#[test]
fn test_render_sep0011_asset_code() {
    fn check_pair(a: &[u8], b: &str) {
        let mut out = std::string::String::new();
        render_sep0011_asset_code(a, &mut out).unwrap();
        assert_eq!(out, b);
    }
    // 4 byte codes
    check_pair(&[0, 0, 0, 0], r"");
    check_pair(&[0, b'X', b'L', b'M'], r"");
    check_pair(&[b'X', 0, 0, 0], r"X");
    check_pair(&[b'X', b'L', 0, 0], r"XL");
    check_pair(&[b'X', b'L', b'M', 0], r"XLM");
    check_pair(&[b'y', b'X', b'L', b'M'], r"yXLM");
    check_pair(&[1, b'X', b'L', b'M'], r"\x01XLM");
    check_pair(&[b'X', 1, b'L', b'M'], r"X\x01LM");
    check_pair(&[b'X', b':', b'L', b'M'], r"X\x3aLM");
    check_pair(&[b'X', b'\\', b'L', b'M'], r"X\x5cLM");
    check_pair(&[b'1', b'!', b'/', b'0'], r"1!/0");

    // 12 byte codes
    check_pair(
        &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        r"\x00\x00\x00\x00\x00",
    );
    check_pair(
        &[0, b'X', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"\x00XLM\x00",
    );
    check_pair(
        &[b'X', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        r"X\x00\x00\x00\x00",
    );
    check_pair(
        &[b'X', b'L', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        r"XL\x00\x00\x00",
    );
    check_pair(
        &[b'X', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0, 0],
        r"XLM\x00\x00",
    );
    check_pair(
        &[b'y', b'X', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"yXLM\x00",
    );
    check_pair(
        &[1, b'X', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"\x01XLM\x00",
    );
    check_pair(
        &[b'X', 1, b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"X\x01LM\x00",
    );
    check_pair(
        &[b'X', b':', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"X\x3aLM\x00",
    );
    check_pair(
        &[b'X', b'\\', b'L', b'M', 0, 0, 0, 0, 0, 0, 0, 0],
        r"X\x5cLM\x00",
    );
    check_pair(
        &[b'X', b'L', b'M', b'L', b'X', 0, 0, 0, 0, 0, 0, 0],
        r"XLMLX",
    );
    check_pair(
        &[b'X', b'L', b'M', b'L', b'X', b'A', 0, 0, 0, 0, 0, 0],
        r"XLMLXA",
    );
    check_pair(
        &[
            b'1', b'!', b'/', b'0', b'a', b'<', b'N', b'!', b'[', b'K', b'z', b'^',
        ],
        r"1!/0a<N![Kz^",
    );
}

fn render_sep0011_asset<const N: usize>(
    e: &Host,
    symbol: String,
    issuer: BytesN<32>,
) -> Result<(String, String), HostError> {
    let symbuf = symbol.to_array::<N>()?;
    let strkey_len = 56;

    // Biggest resulting string has each byte escaped to 4 bytes.
    let capacity = symbuf
        .len()
        .saturating_mul(4)
        .saturating_add(1)
        .saturating_add(strkey_len);

    // We also have to charge for strkey_len again since PublicKey::to_string does
    // a std::string::String allocation of its own.
    let charge = capacity.saturating_add(strkey_len);
    e.charge_budget(crate::xdr::ContractCostType::MemAlloc, Some(charge as u64))?;

    let mut s: std::string::String = std::string::String::with_capacity(capacity);
    render_sep0011_asset_code(&symbuf, &mut s)?;

    // Use the sep-11 (trimmed and/or escaped) asset code we just rendered for the metadata symbol.
    let symbol = String::try_from_val(e, &e.string_new_from_slice(s.as_bytes())?)?;

    // Then follow it with a colon and the issuer's strkey for the metadata name.
    s.push(':');
    s.push_str(&ed25519::PublicKey(issuer.to_array()?).to_string());
    let name = String::try_from_val(e, &e.string_new_from_slice(s.as_bytes())?)?;
    Ok((name, symbol))
}

pub(crate) fn set_metadata(e: &Host) -> Result<(), HostError> {
    let name_and_symbol: (String, String) = match read_asset_info(e)? {
        AssetInfo::Native => {
            let n = String::try_from_val(e, &e.string_new_from_slice(b"native")?)?;
            (n.clone(), n)
        }
        AssetInfo::AlphaNum4(asset) => {
            render_sep0011_asset::<4>(e, asset.asset_code, asset.issuer)?
        }
        AssetInfo::AlphaNum12(asset) => {
            render_sep0011_asset::<12>(e, asset.asset_code, asset.issuer)?
        }
    };

    let metadata = StellarAssetContractMetadata {
        decimal: DECIMAL,
        name: name_and_symbol.0,
        symbol: name_and_symbol.1,
    };

    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    e.put_contract_data(
        key.try_into_val(e)?,
        metadata.try_into_val(e)?,
        StorageType::Instance,
    )?;
    Ok(())
}

pub(crate) fn read_name(e: &Host) -> Result<String, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: StellarAssetContractMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::Instance)?
        .try_into_val(e)?;
    Ok(metadata.name)
}

pub(crate) fn read_symbol(e: &Host) -> Result<String, HostError> {
    let key = SymbolSmall::try_from_str(METADATA_KEY)?;
    let metadata: StellarAssetContractMetadata = e
        .get_contract_data(key.try_into_val(e)?, StorageType::Instance)?
        .try_into_val(e)?;
    Ok(metadata.symbol)
}
