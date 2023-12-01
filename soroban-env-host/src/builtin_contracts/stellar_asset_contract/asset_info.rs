use crate::{
    builtin_contracts::stellar_asset_contract::{
        public_types::AssetInfo, storage_types::InstanceDataKey,
    },
    host::Host,
    xdr::{Asset, ScErrorCode, ScErrorType},
    Env, HostError, StorageType, TryIntoVal, U32Val,
};

pub(crate) fn write_asset_info(e: &Host, asset_info: AssetInfo) -> Result<(), HostError> {
    let key = InstanceDataKey::AssetInfo;
    e.put_contract_data(
        key.try_into_val(e)?,
        asset_info.try_into_val(e)?,
        StorageType::Instance,
    )?;
    Ok(())
}

pub(crate) fn read_asset_info(e: &Host) -> Result<AssetInfo, HostError> {
    let key = InstanceDataKey::AssetInfo;
    let rv = e.get_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    rv.try_into_val(e)
}

pub(crate) fn read_asset(e: &Host) -> Result<Asset, HostError> {
    read_asset_info(e)?.try_into_val(e)
}

pub(crate) fn has_asset_info(e: &Host) -> Result<bool, HostError> {
    let key = InstanceDataKey::AssetInfo;
    let rv = e.has_contract_data(key.try_into_val(e)?, StorageType::Instance)?;
    Ok(rv.try_into()?)
}

pub(crate) fn validate_asset(host: &Host, asset: &Asset) -> Result<(), HostError> {
    // This reimplements the Stellar core `isAssetValid` function:
    // https://github.com/stellar/stellar-core/blob/372b4270166e7aad4b741500335b5394965b14a0/src/util/types.cpp#L147
    let validate_asset_code = |code: &[u8]| {
        let mut had_zero = false;
        let mut non_zero_chars = 0;
        for c in code.iter().copied() {
            if c == 0 {
                had_zero = true;
            } else if had_zero {
                return Err(host.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "asset code can't have 0 character in the middle",
                    &[],
                ));
            } else {
                if c > 0x7f || !c.is_ascii_alphanumeric() {
                    return Err(host.err(
                        ScErrorType::Value,
                        ScErrorCode::InvalidInput,
                        "asset code has invalid character",
                        &[U32Val::from(c as u32).into()],
                    ));
                }
                non_zero_chars += 1;
            }
        }
        if non_zero_chars == 0 {
            return Err(host.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "asset code doesn't have non-0 characters",
                &[],
            ));
        }
        if non_zero_chars <= 4 && code.len() > 4 {
            return Err(host.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "asset codes with 4 characters or less should have `CreditAlphanum4` type",
                &[],
            ));
        }
        Ok(())
    };

    match asset {
        Asset::Native => Ok(()),
        Asset::CreditAlphanum4(alpha_num4) => validate_asset_code(&alpha_num4.asset_code.0),
        Asset::CreditAlphanum12(alpha_num12) => validate_asset_code(&alpha_num12.asset_code.0),
    }
}
