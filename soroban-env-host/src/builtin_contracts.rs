pub(crate) mod base_types;
pub(crate) mod common_types;
pub(crate) mod contract_error;
pub(crate) mod invoker_contract_auth;
pub(crate) mod stellar_asset_contract;
pub(crate) mod storage_utils;

use crate::host::{Host, HostError};
use soroban_env_common::{Symbol, Val};

pub(crate) trait BuiltinContract {
    fn call(&self, func: &Symbol, host: &Host, args: &[Val]) -> Result<Val, HostError>;
}

pub(crate) use stellar_asset_contract::StellarAssetContract;

pub(crate) mod account_contract;

#[cfg(any(test, feature = "testutils"))]
pub(crate) mod testutils;
