use std::cmp::Ordering;

use crate::host::metered_clone::MeteredClone;
use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Bytes, BytesN, Vec};
use crate::native_contract::token::admin::{check_admin, write_administrator};
use crate::native_contract::token::allowance::{read_allowance, spend_allowance, write_allowance};
use crate::native_contract::token::balance::{
    read_balance, read_state, receive_balance, spend_balance, transfer_classic_balance, write_state,
};
use crate::native_contract::token::cryptography::check_auth;
use crate::native_contract::token::error::Error;
use crate::native_contract::token::metadata::{
    has_metadata, read_decimal, read_name, read_symbol, write_metadata,
};
use crate::native_contract::token::nonce::read_nonce;
use crate::native_contract::token::public_types::{Identifier, Metadata, Signature, TokenMetadata};

use soroban_env_common::xdr::Asset;
use soroban_env_common::{CheckedEnv, EnvBase, Symbol, TryFromVal, TryIntoVal};
use soroban_native_sdk_macros::contractimpl;

use super::public_types::{AlphaNum12Metadata, AlphaNum4Metadata};

pub trait TokenTrait {
    /// init_asset can create a contract for a wrapped classic asset
    /// (Native, AlphaNum4, or AlphaNum12). It will fail if the contractID
    /// of this contract does not match the expected contractID for this asset
    /// returned by Host::get_contract_id_from_asset. This function should only be
    /// called by the create_token_from_asset host function for this reason.
    ///
    /// No admin will be set for the Native token, so any function that checks the admin
    /// (burn, freeze, unfreeze, mint, set_admin) will always fail
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), Error>;

    /// init creates a token contract that does not wrap an asset on the classic side.
    fn init(e: &Host, admin: Identifier, metadata: TokenMetadata) -> Result<(), Error>;

    fn nonce(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error>;

    fn approve(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn balance(e: &Host, id: Identifier) -> Result<BigInt, Error>;

    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, Error>;

    fn xfer(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: BigInt,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn freeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error>;

    fn unfreeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error>;

    fn mint(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn burn(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        from: Identifier,
        amount: BigInt,
    ) -> Result<(), Error>;

    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        new_admin: Identifier,
    ) -> Result<(), Error>;

    fn decimals(e: &Host) -> Result<u32, Error>;

    fn name(e: &Host) -> Result<Bytes, Error>;

    fn symbol(e: &Host) -> Result<Bytes, Error>;

    fn import(e: &Host, id: Signature, nonce: BigInt, amount: i64) -> Result<(), Error>;

    fn export(e: &Host, id: Signature, nonce: BigInt, amount: i64) -> Result<(), Error>;
}

pub struct Token;

#[contractimpl]
// Metering: *mostly* covered by components.
impl TokenTrait for Token {
    fn init_asset(e: &Host, asset_bytes: Bytes) -> Result<(), Error> {
        if has_metadata(&e)? {
            return Err(Error::ContractError);
        }

        let asset: Asset = e.metered_from_xdr_obj(asset_bytes.into())?;

        let curr_contract_id = BytesN::<32>::try_from_val(e, e.get_current_contract()?)?;
        let expected_contract_id =
            BytesN::<32>::try_from_val(e, e.get_contract_id_from_asset(asset.clone())?)?;
        if curr_contract_id != expected_contract_id {
            return Err(Error::ContractError);
        }
        match asset {
            Asset::Native => {
                write_metadata(&e, Metadata::Native)?;
                //No admin for the Native token
            }
            Asset::CreditAlphanum4(asset4) => {
                write_administrator(
                    &e,
                    Identifier::Account(asset4.issuer.metered_clone(e.get_budget_ref())?),
                )?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum4(AlphaNum4Metadata {
                        asset_code: BytesN::<4>::try_from_val(
                            e,
                            e.bytes_new_from_slice(&asset4.asset_code.0)?,
                        )?,
                        issuer: asset4.issuer,
                    }),
                )?;
            }
            Asset::CreditAlphanum12(asset12) => {
                write_administrator(
                    &e,
                    Identifier::Account(asset12.issuer.metered_clone(e.get_budget_ref())?),
                )?;
                write_metadata(
                    &e,
                    Metadata::AlphaNum12(AlphaNum12Metadata {
                        asset_code: BytesN::<12>::try_from_val(
                            e,
                            e.bytes_new_from_slice(&asset12.asset_code.0)?,
                        )?,
                        issuer: asset12.issuer,
                    }),
                )?;
            }
        }
        Ok(())
    }

    fn init(e: &Host, admin: Identifier, metadata: TokenMetadata) -> Result<(), Error> {
        if has_metadata(&e)? {
            return Err(Error::ContractError);
        }

        write_administrator(&e, admin)?;
        write_metadata(&e, Metadata::Token(metadata))?;
        Ok(())
    }

    fn nonce(e: &Host, id: Identifier) -> Result<BigInt, Error> {
        read_nonce(e, id)
    }

    fn allowance(e: &Host, from: Identifier, spender: Identifier) -> Result<BigInt, Error> {
        read_allowance(&e, from, spender)
    }

    // Metering: covered by components
    fn approve(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        if amount.compare(&BigInt::from_u64(e, 0)?)? == Ordering::Less {
            return Err(Error::ContractError);
        }
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(spender.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("approve"), args)?;
        write_allowance(&e, from_id, spender, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn balance(e: &Host, id: Identifier) -> Result<BigInt, Error> {
        read_balance(e, id)
    }

    // Metering: covered by components
    fn is_frozen(e: &Host, id: Identifier) -> Result<bool, Error> {
        read_state(&e, id)
    }

    // Metering: covered by components
    fn xfer(
        e: &Host,
        from: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        if amount.compare(&BigInt::from_u64(e, 0)?)? == Ordering::Less {
            return Err(Error::ContractError);
        }
        let from_id = from.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(from.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, from, nonce, Symbol::from_str("xfer"), args)?;
        spend_balance(&e, from_id, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn xfer_from(
        e: &Host,
        spender: Signature,
        nonce: BigInt,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        if amount.compare(&BigInt::from_u64(e, 0)?)? == Ordering::Less {
            return Err(Error::ContractError);
        }
        let spender_id = spender.get_identifier(&e)?;
        let mut args = Vec::new(e)?;
        args.push(spender.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, spender, nonce, Symbol::from_str("xfer_from"), args)?;
        spend_allowance(&e, from.clone(), spender_id, amount.clone())?;
        spend_balance(&e, from, amount.clone())?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn burn(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        from: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        if amount.compare(&BigInt::from_u64(e, 0)?)? == Ordering::Less {
            return Err(Error::ContractError);
        }
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(from.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("burn"), args)?;
        spend_balance(&e, from, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn freeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("freeze"), args)?;
        write_state(&e, id, true)?;
        Ok(())
    }

    // Metering: covered by components
    fn mint(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
    ) -> Result<(), Error> {
        if amount.compare(&BigInt::from_u64(e, 0)?)? == Ordering::Less {
            return Err(Error::ContractError);
        }
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(to.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("mint"), args)?;
        receive_balance(&e, to, amount)?;
        Ok(())
    }

    // Metering: covered by components
    fn set_admin(
        e: &Host,
        admin: Signature,
        nonce: BigInt,
        new_admin: Identifier,
    ) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(new_admin.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("set_admin"), args)?;
        write_administrator(&e, new_admin)?;
        Ok(())
    }

    // Metering: covered by components
    fn unfreeze(e: &Host, admin: Signature, nonce: BigInt, id: Identifier) -> Result<(), Error> {
        check_admin(&e, &admin)?;
        let mut args = Vec::new(e)?;
        args.push(admin.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(id.clone())?;
        check_auth(&e, admin, nonce, Symbol::from_str("unfreeze"), args)?;
        write_state(&e, id, false)?;
        Ok(())
    }

    fn decimals(e: &Host) -> Result<u32, Error> {
        read_decimal(&e)
    }

    fn name(e: &Host) -> Result<Bytes, Error> {
        read_name(&e)
    }

    fn symbol(e: &Host) -> Result<Bytes, Error> {
        read_symbol(&e)
    }

    // Metering: covered by components
    fn import(e: &Host, id: Signature, nonce: BigInt, amount: i64) -> Result<(), Error> {
        if amount < 0 {
            return Err(Error::ContractError);
        }

        let account_id = id.get_account_id(e)?;

        let mut args = Vec::new(e)?;
        args.push(id.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, id, nonce, Symbol::from_str("import"), args)?;

        transfer_classic_balance(e, account_id.metered_clone(e.get_budget_ref())?, -amount)?;
        receive_balance(
            &e,
            Identifier::Account(account_id),
            BigInt::from_u64(&e, amount.try_into().map_err(|_| Error::ContractError)?)?,
        )?;
        Ok(())
    }

    // Metering: covered by components
    fn export(e: &Host, id: Signature, nonce: BigInt, amount: i64) -> Result<(), Error> {
        if amount < 0 {
            return Err(Error::ContractError);
        }

        let account_id = id.get_account_id(e)?;

        let mut args = Vec::new(e)?;
        args.push(id.get_identifier(&e)?)?;
        args.push(nonce.clone())?;
        args.push(amount.clone())?;
        check_auth(&e, id, nonce, Symbol::from_str("export"), args)?;

        transfer_classic_balance(e, account_id.metered_clone(e.get_budget_ref())?, amount)?;
        spend_balance(
            &e,
            Identifier::Account(account_id),
            BigInt::from_u64(&e, amount.try_into().map_err(|_| Error::ContractError)?)?,
        )?;
        Ok(())
    }
}
