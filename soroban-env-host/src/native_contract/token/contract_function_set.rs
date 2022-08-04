use crate::host::Host;
use crate::native_contract::base_types::{BigInt, Bytes};
use crate::native_contract::token::contract::*;
use crate::native_contract::token::public_types::{Authorization, Identifier, KeyedAuthorization};
use crate::native_contract::NativeContract;
use soroban_env_common::{RawVal, Symbol};

pub struct Token;

impl NativeContract for Token {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Result<RawVal, ()> {
        const INITIALIZE_DISCRIMINANT: u64 = Symbol::from_str("initialize").to_raw().get_payload();
        const NONCE_DISCRIMINANT: u64 = Symbol::from_str("nonce").to_raw().get_payload();
        const ALLOWANCE_DISCRIMINANT: u64 = Symbol::from_str("allowance").to_raw().get_payload();
        const APPROVE_DISCRIMINANT: u64 = Symbol::from_str("approve").to_raw().get_payload();
        const BALANCE_DISCRIMINANT: u64 = Symbol::from_str("balance").to_raw().get_payload();
        const IS_FROZEN_DISCRIMINANT: u64 = Symbol::from_str("is_frozen").to_raw().get_payload();
        const XFER_DISCRIMINANT: u64 = Symbol::from_str("xfer").to_raw().get_payload();
        const XFER_FROM_DISCRIMINANT: u64 = Symbol::from_str("xfer_from").to_raw().get_payload();
        const BURN_DISCRIMINANT: u64 = Symbol::from_str("burn").to_raw().get_payload();
        const FREEZE_DISCRIMINANT: u64 = Symbol::from_str("freeze").to_raw().get_payload();
        const MINT_DISCRIMINANT: u64 = Symbol::from_str("mint").to_raw().get_payload();
        const SET_ADMIN_DISCRIMINANT: u64 = Symbol::from_str("set_admin").to_raw().get_payload();
        const UNFREEZE_DISCRIMINANT: u64 = Symbol::from_str("unfreeze").to_raw().get_payload();
        const DECIMALS_DISCRIMINANT: u64 = Symbol::from_str("decimals").to_raw().get_payload();
        const NAME_DISCRIMINANT: u64 = Symbol::from_str("name").to_raw().get_payload();
        const SYMBOL_DISCRIMINANT: u64 = Symbol::from_str("symbol").to_raw().get_payload();
        match func.to_raw().get_payload() {
            INITIALIZE_DISCRIMINANT => {
                (args.len() == 3).then_some(()).ok_or(())?;
                let admin: Identifier = args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let decimal: u32 = args.get(1).cloned().ok_or(())?.try_into().map_err(|_| ())?;
                let name: Bytes = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                let symbol: Bytes = args.get(3).cloned().ok_or(())?.in_env(host).try_into()?;
                initialize(host, admin, decimal, name, symbol)
            }
            NONCE_DISCRIMINANT => {
                (args.len() == 1).then_some(()).ok_or(())?;
                let id: Identifier = args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                nonce(host, id)
            }
            ALLOWANCE_DISCRIMINANT => {
                (args.len() == 2).then_some(()).ok_or(())?;
                let from: Identifier = args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let spender: Identifier =
                    args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                allowance(host, from, spender)
            }
            APPROVE_DISCRIMINANT => {
                (args.len() == 3).then_some(()).ok_or(())?;
                let from: KeyedAuthorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let spender: Identifier =
                    args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                let amount: BigInt = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                approve(host, from, spender, amount)
            }
            BALANCE_DISCRIMINANT => {
                (args.len() == 1).then_some(()).ok_or(())?;
                let id: Identifier = args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                balance(host, id)
            }
            IS_FROZEN_DISCRIMINANT => {
                (args.len() == 1).then_some(()).ok_or(())?;
                let id: Identifier = args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                is_frozen(host, id)
            }
            XFER_DISCRIMINANT => {
                (args.len() == 3).then_some(()).ok_or(())?;
                let from: KeyedAuthorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let to: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                let amount: BigInt = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                xfer(host, from, to, amount)
            }
            XFER_FROM_DISCRIMINANT => {
                (args.len() == 4).then_some(()).ok_or(())?;
                let spender: KeyedAuthorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let from: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                let to: Identifier = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                let amount: BigInt = args.get(3).cloned().ok_or(())?.in_env(host).try_into()?;
                xfer_from(host, spender, from, to, amount)
            }
            BURN_DISCRIMINANT => {
                (args.len() == 3).then_some(()).ok_or(())?;
                let admin: Authorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let from: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                let amount: BigInt = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                burn(host, admin, from, amount)
            }
            FREEZE_DISCRIMINANT => {
                (args.len() == 2).then_some(()).ok_or(())?;
                let admin: Authorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let id: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                freeze(host, admin, id)
            }
            MINT_DISCRIMINANT => {
                (args.len() == 3).then_some(()).ok_or(())?;
                let admin: Authorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let to: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                let amount: BigInt = args.get(2).cloned().ok_or(())?.in_env(host).try_into()?;
                mint(host, admin, to, amount)
            }
            SET_ADMIN_DISCRIMINANT => {
                (args.len() == 2).then_some(()).ok_or(())?;
                let admin: Authorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let new_admin: Identifier =
                    args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                set_admin(host, admin, new_admin)
            }
            UNFREEZE_DISCRIMINANT => {
                (args.len() == 2).then_some(()).ok_or(())?;
                let admin: Authorization =
                    args.get(0).cloned().ok_or(())?.in_env(host).try_into()?;
                let id: Identifier = args.get(1).cloned().ok_or(())?.in_env(host).try_into()?;
                unfreeze(host, admin, id)
            }
            DECIMALS_DISCRIMINANT => {
                (args.len() == 0).then_some(()).ok_or(())?;
                decimals(host)
            }
            NAME_DISCRIMINANT => {
                (args.len() == 0).then_some(()).ok_or(())?;
                name(host)
            }
            SYMBOL_DISCRIMINANT => {
                (args.len() == 0).then_some(()).ok_or(())?;
                symbol(host)
            }
            _ => Err(()),
        }
    }
}
