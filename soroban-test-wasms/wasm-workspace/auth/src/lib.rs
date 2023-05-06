#![no_std]
use soroban_env_common::{AddressObject, Env as _, TryFromVal};
use soroban_sdk::{contractimpl, contracttype, vec, Address, BytesN, Env, IntoVal, Symbol, Vec};

struct AuthContract;

#[derive(Clone)]
#[contracttype]
pub struct TreeNode {
    pub id: BytesN<32>,
    pub need_auth: Vec<bool>,
    pub children: Vec<TreeNode>,
}

#[contractimpl]
impl AuthContract {
    // This function authorizes all the addresses based on the `tree` node and
    // then recursively calls the provided child contracts that also must be
    // an instance of `AuthContract`.
    // This is used for testing auth with arbitrary trees and arbitrary address
    // counts.
    pub fn tree_fn(env: Env, addresses: Vec<Address>, tree: TreeNode) {
        for i in 0..addresses.len() {
            if tree.need_auth.get_unchecked(i).unwrap() {
                addresses
                    .get_unchecked(i)
                    .unwrap()
                    .require_auth_for_args(vec![&env]);
            }
        }
        for child in tree.children.iter() {
            let child = child.unwrap();
            let child_ao: AddressObject = env.contract_id_to_address(child.id.to_object()).unwrap();
            env.invoke_contract::<()>(
                &Address::try_from_val(&env, &child_ao).unwrap(),
                &Symbol::short("tree_fn"),
                (addresses.clone(), child).into_val(&env),
            );
        }
    }

    pub fn order_fn(env: Env, addr: Address, child_id: BytesN<32>) {
        let child_ao: AddressObject = env.contract_id_to_address(child_id.to_object()).unwrap();
        env.invoke_contract::<()>(
            &Address::try_from_val(&env, &child_ao).unwrap(),
            &Symbol::short("do_auth"),
            (&addr, 10_u32).into_val(&env),
        );
        addr.require_auth();
    }

    pub fn do_auth(_env: Env, addr: Address, _val: u32) {
        addr.require_auth();
    }
}
