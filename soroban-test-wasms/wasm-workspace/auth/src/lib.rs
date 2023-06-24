#![no_std]
use soroban_sdk::{
    auth::{ContractContext, InvokerContractAuthEntry, SubContractInvocation},
    contract, contractimpl, contracttype, vec, Address, Env, IntoVal, symbol_short, Vec,
};

#[contract]
struct AuthContract;

#[derive(Clone)]
#[contracttype]
pub struct TreeNode {
    pub contract: Address,
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
            if tree.need_auth.get_unchecked(i) {
                addresses.get_unchecked(i).require_auth_for_args(vec![&env]);
            }
        }
        for child in tree.children.iter() {
            env.invoke_contract::<()>(
                &child.contract.clone(),
                &symbol_short!("tree_fn"),
                (addresses.clone(), child).into_val(&env),
            );
        }
    }

    pub fn order_fn(env: Env, addr: Address, child_id: Address) {
        env.invoke_contract::<()>(
            &child_id,
            &symbol_short!("do_auth"),
            (&addr, 10_u32).into_val(&env),
        );
        addr.require_auth();
    }

    pub fn do_auth(_env: Env, addr: Address, _val: u32) {
        addr.require_auth();
    }

    pub fn invoker_auth_fn(env: Env, tree: TreeNode) {
        // Build auth entries from children - tree root auth works
        // automatically.
        let mut auth_entries = vec![&env];
        for child in tree.children.iter() {
            auth_entries.push_back(tree_to_invoker_contract_auth(&env, &child));
        }
        env.authorize_as_current_contract(auth_entries);
        let curr_address = env.current_contract_address();
        env.invoke_contract::<()>(
            &tree.contract,
            &symbol_short!("tree_fn"),
            (vec![&env, curr_address], tree.clone()).into_val(&env),
        );
    }
}

// Converts the whole provided tree to invoker auth entries, ignoring
// `need_auth` (for covering non-matching tree scenarios).
fn tree_to_invoker_contract_auth(env: &Env, tree: &TreeNode) -> InvokerContractAuthEntry {
    let mut sub_invocations = vec![env];
    for c in tree.children.iter() {
        sub_invocations.push_back(tree_to_invoker_contract_auth(env, &c));
    }
    InvokerContractAuthEntry::Contract(SubContractInvocation {
        context: ContractContext {
            contract: tree.contract.clone(),
            fn_name: symbol_short!("tree_fn"),
            args: vec![&env],
        },
        sub_invocations,
    })
}
