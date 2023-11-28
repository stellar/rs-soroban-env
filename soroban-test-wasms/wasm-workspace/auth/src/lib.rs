#![no_std]

use soroban_sdk::{
    auth::{ContractContext, InvokerContractAuthEntry, SubContractInvocation},
    contract, contractimpl, contracttype, symbol_short, vec, Address, Env, Error, IntoVal, Vec,
};

#[contract]
struct AuthContract;

#[derive(Clone)]
#[contracttype]
pub struct TreeNode {
    pub contract: Address,
    pub need_auth: Vec<bool>,
    pub children: Vec<TreeNode>,
    pub try_call: bool,
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
            if child.try_call {
                let _ = env.try_invoke_contract::<(), Error>(
                    &child.contract.clone(),
                    &symbol_short!("tree_fn"),
                    (addresses.clone(), child).into_val(&env),
                );
            } else {
                env.invoke_contract::<()>(
                    &child.contract.clone(),
                    &symbol_short!("tree_fn"),
                    (addresses.clone(), child).into_val(&env),
                );
            }
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

    pub fn invoker_auth_fn(env: Env, tree: TreeNode, auth_tree: TreeNode) {
        // Build auth entries from `auth_tree` children - tree root auth works
        // automatically.
        let mut auth_entries = vec![&env];
        for child in auth_tree.children.iter() {
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

    // Just authorize the call tree, but don't call anything.
    pub fn invoker_auth_fn_no_call(env: Env, tree: TreeNode) {
        // Build auth entries from children - tree root auth works
        // automatically.
        let mut auth_entries = vec![&env];
        for child in tree.children.iter() {
            auth_entries.push_back(tree_to_invoker_contract_auth(&env, &child));
        }
        env.authorize_as_current_contract(auth_entries);
    }

    // Function for specifically testing that invoker contract auth is never reused,
    // whether the contract call fails or not.
    // `failing_tree` should be different from `tree` (so that `tree` auth is not
    // suitable for it).
    // Note: the contract could build both trees itself, we only pass them externally
    // for convenience (there are test helpers for building these trees).
    pub fn invoker_auth_no_reuse(env: Env, tree: TreeNode, failing_tree: TreeNode) {
        let curr_address = env.current_contract_address();
        let mut auth_entries = vec![&env];
        for child in tree.children.iter() {
            auth_entries.push_back(tree_to_invoker_contract_auth(&env, &child));
        }
        env.authorize_as_current_contract(auth_entries.clone());
        // Make sure authorization succeeds for the `tree`.
        assert!(env
            .try_invoke_contract::<(), Error>(
                &tree.contract,
                &symbol_short!("tree_fn"),
                (vec![&env, curr_address.clone()], tree.clone()).into_val(&env),
            )
            .is_ok());
        // Now the identical call should fail because auth is consumed.
        assert!(env
            .try_invoke_contract::<(), Error>(
                &tree.contract,
                &symbol_short!("tree_fn"),
                (vec![&env, curr_address.clone()], tree.clone()).into_val(&env),
            )
            .is_err());

        // Authorize the next call again.
        env.authorize_as_current_contract(auth_entries.clone());
        let curr_address = env.current_contract_address();
        // Use the `failing_tree` that requires different auth entries.
        assert!(env
            .try_invoke_contract::<(), Error>(
                &tree.contract,
                &symbol_short!("tree_fn"),
                (vec![&env, curr_address.clone()], failing_tree).into_val(&env),
            )
            .is_err());
        // The 'valid' call should still fail because auth entries are
        // consumed even by the failing invocations.
        assert!(env
            .try_invoke_contract::<(), Error>(
                &tree.contract,
                &symbol_short!("tree_fn"),
                (vec![&env, curr_address.clone()], tree.clone()).into_val(&env),
            )
            .is_err());
    }

    pub fn store(env: Env, addresses: Vec<Address>) {
        env.storage()
            .persistent()
            .set(&symbol_short!("addr"), &addresses);
    }

    pub fn stree_fn(env: Env, addresses: Vec<Address>, tree: TreeNode) {
        let stored_addresses: Vec<Address> = env
            .storage()
            .persistent()
            .get(&symbol_short!("addr"))
            .unwrap();
        for i in 0..addresses.len() {
            if tree.need_auth.get_unchecked(i) {
                stored_addresses
                    .get_unchecked(i)
                    .require_auth_for_args(vec![&env]);
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

    // This function covers a really fringe scenario: a combination of
    // graceful failure handling with `try_call`, an implementation
    // of `__check_auth` that might have its results changed during
    // the contract call, and use of auth trees that are not rooted at
    // the root invocation.
    // This expects `address` to be implemeted via `conditional_account`
    // test contract and `contract_address` is another instance of
    // `AuthContract`.
    pub fn conditional_auth(env: Env, address: Address, contract_address: Address) {
        // No `require_auth` at the top level in order to get multiple auth trees.

        // Call contract function that does `require_auth` and allow it to fail.
        let _ = env.try_invoke_contract::<(), Error>(
            &contract_address,
            &symbol_short!("do_auth"),
            (address.clone(), 123_u32).into_val(&env),
        );
        // Call `allow` on the account contract.
        env.invoke_contract::<()>(&address, &symbol_short!("allow"), ().into_val(&env));
        env.invoke_contract::<()>(
            &contract_address,
            &symbol_short!("do_auth"),
            (address.clone(), 123_u32).into_val(&env),
        );
        // Make one more call that may never succeed
        // because `allow` hasn't been called.
        let res = env.try_invoke_contract::<(), Error>(
            &contract_address,
            &symbol_short!("do_auth"),
            (address.clone(), 123_u32).into_val(&env),
        );
        if !res.is_err() {
            panic!("unexpected not allowed auth succeeded");
        }
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
