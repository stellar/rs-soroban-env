#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, Bytes, BytesN, Env, IntoVal};

// This is the test contract for e2e testing of all the lifecycle
// operations (upload Wasm, create contract, update contract Wasm).
#[contract]
pub struct TestDeployerContract;

#[contractimpl]
impl TestDeployerContract {
    // This relies on specific Wasms being provided:
    // `init_wasm` is expected to be `update` test Wasm.
    // `updated_wasm` is expected to be `add_i32` test Wasm.
    pub fn deploy(env: Env, init_wasm: Bytes, updated_wasm: Bytes, salt: BytesN<32>) {
        let init_hash = env.deployer().upload_contract_wasm(init_wasm);
        let contract_id = env.deployer().with_current_contract(salt).deploy(init_hash);
        let updated_hash = env.deployer().upload_contract_wasm(updated_wasm);
        let res: i32 = env.invoke_contract::<i32>(
            &contract_id,
            &symbol_short!("update"),
            (updated_hash, false).into_val(&env),
        );
        assert_eq!(res, 123);

        // Call the updated contract several times to cover subtle interaction between the module
        // cache simulation and uploaded Wasm (specifically that we charge for parsing the freshly
        // uploaded Wasms as we can't cache them).
        let res2: i32 = env.invoke_contract::<i32>(
            &contract_id,
            &symbol_short!("add"),
            (5_i32, 6_i32).into_val(&env),
        );
        assert_eq!(res2, 11);
        let res2: i32 = env.invoke_contract::<i32>(
            &contract_id,
            &symbol_short!("add"),
            (6_i32, 6_i32).into_val(&env),
        );
        assert_eq!(res2, 12);
        let res2: i32 = env.invoke_contract::<i32>(
            &contract_id,
            &symbol_short!("add"),
            (6_i32, 7_i32).into_val(&env),
        );
        assert_eq!(res2, 13);
    }
}
