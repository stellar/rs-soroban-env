#![no_std]

use soroban_sdk::{
    auth::{ContractContext, InvokerContractAuthEntry, SubContractInvocation},
    contract, contractimpl, symbol_short, vec, Address, BytesN, Env, IntoVal, Val, Vec,
};

// This is the test contract for e2e testing of all the lifecycle
// operations (upload Wasm, create contract, update contract Wasm).
#[contract]
pub struct TestDeployerWithConstructorContract;

#[contractimpl]
impl TestDeployerWithConstructorContract {
    pub fn deploy_no_constructor(env: Env, wasm_hash: BytesN<32>) -> Address {
        env.deployer()
            .with_current_contract([0u8; 32])
            .deploy(wasm_hash)
    }

    pub fn deploy_no_constructor_args(env: Env, wasm_hash: BytesN<32>) -> Address {
        env.deployer()
            .with_current_contract([1u8; 32])
            .deploy_with_constructor(wasm_hash, vec![&env])
    }

    // This relies on `wasm_hash` to correspond to `constructor` test contract
    // and `auth_contract` to be an instance of `auth` test contract.
    pub fn deploy_with_contract_auth(
        env: Env,
        wasm_hash: BytesN<32>,
        auth_contract: Address,
    ) -> Address {
        let deployer = env.deployer().with_current_contract([3u8; 32]);
        let constructor_args: Vec<Val> = (
            env.current_contract_address(),
            symbol_short!("key"),
            100_u32,
            &auth_contract,
        )
            .into_val(&env);
        env.authorize_as_current_contract(vec![
            &env,
            // Subtle: current contract only authorizes the 'CreateContract'
            // invocation, but not the constructor invocation. While
            // technically constructor invocation is unambiguously defined by
            // the request to create a new contract, this behavior is
            // consistent with how authorization works (invocations are never
            // merged).
            InvokerContractAuthEntry::Contract(SubContractInvocation {
                context: ContractContext {
                    contract: deployer.deployed_address(),
                    fn_name: "__constructor".into_val(&env),
                    args: constructor_args.clone(),
                },
                sub_invocations: vec![
                    &env,
                    InvokerContractAuthEntry::Contract(SubContractInvocation {
                        context: ContractContext {
                            contract: auth_contract.clone(),
                            fn_name: symbol_short!("do_auth"),
                            args: (env.current_contract_address(), 100_u32).into_val(&env),
                        },
                        sub_invocations: vec![&env],
                    }),
                ],
            }),
        ]);

        let address = deployer.deploy_with_constructor(wasm_hash, constructor_args);
        // Smoke-test the deployed contract
        let val: u32 = env.invoke_contract(
            &address,
            &symbol_short!("get_data"),
            (symbol_short!("key"),).into_val(&env),
        );
        assert_eq!(val, 303);

        address
    }

    // This relies on `wasm_hash` to correspond to `constructor` test contract
    // and `auth_contract` to be an instance of `auth` test contract.
    pub fn deploy_with_external_auth(
        env: Env,
        wasm_hash: BytesN<32>,
        deployer_address: Address,
        auth_contract: Address,
    ) -> Address {
        let deployer = env.deployer().with_address(deployer_address, [4u8; 32]);
        let constructor_args: Vec<Val> = (
            env.current_contract_address(),
            symbol_short!("key"),
            100_u32,
            &auth_contract,
        )
            .into_val(&env);
        env.authorize_as_current_contract(vec![
            &env,
            InvokerContractAuthEntry::Contract(SubContractInvocation {
                context: ContractContext {
                    contract: deployer.deployed_address(),
                    fn_name: "__constructor".into_val(&env),
                    args: constructor_args.clone(),
                },
                sub_invocations: vec![
                    &env,
                    InvokerContractAuthEntry::Contract(SubContractInvocation {
                        context: ContractContext {
                            contract: auth_contract.clone(),
                            fn_name: symbol_short!("do_auth"),
                            args: (env.current_contract_address(), 100_u32).into_val(&env),
                        },
                        sub_invocations: vec![&env],
                    }),
                ],
            }),
        ]);

        let address = deployer.deploy_with_constructor(wasm_hash, constructor_args);

        // Smoke-test the deployed contract
        let val: u32 = env.invoke_contract(
            &address,
            &symbol_short!("get_data"),
            (symbol_short!("key"),).into_val(&env),
        );
        assert_eq!(val, 303);

        address
    }
}
