//! This crate bundles together a bunch of pre-compiled Wasm binaries as
//! constants that can be used elsewhere in end-to-end tests of the host crate,
//! stellar-core, or other embeddings that need to have some well-formed and
//! meaningful Wasm inputs to draw on.
//!
//! They are generated in (and then here copied out of) the embedded workspace
//! `wasm-workspace` which, confusingly, uses both the adjacent (in-repo)
//! `soroban-env-guest` and `soroban-env-common` crates and _also_ the
//! (external) `soroban-sdk` crate, with its dependencies on `soroban-env-guest`
//! and `soroban-env-common` patched to use the local versions in this repo.
//!
//! Regeneration of the Wasms is _not_ totally automated: we do not know exactly
//! when the Wasms need to be regenerated, and so they _aren't_ unless you
//! manually run `make` in the `wasm-workspace` subdirectory.
//!
//! There are 3 reasons for this crate (and its adjacent .rs inputs) to exist:
//!
//!   1. To centralize the logic for compiling and embedding the .rs inputs into
//!      Wasm test binaries in a single location, so that other consumers of
//!      test Wasms can just refer to this crate, in this repo, by its URL + git
//!      hash.
//!
//!   2. To break what would otherwise be an awkward and multi-step cross-repo
//!      cyclical dependency between `rs-soroban-sdk` (or wherever the Wasm test
//!      inputs live) and the `rs-soroban-env` repo. Specifically when we make a
//!      change to the env interface version number in
//!      `rs-soroban-env/soroban-env-common`, we need to regenerate the Wasms so
//!      that the Wasm-based tests in `rs-soroban-env/soroban-env-host` still
//!      pass; but if we store the test Wasms and their .rs inputs in a separate
//!      repo (worst of all the SDK repo) we would need to patch it to refer
//!      back to the local path of the `rs-soroban-env` repo containing our
//!      change-in-progress to `rs-soroban-env/soroban-env-common`, and then
//!      copy the Wasms back into `rs-soroban-env/soroban-env-host`. By storing
//!      the Wasms and their .rs inputs in 'rs-soroban-env', here in the
//!      embedded `wasm-workspace` directory, we can (somewhat) contain this
//!      mess, writing down the necessary patch directive permanently and
//!      including the `.wasm` files directly from the embedded workspace.
//!
//!   3. To decouple the .rs inputs from the SDK, even though they originate as
//!      SDK examples; this allows the SDK to evolve independently, and in
//!      particular to edit its examples to keep them educational / useful for
//!      documentation purpopses without having to worry about breaking tests in
//!      the host here.

// Protocol 20 Wasms.
pub const ADD_I32: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_add_i32.wasm").as_slice();
pub const SUM_I32: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_sum_i32.wasm").as_slice();
pub const ADD_F32: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_add_f32.wasm").as_slice();
pub const ALLOC: &[u8] = include_bytes!("../wasm-workspace/opt/20/example_alloc.wasm").as_slice();
pub const CREATE_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_create_contract.wasm").as_slice();
pub const CONTRACT_STORAGE: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_contract_data.wasm").as_slice();
pub const LINEAR_MEMORY: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_linear_memory.wasm").as_slice();
pub const LOADGEN: &[u8] = include_bytes!("../wasm-workspace/opt/20/loadgen.wasm").as_slice();
pub const VEC: &[u8] = include_bytes!("../wasm-workspace/opt/20/example_vec.wasm").as_slice();
pub const INVOKE_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_invoke_contract.wasm").as_slice();
pub const HOSTILE: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_hostile.wasm").as_slice();
pub const FIB: &[u8] = include_bytes!("../wasm-workspace/opt/20/example_fib.wasm").as_slice();
pub const FANNKUCH: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_fannkuch.wasm").as_slice();
pub const COMPLEX: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_complex.wasm").as_slice();
pub const SIMPLE_ACCOUNT_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_simple_account.wasm").as_slice();
pub const AUTH_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/auth_test_contract.wasm").as_slice();
pub const UPDATEABLE_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_updateable_contract.wasm").as_slice();
pub const UPLOAD_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/example_upload_contract.wasm").as_slice();
pub const DELEGATED_ACCOUNT_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/test_delegated_account.wasm").as_slice();
pub const ERR: &[u8] = include_bytes!("../wasm-workspace/opt/20/example_err.wasm").as_slice();
pub const WRITE_BYTES: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/soroban_write_upgrade_bytes_contract.wasm").as_slice();
pub const CONDITIONAL_ACCOUNT_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/test_conditional_account.wasm").as_slice();
pub const SAC_REENTRY_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/sac_reentry_account.wasm").as_slice();
pub const CONTRACT_SAC_TRANSFER_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/contract_sac_transfer.wasm").as_slice();
pub const RECURSIVE_ACCOUNT_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/recursive_account.wasm").as_slice();

pub const HOSTILE_LARGE_VALUE: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/hostile_large_val.wasm").as_slice();

pub const DEPLOYER_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/20/test_deployer.wasm").as_slice();

// Protocol 21 Wasms.
pub const CONSTRUCTOR_TEST_CONTRACT_P21: &[u8] =
    include_bytes!("../wasm-workspace/opt/21/test_constructor.wasm").as_slice();
pub const NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21: &[u8] =
    include_bytes!("../wasm-workspace/opt/21/test_no_arg_constructor.wasm").as_slice();
pub const CONSTRUCTOR_WITH_RETURN_VALUE_P21: &[u8] =
    include_bytes!("../wasm-workspace/opt/21/test_constructor_with_return_value.wasm").as_slice();

// Protocol 22 Wasms.
pub const CONSTRUCTOR_TEST_CONTRACT_P22: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_constructor.wasm").as_slice();
pub const NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P22: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_no_arg_constructor.wasm").as_slice();
pub const ADD_I32_P22: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/example_add_i32.wasm").as_slice();
pub const DEPLOYER_WITH_CONSTRUCTOR: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_deployer_with_constructor.wasm").as_slice();
pub const CONSTRUCTOR_WITH_RETURN_VALUE_P22: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_constructor_with_return_value.wasm").as_slice();
pub const CONSTRUCTOR_WITH_RESULT: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_constructor_with_result.wasm").as_slice();
pub const CUSTOM_ACCOUNT_CONTEXT_TEST_CONTRACT: &[u8] =
    include_bytes!("../wasm-workspace/opt/22/test_custom_account_context.wasm").as_slice();
