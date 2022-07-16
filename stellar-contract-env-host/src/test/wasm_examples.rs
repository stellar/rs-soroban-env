// We include, as contract code used in the test sub-modules here, a bunch
// of WASM binaries that are compiled in the context of the SDK and
// copied-in to this repository.
//
// To regenerate them, check out the rs-stellar-contract-sdk repo and build,
// then copy the wasms from the target-tiny directory into this crate's
// wasm-examples directory. Something like:
//
//   $ HOST_CRATE=/path/to/stellar-contract-env-host
//   $ git clone https://github.com/stellar/rs-stellar-contract-sdk
//   $ cd rs-stellar-contract-sdk
//   $ make
//   $ cp target-tiny/wasm32-unknown-unknown/release/example_*.wasm $HOST_CRATE/wasm-examples

pub(crate) const ADD_I32: &'static [u8] =
    include_bytes!("../../wasm-examples/example_add_i32.wasm").as_slice();
#[cfg(feature = "testutils")]
pub(crate) const CREATE_CONTRACT: &'static [u8] =
    include_bytes!("../../wasm-examples/example_create_contract.wasm").as_slice();
pub(crate) const LINEAR_MEMORY: &'static [u8] =
    include_bytes!("../../wasm-examples/example_linear_memory.wasm").as_slice();
pub(crate) const VEC: &'static [u8] =
    include_bytes!("../../wasm-examples/example_vec.wasm").as_slice();
pub(crate) const CALL: &'static [u8] =
    include_bytes!("../../wasm-examples/example_CALL.wasm").as_slice();
