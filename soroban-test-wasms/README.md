# soroban-test-wasms

This crate bundles together a bunch of pre-compiled WASM binaries as
constants that can be used elsewhere in end-to-end tests of the host crate,
stellar-core, or other embeddings that need to have some well-formed and
meaningful WASM inputs to draw on.

They are generated in (and then here copied out of) the embedded workspace
`wasm-workspace` which, confusingly, uses both the adjacent (in-repo)
`soroban-env-guest` and `soroban-env-common` crates and _also_ the
(external) `soroban-sdk` crate, with its dependencies on `soroban-env-guest`
and `soroban-env-common` patched to use the local versions in this repo.

Regeneration of the WASMs is _not_ totally automated: we do not know exactly
when the WASMs need to be regenerated, and so they _aren't_ unless you
manually run `make` in the `wasm-workspace` subdirectory.

There are 3 reasons for this crate (and its adjacent .rs inputs) to exist:

  1. To centralize the logic for compiling and embedding the .rs inputs into
     WASM test binaries in a single location, so that other consumers of
     test WASMs can just refer to this crate, in this repo, by its URL + git
     hash.

  2. To break what would otherwise be an awkward and multi-step cross-repo
     cyclical dependency between `rs-soroban-sdk` (or wherever the WASM test
     inputs live) and the `rs-soroban-env` repo. Specifically when we make a
     change to the env interface version number in
     `rs-soroban-env/soroban-env-common`, we need to regenerate the WASMs so
     that the WASM-based tests in `rs-soroban-env/soroban-env-host` still
     pass; but if we store the test WASMs and their .rs inputs in a separate
     repo (worst of all the SDK repo) we would need to patch it to refer
     back to the local path of the `rs-soroban-env` repo containing our
     change-in-progress to `rs-soroban-env/soroban-env-common`, and then
     copy the WASMs back into `rs-soroban-env/soroban-env-host`. By storing
     the WASMs and their .rs inputs in 'rs-soroban-env', here in the
     embedded `wasm-workspace` directory, we can (somewhat) contain this
     mess, writing down the necessary patch directive permanently and
     including the `.wasm` files directly from the embedded workspace.

  3. To decouple the .rs inputs from the SDK, even though they originate as
     SDK examples; this allows the SDK to evolve independently, and in
     particular to edit its examples to keep them educational / useful for
     documentation purpopses without having to worry about breaking tests in
     the host here.
