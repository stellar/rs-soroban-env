# This rebuilds the workspace's .rs files into Wasms that will be embedded as
# constants into the crate `soroban-test-wasms` defined in the enclosing
# directory. See that crate for more details.
# This requires `PROTOCOL_VERSION` variable to be set when running `make`.
# The `PROTOCOL_VERSION` has to match the version of `soroban-env-common`
# specified in `Cargo.toml`. An output Wasm for a given `PROTOCOL_VERSION` is
# considered `frozen` as soon as the corresponding version of Soroban 
# environment has been released. E.g. protocol 20 Wasms can't be modified after
# the Soroban initial release. It's ok though to add new Wasms for the old
# protocols, or omit Wasms or protocols when they don't provide any new behavior
# (e.g. there isn't much point in re-building the initial protocol 20 Wasms for
# the later protocol versions).

ifeq ($(PROTOCOL_VERSION),)
  $(error PROTOCOL_VERSION is not set)
endif

regenerate-test-wasms:
	for i in $(wildcard */Cargo.toml); do \
		(cd $$(dirname $$i) && cargo rustc \
			--target wasm32-unknown-unknown \
			--release \
			--crate-type=cdylib); \
	done
	mkdir -p opt/$(PROTOCOL_VERSION)
	for i in target/wasm32-unknown-unknown/release/*.wasm ; do \
		wasm-opt -Oz "$$i" -o "opt/$(PROTOCOL_VERSION)/$$(basename $$i)"; \
		ls -l "opt/$(PROTOCOL_VERSION)/$$(basename $$i)"; \
	done