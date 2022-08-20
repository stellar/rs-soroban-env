# This rebuilds the workspace's .rs files into WASMs that will be embedded as
# constants into the crate `soroban-test-wasms` defined in the enclosing
# directory. See that crate for more details.

regenerate-test-wasms:
	cargo +nightly build \
		--target wasm32-unknown-unknown \
		--release \
		-Z build-std=std,panic_abort \
		-Z build-std-features=panic_immediate_abort
	for i in target/wasm32-unknown-unknown/release/*.wasm ; do \
		wasm-opt -Oz "$$i" -o "opt/$$(basename $$i)"; \
		ls -l "opt/$$(basename $$i)"; \
	done
