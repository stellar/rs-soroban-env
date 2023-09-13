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
		wasm-opt -Oz "$$i" -o "opt/curr/$$(basename $$i)"; \
		ls -l "opt/curr/$$(basename $$i)"; \
	done
	cargo clean
	cargo +nightly build \
		--target wasm32-unknown-unknown \
		--release \
		--features next \
		-Z build-std=std,panic_abort \
		-Z build-std-features=panic_immediate_abort
	for i in target/wasm32-unknown-unknown/release/*.wasm ; do \
		wasm-opt -Oz "$$i" -o "opt/next/$$(basename $$i)"; \
		ls -l "opt/next/$$(basename $$i)"; \
	done
