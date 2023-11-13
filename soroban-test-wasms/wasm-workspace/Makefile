# This rebuilds the workspace's .rs files into WASMs that will be embedded as
# constants into the crate `soroban-test-wasms` defined in the enclosing
# directory. See that crate for more details.

regenerate-test-wasms:
	for i in $(wildcard */Cargo.toml); do \
		(cd $$(dirname $$i) && cargo rustc \
			--target wasm32-unknown-unknown \
			--release \
			--crate-type=cdylib); \
	done
	for i in target/wasm32-unknown-unknown/release/*.wasm ; do \
		wasm-opt -Oz "$$i" -o "opt/curr/$$(basename $$i)"; \
		ls -l "opt/curr/$$(basename $$i)"; \
	done