all: build test

test:
	cargo hack --each-feature test

build:
	cargo hack --each-feature clippy
	cargo hack clippy --target wasm32-unknown-unknown

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'

fmt:
	cargo fmt --all

clean:
	cargo clean

regenerate-test-wasms:
	make -C soroban-test-wasms regenerate-test-wasms

publish:
	cargo workspaces publish --all --force '*' --from-git --yes

publish-dry-run:
	./publish-dry-run.sh

# Requires: `cargo install cargo-llvm-cov`
coverage:
	rm -f lcov.info
	cargo llvm-cov test --all-features --tests --lcov --output-path=lcov.info
