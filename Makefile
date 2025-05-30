all: build test test-opt

test:
	cargo hack --locked --each-feature test

test-opt:
	cargo hack --locked --each-feature test --profile test-opt

MIN_PROTOCOL := 23
MAX_PROTOCOL := 23

test-all-protocols:
	for i in $$(seq $(MIN_PROTOCOL) $$(($(MAX_PROTOCOL) + 1))); do \
		if [ $$i -le $(MAX_PROTOCOL) ]; then \
		  	echo "Testing protocol $$i for vCurr host build..."; \
			TEST_PROTOCOL=$$i cargo hack test -p soroban-env-host --locked --features testutils; \
			TEST_PROTOCOL=$$i cargo hack test -p soroban-simulation --locked --features testutils; \
		fi; \
		echo "Testing protocol $$i for vNext host build..."; \
		TEST_PROTOCOL=$$i cargo hack test -p soroban-env-host --locked --features testutils,next; \
		TEST_PROTOCOL=$$i cargo hack test -p soroban-simulation --locked --features testutils,next; \
	done

# Note: we use wasm32-unknown-unknown here for the workspace build, not
# wasm32v1-none, because we are testing the ability to build the soroban host
# for use in a browser, which requires a target with a stdlib. wasm32v1-none
# does not have a stdlib and is the target used for building _contracts that run
# in soroban_, not the host itself. So we only check the guest package with it.
build:
	cargo hack --locked --each-feature clippy
	cargo hack --locked --each-feature clippy --target wasm32-unknown-unknown
	cargo --locked clippy --package soroban-env-guest --target wasm32v1-none

# We use "run" to run the soroban-env-host/src/bin/main.rs
# entrypoint, which both excludes dev-deps (noisy) and
# actually includes soroban-env-host itself (rather than
# just its deps). We want to catch ourselves using APIs
# too!
check-apis:
	cargo acl run

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'

fmt:
	cargo fmt --all

clean:
	cargo clean

regenerate-test-wasms:
	make -C soroban-test-wasms regenerate-test-wasms

reobserve-tests:
	for i in $$(seq $(MIN_PROTOCOL) $(MAX_PROTOCOL)); do \
		TEST_PROTOCOL=$$i UPDATE_OBSERVATIONS=1 cargo test --locked -p soroban-env-host --features testutils --profile test-opt; \
	done

# Requires: `cargo install cargo-llvm-cov`
coverage:
	rm -f lcov.info
	cargo llvm-cov test --all-features --tests --lcov --output-path=lcov.info
