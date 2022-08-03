all: check build test

export RUSTFLAGS=-Dwarnings

CARGO_DOC_ARGS?=--open

check: fmt
	cargo hack --feature-powerset --exclude-features docs check --lib --examples --tests
	cargo hack check --release --target wasm32-unknown-unknown

test: fmt
	cargo hack --feature-powerset --exclude-features docs test

build: fmt
	cargo build --target wasm32-unknown-unknown --release
	CARGO_TARGET_DIR=target-tiny cargo +nightly build --target wasm32-unknown-unknown --release \
		-Z build-std=std,panic_abort \
		-Z build-std-features=panic_immediate_abort
	cd target/wasm32-unknown-unknown/release/ && \
		for i in *.wasm ; do \
			wasm-opt -Oz "$$i" -o "$$i.tmp" && mv "$$i.tmp" "$$i"; \
			ls -l "$$i"; \
		done
	cd target-tiny/wasm32-unknown-unknown/release/ && \
		for i in *.wasm ; do \
			wasm-opt -Oz "$$i" -o "$$i.tmp" && mv "$$i.tmp" "$$i"; \
			ls -l "$$i"; \
		done

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'

fmt:
	cargo fmt --all

clean:
	cargo clean

doc: fmt
	cargo test --doc --features testutils
	cargo +nightly doc \
	    --no-deps \
		--package soroban-sdk \
		--features docs,testutils \
		$(CARGO_DOC_ARGS)

watch-doc:
	cargo +nightly watch --clear --watch-when-idle --shell '$(MAKE) doc CARGO_DOC_ARGS='

publish-dry-run-common:
	cargo +stable hack --feature-powerset publish --package soroban-env-common --locked --dry-run
	cargo +stable hack --feature-powerset publish --package soroban-env-common --locked --dry-run --target wasm32-unknown-unknown

publish-dry-run-macros:
	cargo +stable hack --feature-powerset publish --package soroban-env-macros --locked --dry-run
	cargo +stable hack --feature-powerset publish --package soroban-env-macros --locked --dry-run --target wasm32-unknown-unknown

publish-dry-run-host:
	cargo +stable hack --feature-powerset publish --package soroban-env-host --locked --dry-run
	cargo +stable hack --feature-powerset publish --package soroban-env-host --locked --dry-run --target wasm32-unknown-unknown

publish-dry-run-guest:
	cargo +stable hack --feature-powerset publish --package soroban-env-guest --locked --dry-run
	cargo +stable hack --feature-powerset publish --package soroban-env-guest --locked --dry-run --target wasm32-unknown-unknown

publish-dry-run-sdk-macros:
	cd soroban-sdk-macros && cargo +stable hack --feature-powerset publish --locked --dry-run --package soroban-sdk-macros

publish-dry-run-sdk:
	cargo +stable hack --feature-powerset publish --locked --dry-run --exclude-features docs --package soroban-sdk
	cargo +stable hack --feature-powerset publish --locked --dry-run --exclude-features docs,testutils --package soroban-sdk --target wasm32-unknown-unknown

publish-common: publish-dry-run-common
	cargo +stable publish --package soroban-env-common --locked

publish-macros: publish-dry-run-macros
	cargo +stable publish --package soroban-env-macros --locked

publish-host: publish-dry-run-host
	cargo +stable publish --package soroban-env-host --locked

publish-guest: publish-dry-run-guest
	cargo +stable publish --package soroban-env-guest --locked
	CARGO_TARGET_DIR=target-tiny cargo +nightly clean

publish-sdk: publish-dry-run-sdk
	cargo +stable publish --locked --package soroban-sdk

publish-sdk-macros: publish-dry-run-sdk-macros
	cd macros && cargo +stable publish --locked --package soroban-sdk-macros
