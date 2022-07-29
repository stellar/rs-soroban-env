all: build test

export RUSTFLAGS=-Dwarnings

test:
	cargo hack --feature-powerset test

build:
	cargo hack --feature-powerset check

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'

fmt:
	cargo fmt --all

clean:
	cargo clean

# Build all projects as if they are being published to crates.io, and do so for
# all feature and target combinations.
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

# Publish publishes the crate to crates.io. The dry-run is a dependency because
# the dry-run target will verify all feature set combinations.
publish-common: publish-dry-run-common
	cargo +stable publish --package soroban-env-common --locked

publish-macros: publish-dry-run-macros
	cargo +stable publish --package soroban-env-macros --locked

publish-host: publish-dry-run-host
	cargo +stable publish --package soroban-env-host --locked

publish-guest: publish-dry-run-guest
	cargo +stable publish --package soroban-env-guest --locked
