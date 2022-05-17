all: build test

test:
	cargo test -p stellar-contract-env-common --no-default-features --features ''
	cargo test -p stellar-contract-env-common --no-default-features --features 'std'
	cargo test -p stellar-contract-env-common --no-default-features --features 'vm'
	cargo test -p stellar-contract-env-common --no-default-features --features 'std,vm'

	cargo test -p stellar-contract-env-guest --no-default-features --features ''

	cargo test -p stellar-contract-env-host --no-default-features --features ''
	cargo test -p stellar-contract-env-host --no-default-features --features 'vm'

build:
	cargo check --no-default-features --all-targets

	cargo check -p stellar-contract-env-common --no-default-features --features ''
	cargo check -p stellar-contract-env-common --no-default-features --features 'std'
	cargo check -p stellar-contract-env-common --no-default-features --features 'panic_handler'
	cargo check -p stellar-contract-env-common --no-default-features --features 'vm'
	cargo check -p stellar-contract-env-common --no-default-features --features 'std,vm'
	cargo check -p stellar-contract-env-common --no-default-features --features '' --target wasm32-unknown-unknown
	cargo check -p stellar-contract-env-common --no-default-features --features 'panic_handler' --target wasm32-unknown-unknown
	cargo check -p stellar-contract-env-common --no-default-features --features 'vm' --target wasm32-unknown-unknown

	cargo check -p stellar-contract-env-guest --no-default-features --features ''
	cargo check -p stellar-contract-env-guest --no-default-features --features 'panic_handler'
	cargo check -p stellar-contract-env-guest --no-default-features --features '' --release --target wasm32-unknown-unknown
	cargo check -p stellar-contract-env-guest --no-default-features --features 'panic_handler' --release --target wasm32-unknown-unknown

	cargo check -p stellar-contract-env-host --no-default-features --features ''
	cargo check -p stellar-contract-env-host --no-default-features --features 'vm'
	cargo check -p stellar-contract-env-host --no-default-features --features '' --release --target wasm32-unknown-unknown
	cargo check -p stellar-contract-env-host --no-default-features --features 'vm' --release --target wasm32-unknown-unknown

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'
