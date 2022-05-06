test:
	cargo check -p stellar-contract-env-guest --target wasm32-unknown-unknown
	cargo check -p stellar-contract-env-host
	cargo test -p stellar-contract-env-guest
	cargo test -p stellar-contract-env-host

watch:
	cargo watch --clear \
		-x 'check -p stellar-contract-env-guest --target wasm32-unknown-unknown' \
		-x 'check -p stellar-contract-env-host' \
		-x 'test -p stellar-contract-env-common' \
		-x 'test -p stellar-contract-env-guest' \
		-x 'test -p stellar-contract-env-host'
