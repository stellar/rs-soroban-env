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
