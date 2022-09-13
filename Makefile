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

regenerate-test-wasms:
	make -C soroban-test-wasms regenerate-test-wasms

publish:
	cargo workspaces publish --all --force '*' --from-git --yes
