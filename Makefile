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

publish-verify:
	sed -i.bak -r '/git ?=/d' Cargo.toml
	cargo vendor --versioned-dirs
	cargo hack --ignore-private package --no-verify
	for crate in target/package/*.crate; do cargo vendor-add --crate "$crate" --vendor-path vendor; done
	cargo hack --ignore-private --config "source.crates-io.replace-with = 'vendored-sources'" --config "source.vendored-sources.directory = 'vendor'" package

publish:
	cargo workspaces publish --all --force '*' --from-git --yes
