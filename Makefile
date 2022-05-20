all: build test

test:
	cargo test-all-features

build:
	cargo check-all-features

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'
