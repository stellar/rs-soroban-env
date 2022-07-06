all: build test

test:
	cargo hack --feature-powerset test

build:
	cargo hack --feature-powerset check

watch:
	cargo watch --clear --watch-when-idle --shell '$(MAKE)'
