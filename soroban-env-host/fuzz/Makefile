SYSROOT := $(shell rustup which rustc | xargs dirname | xargs dirname)
TARGET_TRIPLE := $(shell rustc --version --verbose | awk -e '/host:/ {print $$2}')
LLVM_COV_PATH := $(SYSROOT)/lib/rustlib/$(TARGET_TRIPLE)/bin/llvm-cov

fuzz-fast-stable-no-sanitizer:
	cargo fuzz run expr --release --sanitizer none -j $$(nproc)

fuzz-debug-stable-no-sanitizer:
	cargo fuzz run expr --dev --sanitizer none

fuzz-slow-nightly-with-sanitizer:
	cargo +nightly fuzz run expr --release -j $$(nproc)

fuzz-cov:
	rm -f fuzz-lcov.info
	rm -rf coverage
	$(MAKE) fuzz-lcov.info

coverage/expr/coverage.profdata:
	cargo fuzz coverage --sanitizer none expr

fuzz-lcov.info: coverage/expr/coverage.profdata
	$(LLVM_COV_PATH) export -format=lcov \
		-instr-profile=coverage/expr/coverage.profdata \
		-object=target/$(TARGET_TRIPLE)/coverage/$(TARGET_TRIPLE)/release/expr \
		>fuzz-lcov.info
	@echo "copying coverage to lcov.info in outer workspace"
	cp fuzz-lcov.info ../../lcov.info

clean:
	rm -f fuzz-lcov.info
	rm -rf target
	rm -rf corpus/* artifacts/* coverage

reset:
	rm -rf corpus/* artifacts/* coverage

.PHONY: clean reset fuzz-cov