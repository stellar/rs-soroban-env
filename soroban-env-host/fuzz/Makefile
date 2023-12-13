FUZZ_TARGET:=expr

SYSROOT := $(shell rustup which rustc | xargs dirname | xargs dirname)
TARGET_TRIPLE := $(shell rustc --version --verbose | awk -e '/host:/ {print $$2}')
LLVM_COV_PATH := $(SYSROOT)/lib/rustlib/$(TARGET_TRIPLE)/bin/llvm-cov
LLVM_COV_TARGET_BIN:=target/$(TARGET_TRIPLE)/coverage/$(TARGET_TRIPLE)/release/$(FUZZ_TARGET)
LLVM_COV_PROFDATA:=coverage/$(FUZZ_TARGET)/coverage.profdata
LLVM_COV_IGNOREFLAGS:=--ignore-filename-regex=cargo
LLVM_COV_FLAGS:=$(LLVM_COV_IGNOREFLAGS) --instr-profile=$(LLVM_COV_PROFDATA) --object=$(LLVM_COV_TARGET_BIN)
LLVM_COV_OUTPUTS:=fuzz-lcov.info coverage-full.txt coverage-summary.txt

fuzz-fixed:
	cargo fuzz run $(FUZZ_TARGET) --release --sanitizer none -- -seed=12345 -max_len=1000 -runs=100000

fuzz-fast-stable-no-sanitizer:
	cargo fuzz run $(FUZZ_TARGET) --release --sanitizer none -j $$(nproc)

fuzz-debug-stable-no-sanitizer:
	cargo fuzz run $(FUZZ_TARGET) --dev --sanitizer none

fuzz-slow-nightly-with-sanitizer:
	cargo +nightly fuzz run $(FUZZ_TARGET) --release -j $$(nproc)

fuzz-cov:  Makefile
	rm -f $(LLVM_COV_OUTPUTS)
	rm -rf coverage
	$(MAKE) $(LLVM_COV_OUTPUTS)

$(LLVM_COV_PROFDATA): Makefile
	cargo fuzz coverage --sanitizer none $(FUZZ_TARGET)

fuzz-lcov.info: $(LLVM_COV_PROFDATA) Makefile
	$(LLVM_COV_PATH) export -format=lcov $(LLVM_COV_FLAGS) >$@
	@echo "copying coverage to lcov.info in outer workspace"
	cp $@ ../../lcov.info

coverage-full.txt: $(LLVM_COV_PROFDATA) Makefile
	$(LLVM_COV_PATH) show $(LLVM_COV_FLAGS) >$@

coverage-summary.txt: $(LLVM_COV_PROFDATA) Makefile
	$(LLVM_COV_PATH) report $(LLVM_COV_FLAGS) >$@

clean:
	rm -f $(LLVM_COV_OUTPUTS)
	rm -rf target
	rm -rf corpus/* artifacts/* coverage

reset:
	rm -rf corpus/* artifacts/* coverage

.PHONY: clean reset fuzz-cov fuzz-fast-stable-no-sanitizer fuzz-debug-stable-no-sanitizer fuzz-slow-nightly-with-sanitizer