FUZZ_TARGET:=expr

SYSROOT := $(shell rustup which rustc | xargs dirname | xargs dirname)
TARGET_TRIPLE := $(shell rustc --version --verbose | awk -e '/host:/ {print $$2}')
LLVM_COV_PATH := $(SYSROOT)/lib/rustlib/$(TARGET_TRIPLE)/bin/llvm-cov
LLVM_COV_TARGET_BIN := target/$(TARGET_TRIPLE)/coverage/$(TARGET_TRIPLE)/release/$(FUZZ_TARGET)
LLVM_COV_PROFDATA := coverage/$(FUZZ_TARGET)/coverage.profdata
LLVM_COV_IGNOREFLAGS := --ignore-filename-regex=cargo
LLVM_COV_FLAGS := $(LLVM_COV_IGNOREFLAGS) --instr-profile=$(LLVM_COV_PROFDATA) --object=$(LLVM_COV_TARGET_BIN)
LLVM_COV_OUTPUTS := fuzz-lcov.info coverage-full.txt coverage-summary.txt

# In order to get a _very_ deterministic fuzz run (one we're comfortable using
# as a baseline to compare against for improvements in coverage) we have to make
# a few interventions:
#
# 1. Run the fuzzer with a fixed seed and fixed number of runs
#
# 2. Disable ASLR, by running under `setarch --addr-no-randomize`
#
# 3. Inject a custom, constant-valued implementation of `getrandom` via
#    LD_PRELOAD because rust hashtables use `getrandom` to seed the hash
#    function and there are a couple uses of hashtables in TLS variables
#    accessed by wasmi.
#
# 4. Actually turn off comparison tracing (fuzz with `--no-trace-compares`)
#    because they seem to sense a tiny additional bit of pointer-value
#    randomness which I think (but am not sure) we get from glibc's _internal_
#    use of `getrandom` when initializing its own malloc subsystem.
#
# There might be some other approach that works, say involving musl, but this
# seems like an acceptable compromise for now.

DISABLE_ASLR := setarch $(shell uname -m) --addr-no-randomize

CUSTOM_GETRANDOM_LIB := $(shell pwd)/custom_getrandom.so
CUSTOM_GETRANDOM_SRC := $(shell pwd)/custom_getrandom.c

$(CUSTOM_GETRANDOM_LIB): $(CUSTOM_GETRANDOM_SRC)
	gcc -shared -fPIC -o $@ $<

export LD_PRELOAD := $(CUSTOM_GETRANDOM_LIB)

fuzz-check: $(CUSTOM_GETRANDOM_LIB) fuzz-lcov-baseline.info Makefile
	cargo install lcov-summary
	$(MAKE) reset
	$(MAKE) fuzz-fixed
	$(MAKE) fuzz-cov
	lcov-summary fuzz-lcov-baseline.info fuzz-lcov.info

fuzz-fixed: $(CUSTOM_GETRANDOM_LIB) Makefile
	$(DISABLE_ASLR) cargo fuzz run $(FUZZ_TARGET) --release --sanitizer none --no-trace-compares -- -seed=12345 -max_len=1000 -runs=100000 >/dev/null 2>&1

fuzz-fast-stable-no-sanitizer: $(CUSTOM_GETRANDOM_LIB) Makefile
	$(DISABLE_ASLR) cargo fuzz run $(FUZZ_TARGET) --release --sanitizer none --no-trace-compares -j $$(nproc)

fuzz-debug-stable-no-sanitizer: $(CUSTOM_GETRANDOM_LIB) Makefile
	$(DISABLE_ASLR) cargo fuzz run $(FUZZ_TARGET) --dev --sanitizer none --no-trace-compares

fuzz-slow-nightly-with-sanitizer: $(CUSTOM_GETRANDOM_LIB) Makefile
	$(DISABLE_ASLR) cargo +nightly fuzz run $(FUZZ_TARGET) --release -j $$(nproc)

fuzz-cov: Makefile
	rm -f $(LLVM_COV_OUTPUTS)
	rm -rf coverage
	$(MAKE) $(LLVM_COV_OUTPUTS)

$(LLVM_COV_PROFDATA): $(CUSTOM_GETRANDOM_LIB) Makefile
	$(DISABLE_ASLR) cargo fuzz coverage --sanitizer none $(FUZZ_TARGET) >/dev/null 2>&1

fuzz-lcov.info: $(LLVM_COV_PROFDATA) $(CUSTOM_GETRANDOM_LIB) Makefile
	$(LLVM_COV_PATH) export -format=lcov $(LLVM_COV_FLAGS) >$@
	@echo "copying coverage to lcov.info in outer workspace"
	cp $@ ../../lcov.info

coverage-full.txt: $(LLVM_COV_PROFDATA) $(CUSTOM_GETRANDOM_LIB) Makefile
	$(LLVM_COV_PATH) show $(LLVM_COV_FLAGS) >$@

coverage-summary.txt: $(LLVM_COV_PROFDATA) $(CUSTOM_GETRANDOM_LIB) Makefile
	$(LLVM_COV_PATH) report $(LLVM_COV_FLAGS) >$@

clean: $(CUSTOM_GETRANDOM_LIB) Makefile
	rm -rf target
	rm -rf corpus/* artifacts/* coverage
	rm -f $(LLVM_COV_OUTPUTS) $(CUSTOM_GETRANDOM_LIB)

reset: $(CUSTOM_GETRANDOM_LIB) Makefile
	rm -rf corpus/* artifacts/* coverage

.PHONY: clean reset fuzz-cov fuzz-fast-stable-no-sanitizer fuzz-debug-stable-no-sanitizer fuzz-slow-nightly-with-sanitizer