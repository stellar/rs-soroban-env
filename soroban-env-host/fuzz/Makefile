fuzz-fast-stable-no-sanitizer:
	cargo fuzz run expr --release --sanitizer none -j $$(nproc)

fuzz-debug-stable-no-sanitizer:
	cargo fuzz run expr --dev --sanitizer none -j $$(nproc)

fuzz-slow-nightly-with-sanitizer:
	cargo +nightly fuzz run expr --release -j $$(nproc)

clean:
	rm -rf target
	rm -rf corpus/* artifacts/*
