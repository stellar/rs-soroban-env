# soroban-env-host/fuzz

This directory is a standalone sub-workspace used for local fuzzing runs with tools like `cargo-fuzz`.

It contains only thin fuzz-target stubs (in `fuzz_targets/`) and the local cargo-fuzz wiring needed to build/link fuzzing engines.

The stubs call into the outer `soroban-fuzz-targets` crate, which holds the actual fuzz target bodies.

## Why this is separate

- `cargo-fuzz` expects a dedicated fuzz workspace layout.
- Embedders of `soroban-env-host` should not need to build fuzzing infrastructure just to use test utilities.
- Embedders of `soroban-env-host` _might_ want to fuzz using their own fuzz harness (in fact stellar-core does).
- Keeping this as a sub-workspace isolates fuzzing-engine dependencies (`libfuzzer-sys`, etc.) from normal host/library builds.

## Rule of thumb

- Put reusable fuzzing logic bodies in `soroban-fuzz-targets`.
- Keep files in this directory as entrypoint stubs and local campaign wiring only.

This split allows external fuzz harnesses to reuse the same fuzz logic without depending on cargo-fuzz-specific plumbing.
