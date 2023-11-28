#!/bin/sh
#
# This script is extracted from github.com/stellar/actions/rust-publish-dry-run/workflow.yml
# which unfortunately has a pile of shell code in YAML which makes it impossible to reuse
# outside of a workflow. So we have a copy of it here.

# Vendor all the dependencies into the vendor/ folder. Temporarily remove
# [patch.crates-io] entries in the workspace Cargo.toml that reference git
# repos. These will be removed when published.
cp Cargo.toml Cargo.toml.bak
sed -r '/(git|rev) ?=/d' Cargo.toml.bak > Cargo.toml
cargo vendor --versioned-dirs
rm Cargo.toml
mv Cargo.toml.bak Cargo.toml

# Package the crates that will be published. Verification is disabled because
# we aren't ready to verify yet.
cargo-hack hack --ignore-private package --locked --no-verify --each-feature

# Add each crate that was packaged to the vendor/ directory.
for crate in target/package/*.crate
    do
        name=$(basename "$crate" .crate)
        tar xvfz "$crate" -C vendor/
        # Crates in the vendor directory require a checksum file, but it doesn't
        # matter if it is empty.
        echo '{"files":{}}' > vendor/$name/.cargo-checksum.json
    done

# Rerun the package command but with verification enabled this time. Tell
# cargo to use the local vendor/ directory as the source for all packages. Run
# the package command on the full feature powerset so that all features of
# each crate are verified to compile.
cargo-hack hack \
    --ignore-private \
    --config "source.crates-io.replace-with = 'vendored-sources'" \
    --config "source.vendored-sources.directory = 'vendor'" \
    package \
    --locked \
    --target x86_64-unknown-linux-gnu
