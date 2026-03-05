FROM mcr.microsoft.com/devcontainers/rust:2-1-trixie

# Install system packages needed for building (e.g. fuzz custom_getrandom.c)
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        gcc \
        libc6-dev \
        jq \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Switch to the non-root user that the base image provides
USER vscode

# Install rustup components from rust-toolchain.toml
RUN rustup component add rustfmt clippy rust-src

# Install additional compilation targets
RUN rustup target add wasm32-unknown-unknown wasm32v1-none

# Install nightly toolchain (used by cargo +nightly fuzz with sanitizers)
RUN rustup toolchain install nightly --profile minimal \
    && rustup component add rust-src --toolchain nightly

# Download pre-built binaries from stellar/binaries (matches CI)
# Map Docker arch (x86_64/aarch64) to the naming convention used by stellar/binaries (X64/ARM64)
ARG STELLAR_BINARIES_REF=v54
RUN ARCH="$(uname -m)" \
    && mkdir -p "${HOME}/.cargo/bin" \
    && case "$ARCH" in \
         x86_64) ARCH=X64 ;; \
         aarch64) ARCH=ARM64 ;; \
         *) echo "Unsupported architecture: ${ARCH}. Only x86_64 and aarch64 are supported for stellar/binaries downloads." >&2; exit 1 ;; \
       esac \
    && BASE=https://github.com/stellar/binaries/releases/download/${STELLAR_BINARIES_REF} \
    && for tool in \
         cargo-hack-0.6.35 \
         cargo-deny-0.16.1 \
         cargo-fuzz-0.13.1 \
         cargo-semver-checks-0.46.0 \
       ; do \
         echo "Installing ${tool}..." \
         && tmp="$(mktemp)" \
         && curl -fL "${BASE}/${tool}-Linux-${ARCH}.tar.gz" -o "$tmp" \
         && tar xz -C "${HOME}/.cargo/bin" -f "$tmp" \
         && rm -f "$tmp" \
       ; done

# Install remaining cargo extensions not available in stellar/binaries
RUN cargo install --locked --version 8.5.2 cargo-watch \
    && cargo install --locked --version 0.6.10 cargo-llvm-cov \
    && cargo install --locked --version 0.3.0 cargo-acl \
    && cargo install --locked --version 0.3.0 lcov-summary

ENV PATH="${HOME}/.cargo/bin:${HOME}/.local/bin:${PATH}"

