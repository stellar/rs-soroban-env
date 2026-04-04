#!/usr/bin/env bash
# Setup sccache with Namespace remote cache.
#
# This script is designed to be _sourced_ (not executed) so that
# environment variables are set in the caller's shell:
#
#   source setup-sccache.sh
#
# It works in both CI (GitHub Actions) and local development.
# Prerequisites: sccache and nsc must be on PATH.

set -e

# Check prerequisites
for cmd in sccache nsc; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "error: $cmd not found on PATH" >&2
        return 1 2>/dev/null || exit 1
    fi
done

# Get credentials from Namespace cache
eval "$(nsc cache sccache setup --cache_name stellar)"

# Export so the sccache daemon (and child processes) inherit them
export SCCACHE_WEBDAV_ENDPOINT
export SCCACHE_WEBDAV_KEY_PREFIX
export SCCACHE_WEBDAV_TOKEN
export SCCACHE_IDLE_TIMEOUT=0
export RUSTC_WRAPPER=sccache

# In GitHub Actions, propagate to subsequent steps and mask the token
if [ -n "$GITHUB_ENV" ]; then
    echo "::add-mask::$SCCACHE_WEBDAV_TOKEN"
    echo "SCCACHE_WEBDAV_ENDPOINT=$SCCACHE_WEBDAV_ENDPOINT" >> "$GITHUB_ENV"
    echo "SCCACHE_WEBDAV_KEY_PREFIX=$SCCACHE_WEBDAV_KEY_PREFIX" >> "$GITHUB_ENV"
    echo "SCCACHE_WEBDAV_TOKEN=$SCCACHE_WEBDAV_TOKEN" >> "$GITHUB_ENV"
    echo "SCCACHE_IDLE_TIMEOUT=0" >> "$GITHUB_ENV"
    echo "RUSTC_WRAPPER=sccache" >> "$GITHUB_ENV"
fi

# (Re)start the sccache daemon with the correct environment
sccache --stop-server 2>/dev/null || true
sccache --start-server

echo "sccache configured with Namespace remote cache:"
sccache -s
