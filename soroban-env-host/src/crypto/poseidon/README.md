# Poseidon and Poseidon2

This directory contains implementations of Poseidon and Poseidon2 hash functions adapted from the reference implementation at https://github.com/HorizenLabs/poseidon2.

The code has been modified with minimal changes for integration with the Soroban host environment (metering, error handling). The core cryptographic algorithms remain unchanged.

Test vectors in `soroban-env-host/src/test/poseidon/` are derived from both the HorizenLabs/poseidon2 repository and the original hadeshash reference implementation at https://extgit.isec.tugraz.at/krypto/hadeshash.

## License

The original HorizenLabs/poseidon2 code is dual-licensed under Apache-2.0 or MIT.

