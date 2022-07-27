// There are two different notions of versioning in this system:
//
//   1. Interface versioning
//   2. Implementation versioning
//
// Interface versioning controls the willingness of a host to _try_ to run a
// _new_ transaction against some contract. For this, a contract embeds an
// interface version number _inside itself_ and the host inspects it before
// starting the contract. If the contract's interface version is outside the
// supported range for a host, the host will fail the transaction with a useful
// error rather than an inscrutable misbehaviour (link error, data corruption,
// etc.) during execution.
//
// During development of the system this number will increase frequently in
// order to notify users of the need to recompile their contracts against
// new-and-incompatible versions of the system-in-development
// system-in-development. Once we declare a commitment to network-wide backwards
// compatibility for contracts, the supported interface version range will
// likely only ever expand to cover newer interfaces, and that range will be
// stored on-chain so that the transactions and contracts accepted-or-rejected
// are identical across all validators.
//
// Implementation versioning is different, and has more to do with ensuring that
// all validators that _do_ decide to execute a transaction -- whether new _or
// old_ -- execute it on an observably-identical software version, such that an
// arbitrarily subtle dependency on implementation quirks does not cause any
// bit-level divergence in the transaction results. Implementation versioning is
// done in stellar-core itself, by maintaining multiple versions of the host
// crate: strictly identical versions for new transactions entering the network,
// and observably-identical ones for replay of historical transactions (with a
// policy to allow expiring old versions that differ only in ways no
// transactions in the public network history actually observe). Implementation
// versioning will also be done by storing a version on-chain that changes by
// consensus, but the host implementation version number is _not_ compiled into
// a contract, since the same contract will be expected to run against multliple
// implementations over a long period of time.

stellar_contract_env_macros::generate_env_meta_consts!(
    interface_version: 2,
);
