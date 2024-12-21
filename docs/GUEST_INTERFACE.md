# Guest interface

Soroban provides an abstract interface `Env`(../soroban-env-common/src/env.rs) -- called "the environment interface" or "the env interface" -- which is implemented by two concrete types:

  - The Soroban [`Host`](../soroban-env-host/src/host.rs) which is compiled to native code, and houses (at runtime) various [_host objects_](../soroban-env-host/src/host/host_object.rs) as well as the implementation of many _host functions_, many of which operate on host objects.
  - The Soroban [`Guest`](../soroban-env-guest/src/guest.rs) which is compiled to Wasm and simply _declares_ the same set of host functions, as Wasm imports.

This document describes the interface _from the perspective of the guest_, explaining only as much about the host implementation as necessary to understand and interact with the interface. Its intended audience is someone writing an SDK for a new programming language that can compile to Wasm, who wants to import and use elements of the guest interface.

## Data Representation

The most important datatype in Soroban is [`Val`](../soroban-env-common/src/val.rs). This type is represented in Wasm as a 64 bit integer, but is a bit-packed union that can represent various data types, either inline or with a numeric "handle" that refers to some host object. `Val`s have two main parts:

- **Tag Bits**: The lower 8 bits of a `Val` are used as a `Tag` to indicate its type.
- **Body Bits**: The high 56 bits of a `Val` are interpreted differently depending on the tag.

The 56 bit _body_ of a `Val` is, in several cases, further subdivided into:
- A 32 bit **major** component, in the high 32 bits.
- A 28 bit **minor** component, in the 28 bits betwen the major component and the tag bits.

In other words, the bit-level encoding of a `Val` looks like the following:

```
 6       5       4       4       3       2       1
 4       6       8       0       2       4       6       8       0 
 +-------+-------+-------+-------+-------+-------+-------+-------+
 |            body.major         |       body.minor      |  tag  |
 +-------------------------------------------------------+-------+
```

The set of types a `Val` can represent are:
- A designated `Void` value.
- Boolean values (`True` and `False`).
- A set of [`Error` values](../soroban-env-common/src/error.rs), with the minor 28 bits as a broad "error type" and the major 32 bits as a per-error-type error code value. One error type is dedicated to contract error codes, and two error types are designated as unreoverable. The rest are reserved for indicating errors in the host.
- [`Symbol`](../soroban-env-common/src/symbol.rs)s, taken from a 64 character repertoire (`[a-zA-Z0-9_]`), in two forms:
  - Those 9 characters or less are bit-packed into the body of a `Val` directly
  - Those 10 characters or longer (up to 32 characters) must be allocated as host objects and handled by reference.
- Several sub-types of [signed and unsigned integers](../soroban-env-common/src/num.rs), in various forms:
  - If the integer type is 32 bits, it is held directly in the high 32 bit major component of the body.
  - If the integer type is larger (64, 128 or 256 bits), there are two sub-cases:
    - If the _actual numeric value_ of the larger integer fits in 56 bits, it is tagged as a "small" integer and its value is held directly in the `Val` body
    - Otherwise the integer must be allocated as a host object and handled indirectly by handle.
  - Special versions of 64 bit integers tagged as `Timepoint` and `Duration`, as hints to display formatting and to help catch errors in relative vs. absolute time. 
- [`String`](../soroban-env-common/src/string.rs) and [`Bytes`](../soroban-env-common/src/bytes.rs) types, both of which always carry handles to host objects. The only difference between the two is that the `String` type _suggests_ that it carries text of some sort, typically in some encoding like UTF-8: this is a hint for encoding or display. There is no enforcement of this suggestion: different programming languages support different types of strings and Soroban is agnostic to the encoding.
- Container types: `Map` and `Vec`. These are also always handles to host objects.
- The `Address` type, which is always a handle to a host object.

## XDR representation

The `Val` data structure described above corresponds to the `ScVal` data structure in [defined in the Stellar network's XDR definitions](https://github.com/stellar/stellar-xdr/blob/curr/Stellar-contract.x). It is however not necessary to instantiate the XDR representation as data structures in a language used for writing contracts. Contracts can deal with `Val` objects and leave the XDR to the host in many cases.

Specifically:
  - Contract arguments are submitted to the network in XDR, as `ScVal`s, but the host automatically deserializes and converts them to `Val`s before passing them to contracts as `Val` arguments. When a contract invocation completes and returns a `Val`, this process is reversed and the host automatically serializes the resulting `Val` to an XDR `ScVal` to write into the ledger history and return over the network as the transaction result.
  - Similarly, while all `CONTRACT_DATA` ledger entries are (on disk) stored as XDR `ScVal`s, guests only need to interact with the ledger in terms of `Val`s: calling a host function that loads a ledger entry automatically causes deserialization of the entry's `ScVal` to a `Val`, and calling a host function that saves a `Val` back to the ledger automatically causes serialization to an `ScVal`.

There are some cases where a guest might want to, and can, observe the XDR `ScVal` encoding of a given `Val`. Specifically there is a pair of host functions that can serialize and deserialize XDR held in a `Bytes` object (which can then be transferred to or from guest linear memory). But most of the time this is not necessary.

## Host object handles

All host object handles are stored in the major component (high 32 bits) of a `Val`. The minor component of such a `Val` must be zero.

Host object handles are always allocated by the host and returned to the guest. The guest should not attempt to allocate or manipulate handles.

Handles actually refer to host objects _indirectly_ via translation tables maintained by the host. All such translation happens automatically when the guest passes a handle as an argument to a host function, and cannot be bypassed. Guests cannot refer to handles of objects outside of the translation table, meaning that guests can only access handles that are explicitly passed to them by other guests or returned from a host function.

Moreover during translation, the tag of a `Val` carrying a handle is checked and must match the type of the object that the handle ultimately refers to.

If any of the checks or translations described above fails, an error is returned.

## Host functions

There are many host functions available to guests, both for manipulating individual host objects and also for interacting with common facilities provided by the host.

Host functions are presented to the guest as simple Wasm function imports. Note that the Wasm 
"component model" is not used: it was still incomplete while Soroban was initially being implemented. It is possible a future version of Soroban may try to use the component model, but as of this writing it does not.

Host functions are grouped together into thematic "modules", but only for the sake of organization and naming; there is no other conceptual relevance to the "modules". Each module, and each host function within each module, has a unique single-character name. The short names help limit the size of the import table, which reduces the size of the Wasm image stored in the ledger.

The "module" groupings (with their respective single-character module names) are:
- **Context Functions** ("x"): Functions related to logging, events, and ledger information.
- **Integer Functions** ("i"): Functions for converting between integers and objects.
- **Map Functions** ("m"): Functions for manipulating map objects.
- **Vector Functions** ("v"): Functions for manipulating vector objects.
- **Ledger Functions** ("l"): Functions for storing and retrieving `CONTRACT_DATA` ledger entries.
- **Call Functions** ("d"): Functions for calling other contracts.
- **Buffer Functions** ("b"): Functions for working with `Bytes` objects and linear memory.
- **Crypto Functions** ("c"): Functions for cryptographic operations.
- **Address Functions** ("a"): Functions for handling addresses and authorization.
- **Test Functions** ("t"): Functions for testing purposes.
- **PRNG Functions** ("p"): Functions for pseudo-random number generation.

The full set of host functions is listed in the file [env.json](../soroban-env-common/env.json), which is used to programmatically generate many elements of both the host and guest sides of the environment interface. Each function listed in that file has a full name, short 1-character name, type signature, documentation and Soroban version compatibility range.

## Linear memory functions

Several host functions provide bulk operations on guest linear memory. For example, `bytes_copy_from_linear_memory(b:BytesObject, b_pos:U32Val, lm_pos:U32Val, len:U32Val)` duplicates a given `BytesObject` in the host, copies a block `len` of bytes from `lm_pos` in the guest's linear memory into `b_pos` in the new object, and returns a handle to the new object. All of this happens _in the host_ and does not require any byte-copying loops in the guest.

All host functions that work with linear memory are designed to be safe against malicious inputs from the guest. The worst that can happen is that the guest corrupts _its own_ linear memory, which the host knows nothing about the structure of. The host does not provide the ability to forge host pointers, access host memory, or read or write outside the bounds of the guest's linear memory.

## Control transfers

All contract invocations begin as XDR transactions carrying `ScVal`s. The Soroban host then:
  1. Deserializes the arguments to `Val`s
  2. Looks up the invoked contract instance
  3. Instantiates the corresponding Wasm, providing any host functions the Wasm imports
  4. Looks up the name of the invoked function in the Wasm
  5. Calls the invoked function, passing the `Val`s
  6. Regains control after the function returns, either with a result `Val` or `Error`
  7. Serializes the result as an `ScVal`

An invoked contract function can choose to invoke another contract while it is running (i.e. inbetwen steps 5 and 6). It does this by calling either the `call` or `try_call` host functions. Both types of sub-invocation will cause a nested repetition of steps 2-6, which can invoke another contract, and so on. Eventually either the invocation will complete with success or failure (possibly due to exceeding its resource budget). All contract invocations are finite.

 The only difference between `call` and `try_call` is in `Error` handling. Specifically a call to the `try_call` host function allows a level of fault-tolerance for so-called "recoverable" errors.

 ## Error handling

 The error handling system is quite subtle and worth understanding in careful detail. In general it is designed to default to fail "safely" by propagating errors and causing transactions to abort. It is assumed that in most cases an aborted transaction (which rolls back all side effects) is probably safer than a committed transaction containing a potentially-overlooked error. Non-default handling should therefore only be attempted with care.
 
There are multiple things to understand about the error system:

  - `Val` has a tag case called `Error`, which as mentioned above has both a _type_ and a _code_ value. The types are taken from the enumeration `ScErrorType` and the codes from the enumeration `ScErrorCode`. Those with `ScErrorType::Contract` carry codes that are defined and interpreted by contracts. All other error types and codes are defined and interpreted by the Soroban host. `Error`s are further classified as either "recoverable" or "unrecoverable". The unrecoverable errors include out-of-budget errors and host internal errors. All other errors (including contract errors) are recoverable.

  - The host itself can also generate `Error`s when attempting to perform some action, either because the contract requested the host perform an invalid action (eg. accessing a nonexistent element of a vector), or because the host detected some logic error in the host's own implementation. When the host fails, it creates a native Rust `HostError`, which wraps an `Error`-tagged `Val` carrying the details of the error, but also returns this "out of band" as the `Err` case of a Rust `Result<_, HostError>`.

  - Wasm VMs can fail for a variety of reasons, such as running illegal operation like a divide-by-zero or writing past the end of their linear memory. These failures are called _traps_. If the VM calls a host function, and the host function returns `Result::Err(HostError)`, then that `HostError` generates a VM trap carrying it. Furthermore whenever the Wasm VM is invoked, it returns a Rust `Result` type of its own, that may carry either a `Ok(Val)` or `Err(Trap)`. The `Err(Trap)` case may carry the cause of a VM-initiated trap, or may carry a `HostError` caused by a host function call. Finally, the `Ok(Val)` may carry a `Val` that has tag `Error`, indicating an "in band" error being returned from a successful invocation of the VM. 

  - A contract can choose, whenever it likes, to manufacture a `Val` tagged as `Error`, pass it around, store it in a data structure, or (most sensibly) _return it from invocation_. Doing so creates an "in band" `Error` as described above: the contract "completes successfully" from the perspective of the Wasm VM, but the value it returns is indicative of an error in the contract. The Soroban _host_ then converts such an "in band" `Ok(Error)` result of invocation to an "out of band" `Err(HostError(...))` result. Note that if a contract returns such an "in band" `Error` from invocation, it _must_ be marked as `ScErrorType::Contract`. Any other error type is considered an illegal return value -- an attempt at spoofing the error handling system -- and is translated into `Error(ScErrorType::Context,ScErrorCode::InvalidAction)`.

Finally we are in a position to understand `call` vs. `try_call`:

  - If contract A `call`s contract B, and B returns any error -- in band or out of band -- A will trap, which will cause the invoker of A to receive an error.
  - If contract A `try_call`s contract B, and B returns a _recoverable error_, then A will resume execution _without trapping_. The error from B will be converted to an "in band" `Val` that is tagged as `Error` but is still just a `Val` that contract A can inspect and decide what it wants to do with. If the recovered error is of type `ScErrorType::Contract` it will be passed through to A unmodified. All other recoverable errors are modified to `Error(ScErrorType::Context,ScErrorCode::InvalidAction)`, to obscure the precise system error type in contract B's execution and thereby reduce the future compatibility burdens on the host.