use super::Symbol;
use super::{Object, RawVal, Status};
use core::any;

/// Base trait extended by the [Env](crate::Env) trait, providing various special-case
/// functions that do _not_ simply call across cross the guest/host interface.
pub trait EnvBase: Sized + Clone {
    /// Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    /// Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);

    /// Used to clone an environment deeply, not just a handle to it.
    fn deep_clone(&self) -> Self;

    // Helpers for methods that wish to pass Rust lifetime-qualified _slices_
    // into the environment. These are _not_ done via Env trait methods to avoid
    // the need to convert, and thus trust (or validate) "raw numbers" coming
    // through that interface as "potentially pointers in the same address space
    // as the host". This is a bit of a defense-in-depth approach as we _could_
    // just accept "numbers as pointers in our address space" on a codepath that
    // is sure its input is coming from a "trusted" contract, and arrange enough
    // other static safety checks elsewhere in the calling path (eg. in the SDK)
    // to ensure that "all callers are trusted" .. but we want to minimize the
    // chance of future maintainers accidentally violating such an invariant,
    // since getting it wrong would let guest code violate memory safety. So the
    // _only_ interface to passing contract pointers to the host is going to be
    // in EnvBase, not Env, and as a bonus we get lifetime checking for free.

    /// Copy a slice of bytes from the caller's memory into an existing `Bytes`
    /// object the host, returning a new `Bytes`.
    fn bytes_copy_from_slice(&self, b: Object, b_pos: RawVal, mem: &[u8])
        -> Result<Object, Status>;

    /// Copy a slice of bytes from a `Bytes` object in the host into the
    /// caller's memory.
    fn bytes_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) -> Result<(), Status>;

    /// Form a new `Bytes` object in the host from a slice of memory in the
    /// caller.
    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<Object, Status>;

    // As with the bytes functions above, these take _slices_ with definite
    // lifetimes. The first slice is interpreted as a (very restricted)
    // format-string -- containing literal text interspersed with some number of
    // `{}` markers which must match the number of other args passed -- with
    // actual formatting delayed until someone asks to see the event (which may
    // never happen). Other args may be static strings, [RawVal]s, or a mix.
    //
    // When the SDK is built with Env = Host, both the format string slice and
    // all static string slice args (and any [RawVal] args) will be passed
    // through into the debug-event subsystem of the host and _stored_
    // unformatted in the debug buffer, until/unless someone dumps some portion
    // of that buffer out. They are therefore quite cheap -- just pushing static
    // pointers and numbers into the debug buffer -- and can be called fairly
    // ubiquitously to provide details on any interesting diagnostic events
    // and/or errors that occur in either SDK or contract code.
    //
    // When Env = Guest, these currently compile as no-ops. We may change this
    // to record a VM-relative guest static string pointer (similar to how the
    // bytes functions above work) into the debug buffer in the future, but it
    // is a little involved to do so and we assume that VM code probably does
    // not want to be carrying static strings at all.

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// a single [RawVal] argument that will be inserted at the marker in the
    /// format string.
    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// a single string-slice argument that will be inserted at the marker in
    /// the format string.
    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// both a [RawVal] and a string-slice argument, that will each be inserted
    /// at markers in the format string.
    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), Status>;

    /// Log a formatted debugging message to the debug log (if present), passing
    /// a simplified format string (supporting only positional `{}` markers) and
    /// both a slice of [RawVal]s and a slice of string-slice argument, that
    /// will be sequentially inserted at markers in the format string.
    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), Status>;
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro definition
///////////////////////////////////////////////////////////////////////////////

// The set of host functions need to be statically reflected-on in a variety of
// contexts (both in this crate and elsewhere in the guest and host crates), so
// we define them through an x-macro (a macro that calls a user-provided macro)
// and call the x-macro from all such contexts.
//
// How this macro works:
//  - It exports a higher-order "x-macro" called
//    call_macro_with_all_host_functions
//  - The x-macro takes the name of some callback macro to call
//  - The x-macro invokes the callback macro once, passing a single large token
//    tree, seen below in the body of the x-macro
//
// To use this macro:
//  - Call sites define a callback macro that matches on the token-tree
//  - Call sites invoke the x-macro passing their callback macro name
//
// The token-tree being passed is arbitrary, but is chosen to satisfy 3
// criteria:
//  - It's relatively easy to read, edit and understand its content
//  - It's easy to decompose with pattern-matching in the callback macros
//  - It contains everything any callback macro wants to match and use
//
// All callback macros have essentially the same token-tree matcher part,
// only their expansion parts differ.

#[doc(hidden)]
#[macro_export]
macro_rules! call_macro_with_all_host_functions {

    // The x-macro takes a single ident, the name of a macro to call ...
    {$macro_to_call_back:ident} => {

        // ... and just calls it back, passing a single large token-tree.
        $macro_to_call_back! {

            // The token-tree we pass to the callback is a sequence of
            // blocks that have the following structure:
            //
            //  mod $mod_id:ident $mod_str:literal {
            //     ...
            //     { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
            //     ...
            //  }
            //
            // Where the sub token-tree $args:tt is a normal parenthesized
            // argument list of comma-separated arg:type pairs

            mod context "x" {
                // This one variant of logging does not take a format string and
                // is live in both Env=Guest and Env=Host configurations.
                {"_", fn log_value(v:RawVal) -> RawVal }
                /// Get the contractID `Bytes` of the contract which invoked the
                /// running contract. Traps if the running contract was not
                /// invoked by a contract.
                {"0", fn get_invoking_contract() -> Object }
                {"1", fn obj_cmp(a:RawVal, b:RawVal) -> i64 }
                /// Records a contract event. `topics` is expected to be a `SCVec` with
                /// length <= 4 that cannot contain `Vec`, `Map`, or `Bytes` with length > 32
                /// On success, returns an `SCStatus::Ok`.
                {"2", fn contract_event(topics:Object, data:RawVal) -> RawVal }
                /// Get the contractID `Bytes` of the contract which invoked the
                /// running contract. Traps if the running contract was not
                /// invoked by a contract.
                {"3", fn get_current_contract() -> Object }
                /// Return the protocol version of the current ledger as a u32.
                {"4", fn get_ledger_version() -> RawVal }
                /// Return the sequence number of the current ledger as a u32.
                {"5", fn get_ledger_sequence() -> RawVal }
                /// Return the timestamp number of the current ledger as a u64.
                {"6", fn get_ledger_timestamp() -> Object }
                /// Return the network passphrase of the current ledger as `Bytes`.
                {"7", fn get_ledger_network_passphrase() -> Object }
                /// Returns the full call stack from the first contract call
                /// to the current one as a vector of vectors, where the inside
                /// vector contains the contract id as Hash, and a function as
                /// a Symbol.
                {"8", fn get_current_call_stack() -> Object }
                /// Causes the currently executing contract to fail immediately
                /// with a provided status code, which must be of error-type
                /// `ScStatusType::ContractError`. Does not actually return.
                {"9", fn fail_with_status(status:Status) -> RawVal }
                // Record a debug event. Fmt must be a Bytes. Args must be a
                // Vec. Void is returned.
                {"a", fn log_fmt_values(fmt:Object, args:Object) -> RawVal }
                /// Get whether the contract invocation is from an account or
                /// another contract. Returns 0 for account, 1 for contract.
                {"b", fn get_invoker_type() -> u64 }
                /// Get the AccountID object type of the account which invoked
                /// the running contract. Traps if the running contract was not
                /// invoked by an account.
                {"c", fn get_invoking_account() -> Object }
                /// Return the network id (sha256 hash of network passphrase) of
                /// the current ledger as `Bytes`. The value is always 32 bytes
                /// in length.
                {"d", fn get_ledger_network_id() -> Object }
            }

            mod u64 "u" {
                {"_", fn obj_from_u64(v:u64) -> Object }
                {"0", fn obj_to_u64(obj:Object) -> u64 }
            }

            /// Functions concerned with the i64 type
            mod i64 "i" {
                /// Convert an i64 to an object containing an i64.
                {"_", fn obj_from_i64(v:i64) -> Object }
                /// Convert an object containing an i64 to an i64.
                {"0", fn obj_to_i64(obj:Object) -> i64 }
            }

            mod map "m" {
                /// Create an empty new map.
                {"_", fn map_new() -> Object }
                /// Insert a key/value mapping into an existing map, and return the map object handle.
                /// If the map already has a mapping for the given key, the previous value is overwritten.
                {"0", fn map_put(m:Object, k:RawVal, v:RawVal) -> Object}
                /// Get the value for a key from a map. Traps if key is not found.
                {"1", fn map_get(m:Object, k:RawVal) -> RawVal}
                /// Remove a key/value mapping from a map if it exists, traps if doesn't.
                {"2", fn map_del(m:Object, k:RawVal) -> Object}
                /// Get the size of a map.
                {"3", fn map_len(m:Object) -> RawVal}
                /// Test for the presence of a key in a map. Returns (SCStatic) TRUE/FALSE.
                {"4", fn map_has(m:Object, k:RawVal) -> RawVal}
                /// Given a key, find the first key less than itself in the map's sorted order.
                /// If such a key does not exist, return an SCStatus containing the error code (TBD).
                {"5", fn map_prev_key(m:Object, k:RawVal) -> RawVal}
                /// Given a key, find the first key greater than itself in the map's sorted order.
                /// If such a key does not exist, return an SCStatus containing the error code (TBD).
                {"6", fn map_next_key(m:Object, k:RawVal) -> RawVal}
                /// Find the minimum key from a map.
                /// If the map is empty, return an SCStatus containing the error code (TBD).
                {"7", fn map_min_key(m:Object) -> RawVal}
                /// Find the maximum key from a map.
                /// If the map is empty, return an SCStatus containing the error code (TBD).
                {"8", fn map_max_key(m:Object) -> RawVal}
                /// Return a new vector containing all the keys in a map.
                /// The new vector is ordered in the original map's key-sorted order.
                {"9", fn map_keys(m:Object) -> Object}
                /// Return a new vector containing all the values in a map.
                /// The new vector is ordered in the original map's key-sorted order.
                {"A", fn map_values(m:Object) -> Object}
            }

            mod vec "v" {
                /// Creates a new vector with an optional capacity hint `c`.
                /// If `c` is `ScStatic::Void`, no hint is assumed and the new vector is empty.
                /// Otherwise, `c` is parsed as an `u32` that represents the initial capacity of the new vector.
                {"_", fn vec_new(c:RawVal) -> Object}
                /// Update the value at index `i` in the vector. Return the new vector.
                /// Trap if the index is out of bounds.
                {"0", fn vec_put(v:Object, i:RawVal, x:RawVal) -> Object}
                /// Returns the element at index `i` of the vector. Traps if the index is out of bound.
                {"1", fn vec_get(v:Object, i:RawVal) -> RawVal}
                /// Delete an element in a vector at index `i`, shifting all elements after it to the left.
                /// Return the new vector. Traps if the index is out of bound.
                {"2", fn vec_del(v:Object, i:RawVal) -> Object}
                /// Returns length of the vector.
                {"3", fn vec_len(v:Object) -> RawVal}
                /// Push a value to the front of a vector.
                {"4", fn vec_push_front(v:Object, x:RawVal) -> Object}
                /// Removes the first element from the vector and returns the new vector.
                /// Traps if original vector is empty.
                {"5", fn vec_pop_front(v:Object) -> Object}
                /// Appends an element to the back of the vector.
                {"6", fn vec_push_back(v:Object, x:RawVal) -> Object}
                /// Removes the last element from the vector and returns the new vector.
                /// Traps if original vector is empty.
                {"7", fn vec_pop_back(v:Object) -> Object}
                /// Return the first element in the vector. Traps if the vector is empty
                {"8", fn vec_front(v:Object) -> RawVal}
                /// Return the last element in the vector. Traps if the vector is empty
                {"9", fn vec_back(v:Object) -> RawVal}
                /// Inserts an element at index `i` within the vector, shifting all elements after it to the right.
                /// Traps if the index is out of bound
                {"A", fn vec_insert(v:Object, i:RawVal, x:RawVal) -> Object}
                /// Clone the vector `v1`, then moves all the elements of vector `v2` into it.
                /// Return the new vector. Traps if number of elements in the vector overflows a u32.
                {"B", fn vec_append(v1:Object, v2:Object) -> Object}
                /// Copy the elements from `start` index until `end` index, exclusive, in the vector and create a new vector from it.
                /// Return the new vector. Traps if the index is out of bound.
                {"C", fn vec_slice(v:Object, start:RawVal, end:RawVal) -> Object}
                /// Get the index of the first occurrence of a given element in the vector.
                /// Returns the u32 index of the value if it's there. Otherwise, it returns `ScStatic::Void`.
                {"D", fn vec_first_index_of(v:Object, x:RawVal) -> RawVal}
                /// Get the index of the last occurrence of a given element in the vector.
                /// Returns the u32 index of the value if it's there. Otherwise, it returns `ScStatic::Void`.
                {"E", fn vec_last_index_of(v:Object, x:RawVal) -> RawVal}
                /// Binary search a sorted vector for a given element.
                /// If it exists, the high-32 bits of the return value is 0x0001 and the low-32 bits
                /// contain the u32 index of the element.
                /// If it does not exist, the high-32 bits of the return value is 0x0000 and the low-32 bits
                /// contain the u32 index at which the element would need to be inserted into the vector to
                /// maintain sorted order.
                {"F", fn vec_binary_search(v:Object, x:RawVal) -> u64}
            }

            mod ledger "l" {
                {"_", fn put_contract_data(k:RawVal, v: RawVal) -> RawVal}
                {"0", fn has_contract_data(k:RawVal) -> RawVal}
                {"1", fn get_contract_data(k:RawVal) -> RawVal}
                {"2", fn del_contract_data(k:RawVal) -> RawVal}
                {"3", fn create_contract_from_ed25519(v: Object, salt: Object, key: Object, sig: Object) -> Object}
                {"4", fn create_contract_from_contract(v: Object, salt: Object) -> Object}
                {"5", fn create_token_from_ed25519(salt: Object, key: Object, sig: Object) -> Object}
                {"6", fn create_token_from_contract(salt: Object) -> Object}
                {"7", fn create_token_from_asset(asset: Object) -> Object}
                /// Create a contract using the source account and salt as input
                /// to the contract ID that gets created. Pass as arguments a
                /// Bytes for the wasm code and a Bytes for the salt. Returned
                /// will be a Bytes of length 32 bytes.
                {"8", fn create_contract_from_source_account(v: Object, salt: Object) -> Object}
                /// Create a token contract using the source account and salt
                /// as input to the contract ID that gets created. Pass as
                /// arguments a Bytes for the salt. Returned
                /// will be a Bytes of length 32 bytes.
                {"9", fn create_token_from_source_account(salt: Object) -> Object}
            }

            mod call "d" {
                /// Calls a function in another contract with arguments contained in vector `args`.
                /// If the call is successful, forwards the result of the called function. Traps otherwise.
                {"_", fn call(contract:Object, func:Symbol, args:Object) -> RawVal}
                /// Calls a function in another contract with arguments contained in vector `args`. Returns:
                /// - if successful, result of the called function.
                /// - otherwise, an `SCStatus` containing the error status code.
                {"0", fn try_call(contract:Object, func:Symbol, args:Object) -> RawVal}
            }

            mod bigint "g" {
                /// Constructs a BigInt from an u64.
                {"_", fn bigint_from_u64(x:u64) -> Object}
                /// Converts a BigInt to an u64. Traps if the value cannot fit into u64.
                {"0", fn bigint_to_u64(x:Object) -> u64}
                /// Constructs a BigInt from an i64.
                {"1", fn bigint_from_i64(x:i64) -> Object}
                /// Converts a BigInt to an i64. Traps if the value cannot fit into i64.
                {"2", fn bigint_to_i64(x:Object) -> i64}
                /// Performs the `+` operation.
                {"3", fn bigint_add(x:Object, y:Object) -> Object}
                /// Performs the `-` operation.
                {"4", fn bigint_sub(x:Object, y:Object) -> Object}
                /// Performs the `*` operation.
                {"5", fn bigint_mul(x:Object, y:Object) -> Object}
                /// Performs the `/` operation. Traps if `y` is zero.
                {"6", fn bigint_div(x:Object, y:Object) -> Object}
                /// Performs the `%` operation. Traps if `y` is zero.
                {"7", fn bigint_rem(x:Object, y:Object) -> Object}
                /// Performs the `&` operation.
                {"8", fn bigint_and(x:Object, y:Object) -> Object}
                /// Performs the `|` operation.
                {"9", fn bigint_or(x:Object, y:Object) -> Object}
                /// Performs the `^` operation.
                {"A", fn bigint_xor(x:Object, y:Object) -> Object}
                /// Performs the `<<` operation. Traps if `y` is negative or larger than the size of u64.
                {"B", fn bigint_shl(x:Object, y:Object) -> Object}
                /// Performs the `>>` operation. Traps if `y` is negative or larger than the size of u64.
                {"C", fn bigint_shr(x:Object, y:Object) -> Object}
                /// Returns true if `x` is equal to the additive identity.
                {"D", fn bigint_is_zero(x:Object) -> RawVal}
                /// Performs the unary `-` operation.
                {"E", fn bigint_neg(x:Object) -> Object}
                /// Performs the unary `!` operation.
                {"F", fn bigint_not(x:Object) -> Object}
                /// Calculates the Greatest Common Divisor (GCD) of `x` and `y`.
                {"G", fn bigint_gcd(x:Object, y:Object) -> Object}
                /// Calculates the Lowest Common Multiple (LCM) of `x` and `y`.
                {"H", fn bigint_lcm(x:Object, y:Object) -> Object}
                /// Calculates `x` to the power `y`. Traps if `y` is negative or larger than the size of u64.
                {"I", fn bigint_pow(x:Object, y:Object) -> Object}
                /// Calculates `(p ^ q) mod m`. Note that this rounds like `mod_floor`, not like the `%` operator, which makes a difference when given a negative `p` or `m`.
                /// The result will be in the interval `[0, m)` for `m > 0`, or in the interval `(m, 0]` for `m < 0`.
                /// Traps if the `q` is negative or the `m` is zero.
                {"J", fn bigint_pow_mod(p:Object, q:Object, m:Object) -> Object}
                /// Calculates the truncated principal square root of `x`. Traps if `x` is negative.
                {"K", fn bigint_sqrt(x:Object) -> Object}
                /// Determines the fewest bits necessary to express `x`, not including the sign.
                {"L", fn bigint_bits(x:Object) -> u64}
                /// Outputs the BigInt's magnitude in big-endian byte order into a byte array. The sign is dropped.
                {"M", fn bigint_to_bytes_be(x:Object) -> Object}
                /// Outputs the BigInt's magnitude in the requested base in big-endian digit order into a byte array.
                /// The sign is dropped. Radix must be in the range 2...256.
                {"N", fn bigint_to_radix_be(x:Object, radix:RawVal) -> Object}
                /// Creates a BigInt from a byte array and i32 sign.
                /// Bytes are in big-endian order. Sign is interpreted: -1 as negative, 0 as zero, 1 as positive
                /// If sign is 0, then the input bytes are ignored and will return a BigInt of 0.
                {"O", fn bigint_from_bytes_be(sign:RawVal, bytes:Object) -> Object}
                /// Creates a BigInt from a byte array `buf`, an i32 sign and an u32 radix.
                /// Each u8 of the byte array is interpreted as one digit of the number and
                /// must therefore be less than the radix. The bytes are in big-endian byte order.
                /// Radix must be in the range 2..=256. Sign follows same rule as in `bigint_from_bytes_be`.
                {"P", fn bigint_from_radix_be(sign:RawVal, buf:Object, radix:RawVal) -> Object}

            }

            mod bytes "b" {
                /// Serializes an (SC)Val into XDR opaque `Bytes` object.
                {"_", fn serialize_to_bytes(v:RawVal) -> Object}
                /// Deserialize a `Bytes` object to get back the (SC)Val.
                {"0", fn deserialize_from_bytes(b:Object) -> RawVal}
                /// Copies a slice of bytes from a `Bytes` object specified at offset `b_pos` with
                /// length `len` into the linear memory at position `lm_pos`.
                /// Traps if either the `Bytes` object or the linear memory doesn't have enough bytes.
                {"1", fn bytes_copy_to_linear_memory(b:Object, b_pos:RawVal, lm_pos:RawVal, len:RawVal) -> RawVal}
                /// Copies a segment of the linear memory specified at position `lm_pos` with
                /// length `len`, into a `Bytes` object at offset `b_pos`. The `Bytes` object may
                /// grow in size to accommodate the new bytes.
                /// Traps if the linear memory doesn't have enough bytes.
                {"2", fn bytes_copy_from_linear_memory(b:Object, b_pos:RawVal, lm_pos:RawVal, len:RawVal) -> Object}
                /// Constructs a new `Bytes` object initialized with bytes copied from a linear memory slice specified at position `lm_pos` with length `len`.
                {"3", fn bytes_new_from_linear_memory(lm_pos:RawVal, len:RawVal) -> Object}
                // These functions below ($3-$F) mirror vector operations
                /// Create an empty new `Bytes` object.
                {"4", fn bytes_new() -> Object}
                /// Update the value at index `i` in the `Bytes` object. Return the new `Bytes`.
                /// Trap if the index is out of bounds.
                {"5", fn bytes_put(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Returns the element at index `i` of the `Bytes` object. Traps if the index is out of bound.
                {"6", fn bytes_get(b:Object, i:RawVal) -> RawVal}
                /// Delete an element in a `Bytes` object at index `i`, shifting all elements after it to the left.
                /// Return the new `Bytes`. Traps if the index is out of bound.
                {"7", fn bytes_del(b:Object, i:RawVal) -> Object}
                /// Returns length of the `Bytes` object.
                {"8", fn bytes_len(b:Object) -> RawVal}
                /// Appends an element to the back of the `Bytes` object.
                {"9", fn bytes_push(b:Object, u:RawVal) -> Object}
                /// Removes the last element from the `Bytes` object and returns the new `Bytes`.
                /// Traps if original `Bytes` is empty.
                {"A", fn bytes_pop(b:Object) -> Object}
                /// Return the first element in the `Bytes` object. Traps if the `Bytes` is empty
                {"B", fn bytes_front(b:Object) -> RawVal}
                /// Return the last element in the `Bytes` object. Traps if the `Bytes` is empty
                {"C", fn bytes_back(b:Object) -> RawVal}
                /// Inserts an element at index `i` within the `Bytes` object, shifting all elements after it to the right.
                /// Traps if the index is out of bound
                {"D", fn bytes_insert(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Clone the `Bytes` object `b1`, then moves all the elements of `Bytes` object `b2` into it.
                /// Return the new `Bytes`. Traps if its length overflows a u32.
                {"E", fn bytes_append(b1:Object, b2:Object) -> Object}
                /// Copies the elements from `start` index until `end` index, exclusive, in the `Bytes` object and creates a new `Bytes` from it.
                /// Returns the new `Bytes`. Traps if the index is out of bound.
                {"F", fn bytes_slice(b:Object, start:RawVal, end:RawVal) -> Object}
            }

            mod hash "h" {
                {"_", fn hash_from_bytes(x:Object) -> Object}
                {"0", fn hash_to_bytes(x:Object) -> Object}
            }

            mod key "k" {
                {"_", fn public_key_from_bytes(x:Object) -> Object}
                {"0", fn public_key_to_bytes(x:Object) -> Object}
            }

            mod crypto "c" {
                {"_", fn compute_hash_sha256(x:Object) -> Object}
                {"0", fn verify_sig_ed25519(x:Object, k:Object, s:Object) -> RawVal}
            }

            mod account "a" {
                /// Get the low threshold for the account with ID `a` (`a` is
                /// `AccountId`). Traps if no such account exists.
                {"_", fn account_get_low_threshold(a:Object) -> RawVal}
                /// Get the medium threshold for the account with ID `a` (`a` is
                /// `AccountId`). Traps if no such account exists.
                {"0", fn account_get_medium_threshold(a:Object) -> RawVal}
                /// Get the high threshold for the account with ID `a` (`a` is
                /// `AccountId`). Traps if no such account exists.
                {"1", fn account_get_high_threshold(a:Object) -> RawVal}
                /// Get the signer weight for the signer with ed25519 public key
                /// `s` (`s` is `Bytes`) on the account with ID `a` (`a`
                /// is `AccountId`). Returns the master weight if the signer is the
                /// master, and returns 0 if no such signer exists. Traps if no
                /// such account exists.
                {"2", fn account_get_signer_weight(a:Object, s:Object) -> RawVal}
                /// Given an ID `a` (`a` is `AccountId`) of an account, check if
                /// it exists. Returns (SCStatic) TRUE/FALSE.
                {"3", fn account_exists(a:Object) -> RawVal}
            }

            mod test "t" {
                /// A dummy function taking 0 arguments and performs no-op.
                /// This function is for test purpose only, for measuring the roundtrip cost of
                /// invoking a host function, i.e. host->Vm->host.
                {"_", fn dummy0() -> RawVal}
            }
        }
    };
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: defining trait Env
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by generate_env_trait below. It consumes
// a token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method declaration to be used in the Env
// trait.
macro_rules! host_function_helper {
    {
        $(#[$attr:meta])*
        fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        $(#[$attr])*
        fn $fn_id(&self, $($arg:$type),*) -> $ret;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of method
// declarations, which it places in the body of the declaration of the Env
// trait.
macro_rules! generate_env_trait {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            $(#[$mod_attr:meta])*
            mod $mod_id:ident $mod_str:literal
            {
                $(
                    // This inner pattern matches a single function description
                    // inside a 'mod' block in the token-tree passed from the
                    // x-macro to this macro. It is embedded in a `$()*`
                    // pattern-repetition matcher so that it will match all such
                    // descriptions.
                    $(#[$fn_attr:meta])*
                    { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
                )*
            }
        )*
    }

    => // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: the Env trait.

        /// This trait represents the interface between Host and Guest, used by
        /// client contract code and implemented (via [CheckedEnv](crate::CheckedEnv)) by the host.
        /// It consists of functions that take or return only 64-bit values such
        /// as [RawVal] or [u64].
        pub trait Env: EnvBase
        {
            $(
                $(
                    // This invokes the host_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the Env trait.
                    host_function_helper!{$(#[$fn_attr])* fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { generate_env_trait }
