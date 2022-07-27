use super::Symbol;
use super::{Object, RawVal};
use core::any;

pub trait EnvBase: Sized + Clone {
    // Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    // Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);

    // Used to clone an environment deeply, not just a handle to it.
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
    fn binary_copy_from_slice(&self, b: Object, b_pos: RawVal, mem: &[u8]) -> Object;
    fn binary_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]);
    fn binary_new_from_slice(&self, mem: &[u8]) -> Object;

    // As with the binary functions above, these take _slices_ with definite
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
    // binary functions above work) into the debug buffer in the future, but it
    // is a little involved to do so and we assume that VM code probably does
    // not want to be carrying static strings at all.
    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal);
    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str);
    fn log_static_fmt_val_static_str(&self, fmt: &'static str, v: RawVal, s: &'static str);
    fn log_static_fmt_general(&self, fmt: &'static str, vals: &[RawVal], strs: &[&'static str]);
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

                /// Get the binary contractID of the contract which invoked the
                /// running contract. Traps if the running contract was not
                /// invoked by a contract.
                {"0", fn get_invoking_contract() -> Object }
                {"1", fn obj_cmp(a:RawVal, b:RawVal) -> i64 }
                {"2", fn contract_event(v:RawVal) -> RawVal }
                {"3", fn system_event(v:RawVal) -> RawVal }
                /// Get the binary contractID of the contract which invoked the
                /// running contract. Traps if the running contract was not
                /// invoked by a contract.
                {"4", fn get_current_contract() -> Object }
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
                /// Appends an element to the back of the vector.
                {"4", fn vec_push(v:Object, x:RawVal) -> Object}
                /// Removes the last element from the vector and returns the new vector.
                /// Traps if original vector is empty.
                {"5", fn vec_pop(v:Object) -> Object}
                /// Return the first element in the vector. Traps if the vector is empty
                {"6", fn vec_front(v:Object) -> RawVal}
                /// Return the last element in the vector. Traps if the vector is empty
                {"7", fn vec_back(v:Object) -> RawVal}
                /// Inserts an element at index `i` within the vector, shifting all elements after it to the right.
                /// Traps if the index is out of bound
                {"8", fn vec_insert(v:Object, i:RawVal, x:RawVal) -> Object}
                /// Clone the vector `v1`, then moves all the elements of vector `v2` into it.
                /// Return the new vector. Traps if number of elements in the vector overflows a u32.
                {"9", fn vec_append(v1:Object, v2:Object) -> Object}
                /// Copy the elements from `start` index until `end` index, exclusive, in the vector and create a new vector from it.
                /// Return the new vector. Traps if the index is out of bound.
                {"A", fn vec_slice(v:Object, start:RawVal, end:RawVal) -> Object}
            }

            mod ledger "l" {
                {"_", fn put_contract_data(k:RawVal, v: RawVal) -> RawVal}
                {"0", fn has_contract_data(k:RawVal) -> RawVal}
                {"1", fn get_contract_data(k:RawVal) -> RawVal}
                {"2", fn del_contract_data(k:RawVal) -> RawVal}
                {"3", fn create_contract_from_ed25519(v: Object, salt: Object, key: Object, sig: Object) -> Object}
                {"4", fn create_contract_from_contract(v: Object, salt: Object) -> Object}
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
                /// Returns an ordering between `x` and `y`: -1 (less), 0 (equal) or 1 (greater).
                {"D", fn bigint_cmp(x:Object, y:Object) -> RawVal}
                /// Returns true if `x` is equal to the additive identity.
                {"E", fn bigint_is_zero(x:Object) -> RawVal}
                /// Performs the unary `-` operation.
                {"F", fn bigint_neg(x:Object) -> Object}
                /// Performs the unary `!` operation.
                {"G", fn bigint_not(x:Object) -> Object}
                /// Calculates the Greatest Common Divisor (GCD) of `x` and `y`.
                {"H", fn bigint_gcd(x:Object, y:Object) -> Object}
                /// Calculates the Lowest Common Multiple (LCM) of `x` and `y`.
                {"I", fn bigint_lcm(x:Object, y:Object) -> Object}
                /// Calculates `x` to the power `y`. Traps if `y` is negative or larger than the size of u64.
                {"J", fn bigint_pow(x:Object, y:Object) -> Object}
                /// Calculates `(p ^ q) mod m`. Note that this rounds like `mod_floor`, not like the `%` operator, which makes a difference when given a negative `p` or `m`.
                /// The result will be in the interval `[0, m)` for `m > 0`, or in the interval `(m, 0]` for `m < 0`.
                /// Traps if the `q` is negative or the `m` is zero.
                {"K", fn bigint_pow_mod(p:Object, q:Object, m:Object) -> Object}
                /// Calculates the truncated principal square root of `x`. Traps if `x` is negative.
                {"L", fn bigint_sqrt(x:Object) -> Object}
                /// Determines the fewest bits necessary to express `x`, not including the sign.
                {"M", fn bigint_bits(x:Object) -> u64}
                /// Outputs the BigInt's magnitude in big-endian byte order into a binary array. The sign is dropped.
                {"N", fn bigint_to_bytes_be(x:Object) -> Object}
                /// Outputs the BigInt's magnitude in the requested base in big-endian digit order into a binary array.
                /// The sign is dropped. Radix must be in the range 2...256.
                {"O", fn bigint_to_radix_be(x:Object, radix:RawVal) -> Object}
            }

            mod binary "b" {
                /// Serializes an (SC)Val into XDR opaque binary array.
                {"_", fn serialize_to_binary(v:RawVal) -> Object}
                /// Deserialize a binary array to get back the (SC)Val.
                {"0", fn deserialize_from_binary(b:Object) -> RawVal}
                /// Copies a slice of bytes from a binary array specified at offset `b_pos` with length `len` into the linear memory at position `lm_pos`.
                /// Traps if either the binary array or the linear memory doesn't have enough bytes.
                {"1", fn binary_copy_to_linear_memory(b:Object, b_pos:RawVal, lm_pos:RawVal, len:RawVal) -> RawVal}
                /// Copies a segment of the linear memory specified at position `lm_pos` with length `len`, into a binary array at offset `b_pos`. The binary array may grow in size to accommodate the new bytes.
                /// Traps if the linear memory doesn't have enough bytes.
                {"2", fn binary_copy_from_linear_memory(b:Object, b_pos:RawVal, lm_pos:RawVal, len:RawVal) -> Object}
                /// Constructs a new binary array initialized with bytes copied from a linear memory slice specified at position `lm_pos` with length `len`.
                {"3", fn binary_new_from_linear_memory(lm_pos:RawVal, len:RawVal) -> Object}
                // These functions below ($3-$F) mirror vector operations
                /// Create an empty new binary.
                {"4", fn binary_new() -> Object}
                /// Update the value at index `i` in the binary. Return the new binary.
                /// Trap if the index is out of bounds.
                {"5", fn binary_put(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Returns the element at index `i` of the binary. Traps if the index is out of bound.
                {"6", fn binary_get(b:Object, i:RawVal) -> RawVal}
                /// Delete an element in a binary at index `i`, shifting all elements after it to the left.
                /// Return the new binary. Traps if the index is out of bound.
                {"7", fn binary_del(b:Object, i:RawVal) -> Object}
                /// Returns length of the binary.
                {"8", fn binary_len(b:Object) -> RawVal}
                /// Appends an element to the back of the binary.
                {"9", fn binary_push(b:Object, u:RawVal) -> Object}
                /// Removes the last element from the binary and returns the new binary.
                /// Traps if original binary is empty.
                {"A", fn binary_pop(b:Object) -> Object}
                /// Return the first element in the binary. Traps if the binary is empty
                {"B", fn binary_front(b:Object) -> RawVal}
                /// Return the last element in the binary. Traps if the binary is empty
                {"C", fn binary_back(b:Object) -> RawVal}
                /// Inserts an element at index `i` within the binary, shifting all elements after it to the right.
                /// Traps if the index is out of bound
                {"D", fn binary_insert(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Clone the binary `b1`, then moves all the elements of binary `b2` into it.
                /// Return the new binary. Traps if number of elements in the binary overflows a u32.
                {"E", fn binary_append(b1:Object, b2:Object) -> Object}
                /// Copies the elements from `start` index until `end` index, exclusive, in the binary and creates a new binary from it.
                /// Returns the new binary. Traps if the index is out of bound.
                {"F", fn binary_slice(b:Object, start:RawVal, end:RawVal) -> Object}
            }

            mod hash "h" {
                {"_", fn hash_from_binary(x:Object) -> Object}
                {"0", fn hash_to_binary(x:Object) -> Object}
            }

            mod key "k" {
                {"_", fn public_key_from_binary(x:Object) -> Object}
                {"0", fn public_key_to_binary(x:Object) -> Object}
            }

            mod crypto "c" {
                {"_", fn compute_hash_sha256(x:Object) -> Object}
                {"0", fn verify_sig_ed25519(x:Object, k:Object, s:Object) -> RawVal}
            }

            mod account "a" {
                /// Get the low threshold for the account with ed25519 public
                /// key a (a is binary). Traps if no such account exists.
                {"_", fn account_get_low_threshold(a:Object) -> RawVal}
                /// Get the medium threshold for the account with ed25519 public
                /// key a (a is binary). Traps if no such account exists.
                {"0", fn account_get_medium_threshold(a:Object) -> RawVal}
                /// Get the high threshold for the account with ed25519 public
                /// key a (a is binary). Traps if no such account exists.
                {"1", fn account_get_high_threshold(a:Object) -> RawVal}
                /// Get the signer weight for the signer with ed25519 public key
                /// s (s is binary) on the account with ed25519 public key a (a
                /// is binary). Returns the master weight if the signer is the
                /// master, and returns 0 if no such signer exists. Traps if no
                /// such account exists.
                {"2", fn account_get_signer_weight(a:Object, s:Object) -> RawVal}
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
        // This macro expands to a single item: the Env trait used to define the
        // interface implemented by Host and Guest, and used by client contract
        // code.
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
