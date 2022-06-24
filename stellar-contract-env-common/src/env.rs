use super::{Object, RawVal, Symbol};
use core::any;

pub trait EnvBase: Sized + Clone {
    // Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    // Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);

    // Used to clone an environment deeply, not just a handle to it.
    fn deep_clone(&self) -> Self;
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
                {"_", fn log_value(v:RawVal) -> RawVal }
                /// Get the binary contractID of the contract which invoked the
                /// running contract. Traps if the running contract was not
                /// invoked by a contract.
                {"0", fn get_invoking_contract() -> Object }
                {"1", fn obj_cmp(a:RawVal, b:RawVal) -> i64 }
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
                /// Create an empty new vector.
                {"_", fn vec_new() -> Object}
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
                /// Copy the elements from `i` until length `l` in the vector and create a new vector from it.
                /// Return the new vector. Traps if the index is out of bound.
                {"A", fn vec_slice(v:Object, i:RawVal, l:RawVal) -> Object}
            }

            mod ledger "l" {
                {"_", fn put_contract_data(k:RawVal, v: RawVal) -> RawVal}
                {"0", fn has_contract_data(k:RawVal) -> RawVal}
                {"1", fn get_contract_data(k:RawVal) -> RawVal}
                {"2", fn del_contract_data(k:RawVal) -> RawVal}
                {"3", fn create_contract(v: Object, salt: Object, key: Object, sig: Object) -> Object}
                {"4", fn create_contract_using_parent_id(v: Object, salt: Object) -> Object}
            }

            mod call "c" {
                {"_", fn call(contract:Object, func:Symbol, args:Object) -> RawVal}
            }

            mod bigint "g" {
                {"_", fn bigint_from_u64(x:u64) -> Object}
                {"0", fn bigint_to_u64(x:Object) -> u64}
                {"1", fn bigint_from_i64(x:i64) -> Object}
                {"2", fn bigint_to_i64(x:Object) -> i64}
                {"3", fn bigint_add(x:Object, y:Object) -> Object}
                {"4", fn bigint_sub(x:Object, y:Object) -> Object}
                {"5", fn bigint_mul(x:Object, y:Object) -> Object}
                {"6", fn bigint_div(x:Object, y:Object) -> Object}
                {"7", fn bigint_rem(x:Object, y:Object) -> Object}
                {"8", fn bigint_and(x:Object, y:Object) -> Object}
                {"9", fn bigint_or(x:Object, y:Object) -> Object}
                {"A", fn bigint_xor(x:Object, y:Object) -> Object}
                {"B", fn bigint_shl(x:Object, y:RawVal) -> Object}
                {"C", fn bigint_shr(x:Object, y:RawVal) -> Object}
                {"D", fn bigint_cmp(x:Object, y:Object) -> RawVal}
                {"E", fn bigint_is_zero(x:Object) -> RawVal}
                {"F", fn bigint_neg(x:Object) -> Object}
                {"G", fn bigint_not(x:Object) -> Object}
                {"H", fn bigint_gcd(x:Object, y:Object) -> Object}
                {"I", fn bigint_lcm(x:Object, y:Object) -> Object}
                {"J", fn bigint_pow(x:Object, y:Object) -> Object}
                {"K", fn bigint_pow_mod(p:Object, q:Object, m:Object) -> Object}
                {"L", fn bigint_sqrt(x:Object) -> Object}
                {"M", fn bigint_bits(x:Object) -> RawVal}
            }

            mod binary "b" {
                {"_", fn serialize_to_binary(b:Object) -> Object}
                {"0", fn deserialize_from_binary(b:Object) -> Object}
                {"1", fn binary_copy_to_guest_mem(b:Object, i:RawVal, j:RawVal, l:RawVal) -> RawVal}
                {"2", fn binary_copy_from_guest_mem(b:Object, i:RawVal, j:RawVal, l:RawVal) -> RawVal}
                // These functions below ($3-$F) mirror vector operations
                /// Create an empty new binary.
                {"3", fn binary_new() -> Object}
                /// Update the value at index `i` in the binary. Return the new binary.
                /// Trap if the index is out of bounds.
                {"4", fn binary_put(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Returns the element at index `i` of the binary. Traps if the index is out of bound.
                {"5", fn binary_get(b:Object, i:RawVal) -> RawVal}
                /// Delete an element in a binary at index `i`, shifting all elements after it to the left.
                /// Return the new binary. Traps if the index is out of bound.
                {"6", fn binary_del(b:Object, i:RawVal) -> Object}
                /// Returns length of the binary.
                {"7", fn binary_len(b:Object) -> RawVal}
                /// Appends an element to the back of the binary.
                {"8", fn binary_push(b:Object, u:RawVal) -> Object}
                /// Removes the last element from the binary and returns the new binary.
                /// Traps if original binary is empty.
                {"9", fn binary_pop(b:Object) -> Object}
                /// Return the first element in the binary. Traps if the binary is empty
                {"A", fn binary_front(b:Object) -> RawVal}
                /// Return the last element in the binary. Traps if the binary is empty
                {"B", fn binary_back(b:Object) -> RawVal}
                /// Inserts an element at index `i` within the binary, shifting all elements after it to the right.
                /// Traps if the index is out of bound
                {"C", fn binary_insert(b:Object, i:RawVal, u:RawVal) -> Object}
                /// Clone the binary `b1`, then moves all the elements of binary `b2` into it.
                /// Return the new binary. Traps if number of elements in the binary overflows a u32.
                {"D", fn binary_append(b1:Object, b2:Object) -> Object}
                /// Copy the elements from `i` until length `l` in the binary and create a new binary from it.
                /// Return the new binary. Traps if the index is out of bound.
                {"E", fn binary_slice(b:Object, i:RawVal, l:RawVal) -> Object}
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
