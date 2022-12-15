extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{quote, ToTokens};
use syn::{parse::Parse, parse_macro_input, Ident, LitInt, Token};

use stellar_xdr::{ScEnvMetaEntry, WriteXdr};

struct MetaInput {
    pub interface_version: u64,
}

impl Parse for MetaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(MetaInput {
            interface_version: {
                assert_eq!(input.parse::<Ident>()?, "interface_version");
                input.parse::<Token![:]>()?;
                let v = input.parse::<LitInt>()?.base10_parse()?;
                input.parse::<Token![,]>()?;
                v
            },
        })
    }
}

struct MetaConstsOutput {
    pub input: MetaInput,
}

impl MetaConstsOutput {
    pub fn to_meta_entries(&self) -> Vec<ScEnvMetaEntry> {
        vec![ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(
            self.input.interface_version,
        )]
    }
}

impl ToTokens for MetaConstsOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // Build params for expressing the interface version.
        let interface_version = self.input.interface_version;

        // Build params for expressing the meta xdr.
        let meta_xdr = self
            .to_meta_entries()
            .into_iter()
            .map(|entry| entry.to_xdr())
            .collect::<Result<Vec<Vec<u8>>, stellar_xdr::Error>>()
            .unwrap()
            .concat();
        let meta_xdr_len = meta_xdr.len();
        let meta_xdr_lit = proc_macro2::Literal::byte_string(meta_xdr.as_slice());

        // Output.
        tokens.extend(quote! {
            pub const INTERFACE_VERSION: u64 = #interface_version;
            pub const XDR: [u8; #meta_xdr_len] = *#meta_xdr_lit;
        });
    }
}

#[proc_macro]
pub fn generate_env_meta_consts(input: TokenStream) -> TokenStream {
    let meta_input = parse_macro_input!(input as MetaInput);
    let meta_consts_output = MetaConstsOutput { input: meta_input };
    quote! { #meta_consts_output }.into()
}

#[proc_macro]
pub fn generate_call_macro_with_all_host_functions(input: TokenStream) -> TokenStream {
    let _file = parse_macro_input!(input as Literal);
    quote! {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! _call_macro_with_all_host_functions {

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

                    /// Functions concerned with boxed integer types
                    mod i64 "i" {

                        /// Convert a u64 to an object containing a u64.
                        {"_", fn obj_from_u64(v:u64) -> Object }
                        /// Convert an object containing a i64 to a u64.
                        {"0", fn obj_to_u64(obj:Object) -> u64 }
                        /// Convert an i64 to an object containing an i64.
                        {"1", fn obj_from_i64(v:i64) -> Object }
                        /// Convert an object containing an i64 to an i64.
                        {"2", fn obj_to_i64(obj:Object) -> i64 }

                        /// Convert the low and high 64-bit words of a u128 to an
                        /// object containing a u128.
                        {"5", fn obj_from_u128_pieces(lo:u64,hi:u64) -> Object }
                        /// Extract the low 64 bits from an object containing a u128.
                        {"6", fn obj_to_u128_lo64(obj:Object) -> u64 }
                        /// Extract the high 64 bits from an object containing a u128.
                        {"7", fn obj_to_u128_hi64(obj:Object) -> u64 }

                        /// Convert the lo and hi 64-bit words of an i128 to an
                        /// object containing an i128.
                        {"8", fn obj_from_i128_pieces(lo:u64,hi:u64) -> Object }
                        /// Extract the low 64 bits from an object containing an i128.
                        {"9", fn obj_to_i128_lo64(obj:Object) -> u64 }
                        /// Extract the high 64 bits from an object containing an i128.
                        {"a", fn obj_to_i128_hi64(obj:Object) -> u64 }
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
                        /// Deploys a contract from the current contract. `wasm_hash` must
                        /// be a hash of the contract code that has already been installed
                        /// on this network. `salt` is used to create a unique contract id.
                        {"3", fn create_contract_from_contract(wasm_hash: Object, salt: Object) -> Object}
                        /// Deploys a built-in token contract from the current contract.
                        /// `salt` is used to create a unique contract id for the token.
                        {"4", fn create_token_from_contract(salt: Object) -> Object}
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
        pub use _call_macro_with_all_host_functions as call_macro_with_all_host_functions;
    }.into()
}
