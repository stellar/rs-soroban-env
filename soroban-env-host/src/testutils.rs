use crate::e2e_invoke::ledger_entry_to_ledger_key;
use crate::storage::EntryWithLiveUntil;
use crate::{
    budget::Budget,
    builtin_contracts::testutils::create_account,
    storage::{SnapshotSource, Storage},
    xdr::{
        AccountId, ContractCostType, LedgerEntry, LedgerKey, PublicKey, ScAddress, ScVal, ScVec,
        Uint256,
    },
    AddressObject, BytesObject, Env, EnvBase, Host, HostError, LedgerInfo, MeteredOrdMap,
    StorageType, SymbolSmall, Val, VecObject,
};
use rand::RngCore;
use std::panic::{catch_unwind, set_hook, take_hook, UnwindSafe};
use std::{cell::Cell, collections::BTreeMap, rc::Rc, sync::Once};

/// Catch panics while suppressing the default panic hook that prints to the
/// console.
///
/// For the purposes of test reporting we don't want every panicking (but
/// caught) contract call to print to the console. This requires overriding
/// the panic hook, a global resource. This is an awkward thing to do with
/// tests running in parallel.
///
/// This function lazily performs a one-time wrapping of the existing panic
/// hook. It then uses a thread local variable to track contract call depth.
/// If a panick occurs during a contract call the original hook is not
/// called, otherwise it is called.
pub fn call_with_suppressed_panic_hook<C, R>(closure: C) -> std::thread::Result<R>
where
    C: FnOnce() -> R + UnwindSafe,
{
    thread_local! {
        static TEST_CONTRACT_CALL_COUNT: Cell<u64> = const { Cell::new(0) };
    }

    static WRAP_PANIC_HOOK: Once = Once::new();

    WRAP_PANIC_HOOK.call_once(|| {
        let existing_panic_hook = take_hook();
        set_hook(Box::new(move |info| {
            let calling_test_contract = TEST_CONTRACT_CALL_COUNT.with(|c| c.get() != 0);
            if !calling_test_contract {
                existing_panic_hook(info)
            }
        }))
    });

    TEST_CONTRACT_CALL_COUNT.with(|c| {
        let old_count = c.get();
        let new_count = old_count.checked_add(1).expect("overflow");
        c.set(new_count);
    });

    let res = catch_unwind(closure);

    TEST_CONTRACT_CALL_COUNT.with(|c| {
        let old_count = c.get();
        let new_count = old_count.checked_sub(1).expect("overflow");
        c.set(new_count);
    });

    res
}

// Test utilities for the host, used in various tests in sub-modules.
pub trait AsScVal {
    fn as_scval(&self) -> ScVal;
}

impl AsScVal for u32 {
    fn as_scval(&self) -> ScVal {
        ScVal::U32(*self)
    }
}

impl AsScVal for i32 {
    fn as_scval(&self) -> ScVal {
        ScVal::I32(*self)
    }
}

impl AsScVal for u64 {
    fn as_scval(&self) -> ScVal {
        ScVal::U64(*self)
    }
}

impl AsScVal for i64 {
    fn as_scval(&self) -> ScVal {
        ScVal::I64(*self)
    }
}

impl AsScVal for ScVec {
    fn as_scval(&self) -> ScVal {
        ScVal::Vec(Some(self.clone()))
    }
}

pub fn generate_account_id(host: &Host) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
        generate_bytes_array(host),
    )))
}

pub fn generate_bytes_array(host: &Host) -> [u8; 32] {
    let mut bytes: [u8; 32] = Default::default();
    host.with_test_prng(|chacha| {
        chacha.fill_bytes(&mut bytes);
        Ok(())
    })
    .unwrap();
    bytes
}

pub struct MockSnapshotSource(BTreeMap<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>);

impl MockSnapshotSource {
    pub fn new() -> Self {
        Self(BTreeMap::<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>::new())
    }

    pub fn from_entries(entries: Vec<(LedgerEntry, Option<u32>)>) -> Self {
        let mut map = BTreeMap::<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>::new();
        let dummy_budget = Budget::default();
        for (e, maybe_ttl) in entries {
            let key = Rc::new(ledger_entry_to_ledger_key(&e, &dummy_budget).unwrap());
            map.insert(key, (Rc::new(e), maybe_ttl));
        }
        Self(map)
    }
}

impl SnapshotSource for MockSnapshotSource {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<Option<EntryWithLiveUntil>, HostError> {
        if let Some((entry, live_until)) = self.0.get(key) {
            Ok(Some((Rc::clone(entry), *live_until)))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub(crate) fn interface_meta_with_custom_versions(proto: u32, pre: u32) -> Vec<u8> {
    use crate::xdr::{Limited, Limits, ScEnvMetaEntry, WriteXdr};
    let iv = crate::meta::make_interface_version(proto, pre);
    let entry = ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(iv);
    let bytes = Vec::<u8>::new();
    let mut w = Limited::new(bytes, Limits::none());
    entry.write_xdr(&mut w).unwrap();
    w.inner
}

impl Host {
    pub const TEST_PRNG_SEED: &'static [u8; 32] = b"12345678901234567890123456789012";

    pub fn set_test_prng(&self) {
        self.set_base_prng_seed(*Self::TEST_PRNG_SEED).unwrap();
    }

    pub fn current_test_protocol() -> u32 {
        use crate::meta::{get_ledger_protocol_version, INTERFACE_VERSION};
        if let Ok(vers) = std::env::var("TEST_PROTOCOL") {
            vers.parse().unwrap()
        } else {
            get_ledger_protocol_version(INTERFACE_VERSION)
        }
    }

    pub fn set_test_ledger_info_with_current_test_protocol(&self) {
        self.set_ledger_info(LedgerInfo {
            protocol_version: Self::current_test_protocol(),
            sequence_number: 0,
            timestamp: 0,
            network_id: [0; 32],
            base_reserve: 0,
            min_persistent_entry_ttl: 4096,
            min_temp_entry_ttl: 16,
            max_entry_ttl: 6_312_000,
        })
        .unwrap();
    }

    pub fn test_host() -> Self {
        let host = Host::default();
        host.set_test_ledger_info_with_current_test_protocol();
        host
    }

    pub fn test_host_with_prng() -> Self {
        let host = Self::test_host();
        host.set_test_prng();
        host
    }

    pub fn test_host_with_recording_footprint() -> Self {
        let snapshot_source = Rc::<MockSnapshotSource>::new(MockSnapshotSource::new());
        let storage = Storage::with_recording_footprint(snapshot_source);
        let host = Host::with_storage_and_budget(storage, Budget::default());
        host.set_test_ledger_info_with_current_test_protocol();
        host.set_test_prng();
        host
    }

    pub fn test_budget(self, cpu: u64, mem: u64) -> Self {
        self.with_budget(|budget| {
            budget.reset_limits(cpu, mem)?; // something big but finite that we may exceed
            budget.reset_models()?;
            Ok(())
        })
        .unwrap();
        self
    }

    pub fn enable_model(
        self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: u64,
        const_mem: u64,
        lin_mem: u64,
    ) -> Self {
        self.with_budget(|budget| {
            budget.override_model_with_unscaled_params(ty, const_cpu, lin_cpu, const_mem, lin_mem)
        })
        .unwrap();
        self
    }

    pub fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<VecObject, HostError> {
        let v = self.test_scvec(vals)?;
        Ok(self.to_host_val(&ScVal::Vec(Some(v)))?.try_into()?)
    }

    pub fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<Val, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Vec(Some(v)))
    }

    pub fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScVal, HostError> {
        Ok(ScVal::Bytes(self.map_err(vals.to_vec().try_into())?))
    }

    pub fn test_bin_obj(&self, vals: &[u8]) -> Result<BytesObject, HostError> {
        let scval: ScVal = self.test_bin_scobj(vals)?;
        let val: Val = self.to_host_val(&scval)?;
        Ok(val.try_into()?)
    }

    // Registers a contract with provided Wasm code and returns the registered
    // contract's address.
    // The contract address deterministically depends on the input account and
    // salt, so this can be used with enforcing ledger footprint (but the
    // footprint still has to be specified outside of this).
    pub fn register_test_contract_wasm_from_source_account(
        &self,
        contract_wasm: &[u8],
        account: AccountId,
        salt: [u8; 32],
    ) -> Result<AddressObject, HostError> {
        let _span = tracy_span!("register_test_contract_wasm_from_source_account");
        // Use source account-based auth in order to avoid using nonces which
        // won't work well with enforcing ledger footprint.
        let prev_source_account = self.source_account_id()?;
        // Use recording auth to skip specifying the auth payload.
        let prev_auth_manager = self.snapshot_auth_manager()?;
        self.switch_to_recording_auth(true)?;

        let wasm_hash = self.upload_wasm(self.bytes_new_from_slice(contract_wasm)?)?;
        self.set_source_account(account.clone())?;
        let contract_address = self.create_contract(
            self.add_host_object(ScAddress::Account(account.clone()))?,
            wasm_hash,
            self.bytes_new_from_slice(&salt)?,
        )?;
        if let Some(prev_account) = prev_source_account {
            self.set_source_account(prev_account)?;
        }
        self.set_auth_manager(prev_auth_manager)?;
        Ok(contract_address)
    }

    // Registers a contract with provided Wasm code and returns the registered
    // contract's address.
    // The contract address will be generated randomly, so this won't work with
    // enforcing ledger footprint.
    pub fn register_test_contract_wasm(&self, contract_wasm: &[u8]) -> AddressObject {
        self.register_test_contract_wasm_from_source_account(
            contract_wasm,
            generate_account_id(self),
            generate_bytes_array(self),
        )
        .unwrap()
    }

    pub fn new_recording_fuzz_host(
        contract_wasms: &[&[u8]],
        data_keys: &BTreeMap<ScVal, (StorageType, bool)>,
        n_signers: usize,
    ) -> (Host, Vec<AddressObject>, Vec<ed25519_dalek::SigningKey>) {
        use crate::builtin_contracts::testutils::{
            generate_signing_key, signing_key_to_account_id,
        };

        let host = Self::test_host_with_recording_footprint();
        host.switch_to_recording_auth(false).unwrap();
        host.with_budget(|budget| {
            budget.reset_unlimited()?;
            Ok(())
        })
        .unwrap();
        let mut contract_addresses = Vec::new();
        for contract_wasm in contract_wasms.iter() {
            contract_addresses.push(host.register_test_contract_wasm(contract_wasm));
        }
        let ScAddress::Contract(contract_hash) =
            host.scaddress_from_address(contract_addresses[0]).unwrap()
        else {
            panic!()
        };

        let test = SymbolSmall::try_from_str("test").unwrap();

        // First step: insert all the data values in question into the storage map.
        host.with_test_contract_frame(contract_hash.clone(), test.into(), || {
            for (k, (t, _)) in data_keys.iter() {
                let v = host.to_host_val(k).unwrap();
                host.put_contract_data(v, v, *t).unwrap();
            }
            Ok(Val::VOID.into())
        })
        .unwrap();

        // Second step: generate some accounts to sign things with.
        let signing_keys = (0..n_signers)
            .map(|_| generate_signing_key(&host))
            .collect();
        for signing_key in &signing_keys {
            create_account(
                &host,
                &signing_key_to_account_id(signing_key),
                vec![(&signing_key, 1)],
                100_000_000,
                1,
                [1, 0, 0, 0],
                None,
                None,
                0,
            )
        }

        (host, contract_addresses, signing_keys)
    }

    pub fn switch_fuzz_host_to_enforcing(
        &self,
        data_keys: &BTreeMap<ScVal, (StorageType, bool)>,
        signing_keys: &[ed25519_dalek::SigningKey],
    ) {
        use crate::builtin_contracts::testutils::TestSigner;
        use crate::xdr::{
            HashIdPreimage, HashIdPreimageSorobanAuthorization, SorobanAddressCredentials,
            SorobanAuthorizationEntry, SorobanCredentials,
        };
        self.with_budget(|budget| {
            budget.reset_unlimited()?;
            Ok(())
        })
        .unwrap();

        // Modify footprint entries to read-only-ness as required, synthesize
        // empty storage-map entries that were accessed by keys the contract made
        // up, and switch to enforcing mode.
        self.with_mut_storage(|storage| {
            storage.footprint.0 = MeteredOrdMap::from_exact_iter(
                storage
                    .footprint
                    .0
                    .iter(self.budget_ref())
                    .unwrap()
                    .map(|(k, accesstype)| {
                        let mut accesstype = *accesstype;
                        if let LedgerKey::ContractData(k) = k.as_ref() {
                            if let Some((_, ro)) = data_keys.get(&k.key) {
                                if *ro {
                                    accesstype = crate::storage::AccessType::ReadOnly;
                                } else {
                                    accesstype = crate::storage::AccessType::ReadWrite;
                                }
                            }
                        }
                        (k.clone(), accesstype)
                    }),
                self.budget_ref(),
            )
            .unwrap();

            // Synthesize empty entries for anything the contract made up (these
            // will be in the footprint but not yet in the map, which is an
            // invariant violation we need to repair here).
            let mut map = BTreeMap::new();
            for (k, v) in storage.map.iter(self.budget_ref()).unwrap() {
                map.insert(k.clone(), v.clone());
            }
            for (k, _) in storage.footprint.0.iter(self.budget_ref()).unwrap() {
                if !map.contains_key(k) {
                    map.insert(k.clone(), None);
                }
            }
            // Reset any nonces so they can be consumed.
            for (k, v) in map.iter_mut() {
                if let LedgerKey::ContractData(k) = k.as_ref() {
                    if let ScVal::LedgerKeyNonce(_) = &k.key {
                        *v = None;
                    }
                }
            }
            storage.map = MeteredOrdMap::from_exact_iter(
                map.iter().map(|(k, v)| (k.clone(), v.clone())),
                self.budget_ref(),
            )
            .unwrap();
            storage.mode = crate::storage::FootprintMode::Enforcing;
            Ok(())
        })
        .unwrap();

        // Sign and install auth entries for all the recorded auth payloads.
        let mut auth_entries = Vec::new();
        let account_signers = signing_keys
            .iter()
            .map(TestSigner::account)
            .collect::<Vec<_>>();
        for payload in self.get_recorded_auth_payloads().unwrap().iter() {
            for signer in account_signers.iter() {
                let Some(address) = &payload.address else {
                    continue;
                };
                let Some(nonce) = payload.nonce else { continue };
                if *address == ScAddress::Account(signer.account_id()) {
                    let address = address.clone();
                    let signature_expiration_ledger =
                        u32::from(self.get_ledger_sequence().unwrap()) + 10000;
                    let network_id = self
                        .with_ledger_info(|li: &LedgerInfo| Ok(li.network_id))
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let signature_payload_preimage =
                        HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                            network_id,
                            invocation: payload.invocation.clone(),
                            nonce,
                            signature_expiration_ledger,
                        });
                    let signature_payload =
                        self.metered_hash_xdr(&signature_payload_preimage).unwrap();
                    let signature = signer.sign(&self, &signature_payload);
                    let credentials = SorobanCredentials::Address(SorobanAddressCredentials {
                        address: address.clone(),
                        nonce: nonce,
                        signature_expiration_ledger,
                        signature,
                    });
                    let entry = SorobanAuthorizationEntry {
                        credentials,
                        root_invocation: payload.invocation.clone(),
                    };
                    auth_entries.push(entry);
                }
            }
        }
        self.set_authorization_entries(auth_entries).unwrap();
    }

    #[cfg(all(test, feature = "testutils"))]
    pub(crate) fn measured_call(
        &self,
        contract: AddressObject,
        func: crate::Symbol,
        args: VecObject,
    ) -> Result<Val, HostError> {
        use crate::{budget::AsBudget, host::TraceEvent};
        use soroban_bench_utils::HostTracker;
        use std::cell::RefCell;

        let _span = tracy_span!("measured_call");
        let budget = self.as_budget();
        budget.reset_unlimited()?;
        let ht = Rc::new(RefCell::new(HostTracker::new()));

        if std::env::var("EXCLUDE_VM_INSTANTIATION").is_ok() {
            let ht2 = ht.clone();
            let budget2 = budget.clone();
            self.set_trace_hook(Some(Rc::new(move |_, evt| {
                if let TraceEvent::PushCtx(_) = evt {
                    budget2.reset_unlimited()?;
                    ht2.borrow_mut().start(None);
                }
                Ok(())
            })))?;
        } else {
            ht.borrow_mut().start(None);
        }
        let val = self.call(contract, func, args);
        self.set_trace_hook(None)?;

        let (cpu_actual, mem_actual, time_nsecs) = Rc::into_inner(ht).unwrap().into_inner().stop();

        let cpu_metered = budget
            .get_cpu_insns_consumed()
            .expect("unable to retrieve cpu consumed");
        let mem_metered = budget
            .get_mem_bytes_consumed()
            .expect("unable to retrieve mem consumed");

        let cpu_diff = (cpu_metered - cpu_actual) as i64;
        let cpu_metered_diff_percent = 100 * cpu_diff / (cpu_metered as i64).max(1);
        let mem_diff = (mem_metered - mem_actual) as i64;
        let mem_metered_diff_percent = 100 * mem_diff / (mem_metered as i64).max(1);
        let metered_insn_nsecs_ratio: f64 = (cpu_metered as f64) / (time_nsecs as f64).max(1.0);
        let actual_insn_nsecs_ratio: f64 = (cpu_actual as f64) / (time_nsecs as f64).max(1.0);

        println!();
        println!(
            "metered cpu insns: {}, actual cpu insns {}, diff: {} ({:.3}%)",
            cpu_metered, cpu_actual, cpu_diff, cpu_metered_diff_percent
        );
        println!(
            "metered mem bytes: {}, actual mem bytes {}, diff: {} ({:.3}%)",
            mem_metered, mem_actual, mem_diff, mem_metered_diff_percent
        );
        println!("time_nsecs: {}", time_nsecs);
        println!(
            "metered cpu_insn/time_nsecs ratio: {:.3}",
            metered_insn_nsecs_ratio
        );
        println!(
            "actual cpu_insn/time_nsecs ratio: {:.3}",
            actual_insn_nsecs_ratio
        );
        println!();

        val
    }
}

#[cfg(test)]
pub(crate) mod wasm {
    use crate::{Symbol, Tag, U32Val, Val};
    use soroban_synth_wasm::{Arity, FuncRef, LocalRef, ModEmitter, Operand};
    use wasm_encoder::{ConstExpr, Elements, RefType};

    pub(crate) fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
        let mut fe = ModEmitter::default_with_test_protocol().func(Arity(1), 0);
        let arg = fe.args[0];
        fe.push(Operand::Const64(1));
        for i in 0..n {
            fe.push(arg.0);
            fe.push(Operand::Const64(i as i64));
            fe.i64_mul();
            fe.i64_add();
        }
        fe.drop();
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_n_funcs_no_export(n: usize) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        for _i in 0..n {
            let mut fe = me.func(Arity(0), 0);
            fe.push(Symbol::try_from_small_str("pass").unwrap());
            me = fe.finish().0;
        }
        me.finish()
    }

    pub(crate) fn wasm_module_with_repeated_exporting_the_same_func(n: usize) -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        let (mut me, fid) = fe.finish();
        for i in 0..n {
            me.export_func(fid, format!("test{}", i).as_str());
        }
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_mem_grow(n_pages: usize) -> Vec<u8> {
        let mut fe = ModEmitter::default_with_test_protocol().func(Arity(0), 0);
        fe.push(Operand::Const32(n_pages as i32));
        fe.memory_grow();
        fe.drop();
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_linear_memory_logging() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // log_from_linear_memory
        let f0 = me.import_func("x", "_", Arity(4));
        // the caller
        let mut fe = me.func(Arity(4), 0);
        fe.push(Operand::Local(LocalRef(0)));
        fe.push(Operand::Local(LocalRef(1)));
        fe.push(Operand::Local(LocalRef(2)));
        fe.push(Operand::Local(LocalRef(3)));
        fe.call_func(f0);
        fe.drop();
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_unreachable() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        fe.trap();
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_indirect_call() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // an imported function
        let f0 = me.import_func("t", "_", Arity(0));
        // a local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        let (me, f1) = fe.finish();
        // another local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass2").unwrap());
        let (mut me, f2) = fe.finish();
        // store in table
        me.define_elem_funcs(&[f0, f1, f2]);
        let ty = me.get_fn_type(Arity(0), Arity(1));
        // the caller
        fe = me.func(Arity(1), 0);
        fe.local_get(LocalRef(0));
        fe.i32_wrap_i64();
        // fe.i32_const(0);
        fe.call_func_indirect(ty);
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_div_by_zero() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        fe.push(Operand::Const64(123));
        fe.push(Operand::Const64(0));
        fe.i64_div_s();
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_integer_overflow() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        fe.push(Operand::Const64(i64::MIN));
        fe.push(Operand::Const64(-1));
        // interestingly the only operation that can trigger `IntegerOverflow`
        // is an overflowing division. Other arithmatic opeartions add, sub, mul
        // are wrapping.
        fe.i64_div_s();
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_user_specified_initial_size(
        mem_pages: u32,
        elem_count: u32,
    ) -> Vec<u8> {
        let me = ModEmitter::from_configs(mem_pages, elem_count);
        // a local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_large_data_segment(
        mem_pages: u32,
        mem_offset: u32,
        len: u32,
    ) -> Vec<u8> {
        let mut me = ModEmitter::from_configs(mem_pages, 128);
        me.define_data_segment(mem_offset, vec![0; len as usize]);
        // a local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_multiple_data_segments(
        num_pages: u32,
        num_sgmts: u32,
        seg_size: u32,
    ) -> Vec<u8> {
        let mut me = ModEmitter::from_configs(num_pages, 128);
        let mem_len = num_pages * 0x10_000;
        // we just make sure the total memory can fit one segments the segments
        // will just cycle through the space and possibly override earlier ones
        let max_segments = (mem_len / seg_size.max(1)).max(1);
        for _i in 0..num_sgmts % max_segments {
            me.define_data_segment(0, vec![0; seg_size as usize]);
        }
        // a local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_large_bytes_from_linear_memory(
        num_bytes: u32,
        initial: u8,
    ) -> Vec<u8> {
        let num_pages = num_bytes / 0x10_000 + 1;
        let mut me = ModEmitter::from_configs(num_pages, 128);
        me.define_data_segment(0, vec![initial; num_bytes as usize]);
        let mut fe = me.func(Arity(0), 0);
        fe.bytes_new_from_linear_memory(U32Val::from(0), U32Val::from(num_bytes));
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_large_vector_from_linear_memory(
        num_vals: u32,
        initial: Val,
    ) -> Vec<u8> {
        let num_pages = num_vals * 8 / 0x10_000 + 1;
        let mut me = ModEmitter::from_configs(num_pages, 128);
        let bytes: Vec<u8> = (0..num_vals)
            .into_iter()
            .map(|_| initial.get_payload().to_le_bytes())
            .flat_map(|a| a.into_iter())
            .collect();
        me.define_data_segment(0, bytes);
        let mut fe = me.func(Arity(0), 0);
        fe.vec_new_from_linear_memory(U32Val::from(0), U32Val::from(num_vals));
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_large_map_from_linear_memory(
        num_vals: u32,
        initial: Val,
    ) -> Vec<u8> {
        // slice is 8 bytes, we choose 8 byte key, val is 8 bytes
        let num_pages = (num_vals * 8) * 3 / 0x10_000 + 1;
        let mut me = ModEmitter::from_configs(num_pages, 128);

        let key_bytes: Vec<u8> = (0..num_vals)
            .into_iter()
            .map(|i| format!("{:0>width$}", i, width = 8))
            .flat_map(|s| s.into_bytes().into_iter())
            .collect();

        let val_bytes: Vec<u8> = (0..num_vals)
            .into_iter()
            .map(|_| initial.get_payload().to_le_bytes())
            .flat_map(|a| a.into_iter())
            .collect();

        let slices: Vec<u8> = (0..num_vals)
            .into_iter()
            .map(|ptr| {
                let slice = 8_u64 << 32 | (ptr * 8) as u64;
                slice.to_le_bytes()
            })
            .flat_map(|s| s.into_iter())
            .collect();

        let bytes: Vec<u8> = key_bytes
            .into_iter()
            .chain(val_bytes.into_iter())
            .chain(slices.into_iter())
            .collect();

        me.define_data_segment(0, bytes);
        let mut fe = me.func(Arity(0), 0);

        let vals_pos = U32Val::from(num_vals * 8);
        let keys_pos = U32Val::from(num_vals * 16);
        fe.map_new_from_linear_memory(keys_pos, vals_pos, U32Val::from(num_vals));

        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_data_count(
        num_sgmts: u32,
        seg_size: u32,
        data_count: u32,
    ) -> Vec<u8> {
        // first calculate the number of memory pages needed to fit all the data
        // segments non-overlapping
        let pages = num_sgmts * seg_size / 0x10_000 + 1;
        let mut me = ModEmitter::from_configs(pages, 128);
        for i in 0..num_sgmts {
            me.define_data_segment(i * seg_size, vec![7; seg_size as usize]);
        }
        // define the data count, if count != num_sgmts, then the validator is
        // supposed to catch it
        me.data_count(data_count);
        me.finish_no_validate()
    }

    // Emit a wasm module that uses post-MVP WASM features. Specifically
    // mutable-globals and sign-ext.
    pub fn post_mvp_wasm_module() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();

        // Emit an exported mutable global
        me.define_global_i64(-100, true, Some("global"));

        let mut fe = me.func(Arity(0), 0);
        fe.i64_const(0x0000_0000_ffff_abcd_u64 as i64);

        // Emit an op from the new sign-ext repertoire
        fe.i64_extend32s();

        // Turn this into a I64Small
        fe.i64_const(8);
        fe.i64_shl();
        fe.i64_const(Tag::I64Small as i64);
        fe.i64_or();

        fe.finish_and_export("test").finish()
    }

    pub fn empty_wasm_module() -> Vec<u8> {
        ModEmitter::new().finish()
    }

    pub fn wasm_module_with_custom_section(name: &str, data: &[u8]) -> Vec<u8> {
        let mut me = ModEmitter::new();
        me.custom_section(name, data);
        me.finish()
    }

    pub fn wasm_module_with_floating_point_ops() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        fe.f64_const(1.1f64);
        fe.f64_const(2.2f64);
        fe.f64_add();
        fe.drop();
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub fn wasm_module_with_multiple_memories() -> Vec<u8> {
        let mut me = ModEmitter::new();
        me.memory(1, None, false, false);
        me.memory(1, None, false, false);
        me.finish()
    }

    pub fn wasm_module_lying_about_import_function_type() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // function {"t", "_"} is "dummy0", taking 0 arguments
        // this will not pass the wasmi linker
        me.import_func("t", "_", Arity(1));
        me.finish()
    }

    pub fn wasm_module_importing_nonexistent_function() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        me.import_func("t", "z", Arity(1));
        me.finish()
    }

    pub fn wasm_module_with_duplicate_function_import(n: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        for _ in 0..n {
            me.import_func_no_check("t", "_", Arity(0));
        }
        me.finish()
    }

    pub fn wasm_module_with_nonexistent_function_export() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        me.import_func("t", "_", Arity(0));
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        let (mut me, fid) = fe.finish();
        // exporting a function I defined is okay
        me.export_func(fid, "test");
        // exporting an imported function is also okay, although weird
        me.export_func(FuncRef(0), "test0");
        // importing an non-existent function will not pass validation
        me.export_func(FuncRef(100), "test100");
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_nonexistent_func_element() -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // an imported function
        let f0 = me.import_func("t", "_", Arity(0));
        // a local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        let (me, f1) = fe.finish();
        // another local wasm function
        let mut fe = me.func(Arity(0), 0);
        fe.push(Symbol::try_from_small_str("pass2").unwrap());
        let (mut me, f2) = fe.finish();
        // store in table, FuncRef(100) is invalid
        me.define_elem_funcs(&[f0, f1, f2, FuncRef(100)]);
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_start_function() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let fe = me.func_with_arity_and_ret(Arity(0), Arity(0), 0);
        let (mut me, fid) = fe.finish();
        me.export_func(fid.clone(), "start");
        me.start(fid);
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_multi_value() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func_with_arity_and_ret(Arity(0), Arity(2), 0);
        fe.push(Symbol::try_from_small_str("pass1").unwrap());
        fe.push(Symbol::try_from_small_str("pass2").unwrap());
        fe.finish_and_export("test").finish()
    }

    // if n > m, we have oob elements
    pub(crate) fn wasm_module_large_elements(m: u32, n: u32) -> Vec<u8> {
        let mut me = ModEmitter::from_configs(1, m);
        // an imported function
        let f0 = me.import_func("t", "_", Arity(0));
        me.define_elem_funcs(vec![f0; n as usize].as_slice());
        me.finish()
    }

    pub(crate) fn wasm_module_various_constexpr_in_elements(case: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // an imported function
        let f0 = me.import_func("t", "_", Arity(0));

        match case {
            0 =>
            // err: wrong type, expect i32, passing i64
            {
                me.define_active_elements(
                    None,
                    &ConstExpr::i64_const(0),
                    Elements::Functions(&[f0.0]),
                )
            }
            // err: fp
            1 => me.define_active_elements(
                None,
                &ConstExpr::f64_const(1.0),
                Elements::Functions(&[f0.0]),
            ),
            // err: simd
            2 => me.define_active_elements(
                None,
                &ConstExpr::v128_const(1),
                Elements::Functions(&[f0.0]),
            ),
            // err: wrong type, expect i32, passing funcref
            3 => me.define_active_elements(
                None,
                &ConstExpr::ref_func(f0.0),
                Elements::Functions(&[f0.0]),
            ),
            _ => panic!("not a valid option"),
        }
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_large_globals(n: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        for i in 0..n {
            me.define_global_i64(i as i64, true, None);
        }
        me.finish()
    }

    pub(crate) fn wasm_module_various_constexr_in_global(case: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        match case {
            0 =>
            // err: mismatch
            {
                me.define_global(wasm_encoder::ValType::I32, true, &ConstExpr::i64_const(1))
            }
            1 =>
            // err: fp
            {
                me.define_global(wasm_encoder::ValType::F32, true, &ConstExpr::f32_const(1.0))
            }
            2 =>
            // err: simd
            {
                me.define_global(wasm_encoder::ValType::V128, true, &ConstExpr::v128_const(1))
            }
            3 =>
            // okay: func
            {
                let fr = me.import_func("t", "_", Arity(0));
                me.define_global(
                    wasm_encoder::ValType::Ref(RefType::FUNCREF),
                    true,
                    &ConstExpr::ref_func(fr.0),
                )
            }
            _ => panic!("not a valid option"),
        }
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_various_constexr_in_data_segment(case: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // an imported function
        let f0 = me.import_func("t", "_", Arity(0));

        match case {
            0 =>
            // err: wrong type, expect i32, passing i64
            {
                me.define_active_data(0, &ConstExpr::i64_const(0), vec![0; 8]);
            }
            // err: fp
            1 => {
                me.define_active_data(0, &ConstExpr::f64_const(0.0), vec![0; 8]);
            }
            // err: simd
            2 => {
                me.define_active_data(0, &ConstExpr::v128_const(0), vec![0; 8]);
            }
            // err: wrong type, expect i32, passing funcref
            3 => {
                me.define_active_data(0, &ConstExpr::ref_func(f0.0), vec![0; 8]);
            }
            _ => panic!("not a valid option"),
        }
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_extern_ref() -> Vec<u8> {
        let mut me = ModEmitter::new();
        me.table(RefType::EXTERNREF, 2, None);
        me.add_test_protocol_version_meta();
        me.finish_no_validate()
    }

    pub(crate) fn wasm_module_with_additional_tables(n: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        // by default, module already includes a table, here we are creating
        // additional ones
        for _i in 0..n {
            me.table(RefType::FUNCREF, 2, None);
        }
        // wasmparser has an limit of 100 tables. wasmi does not have such a limit
        me.finish_no_validate()
    }

    // The only type allowed in the MVP is the function type. There are more
    // composite types defined by the GC proposal, which should not be allowed
    // and we will verify that in another test.
    pub(crate) fn wasm_module_with_many_func_types(n: u32) -> Vec<u8> {
        let mut me = ModEmitter::default_with_test_protocol();
        for _i in 0..n {
            // it is allowed to define the same types over and over
            me.add_fn_type_no_check(Arity(0), Arity(0));
        }
        me.finish()
    }

    pub(crate) fn wasm_module_with_simd_add_i32x4() -> Vec<u8> {
        let me = ModEmitter::default_with_test_protocol();
        let mut fe = me.func(Arity(0), 0);
        // we load [u32, u32, u32, u32] x 2, add them and store back
        fe.i32_const(32); // ptr for storing the result
        fe.i32_const(0); // ptr for the first 4xi32
        fe.v128_load(0, 0);
        fe.i32_const(16); // ptr for the second 4xi32
        fe.v128_load(0, 0);
        fe.i32x4_add();
        fe.v128_store(0, 0);
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_calling_protocol_gated_host_fn(wasm_proto: u32) -> Vec<u8> {
        let mut me = ModEmitter::new();
        me.add_protocol_version_meta(wasm_proto);
        // protocol_gated_dummy
        let f0 = me.import_func("t", "0", Arity(0));
        // the caller
        let mut fe = me.func(Arity(0), 0);
        fe.call_func(f0);
        fe.finish_and_export("test").finish()
    }

    pub(crate) fn wasm_module_with_a_bit_of_everything(wasm_proto: u32) -> Vec<u8> {
        let mut me = ModEmitter::new();
        me.add_protocol_version_meta(wasm_proto);
        me.table(RefType::FUNCREF, 128, None);
        me.memory(1, None, false, false);
        me.global(wasm_encoder::ValType::I64, true, &ConstExpr::i64_const(42));
        me.export("memory", wasm_encoder::ExportKind::Memory, 0);
        let _f0 = me.import_func("t", "_", Arity(0));
        let mut fe = me.func(Arity(0), 0);
        fe.push(Operand::Const64(1));
        fe.push(Operand::Const64(2));
        fe.i64_add();
        fe.drop();
        fe.push(Symbol::try_from_small_str("pass").unwrap());
        let (mut me, fid) = fe.finish();
        me.export_func(fid, "test");
        me.define_elem_funcs(&[fid]);
        me.define_data_segment(0x1234, vec![0; 512]);
        me.finish()
    }
}
