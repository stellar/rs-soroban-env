use std::{convert::TryInto, rc::Rc};

use crate::builtin_contracts::base_types::BytesN;
use crate::testutils::simple_account_sign_fn;
use crate::{
    auth::RecordedAuthPayload,
    builtin_contracts::{
        base_types::Address,
        contract_error::ContractError,
        stellar_asset_contract::test_stellar_asset_contract::TestStellarAssetContract,
        testutils::{
            account_to_address, authorize_single_invocation,
            authorize_single_invocation_with_nonce, contract_id_to_address, create_account,
            generate_signing_key, new_ledger_entry_from_data, signing_key_to_account_id,
            AccountContractSigner, AccountSigner, TestSigner,
        },
    },
    host::{frame::TestContractFrame, Frame},
    testutils::generate_bytes_array,
    xdr::{
        self, AccountFlags, AccountId, AlphaNum12, AlphaNum4, Asset, AssetCode12, AssetCode4,
        ContractEventType, ContractExecutable, Hash, InvokeContractArgs, LedgerEntryData,
        LedgerKey, Liabilities, PublicKey, ScAddress, ScContractInstance, ScErrorCode, ScErrorType,
        ScSymbol, ScVal, SorobanAuthorizedFunction, SorobanAuthorizedInvocation, TrustLineEntry,
        TrustLineEntryExt, TrustLineEntryV1, TrustLineEntryV1Ext, TrustLineFlags,
    },
    Env, EnvBase, Host, HostError, LedgerInfo, Symbol, TryFromVal, TryIntoVal, Val,
};
use ed25519_dalek::SigningKey;
use soroban_test_wasms::{
    ERR, INVOKE_CONTRACT, SAC_REENTRY_TEST_CONTRACT, SIMPLE_ACCOUNT_CONTRACT,
};
use stellar_strkey::ed25519;

use super::observe::ObservedHost;

struct StellarAssetContractTest {
    // The `obs` field here just exists to
    // live as long as Host, then be dropped.
    #[allow(dead_code)]
    obs: ObservedHost,
    host: Host,
    issuer_key: SigningKey,
    user_key: SigningKey,
    user_key_2: SigningKey,
    user_key_3: SigningKey,
    user_key_4: SigningKey,
    asset_code: [u8; 4],
}

impl StellarAssetContractTest {
    fn setup(testname: &'static str) -> Self {
        let host = Host::test_host_with_recording_footprint();
        let obs = ObservedHost::new(testname, host.clone());
        let protocol_version = host.get_ledger_protocol_version().unwrap();
        host.set_ledger_info(LedgerInfo {
            protocol_version,
            sequence_number: 123,
            timestamp: 123456,
            network_id: [5; 32],
            base_reserve: 5_000_000,
            min_persistent_entry_ttl: 4096,
            min_temp_entry_ttl: 16,
            max_entry_ttl: 6_312_000,
        })
        .unwrap();
        Self {
            obs,
            host: host.clone(),
            issuer_key: generate_signing_key(&host),
            user_key: generate_signing_key(&host),
            user_key_2: generate_signing_key(&host),
            user_key_3: generate_signing_key(&host),
            user_key_4: generate_signing_key(&host),
            asset_code: [b'a'; 4],
        }
    }

    fn default_stellar_asset_contract_with_admin_id(
        &self,
        new_admin: &Address,
    ) -> TestStellarAssetContract {
        let contract = self.default_stellar_asset_contract();
        let issuer = TestSigner::account(&self.issuer_key);
        contract.set_admin(&issuer, new_admin.clone()).unwrap();
        contract
    }

    fn default_stellar_asset_contract(&self) -> TestStellarAssetContract {
        let issuer_id = signing_key_to_account_id(&self.issuer_key);
        self.create_account(
            &issuer_id,
            vec![(&self.issuer_key, 100)],
            10_000_000,
            1,
            [1, 0, 0, 0],
            None,
            None,
            AccountFlags::RevocableFlag as u32 | AccountFlags::ClawbackEnabledFlag as u32,
        );

        let asset = Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(self.asset_code),
            issuer: issuer_id,
        });

        TestStellarAssetContract::new_from_asset(&self.host, asset).unwrap()
    }

    fn create_default_account(&self, user: &TestSigner) {
        let signers = match user {
            TestSigner::AccountInvoker(_) => vec![],
            TestSigner::Account(acc_signer) => acc_signer.signers.iter().map(|s| (*s, 1)).collect(),
            TestSigner::AccountContract(_) | TestSigner::ContractInvoker(_) => unreachable!(),
        };
        self.create_account(
            &user.account_id(),
            signers,
            0,
            1,
            [1, 0, 0, 0],
            None,
            None,
            0,
        );
    }

    fn create_default_trustline(&self, user: &TestSigner) -> Rc<LedgerKey> {
        self.create_trustline(
            &user.account_id(),
            &signing_key_to_account_id(&self.issuer_key),
            &self.asset_code,
            0,
            i64::MAX,
            TrustLineFlags::AuthorizedFlag as u32
                | TrustLineFlags::TrustlineClawbackEnabledFlag as u32,
            None,
        )
    }

    fn get_native_balance(&self, account_id: &AccountId) -> i64 {
        let account = self.host.load_account(account_id.clone()).unwrap();
        account.balance
    }

    fn get_trustline_balance(&self, key: &Rc<LedgerKey>) -> i64 {
        self.host
            .with_mut_storage(
                |s| match &s.get_with_host(key, &self.host, None).unwrap().data {
                    LedgerEntryData::Trustline(trustline) => Ok(trustline.balance),
                    _ => unreachable!(),
                },
            )
            .unwrap()
    }
    #[allow(clippy::too_many_arguments)]
    fn create_account(
        &self,
        account_id: &AccountId,
        signers: Vec<(&SigningKey, u32)>,
        balance: i64,
        num_sub_entries: u32,
        thresholds: [u8; 4],
        // (buying, selling) liabilities
        liabilities: Option<(i64, i64)>,
        // (num_sponsored, num_sponsoring) counts
        sponsorships: Option<(u32, u32)>,
        flags: u32,
    ) {
        create_account(
            &self.host,
            account_id,
            signers,
            balance,
            num_sub_entries,
            thresholds,
            liabilities,
            sponsorships,
            flags,
        );
    }

    fn update_account_flags(&self, key: &Rc<LedgerKey>, new_flags: u32) {
        self.host
            .with_mut_storage(|s| {
                let entry = s.get_with_host(key, &self.host, None).unwrap();
                match entry.data.clone() {
                    LedgerEntryData::Account(mut account) => {
                        account.flags = new_flags;
                        let update = Host::modify_ledger_entry_data(
                            &self.host,
                            &entry,
                            LedgerEntryData::Account(account),
                        )?;
                        s.put_with_host(key, &update, None, &self.host, None)
                    }
                    _ => unreachable!(),
                }
            })
            .unwrap();
    }
    #[allow(clippy::too_many_arguments)]
    fn create_trustline(
        &self,
        account_id: &AccountId,
        issuer: &AccountId,
        asset_code: &[u8],
        balance: i64,
        limit: i64,
        flags: u32,
        // (buying, selling) liabilities
        liabilities: Option<(i64, i64)>,
    ) -> Rc<LedgerKey> {
        let asset = match asset_code.len() {
            4 => {
                let mut code = [0_u8; 4];
                code.copy_from_slice(asset_code);
                self.host.create_tl_asset_4(code, issuer.clone())
            }
            12 => {
                let mut code = [0_u8; 12];
                code.copy_from_slice(asset_code);
                self.host.create_tl_asset_12(code, issuer.clone())
            }
            _ => unreachable!(),
        };
        let key = self
            .host
            .to_trustline_key(account_id.clone(), asset.clone())
            .unwrap();
        let ext = if let Some((buying, selling)) = liabilities {
            TrustLineEntryExt::V1({
                TrustLineEntryV1 {
                    liabilities: Liabilities { buying, selling },
                    ext: TrustLineEntryV1Ext::V0,
                }
            })
        } else {
            TrustLineEntryExt::V0
        };
        let trustline_entry = TrustLineEntry {
            account_id: account_id.clone(),
            asset,
            balance,
            limit,
            flags,
            ext,
        };

        self.host
            .add_ledger_entry(
                &key,
                &new_ledger_entry_from_data(LedgerEntryData::Trustline(trustline_entry)),
                None,
            )
            .unwrap();

        key
    }

    fn update_trustline_flags(&self, key: &Rc<LedgerKey>, new_flags: u32) {
        self.host
            .with_mut_storage(|s| {
                let entry = s.get_with_host(key, &self.host, None).unwrap();
                match entry.data.clone() {
                    LedgerEntryData::Trustline(mut trustline) => {
                        trustline.flags = new_flags;
                        let update = Host::modify_ledger_entry_data(
                            &self.host,
                            &entry,
                            LedgerEntryData::Trustline(trustline),
                        )?;
                        s.put_with_host(key, &update, None, &self.host, None)
                    }
                    _ => unreachable!(),
                }
            })
            .unwrap();
    }

    fn run_from_contract<T, F>(
        &self,
        contract_id_bytes: &BytesN<32>,
        f: F,
    ) -> Result<Val, HostError>
    where
        T: Into<Val>,
        F: FnOnce() -> Result<T, HostError>,
    {
        self.host.with_frame(
            Frame::TestContract(TestContractFrame::new(
                Hash(contract_id_bytes.to_array().unwrap()),
                Symbol::try_from_small_str("foo").unwrap(),
                vec![],
                ScContractInstance {
                    executable: ContractExecutable::Wasm(Hash(Default::default())),
                    storage: None,
                },
            )),
            || {
                let res = f();
                match res {
                    Ok(v) => Ok(v.into()),
                    Err(e) => Err(e),
                }
            },
        )
    }

    fn run_from_account<T, F>(&self, account_id: AccountId, f: F) -> Result<T, HostError>
    where
        T: Into<Val>,
        F: FnOnce() -> Result<T, HostError>,
    {
        let prev_source_account = self.host.source_account_id()?;
        self.host.set_source_account(account_id)?;
        let res = f();
        if let Some(acc) = prev_source_account {
            self.host.set_source_account(acc)?;
        }
        res
    }
}

fn to_contract_err(e: HostError) -> ContractError {
    assert!(e.error.is_type(ScErrorType::Contract));
    num_traits::FromPrimitive::from_u32(e.error.get_code()).unwrap()
}

#[test]
fn test_stellar_asset_contract_smart_roundtrip() {
    let test = StellarAssetContractTest::setup(function_name!());

    let account_id = signing_key_to_account_id(&test.user_key);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    let contract = TestStellarAssetContract::new_from_asset(&test.host, Asset::Native).unwrap();
    let expected_stellar_asset_contract_address =
        ScAddress::Contract(test.host.get_asset_contract_id_hash(Asset::Native).unwrap());

    assert_eq!(
        contract.address.to_sc_address().unwrap(),
        expected_stellar_asset_contract_address
    );

    assert_eq!(contract.symbol().unwrap().to_string(), "native");
    assert_eq!(contract.decimals().unwrap(), 7);
    assert_eq!(contract.name().unwrap().to_string(), "native");

    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);

    // Also can't set a new admin (and there is no admin in the first place).
    assert!(contract.set_admin(&user, user.address(&test.host)).is_err());
    assert!(contract.admin().is_err());

    assert_eq!(test.get_native_balance(&account_id), 100_000_000);
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );
}

fn create_asset(issuer_id: &AccountId, asset_code: &[u8]) -> Asset {
    if asset_code.len() == 4 {
        let mut code = [0_u8; 4];
        code.clone_from_slice(asset_code);
        Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(code),
            issuer: issuer_id.clone(),
        })
    } else {
        let mut code = [0_u8; 12];
        code.clone_from_slice(asset_code);
        Asset::CreditAlphanum12(AlphaNum12 {
            asset_code: AssetCode12(code),
            issuer: issuer_id.clone(),
        })
    }
}

fn test_asset_init(testname: &'static str, asset_code: &[u8]) {
    let test = StellarAssetContractTest::setup(testname);
    let issuer_id = signing_key_to_account_id(&test.issuer_key);
    let asset = create_asset(&issuer_id, asset_code);

    let contract = TestStellarAssetContract::new_from_asset(&test.host, asset.clone()).unwrap();
    let expected_stellar_asset_contract_address =
        ScAddress::Contract(test.host.get_asset_contract_id_hash(asset).unwrap());
    assert_eq!(
        contract.address.to_sc_address().unwrap(),
        expected_stellar_asset_contract_address
    );

    let asset_code_trimmed = String::from_utf8(asset_code.to_vec())
        .unwrap()
        .trim_matches(char::from(0))
        .to_string();
    assert_eq!(contract.symbol().unwrap().to_string(), asset_code_trimmed);

    assert_eq!(contract.decimals().unwrap(), 7);

    let name = contract.name().unwrap().to_string();
    let k = ed25519::PublicKey(test.issuer_key.verifying_key().to_bytes());
    assert_eq!(name, format!("{}:{}", asset_code_trimmed, k.to_string()));

    let account_id = signing_key_to_account_id(&test.user_key);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let trustline_key = test.create_trustline(
        &account_id,
        &issuer_id,
        asset_code,
        10_000_000,
        100_000_000,
        TrustLineFlags::AuthorizedFlag as u32,
        None,
    );
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);

    assert_eq!(test.get_trustline_balance(&trustline_key), 10_000_000);
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        10_000_000
    );
}

#[test]
fn test_asset4_init() {
    test_asset_init(function_name!(), b"zA9Z");
}

#[test]
fn test_asset12_init() {
    test_asset_init(function_name!(), b"123def456XYZ");
}

#[test]
fn test_asset4_trailing_zero_init() {
    test_asset_init(function_name!(), b"Az\0\0");
}

#[test]
fn test_asset12_trailing_zero_init() {
    test_asset_init(function_name!(), b"789Az\0\0\0\0\0\0\0");
}

#[test]
fn test_do_not_init_contract_for_invalid_asset() {
    let test = StellarAssetContractTest::setup(function_name!());
    let issuer_id = signing_key_to_account_id(&test.issuer_key);

    let run_test = |asset_code| {
        let asset = create_asset(&issuer_id, asset_code);
        let res = TestStellarAssetContract::new_from_asset(&test.host, asset.clone());
        assert!(res.is_err());
        let e = res.err().unwrap();
        assert!(e.error.is_type(ScErrorType::Value));
        assert!(e.error.is_code(ScErrorCode::InvalidInput));
    };
    // All zero
    run_test(b"\0\0\0\0");
    run_test(b"\0\0\0\0\0\0\0\0\0\0\0\0");
    // Leading zero
    run_test(b"\0ABC");
    run_test(b"\0Abc12\0\0\0\0\0\0");
    // Zero in the middle
    run_test(b"A\0BC");
    run_test(b"Abc\012\0\0\0\0\0\0");
    // Non-alphanumberic character
    run_test(b"$ABC");
    run_test(b"A_BC");
    run_test(b"A\x11\0\0");
    run_test(b"Abc12!\0\0\0\0\0\0");
    run_test(b"Abc1 2\0\0\0\0\0\0");
    run_test(b"Abc1\xff\0\0\0\0\0\0\0");
    // Too short alphanum-12 asset
    run_test(b"Abc1\0\0\0\0\0\0\0\0");
}

#[test]
fn test_zero_amounts() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);

    let user_contract_id = generate_bytes_array(&test.host);
    let user_contract_address = contract_id_to_address(&test.host, user_contract_id);

    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .transfer(&user, user_2.address(&test.host), 0)
        .unwrap();
    contract.burn(&user, 0).unwrap();
    contract
        .burn_from(&user, user_2.address(&test.host), 0)
        .unwrap();

    contract
        .burn_from(&user, user_contract_address.clone(), 0)
        .unwrap();

    // clawback on 0 is fine if a balance or trustline exists
    contract
        .clawback(&admin, user_2.address(&test.host), 0)
        .unwrap();

    //balance doesn't exist
    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user_contract_address.clone(), 0)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    //this will create a 0 balance with clawback enabled because the issuer has the clawback flag set
    contract
        .set_authorized(&admin, user_contract_address.clone(), true)
        .unwrap();
    contract.clawback(&admin, user_contract_address, 0).unwrap();
}

#[test]
fn test_direct_transfer() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 0);

    // Transfer some balance from user 1 to user 2.
    contract
        .transfer(&user, user_2.address(&test.host), 9_999_999)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        90_000_001
    );
    assert_eq!(
        contract.balance(user_2.address(&test.host)).unwrap(),
        9_999_999
    );

    // Can't transfer more than the balance from user 2.
    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user_2, user.address(&test.host), 10_000_000)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // Transfer some balance back from user 2 to user 1.
    contract
        .transfer(&user_2, user.address(&test.host), 999_999)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        91_000_000
    );
    assert_eq!(
        contract.balance(user_2.address(&test.host)).unwrap(),
        9_000_000
    );
}

#[test]
fn test_transfer_with_allowance() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    let user_3 = TestSigner::account(&test.user_key_3);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_account(&user_3);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);
    test.create_default_trustline(&user_3);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 0);
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_3.address(&test.host))
            .unwrap(),
        0
    );

    // Allow 10_000_000 units of contract to be transferred from user by user 3.
    contract
        .approve(&user, user_3.address(&test.host), 10_000_000, 200)
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_3.address(&test.host))
            .unwrap(),
        10_000_000
    );

    // Transfer 5_000_000 of allowance to user 2.
    contract
        .transfer_from(
            &user_3,
            user.address(&test.host),
            user_2.address(&test.host),
            6_000_000,
        )
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        94_000_000
    );
    assert_eq!(
        contract.balance(user_2.address(&test.host)).unwrap(),
        6_000_000
    );
    assert_eq!(contract.balance(user_3.address(&test.host)).unwrap(), 0);
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_3.address(&test.host))
            .unwrap(),
        4_000_000
    );

    // Can't transfer more than remaining allowance.
    assert_eq!(
        to_contract_err(
            contract
                .transfer_from(
                    &user_3,
                    user.address(&test.host),
                    user_3.address(&test.host),
                    4_000_001,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );
    // Set allowance to 0
    contract
        .approve(&user, user_3.address(&test.host), 0, 0)
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_3.address(&test.host))
            .unwrap(),
        0
    );

    contract
        .approve(&user, user_3.address(&test.host), 4_000_000, 200)
        .unwrap();
    // Transfer the remaining allowance to user 3.
    contract
        .transfer_from(
            &user_3,
            user.address(&test.host),
            user_3.address(&test.host),
            4_000_000,
        )
        .unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        90_000_000
    );
    assert_eq!(
        contract.balance(user_2.address(&test.host)).unwrap(),
        6_000_000
    );
    assert_eq!(
        contract.balance(user_3.address(&test.host)).unwrap(),
        4_000_000
    );
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_3.address(&test.host))
            .unwrap(),
        0
    );

    // Can't transfer anything at all now.
    assert_eq!(
        to_contract_err(
            contract
                .transfer_from(
                    &user_3,
                    user.address(&test.host),
                    user_3.address(&test.host),
                    1,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );

    let ledger_num: u32 = test.host.get_ledger_sequence().unwrap().into();
    contract
        .approve(&user, user_3.address(&test.host), 10, ledger_num)
        .unwrap();

    contract
        .transfer_from(
            &user_3,
            user.address(&test.host),
            user_3.address(&test.host),
            5,
        )
        .unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        89_999_995
    );
    assert_eq!(
        contract.balance(user_3.address(&test.host)).unwrap(),
        4_000_005
    );

    // Advance ledger num by one. Allowance should no longer be valid.
    test.host
        .with_mut_ledger_info(|li| li.sequence_number = ledger_num + 1)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .transfer_from(
                    &user_3,
                    user.address(&test.host),
                    user_3.address(&test.host),
                    1,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );
}

#[test]
fn test_allowance_live_until() {
    let test = StellarAssetContractTest::setup(function_name!());
    test.host.enable_debug().unwrap();

    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .mint(&admin, user.address(&test.host), 1000)
        .unwrap();
    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 1000);

    // Allow 10_000_000 units of contract to be transferred from user by user 3.
    contract
        .approve(&user, user_2.address(&test.host), 1000, 200)
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        1000
    );

    //transfer 10
    contract
        .transfer_from(
            &user_2,
            user.address(&test.host),
            user_2.address(&test.host),
            10,
        )
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        990
    );
    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 10);

    // advance the ledger past the live_until of the allowance. Allowance is no longer usable
    test.host
        .with_mut_ledger_info(|li| li.sequence_number = 201)
        .unwrap();
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        0
    );

    assert_eq!(
        to_contract_err(
            contract
                .transfer_from(
                    &user_2,
                    user.address(&test.host),
                    user_2.address(&test.host),
                    1,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );

    // live_until ledger is too low
    assert_eq!(
        to_contract_err(
            contract
                .approve(&user, user_2.address(&test.host), 1000, 200)
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );

    // set new allowance with live_until == ledger sequence_number
    contract
        .approve(&user, user_2.address(&test.host), 10_000, 201)
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        10_000
    );
}

#[test]
fn test_burn() {
    let test = StellarAssetContractTest::setup(function_name!());

    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 0);
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        0
    );

    // Mint to issuer not allowed
    assert_eq!(
        to_contract_err(
            contract
                .mint(&admin, admin.address(&test.host), 1)
                .err()
                .unwrap()
        ),
        ContractError::OperationNotSupportedError
    );

    // Burn from issuer not allowed
    assert_eq!(
        to_contract_err(contract.burn(&admin, 1).err().unwrap()),
        ContractError::OperationNotSupportedError
    );

    // Allow 10_000_000 units of contract to be transferred from user by user 3.
    contract
        .approve(&user, user_2.address(&test.host), 10_000_000, 200)
        .unwrap();

    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        10_000_000
    );

    // Burn 5_000_000 of allowance from user.
    contract
        .burn_from(&user_2, user.address(&test.host), 6_000_000)
        .unwrap();
    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        94_000_000
    );

    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 0);
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        4_000_000
    );

    // Can't burn more than remaining allowance.
    assert_eq!(
        to_contract_err(
            contract
                .burn_from(&user_2, user.address(&test.host), 4_000_001)
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );

    // Burn the remaining allowance to user 3.
    contract
        .burn_from(&user_2, user.address(&test.host), 4_000_000)
        .unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        90_000_000
    );
    assert_eq!(contract.balance(user_2.address(&test.host)).unwrap(), 0);
    assert_eq!(
        contract
            .allowance(user.address(&test.host), user_2.address(&test.host))
            .unwrap(),
        0
    );

    // Now call burn
    contract.burn(&user, 45_000_000).unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        45_000_000
    );

    // Deauthorize the balance of `user` and then try to burn.
    contract
        .set_authorized(&admin, user.address(&test.host), false)
        .unwrap();

    // Can't burn while deauthorized
    assert_eq!(
        to_contract_err(contract.burn(&user, 100).err().unwrap()),
        ContractError::BalanceDeauthorizedError
    );

    // Authorize the balance of `user` and then burn.
    contract
        .set_authorized(&admin, user.address(&test.host), true)
        .unwrap();

    contract.burn(&user, 1_000_000).unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        44_000_000
    );
}

#[test]
fn test_cannot_burn_native() {
    let test = StellarAssetContractTest::setup(function_name!());
    let contract = TestStellarAssetContract::new_from_asset(&test.host, Asset::Native).unwrap();
    let user_acc_id = signing_key_to_account_id(&test.user_key);

    let user = TestSigner::account_with_multisig(&user_acc_id, vec![&test.user_key]);
    let user2 = TestSigner::account(&test.user_key_2);

    test.create_account(
        &user_acc_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );

    assert_eq!(
        to_contract_err(contract.burn(&user, 1).err().unwrap()),
        ContractError::OperationNotSupportedError
    );

    contract
        .approve(&user, user2.address(&test.host), 100, 200)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .burn_from(&user2, user.address(&test.host), 1)
                .err()
                .unwrap()
        ),
        ContractError::OperationNotSupportedError
    );
}

#[test]
fn test_stellar_asset_contract_authorization() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();
    contract
        .mint(&admin, user_2.address(&test.host), 200_000_000)
        .unwrap();

    assert!(contract.authorized(user.address(&test.host)).unwrap());

    // Deauthorize the balance of `user`.
    contract
        .set_authorized(&admin, user.address(&test.host), false)
        .unwrap();

    assert!(!contract.authorized(user.address(&test.host)).unwrap());
    // Make sure neither outgoing nor incoming balance transfers are possible.
    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user, user_2.address(&test.host), 1)
                .err()
                .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );
    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user_2, user.address(&test.host), 1)
                .err()
                .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );

    // Authorize the balance of `user`.
    contract
        .set_authorized(&admin, user.address(&test.host), true)
        .unwrap();

    assert!(contract.authorized(user.address(&test.host)).unwrap());
    // Make sure balance transfers are possible now.
    contract
        .transfer(&user, user_2.address(&test.host), 1)
        .unwrap();
    contract
        .transfer(&user_2, user.address(&test.host), 1)
        .unwrap();
}

#[test]
fn test_clawback_on_account() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let tl_key = test.create_default_trustline(&user);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        100_000_000
    );

    contract
        .clawback(&admin, user.address(&test.host), 40_000_000)
        .unwrap();

    assert_eq!(
        contract.balance(user.address(&test.host)).unwrap(),
        60_000_000
    );

    // Can't clawback more than the balance
    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user.address(&test.host), 60_000_001)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // disable clawback on the trustline
    test.update_trustline_flags(&tl_key, 0);
    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user.address(&test.host), 60_000_000)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // enable clawback on the trustline
    test.update_trustline_flags(&tl_key, TrustLineFlags::TrustlineClawbackEnabledFlag as u32);

    // Clawback everything else
    contract
        .clawback(&admin, user.address(&test.host), 60_000_000)
        .unwrap();
    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 0);
}

#[test]
fn test_clawback_on_contract() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let issuer_ledger_key = test
        .host
        .to_account_key(signing_key_to_account_id(&test.issuer_key))
        .unwrap();

    let user_1 = generate_bytes_array(&test.host);
    let user_2 = generate_bytes_array(&test.host);
    let user_1_addr = contract_id_to_address(&test.host, user_1);
    let user_2_addr = contract_id_to_address(&test.host, user_2);

    contract
        .mint(&admin, user_1_addr.clone(), 100_000_000)
        .unwrap();

    //disable clawback before minting to user_2
    test.update_account_flags(&issuer_ledger_key, 0);
    contract
        .mint(&admin, user_2_addr.clone(), 100_000_000)
        .unwrap();

    assert_eq!(contract.balance(user_1_addr.clone()).unwrap(), 100_000_000);

    assert_eq!(contract.balance(user_2_addr.clone()).unwrap(), 100_000_000);

    // issuer did not have clawback enabled when user_2 balance was created, so this should fail
    contract
        .clawback(&admin, user_1_addr.clone(), 40_000_000)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user_2_addr.clone(), 40_000_000)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // enable clawback on the issuer again. Nothing should change for existing balances
    test.update_account_flags(&issuer_ledger_key, AccountFlags::ClawbackEnabledFlag as u32);

    contract
        .clawback(&admin, user_1_addr.clone(), 40_000_000)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user_2_addr.clone(), 40_000_000)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    assert_eq!(contract.balance(user_1_addr).unwrap(), 20_000_000);

    assert_eq!(contract.balance(user_2_addr).unwrap(), 100_000_000);
}

#[test]
fn test_auth_revocable_on_contract() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let issuer_ledger_key = test
        .host
        .to_account_key(signing_key_to_account_id(&test.issuer_key))
        .unwrap();

    let user_1 = generate_bytes_array(&test.host);
    let user_1_addr = contract_id_to_address(&test.host, user_1);

    // contract is authorized by default
    assert!(contract.authorized(user_1_addr.clone()).unwrap());

    // clear all flags
    test.update_account_flags(&issuer_ledger_key, 0);

    // can't deauthorize because issuer does not have auth revocable set
    assert_eq!(
        to_contract_err(
            contract
                .set_authorized(&admin, user_1_addr.clone(), false)
                .err()
                .unwrap()
        ),
        ContractError::OperationNotSupportedError
    );

    // set auth revocable
    test.update_account_flags(&issuer_ledger_key, AccountFlags::RevocableFlag as u32);

    // Now auth can be revoked
    contract
        .set_authorized(&admin, user_1_addr.clone(), false)
        .unwrap();

    assert!(!contract.authorized(user_1_addr.clone()).unwrap());

    // clear auth revocable and authorize contract, testing that auth
    // revocable only affects deauthorization
    test.update_account_flags(&issuer_ledger_key, 0);

    contract
        .set_authorized(&admin, user_1_addr.clone(), true)
        .unwrap();
    assert!(contract.authorized(user_1_addr).unwrap());
}

#[test]
fn test_auth_required() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let issuer_ledger_key = test
        .host
        .to_account_key(signing_key_to_account_id(&test.issuer_key))
        .unwrap();

    let user_1 = generate_bytes_array(&test.host);
    let user_2 = generate_bytes_array(&test.host);
    let user_1_addr = contract_id_to_address(&test.host, user_1);
    let user_2_addr = contract_id_to_address(&test.host, user_2);

    let user_1_invoker = TestSigner::ContractInvoker(Hash(user_1));
    let user_1_bytes = BytesN::<32>::try_from_val(
        &test.host,
        &test.host.bytes_new_from_slice(&user_1).unwrap(),
    )
    .unwrap();

    contract
        .mint(&admin, user_1_addr.clone(), 100_000_000)
        .unwrap();

    // Set auth required on the issuer. New contract balances will need to be authorized manually
    test.update_account_flags(&issuer_ledger_key, AccountFlags::RequiredFlag as u32);

    // The user_2 balance needs to be authorized first
    assert_eq!(
        to_contract_err(
            test.run_from_contract(&user_1_bytes, || {
                contract.transfer(&user_1_invoker, user_2_addr.clone(), 1)
            })
            .err()
            .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );

    // authorize user_2
    contract
        .set_authorized(&admin, user_2_addr.clone(), true)
        .unwrap();

    test.run_from_contract(&user_1_bytes, || {
        contract.transfer(&user_1_invoker, user_2_addr.clone(), 1)
    })
    .unwrap();

    assert_eq!(contract.balance(user_1_addr.clone()).unwrap(), 99_999_999);
    assert_eq!(contract.balance(user_2_addr.clone()).unwrap(), 1);
}

#[test]
fn test_set_admin() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();
    let new_admin = TestSigner::account(&test.user_key);
    let user = TestSigner::account(&test.user_key_2);
    test.create_default_account(&new_admin);
    test.create_default_trustline(&new_admin);
    test.create_default_account(&user);
    test.create_default_trustline(&user);

    assert_eq!(
        contract.admin().unwrap().to_sc_address().unwrap(),
        admin.address(&test.host).to_sc_address().unwrap()
    );

    // Give admin rights to the new admin.
    contract
        .set_admin(&admin, new_admin.address(&test.host))
        .unwrap();

    assert_eq!(
        contract.admin().unwrap().to_sc_address().unwrap(),
        new_admin.address(&test.host).to_sc_address().unwrap()
    );

    // Make sure admin functions are unavailable to the old admin.
    assert_eq!(
        contract
            .set_admin(&admin, new_admin.address(&test.host))
            .err()
            .unwrap()
            .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
    assert_eq!(
        contract
            .mint(&admin, new_admin.address(&test.host), 1)
            .err()
            .unwrap()
            .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
    assert_eq!(
        contract
            .clawback(&admin, new_admin.address(&test.host), 1)
            .err()
            .unwrap()
            .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
    assert_eq!(
        contract
            .set_authorized(&admin, new_admin.address(&test.host), false)
            .err()
            .unwrap()
            .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
    assert_eq!(
        contract
            .set_authorized(&admin, new_admin.address(&test.host), true)
            .err()
            .unwrap()
            .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );

    // The admin functions are now available to the new admin.
    contract
        .mint(&new_admin, user.address(&test.host), 1)
        .unwrap();
    contract
        .clawback(&new_admin, user.address(&test.host), 1)
        .unwrap();
    contract
        .set_authorized(&new_admin, user.address(&test.host), false)
        .unwrap();
    contract
        .set_authorized(&new_admin, user.address(&test.host), true)
        .unwrap();

    // Return the admin rights to the old admin
    contract
        .set_admin(&new_admin, admin.address(&test.host))
        .unwrap();
    // Make sure old admin can now perform admin operations
    contract.mint(&admin, user.address(&test.host), 1).unwrap();
}

#[test]
fn test_account_balance() {
    let test = StellarAssetContractTest::setup(function_name!());
    let contract = TestStellarAssetContract::new_from_asset(&test.host, Asset::Native).unwrap();
    let user_acc_id = signing_key_to_account_id(&test.user_key);
    let user_addr = account_to_address(&test.host, user_acc_id.clone());

    test.create_account(
        &user_acc_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    assert_eq!(contract.balance(user_addr.clone()).unwrap(), 100_000_000);
}

#[test]
fn test_trustline_auth() {
    let test = StellarAssetContractTest::setup(function_name!());
    // the admin is the issuer_key
    let admin_acc_id = signing_key_to_account_id(&test.issuer_key);
    let user_acc_id = signing_key_to_account_id(&test.user_key);

    let admin = TestSigner::account_with_multisig(&admin_acc_id, vec![&test.issuer_key]);
    let user = TestSigner::account_with_multisig(&user_acc_id, vec![&test.user_key]);
    let contract = test.default_stellar_asset_contract_with_admin_id(&admin.address(&test.host));

    test.create_account(
        &admin_acc_id,
        vec![(&test.issuer_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    test.create_account(
        &user_acc_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    // create a trustline for user_acc so the issuer can mint into it
    test.create_trustline(
        &user_acc_id,
        &admin_acc_id,
        &test.asset_code,
        0,
        10000,
        TrustLineFlags::AuthorizedFlag as u32,
        Some((0, 0)),
    );

    //mint some contract to the user
    contract
        .mint(&admin, user.address(&test.host), 1000)
        .unwrap();

    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 1000);

    // transfer 1 back to the issuer (which gets burned)
    contract
        .transfer(&user, admin.address(&test.host), 1)
        .unwrap();

    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 999);
    assert_eq!(
        contract.balance(admin.address(&test.host)).unwrap(),
        i128::from(i64::MAX)
    );

    // try to deauthorize trustline, but fail because RevocableFlag is not set
    // on the issuer
    assert_eq!(
        to_contract_err(
            contract
                .set_authorized(&admin, user.address(&test.host), false)
                .err()
                .unwrap()
        ),
        ContractError::OperationNotSupportedError
    );

    // Add RevocableFlag to the issuer
    test.create_account(
        &admin_acc_id,
        vec![(&test.issuer_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        AccountFlags::RevocableFlag as u32,
    );

    // trustline should be deauthorized now.

    contract
        .set_authorized(&admin, user.address(&test.host), false)
        .unwrap();

    // transfer should fail from deauthorized trustline
    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user, admin.address(&test.host), 1)
                .err()
                .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );

    // mint should also fail for the same reason
    assert_eq!(
        to_contract_err(
            contract
                .mint(&admin, user.address(&test.host), 1000)
                .err()
                .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );

    // Now authorize trustline
    contract
        .set_authorized(&admin, user.address(&test.host), true)
        .unwrap();

    // Balance operations are possible now.
    contract
        .approve(&user, admin.address(&test.host), 500, 200)
        .unwrap();
    contract
        .transfer_from(
            &admin,
            user.address(&test.host),
            admin.address(&test.host),
            500,
        )
        .unwrap();
    contract.mint(&admin, user.address(&test.host), 1).unwrap();

    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 500);

    // try to clawback
    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user.address(&test.host), 10)
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // set TrustlineClawbackEnabledFlag on trustline
    // Also add selling liabilities to test spendable balance
    test.create_trustline(
        &user_acc_id,
        &admin_acc_id,
        &test.asset_code,
        500,
        10000,
        TrustLineFlags::TrustlineClawbackEnabledFlag as u32,
        Some((0, 10)),
    );

    contract
        .clawback(&admin, user.address(&test.host), 10)
        .unwrap();

    assert_eq!(contract.balance(user.address(&test.host)).unwrap(), 490);
}

#[test]
fn test_account_invoker_auth_with_issuer_admin() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin_acc = signing_key_to_account_id(&test.issuer_key);
    let user_acc = signing_key_to_account_id(&test.user_key);

    test.create_account(
        &admin_acc,
        vec![(&test.issuer_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    test.create_account(
        &user_acc,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let admin_address = account_to_address(&test.host, admin_acc.clone());
    let user_address = account_to_address(&test.host, user_acc.clone());
    let contract = test.default_stellar_asset_contract_with_admin_id(&admin_address);
    // create a trustline for user_acc so the issuer can mint into it
    test.create_trustline(
        &user_acc,
        &admin_acc,
        &test.asset_code,
        0,
        10000,
        TrustLineFlags::AuthorizedFlag as u32,
        Some((0, 0)),
    );

    // Admin invoker can perform admin operation.
    test.run_from_account(admin_acc.clone(), || {
        contract.mint(
            &TestSigner::AccountInvoker(admin_acc.clone()),
            user_address.clone(),
            1000,
        )
    })
    .unwrap();

    // Make another succesful call.
    test.run_from_account(admin_acc.clone(), || {
        contract.mint(
            &TestSigner::AccountInvoker(admin_acc.clone()),
            user_address.clone(),
            2000,
        )
    })
    .unwrap();

    assert_eq!(contract.balance(user_address.clone()).unwrap(), 3000);
    assert_eq!(
        contract.balance(admin_address.clone()).unwrap(),
        i128::from(i64::MAX)
    );

    // User invoker can't perform admin operation.
    assert_eq!(
        test.run_from_account(user_acc.clone(), || {
            contract.mint(
                &TestSigner::AccountInvoker(user_acc.clone()),
                user_address.clone(),
                1000,
            )
        })
        .err()
        .unwrap()
        .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
    // Invoke a transaction with non-matching address - this will fail in host
    // due to invoker mismatching with admin.
    assert!(test
        .run_from_account(user_acc.clone(), || {
            contract.mint(
                &TestSigner::AccountInvoker(admin_acc.clone()),
                user_address.clone(),
                1000,
            )
        })
        .is_err());

    // Perform transfers based on the invoker id.
    test.run_from_account(user_acc.clone(), || {
        contract.transfer(
            &TestSigner::AccountInvoker(user_acc.clone()),
            admin_address.clone(),
            500,
        )
    })
    .unwrap();

    test.run_from_account(admin_acc.clone(), || {
        contract.transfer(
            &TestSigner::AccountInvoker(admin_acc.clone()),
            user_address.clone(),
            800,
        )
    })
    .unwrap();

    assert_eq!(contract.balance(user_address.clone()).unwrap(), 3300);
    assert_eq!(
        contract.balance(admin_address.clone()).unwrap(),
        i128::from(i64::MAX)
    );

    // Contract invoker can't perform unauthorized admin operation.
    let contract_id = generate_bytes_array(&test.host);
    let contract_invoker = TestSigner::ContractInvoker(Hash(contract_id));
    let contract_id_bytes = BytesN::<32>::try_from_val(
        &test.host,
        &test.host.bytes_new_from_slice(&contract_id).unwrap(),
    )
    .unwrap();
    assert_eq!(
        test.run_from_contract(&contract_id_bytes, || {
            contract.mint(&contract_invoker, user_address.clone(), 1000)
        })
        .err()
        .unwrap()
        .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
}

#[test]
fn test_contract_invoker_auth() {
    let test = StellarAssetContractTest::setup(function_name!());

    let admin_contract_id = generate_bytes_array(&test.host);
    let user_contract_id = generate_bytes_array(&test.host);
    let admin_contract_invoker = TestSigner::ContractInvoker(Hash(admin_contract_id));
    let user_contract_invoker = TestSigner::ContractInvoker(Hash(user_contract_id));
    let admin_contract_address = contract_id_to_address(&test.host, admin_contract_id);
    let user_contract_address = contract_id_to_address(&test.host, user_contract_id);
    let admin_contract_id_bytes = BytesN::<32>::try_from_val(
        &test.host,
        &test.host.bytes_new_from_slice(&admin_contract_id).unwrap(),
    )
    .unwrap();
    let user_contract_id_bytes = BytesN::<32>::try_from_val(
        &test.host,
        &test.host.bytes_new_from_slice(&user_contract_id).unwrap(),
    )
    .unwrap();
    let contract = test.default_stellar_asset_contract_with_admin_id(&admin_contract_address);

    test.run_from_contract(&admin_contract_id_bytes, || {
        contract.mint(&admin_contract_invoker, user_contract_address.clone(), 1000)
    })
    .unwrap();

    // Make another succesful call
    test.run_from_contract(&admin_contract_id_bytes, || {
        contract.mint(
            &admin_contract_invoker,
            admin_contract_address.clone(),
            2000,
        )
    })
    .unwrap();

    assert_eq!(
        contract.balance(user_contract_address.clone()).unwrap(),
        1000
    );
    assert_eq!(
        contract.balance(admin_contract_address.clone()).unwrap(),
        2000
    );

    // User contract invoker can't perform admin operation.
    assert_eq!(
        test.run_from_contract(&user_contract_id_bytes, || {
            contract.mint(&user_contract_invoker, user_contract_address.clone(), 1000)
        })
        .err()
        .unwrap()
        .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );

    // Also don't allow an incorrect contract invoker (not a contract error, should
    // be some auth error)
    assert!(test
        .run_from_contract(&user_contract_id_bytes, || {
            contract.mint(&admin_contract_invoker, user_contract_address.clone(), 1000)
        })
        .is_err());

    // Perform transfers based on the invoker id.
    test.run_from_contract(&user_contract_id_bytes, || {
        contract.transfer(&user_contract_invoker, admin_contract_address.clone(), 500)
    })
    .unwrap();

    test.run_from_contract(&admin_contract_id_bytes, || {
        contract.transfer(&admin_contract_invoker, user_contract_address.clone(), 800)
    })
    .unwrap();

    assert_eq!(
        contract.balance(user_contract_address.clone()).unwrap(),
        1300
    );
    assert_eq!(
        contract.balance(admin_contract_address.clone()).unwrap(),
        1700
    );

    // Account invoker can't perform unauthorized admin operation.
    let acc_invoker = TestSigner::AccountInvoker(signing_key_to_account_id(&test.issuer_key));
    assert_eq!(
        test.run_from_account(signing_key_to_account_id(&test.issuer_key), || {
            contract.mint(&acc_invoker, user_contract_address.clone(), 1000)
        })
        .err()
        .unwrap()
        .error,
        (ScErrorType::Auth, ScErrorCode::InvalidAction).into()
    );
}

#[test]
fn test_auth_rejected_for_incorrect_nonce() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();
    let user = TestSigner::account(&test.user_key);
    test.create_default_account(&user);
    test.create_default_trustline(&user);

    let args = test_vec![&test.host, user.address(&test.host), 100_i128];

    // No longer live nonce
    authorize_single_invocation_with_nonce(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        args.clone(),
        Some((12345, 122)),
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Too early for nonce
    authorize_single_invocation_with_nonce(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        args.clone(),
        Some((12345, 123 + 6_312_000)),
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Correct call to consume nonce that is live only until the current ledger.
    authorize_single_invocation_with_nonce(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        args.clone(),
        Some((12345, 123)),
    );
    test.host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .unwrap();

    // Correct call to consume nonce that is live until the maximum possible
    // entry lifetime.
    authorize_single_invocation_with_nonce(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        args.clone(),
        Some((123456, 123 + 10000 - 1)),
    );
    test.host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .unwrap();

    // Try using the consumed nonce
    authorize_single_invocation_with_nonce(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        args.clone(),
        Some((12345, 200)),
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());
}

#[test]
fn test_auth_rejected_for_incorrect_payload() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();
    let user = TestSigner::account(&test.user_key);
    test.create_default_account(&user);
    test.create_default_trustline(&user);

    let args = test_vec![&test.host, user.address(&test.host), 100_i128];

    // Incorrect signer.
    authorize_single_invocation(&test.host, &user, &contract.address, "mint", args.clone());
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Incorrect function.
    authorize_single_invocation(&test.host, &admin, &contract.address, "burn", args.clone());
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Incorrect argument values.
    authorize_single_invocation(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        test_vec![
            &test.host,
            admin.address(&test.host),
            user.address(&test.host),
            1_i128
        ],
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Incorrect argument order.
    authorize_single_invocation(
        &test.host,
        &admin,
        &contract.address,
        "mint",
        test_vec![
            &test.host,
            user.address(&test.host),
            admin.address(&test.host),
            100_i128
        ],
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .is_err());

    // Correct signer and payload result in success.
    authorize_single_invocation(&test.host, &admin, &contract.address, "mint", args.clone());

    test.host
        .call(
            contract.address.into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.into(),
        )
        .unwrap();
}

#[test]
fn test_auth_rejected_for_bad_signature_type() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();
    let user = TestSigner::account(&test.user_key);
    test.create_default_account(&user);
    test.create_default_trustline(&user);

    let args = test_vec![&test.host, user.address(&test.host), 100_i128];

    // Use incorrectly typed value for the signature.
    let admin_bad_val_signer = TestSigner::AccountContract(AccountContractSigner {
        address: admin.address(&test.host),
        sign: Box::new(|_| Val::VOID.into()),
    });
    authorize_single_invocation(
        &test.host,
        &admin_bad_val_signer,
        &contract.address,
        "mint",
        args.clone(),
    );
    assert!(test
        .host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));
}

#[test]
fn test_classic_account_multisig_auth() {
    let test = StellarAssetContractTest::setup(function_name!());

    let account_id = signing_key_to_account_id(&test.user_key);
    test.create_account(
        &account_id,
        vec![
            (&test.user_key_2, u32::MAX),
            (&test.user_key_3, 60),
            (&test.user_key_4, 59),
        ],
        100_000_000,
        1,
        // NB: first threshold is in fact weight of account_id signature.
        [40, 10, 100, 150],
        None,
        None,
        0,
    );
    let contract = TestStellarAssetContract::new_from_asset(&test.host, Asset::Native).unwrap();
    let receiver = TestSigner::account(&test.user_key).address(&test.host);

    // Success: account weight (60) + 40 = 100
    contract
        .transfer(
            &TestSigner::account_with_multisig(&account_id, vec![&test.user_key, &test.user_key_3]),
            receiver.clone(),
            100,
        )
        .unwrap();

    // Success: 1 high weight signer (u32::MAX)
    contract
        .transfer(
            &TestSigner::account_with_multisig(&account_id, vec![&test.user_key_2]),
            receiver.clone(),
            100,
        )
        .unwrap();

    // Success: 60 + 59 > 100, no account signature
    contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key_3, &test.user_key_4],
            ),
            receiver.clone(),
            100,
        )
        .unwrap();

    // Success: 40 + 60 + 59 > 100
    contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key, &test.user_key_3, &test.user_key_4],
            ),
            receiver.clone(),
            100,
        )
        .unwrap();

    // Success: all signers
    contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![
                    &test.user_key,
                    &test.user_key_2,
                    &test.user_key_3,
                    &test.user_key_4,
                ],
            ),
            receiver.clone(),
            100,
        )
        .unwrap();

    // Failure: only account weight (40)
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(&account_id, vec![&test.user_key]),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: 40 + 59 < 100
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(&account_id, vec![&test.user_key, &test.user_key_4]),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: 60 < 100, duplicate signatures
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key_3, &test.user_key_3],
            ),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: 60 + 59 > 100, duplicate signatures
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key_3, &test.user_key_4, &test.user_key_3],
            ),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: 60 < 100 and incorrect signer
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key_3, &test.issuer_key],
            ),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: 60 + 59 > 100, but have incorrect signer
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(
                &account_id,
                vec![&test.user_key_3, &test.user_key_4, &test.issuer_key],
            ),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: too many signatures (even though weight would be enough after
    // deduplication).
    let mut too_many_sigs = vec![];
    for _ in 0..21 {
        too_many_sigs.push(&test.user_key_2);
    }
    assert!(contract
        .transfer(
            &TestSigner::account_with_multisig(&account_id, too_many_sigs),
            receiver.clone(),
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));

    // Failure: out of order signers
    let mut out_of_order_signers = vec![
        &test.user_key,
        &test.user_key_2,
        &test.user_key_3,
        &test.user_key_4,
    ];
    out_of_order_signers.sort_by_key(|k| *k.verifying_key().as_bytes());
    out_of_order_signers.swap(1, 2);
    assert!(contract
        .transfer(
            &TestSigner::Account(AccountSigner {
                account_id: account_id,
                signers: out_of_order_signers,
            }),
            receiver,
            100,
        )
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));
}

#[test]
fn test_negative_amounts_are_not_allowed() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    let user = TestSigner::account(&test.user_key);
    let user_2 = TestSigner::account(&test.user_key_2);
    test.create_default_account(&user);
    test.create_default_account(&user_2);
    test.create_default_trustline(&user);
    test.create_default_trustline(&user_2);

    contract
        .mint(&admin, user.address(&test.host), 100_000_000)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .mint(&admin, user.address(&test.host), -1)
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            contract
                .clawback(&admin, user.address(&test.host), -1)
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user, user_2.address(&test.host), -1)
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            contract
                .approve(&user, user_2.address(&test.host), -1, 200)
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    // Approve some balance before doing the negative transfer_from.
    contract
        .approve(&user, user_2.address(&test.host), 10_000, 200)
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .transfer_from(
                    &user_2,
                    user.address(&test.host),
                    user_2.address(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );
}

fn test_stellar_asset_contract_classic_balance_boundaries(
    test: &StellarAssetContractTest,
    user: &TestSigner,
    account_id: &AccountId,
    init_balance: i64,
    expected_min_balance: i64,
    expected_max_balance: i64,
) {
    let contract = TestStellarAssetContract::new_from_asset(&test.host, Asset::Native).unwrap();

    let new_balance_key = generate_signing_key(&test.host);
    let new_balance_acc = signing_key_to_account_id(&new_balance_key);
    let new_balance_signer =
        TestSigner::account_with_multisig(&new_balance_acc, vec![&new_balance_key]);
    test.create_account(
        &new_balance_acc,
        vec![(&new_balance_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    // Try to do transfer that would leave balance lower than min.
    assert_eq!(
        to_contract_err(
            contract
                .transfer(
                    &user,
                    new_balance_signer.address(&test.host),
                    (init_balance - expected_min_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // now transfer spendable balance
    contract
        .transfer(
            &user,
            new_balance_signer.address(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();
    assert_eq!(test.get_native_balance(account_id), expected_min_balance);

    // now transfer back
    contract
        .transfer(
            &new_balance_signer,
            user.address(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    // Create an account with balance close to i64::MAX and transfer it
    // to the account being tested. That's not a realistic scenario
    // given limited XLM supply, but that's the only way to
    // cover max_balance.
    let large_balance_key = generate_signing_key(&test.host);
    let large_balance_acc = signing_key_to_account_id(&large_balance_key);
    let large_balance_signer =
        TestSigner::account_with_multisig(&large_balance_acc, vec![&large_balance_key]);
    test.create_account(
        &large_balance_acc,
        vec![(&large_balance_key, 100)],
        i64::MAX,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    // Transferring a balance that would exceed
    // expected_max_balance shouldn't be possible.
    assert_eq!(
        to_contract_err(
            contract
                .transfer(
                    &large_balance_signer,
                    user.address(&test.host),
                    (expected_max_balance - init_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // ...but transferring exactly up to expected_max_balance should be
    // possible.
    contract
        .transfer(
            &large_balance_signer,
            user.address(&test.host),
            (expected_max_balance - init_balance).into(),
        )
        .unwrap();

    assert_eq!(test.get_native_balance(account_id), expected_max_balance);
}

#[test]
fn test_stellar_asset_contract_classic_balance_boundaries_simple() {
    let test = StellarAssetContractTest::setup(function_name!());

    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    // Account with no liabilities/sponsorships.
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        5,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    // Min balance should be
    // (2 (account) + 5 (subentries)) * base_reserve = 35_000_000.
    test_stellar_asset_contract_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        100_000_000,
        35_000_000,
        i64::MAX,
    );
}

#[test]
fn test_stellar_asset_contract_classic_balance_boundaries_with_liabilities() {
    let test = StellarAssetContractTest::setup(function_name!());
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        1_000_000_000,
        8,
        [1, 0, 0, 0],
        Some((300_000_000, 500_000_000)),
        None,
        0,
    );
    // Min balance should be
    // (2 (account) + 8 (subentries)) * base_reserve + 500_000_000 (selling
    // liabilities) = 550_000_000.
    // Max balance should be i64::MAX - 300_000_000 (buying liabilities).
    test_stellar_asset_contract_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        1_000_000_000,
        550_000_000,
        i64::MAX - 300_000_000,
    );
}

#[test]
fn test_stellar_asset_contract_classic_balance_boundaries_with_sponsorships() {
    let test = StellarAssetContractTest::setup(function_name!());
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        5,
        [1, 0, 0, 0],
        None,
        Some((3, 6)),
        0,
    );
    // Min balance should be
    // (2 (account) + 5 (subentries) + (6 sponsoring - 3 sponsored)) * base_reserve
    // = 50_000_000.
    test_stellar_asset_contract_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        100_000_000,
        50_000_000,
        i64::MAX,
    );
}

#[test]
fn test_stellar_asset_contract_classic_balance_boundaries_with_sponsorships_and_liabilities() {
    let test = StellarAssetContractTest::setup(function_name!());
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        1_000_000_000,
        5,
        [1, 0, 0, 0],
        Some((300_000_000, 500_000_000)),
        Some((3, 6)),
        0,
    );
    // Min balance should be
    // (2 (account) + 5 (subentries) + (6 sponsoring - 3 sponsored)) * base_reserve
    // + 500_000_000 = 550_000_000.
    // Max balance should be i64::MAX - 300_000_000 (buying liabilities).
    test_stellar_asset_contract_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        1_000_000_000,
        550_000_000,
        i64::MAX - 300_000_000,
    );
}

#[test]
fn test_stellar_asset_contract_classic_balance_boundaries_with_large_values() {
    let test = StellarAssetContractTest::setup(function_name!());
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        i64::MAX - i64::MAX / 4,
        u32::MAX,
        [1, 0, 0, 0],
        Some((i64::MAX / 4, i64::MAX / 5)),
        Some((0, u32::MAX)),
        0,
    );
    test_stellar_asset_contract_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        i64::MAX - i64::MAX / 4,
        (2 /* account */ + u32::MAX as i64 /* sub-entries */ + u32::MAX as i64/* sponsoring - sponsored */)
            * 5_000_000
            + i64::MAX / 5, /* Selling liabilities */
        i64::MAX - i64::MAX / 4, /* Buying liabilities */
    );
}

fn test_wrapped_asset_classic_balance_boundaries(
    testname: &'static str,
    init_balance: i64,
    expected_min_balance: i64,
    expected_max_balance: i64,
    // (buying, selling) liabilities
    liabilities: Option<(i64, i64)>,
    limit: i64,
) {
    let test = StellarAssetContractTest::setup(testname);
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account_with_multisig(&account_id, vec![&test.user_key]);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let account_id2 = signing_key_to_account_id(&test.user_key_2);
    let user2 = TestSigner::account_with_multisig(&account_id2, vec![&test.user_key_2]);
    test.create_account(
        &account_id2,
        vec![(&test.user_key_2, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let issuer_id = signing_key_to_account_id(&test.issuer_key);
    let issuer = TestSigner::account_with_multisig(&issuer_id, vec![&test.issuer_key]);
    test.create_account(
        &issuer_id,
        vec![(&test.issuer_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let trustline_key = test.create_trustline(
        &account_id,
        &issuer_id,
        &[100; 12],
        init_balance,
        limit,
        TrustLineFlags::AuthorizedFlag as u32,
        liabilities,
    );

    let trustline_key2 = test.create_trustline(
        &account_id2,
        &issuer_id,
        &[100; 12],
        0,
        limit,
        TrustLineFlags::AuthorizedFlag as u32,
        Some((0, 0)),
    );

    let contract = TestStellarAssetContract::new_from_asset(
        &test.host,
        Asset::CreditAlphanum12(AlphaNum12 {
            asset_code: AssetCode12([100; 12]),
            issuer: AccountId(PublicKey::PublicKeyTypeEd25519(
                test.host.u256_from_account(&issuer_id).unwrap(),
            )),
        }),
    )
    .unwrap();
    // Try to do transfer that would leave balance lower than min.
    assert_eq!(
        to_contract_err(
            contract
                .transfer(
                    &user,
                    user2.address(&test.host),
                    (init_balance - expected_min_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // Send spendable balance
    contract
        .transfer(
            &user,
            user2.address(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();
    assert_eq!(
        test.get_trustline_balance(&trustline_key),
        expected_min_balance
    );

    assert_eq!(
        test.get_trustline_balance(&trustline_key2),
        init_balance - expected_min_balance
    );

    // Send back
    contract
        .transfer(
            &user2,
            user.address(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    // Minting amount that would exceed expected_max_balance
    // shouldn't be possible.
    assert_eq!(
        to_contract_err(
            contract
                .mint(
                    &issuer,
                    user.address(&test.host),
                    (expected_max_balance - init_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // ...but transferring exactly up to expected_max_balance should be
    // possible.
    contract
        .mint(
            &issuer,
            user.address(&test.host),
            (expected_max_balance - init_balance).into(),
        )
        .unwrap();

    assert_eq!(
        test.get_trustline_balance(&trustline_key),
        expected_max_balance
    );
    assert_eq!(test.get_trustline_balance(&trustline_key2), 0);

    contract
        .mint(&issuer, user2.address(&test.host), 1.into())
        .unwrap();

    assert_eq!(
        to_contract_err(
            contract
                .transfer(
                    &user,
                    user2.address(&test.host),
                    (expected_max_balance).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );
}

#[test]
fn test_asset_stellar_asset_contract_classic_balance_boundaries_simple() {
    test_wrapped_asset_classic_balance_boundaries(
        function_name!(),
        100_000_000,
        0,
        i64::MAX,
        None,
        i64::MAX,
    );
}

#[test]
fn test_asset_stellar_asset_contract_classic_balance_boundaries_with_trustline_limit() {
    test_wrapped_asset_classic_balance_boundaries(
        function_name!(),
        100_000_000,
        0,
        200_000_000,
        None,
        200_000_000,
    );
}

#[test]
fn test_asset_stellar_asset_contract_classic_balance_boundaries_with_liabilities() {
    test_wrapped_asset_classic_balance_boundaries(
        function_name!(),
        100_000_000,
        300_000,
        i64::MAX - 500_000,
        Some((500_000, 300_000)),
        i64::MAX,
    );
}

#[test]
fn test_asset_stellar_asset_contract_classic_balance_boundaries_with_limit_and_liabilities() {
    test_wrapped_asset_classic_balance_boundaries(
        function_name!(),
        100_000_000,
        300_000,
        150_000_000, /* = 200_000_000 (limit) - 50_000_000 (buying liabilities) */
        Some((50_000_000, 300_000)),
        200_000_000,
    );
}

#[test]
fn test_asset_stellar_asset_contract_classic_balance_boundaries_large_values() {
    test_wrapped_asset_classic_balance_boundaries(
        function_name!(),
        i64::MAX - i64::MAX / 5,
        i64::MAX / 4,
        i64::MAX - i64::MAX / 5,
        Some((i64::MAX / 5, i64::MAX / 4)),
        i64::MAX,
    );
}

#[test]
fn test_classic_transfers_not_possible_for_unauthorized_asset() {
    let test = StellarAssetContractTest::setup(function_name!());
    let account_id = signing_key_to_account_id(&test.user_key);
    let user = TestSigner::account(&test.user_key);
    test.create_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let issuer_id = signing_key_to_account_id(&test.issuer_key);

    let trustline_key = test.create_trustline(
        &account_id,
        &issuer_id,
        &[99; 4],
        100_000_000,
        i64::MAX,
        TrustLineFlags::AuthorizedFlag as u32,
        None,
    );

    assert_eq!(test.get_trustline_balance(&trustline_key), 100_000_000);

    let contract = TestStellarAssetContract::new_from_asset(
        &test.host,
        Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4([99; 4]),
            issuer: AccountId(PublicKey::PublicKeyTypeEd25519(
                test.host.u256_from_account(&issuer_id).unwrap(),
            )),
        }),
    )
    .unwrap();

    // Authorized to transfer
    contract
        .transfer(&user, user.address(&test.host), 1_i128)
        .unwrap();

    // Override the trustline authorization flag.
    let trustline_key = test.create_trustline(
        &account_id,
        &issuer_id,
        &[99; 4],
        100_000_000,
        i64::MAX,
        0,
        None,
    );

    // No longer authorized
    assert_eq!(
        to_contract_err(
            contract
                .transfer(&user, user.address(&test.host), 1_i128)
                .err()
                .unwrap()
        ),
        ContractError::BalanceDeauthorizedError
    );

    // Trustline balance stays the same.
    assert_eq!(test.get_trustline_balance(&trustline_key), 100_000_000);
}

#[test]
fn test_custom_account_auth() {
    let test = StellarAssetContractTest::setup(function_name!());
    let admin_kp = generate_signing_key(&test.host);
    let account_contract_addr_obj = test
        .host
        .register_test_contract_wasm(SIMPLE_ACCOUNT_CONTRACT);
    let account_contract_addr: Address =
        account_contract_addr_obj.try_into_val(&test.host).unwrap();
    let admin = TestSigner::AccountContract(AccountContractSigner {
        address: account_contract_addr.clone(),
        sign: simple_account_sign_fn(&test.host, &admin_kp),
    });

    let admin_public_key = BytesN::<32>::try_from_val(
        &test.host,
        &test
            .host
            .bytes_new_from_slice(admin_kp.verifying_key().as_bytes().as_slice())
            .unwrap(),
    )
    .unwrap();
    // Initialize the admin account
    test.host
        .call(
            account_contract_addr_obj,
            Symbol::try_from_small_str("init").unwrap(),
            test_vec![&test.host, admin_public_key].into(),
        )
        .unwrap();

    let contract = test.default_stellar_asset_contract_with_admin_id(&admin.address(&test.host));
    let user = TestSigner::account(&test.user_key);
    let user_address = user.address(&test.host);
    test.create_default_account(&user);
    test.create_default_trustline(&user);

    contract.mint(&admin, user_address.clone(), 100).unwrap();
    assert_eq!(contract.balance(user_address.clone()).unwrap(), 100);

    // Create a signer for the new admin, but not yet set its key as the account
    // owner.
    let new_admin_kp = generate_signing_key(&test.host);
    let new_admin = TestSigner::AccountContract(AccountContractSigner {
        address: account_contract_addr,
        sign: simple_account_sign_fn(&test.host, &new_admin_kp),
    });
    let new_admin_public_key = BytesN::<32>::try_from_val(
        &test.host,
        &test
            .host
            .bytes_new_from_slice(new_admin_kp.verifying_key().as_bytes().as_slice())
            .unwrap(),
    )
    .unwrap();
    // The new signer can't authorize admin ops.
    assert!(contract
        .mint(&new_admin, user_address.clone(), 100)
        .is_err());

    // Authorize the 'set_owner' invocation using the current owner signature.
    authorize_single_invocation(
        &test.host,
        &admin,
        &account_contract_addr_obj.try_into_val(&test.host).unwrap(),
        "set_owner",
        test_vec![&test.host, new_admin_public_key],
    );

    // Change the owner of the account.
    test.host
        .call(
            account_contract_addr_obj,
            Symbol::try_from_small_str("set_owner").unwrap(),
            test_vec![&test.host, new_admin_public_key].into(),
        )
        .unwrap();

    // Now the contract ops should work with the signatures from the new admin
    // account owner.
    contract
        .mint(&new_admin, user_address.clone(), 100)
        .unwrap();
    assert_eq!(contract.balance(user_address.clone()).unwrap(), 200);

    // And they shouldn't work with the old owner signatures.
    assert!(contract.mint(&admin, user_address.clone(), 100).is_err());

    // Remove the contract instance completely to represent the scenario
    // when admin account doesn't exist.
    test.host
        .with_mut_storage(|storage| {
            let contract_id = test
                .host
                .contract_id_from_address(account_contract_addr_obj)?;
            let key = test.host.contract_instance_ledger_key(&contract_id)?;
            // Note, that this represents 'correct footprint, missing value' scenario.
            // Incorrect footprint scenario is not covered (it's not auth specific).
            storage.del_with_host(&key, &test.host, None)
        })
        .unwrap();

    // Operations may no longer be authorized.
    assert!(contract
        .mint(&new_admin, user_address.clone(), 100)
        .is_err());
}

#[test]
fn test_recording_auth_for_stellar_asset_contract() {
    let test = StellarAssetContractTest::setup(function_name!());

    let contract = test.default_stellar_asset_contract();

    let admin = TestSigner::account(&test.issuer_key);
    let user = TestSigner::account(&test.user_key);
    test.create_default_account(&user);
    test.create_default_trustline(&user);
    test.host.switch_to_recording_auth(true).unwrap();

    let args = test_vec![&test.host, user.address(&test.host), 100_i128];
    test.host
        .call(
            contract.address.clone().into(),
            Symbol::try_from_small_str("mint").unwrap(),
            args.clone().into(),
        )
        .unwrap();
    let recorded_payloads = test.host.get_recorded_auth_payloads().unwrap();

    assert_eq!(
        recorded_payloads,
        vec![RecordedAuthPayload {
            address: Some(admin.address(&test.host).to_sc_address().unwrap()),
            nonce: Some(0),
            invocation: SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                    contract_address: contract.address.to_sc_address().unwrap(),
                    function_name: xdr::ScSymbol("mint".try_into().unwrap()),
                    args: vec![
                        ScVal::try_from_val(
                            &test.host,
                            &Val::try_from_val(&test.host, &user.address(&test.host)).unwrap(),
                        )
                        .unwrap(),
                        ScVal::try_from_val(
                            &test.host,
                            &Val::try_from_val(&test.host, &100_i128).unwrap(),
                        )
                        .unwrap(),
                    ]
                    .try_into()
                    .unwrap(),
                }),
                sub_invocations: Default::default(),
            },
        }]
    );

    assert_eq!(
        test.host.get_authenticated_authorizations().unwrap(),
        vec![(
            admin.address(&test.host).to_sc_address().unwrap(),
            SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                    contract_address: contract.address.to_sc_address().unwrap(),
                    function_name: ScSymbol("mint".try_into().unwrap()),
                    args: test.host.vecobject_to_scval_vec(args.into()).unwrap(),
                }),
                sub_invocations: Default::default(),
            }
        )]
    );
}

#[test]
fn verify_nested_try_call_rollback() -> Result<(), HostError> {
    // This test calls "invoke" in the ivocation contract, which then dispatches a call
    // to "fail_after_updates". "fail_after_updates" emits an event, saves data, and does a
    // SAC token transfer before emitting an error.

    let test = StellarAssetContractTest::setup(function_name!());

    let invoke_id_obj = test.host.register_test_contract_wasm(INVOKE_CONTRACT);
    let err_id_obj = test.host.register_test_contract_wasm(ERR);
    let err_address = Address::try_from_val(&test.host, &err_id_obj)?;

    let admin = TestSigner::account(&test.issuer_key);
    let contract = test.default_stellar_asset_contract();

    contract
        .mint(&admin, err_address.clone(), 100_000_000)
        .unwrap();

    test.host.call(
        invoke_id_obj,
        Symbol::try_from_small_str("invoke")?,
        test_vec![
            &test.host,
            err_id_obj,
            Symbol::try_from_val(&test.host, &"fail_after_updates").unwrap(),
            contract.address.as_object()
        ]
        .into(),
    )?;

    assert_eq!(contract.balance(err_address.clone()).unwrap(), 100_000_000);
    assert_eq!(contract.balance(contract.address.clone()).unwrap(), 0);

    assert_eq!(
        bool::try_from_val(
            &test.host,
            &test.host.call(
                err_id_obj,
                Symbol::try_from_val(&test.host, &"storage_updated")?,
                test_vec![&test.host].into(),
            )?,
        )?,
        false
    );

    let events = test.host.get_events()?.0;

    // Make sure the event emitted in "fail_after_updates" is marked as failed
    assert!(
        events
            .iter()
            .find(|e| {
                if e.event.type_ == ContractEventType::Contract && e.event.contract_id.is_some() {
                    let address = ScAddress::Contract(e.event.contract_id.clone().unwrap());
                    let id = test.host.add_host_object(address).unwrap();
                    test.host.obj_cmp(err_id_obj.to_val(), id.to_val()).unwrap() == 0
                } else {
                    false
                }
            })
            .unwrap()
            .failed_call
    );

    Ok(())
}

#[test]
fn test_sac_reentry_is_not_allowed() {
    let test = StellarAssetContractTest::setup(function_name!());
    let issuer = TestSigner::account(&test.issuer_key);
    let issuer_id = signing_key_to_account_id(&test.issuer_key);
    test.create_default_account(&issuer);
    let asset_1 = Asset::CreditAlphanum4(AlphaNum4 {
        asset_code: AssetCode4([b'c'; 4]),
        issuer: issuer_id.clone(),
    });
    let asset_2 = Asset::CreditAlphanum4(AlphaNum4 {
        asset_code: AssetCode4([b'Z'; 4]),
        issuer: issuer_id,
    });

    let contract_1 = TestStellarAssetContract::new_from_asset(&test.host, asset_1).unwrap();
    let contract_2 = TestStellarAssetContract::new_from_asset(&test.host, asset_2).unwrap();

    let account_contract_addr_obj = test
        .host
        .register_test_contract_wasm(SAC_REENTRY_TEST_CONTRACT);
    let account_contract_addr: Address =
        account_contract_addr_obj.try_into_val(&test.host).unwrap();

    contract_1
        .mint(&issuer, account_contract_addr.clone(), 100000)
        .unwrap();
    contract_2
        .mint(&issuer, account_contract_addr.clone(), 100000)
        .unwrap();

    let account_signer = TestSigner::AccountContract(AccountContractSigner {
        address: account_contract_addr.clone(),
        sign: Box::new(|_| Val::VOID.into()),
    });
    test.host
        .call(
            account_contract_addr_obj,
            Symbol::try_from_small_str("set_addr").unwrap(),
            test_vec![&test.host, contract_1.address].into(),
        )
        .unwrap();

    contract_2
        .transfer(&account_signer, contract_1.address.clone(), 100)
        .unwrap();

    assert!(contract_1
        .transfer(&account_signer, contract_2.address.clone(), 100)
        .err()
        .unwrap()
        .error
        .is_type(ScErrorType::Auth));
}
