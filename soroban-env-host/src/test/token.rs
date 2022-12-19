use std::{convert::TryInto, rc::Rc};

use crate::{
    budget::{AsBudget, Budget},
    host::{Frame, TestContractFrame},
    host_vec,
    native_contract::{
        testutils::{
            generate_bytes, generate_keypair, sign_args, signer_to_account_id, signer_to_id_bytes,
            AccountSigner, HostVec, TestSigner,
        },
        token::{
            error::ContractError,
            public_types::{Ed25519Signature, Identifier, Signature},
            test_token::TestToken,
        },
    },
    storage::{test_storage::MockSnapshotSource, Storage},
    Host, HostError, LedgerInfo,
};
use ed25519_dalek::Keypair;
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
        AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountFlags, AccountId, AlphaNum12,
        AlphaNum4, Asset, AssetCode12, AssetCode4, Hash, HostFunctionType, LedgerEntryData,
        LedgerKey, Liabilities, PublicKey, ScStatusType, SequenceNumber, SignerKey, Thresholds,
        TrustLineEntry, TrustLineEntryExt, TrustLineEntryV1, TrustLineEntryV1Ext, TrustLineFlags,
    },
    RawVal,
};
use soroban_env_common::{CheckedEnv, Symbol, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::BytesN;

struct TokenTest {
    host: Host,
    admin_key: Keypair,
    issuer_key: Keypair,
    user_key: Keypair,
    user_key_2: Keypair,
    user_key_3: Keypair,
    user_key_4: Keypair,
    asset_code: [u8; 4],
}

impl TokenTest {
    fn setup() -> Self {
        let snapshot_source = Rc::<MockSnapshotSource>::new(MockSnapshotSource::new());
        let storage = Storage::with_recording_footprint(snapshot_source);
        let host = Host::with_storage_and_budget(storage, Budget::default());
        host.set_ledger_info(LedgerInfo {
            protocol_version: 20,
            sequence_number: 123,
            timestamp: 123456,
            network_passphrase: vec![1, 2, 3, 4],
            base_reserve: 5_000_000,
        });
        Self {
            host,
            admin_key: generate_keypair(),
            issuer_key: generate_keypair(),
            user_key: generate_keypair(),
            user_key_2: generate_keypair(),
            user_key_3: generate_keypair(),
            user_key_4: generate_keypair(),
            asset_code: [0_u8; 4],
        }
    }

    fn default_token_with_admin_id(&self, new_admin: Identifier) -> TestToken {
        let issuer_id = signer_to_account_id(&self.host, &self.issuer_key);
        self.create_classic_account(
            &issuer_id,
            vec![(&self.issuer_key, 100)],
            10_000_000,
            1,
            [1, 0, 0, 0],
            None,
            None,
            0,
        );

        let asset = Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(self.asset_code),
            issuer: issuer_id.clone(),
        });

        let token = TestToken::new_from_asset(&self.host, asset.clone());

        if let Identifier::Account(new_admin_id) = new_admin.clone() {
            if new_admin_id == issuer_id {
                return token;
            }
        }

        let issuer = TestSigner::account(&issuer_id, vec![&self.issuer_key]);
        token
            .set_admin(
                &issuer,
                token.nonce(issuer.get_identifier(&self.host)).unwrap(),
                new_admin,
            )
            .unwrap();
        token
    }

    fn default_token(&self, admin: &TestSigner) -> TestToken {
        self.default_token_with_admin_id(admin.get_identifier(&self.host))
    }

    fn get_native_balance(&self, account_id: &AccountId) -> i64 {
        let account = self.host.load_account(account_id.clone()).unwrap();
        account.balance
    }

    fn get_classic_trustline_balance(&self, key: &LedgerKey) -> i64 {
        self.host
            .with_mut_storage(|s| match s.get(key, self.host.as_budget()).unwrap().data {
                LedgerEntryData::Trustline(trustline) => Ok(trustline.balance),
                _ => unreachable!(),
            })
            .unwrap()
    }

    fn create_classic_account(
        &self,
        account_id: &AccountId,
        signers: Vec<(&Keypair, u32)>,
        balance: i64,
        num_sub_entries: u32,
        thresholds: [u8; 4],
        // (buying, selling) liabilities
        liabilities: Option<(i64, i64)>,
        // (num_sponsored, num_sponsoring) counts
        sponsorships: Option<(u32, u32)>,
        flags: u32,
    ) {
        let key = self.host.to_account_key(account_id.clone().into());
        let account_id = match &key {
            LedgerKey::Account(acc) => acc.account_id.clone(),
            _ => unreachable!(),
        };
        let mut acc_signers = vec![];
        for (signer, weight) in signers {
            acc_signers.push(soroban_env_common::xdr::Signer {
                key: SignerKey::Ed25519(
                    self.host
                        .to_u256(signer_to_id_bytes(&self.host, signer).into())
                        .unwrap(),
                ),
                weight,
            });
        }
        let ext = if sponsorships.is_some() || liabilities.is_some() {
            AccountEntryExt::V1(AccountEntryExtensionV1 {
                liabilities: if let Some((buying, selling)) = liabilities {
                    Liabilities { buying, selling }
                } else {
                    Liabilities {
                        buying: 0,
                        selling: 0,
                    }
                },
                ext: if let Some((num_sponsored, num_sponsoring)) = sponsorships {
                    AccountEntryExtensionV1Ext::V2(AccountEntryExtensionV2 {
                        num_sponsored,
                        num_sponsoring,
                        signer_sponsoring_i_ds: Default::default(),
                        ext: AccountEntryExtensionV2Ext::V0 {},
                    })
                } else {
                    AccountEntryExtensionV1Ext::V0
                },
            })
        } else {
            AccountEntryExt::V0
        };
        let acc_entry = AccountEntry {
            account_id,
            balance: balance,
            seq_num: SequenceNumber(0),
            num_sub_entries,
            inflation_dest: None,
            flags,
            home_domain: Default::default(),
            thresholds: Thresholds(thresholds),
            signers: acc_signers.try_into().unwrap(),
            ext,
        };

        self.host
            .add_ledger_entry(
                key,
                Host::ledger_entry_from_data(LedgerEntryData::Account(acc_entry)),
            )
            .unwrap();
    }

    fn create_classic_trustline(
        &self,
        account_id: &AccountId,
        issuer: &AccountId,
        asset_code: &[u8],
        balance: i64,
        limit: i64,
        flags: u32,
        // (buying, selling) liabilities
        liabilities: Option<(i64, i64)>,
    ) -> LedgerKey {
        let asset = match asset_code.len() {
            4 => {
                let mut code = [0_u8; 4];
                code.copy_from_slice(asset_code);
                self.host.create_asset_4(code, issuer.clone())
            }
            12 => {
                let mut code = [0_u8; 12];
                code.copy_from_slice(asset_code);
                self.host.create_asset_12(code, issuer.clone())
            }
            _ => unreachable!(),
        };
        let key = self
            .host
            .to_trustline_key(account_id.clone(), asset.clone());
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
                key.clone(),
                Host::ledger_entry_from_data(LedgerEntryData::Trustline(trustline_entry)),
            )
            .unwrap();

        key
    }

    fn run_from_contract<T, F>(
        &self,
        contract_id_bytes: &BytesN<32>,
        f: F,
    ) -> Result<RawVal, HostError>
    where
        T: Into<RawVal>,
        F: FnOnce() -> Result<T, HostError>,
    {
        self.host.with_frame(
            Frame::TestContract(TestContractFrame::new(
                Hash(contract_id_bytes.to_array().unwrap()),
                Symbol::from_str("foo"),
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

    fn run_from_account<T, F>(&self, account_id: AccountId, f: F) -> Result<RawVal, HostError>
    where
        T: Into<RawVal>,
        F: FnOnce() -> Result<T, HostError>,
    {
        self.host.set_source_account(account_id);
        self.host.with_frame(
            Frame::HostFunction(HostFunctionType::InvokeContract),
            || {
                let res = f();
                match res {
                    Ok(v) => Ok(v.into()),
                    Err(e) => Err(e),
                }
            },
        )
    }
}

fn to_contract_err(e: HostError) -> ContractError {
    assert!(e.status.is_type(ScStatusType::ContractError));
    num_traits::FromPrimitive::from_u32(e.status.get_code()).unwrap()
}

#[test]
fn test_native_token_smart_roundtrip() {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        100_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    let token = TestToken::new_from_asset(&test.host, Asset::Native);
    let expected_token_id = BytesN::<32>::try_from_val(
        &test.host,
        test.host.get_contract_id_from_asset(Asset::Native).unwrap(),
    )
    .unwrap();

    assert_eq!(token.id.to_vec(), expected_token_id.to_vec());

    assert_eq!(token.symbol().unwrap().to_vec(), b"native".to_vec());
    assert_eq!(token.decimals().unwrap(), 7);
    assert_eq!(token.name().unwrap().to_vec(), b"native".to_vec());

    let user = TestSigner::account(&account_id, vec![&test.user_key]);

    // Also can't set a new admin (and there is no admin in the first place).
    assert!(token
        .set_admin(&user, 0, user.get_identifier(&test.host))
        .is_err());

    assert_eq!(test.get_native_balance(&account_id), 100_000_000);
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(token.nonce(user.get_identifier(&test.host)).unwrap(), 0);
}

fn test_classic_asset_init(asset_code: &[u8]) {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let issuer_id = signer_to_account_id(&test.host, &test.admin_key);

    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let trustline_key = test.create_classic_trustline(
        &account_id,
        &issuer_id,
        asset_code,
        10_000_000,
        100_000_000,
        TrustLineFlags::AuthorizedFlag as u32,
        None,
    );
    let asset = if asset_code.len() == 4 {
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
    };
    let token = TestToken::new_from_asset(&test.host, asset.clone());
    let expected_token_id = BytesN::<32>::try_from_val(
        &test.host,
        test.host.get_contract_id_from_asset(asset).unwrap(),
    )
    .unwrap();
    assert_eq!(token.id.to_vec(), expected_token_id.to_vec());

    assert_eq!(token.symbol().unwrap().to_vec(), asset_code.to_vec());
    assert_eq!(token.decimals().unwrap(), 7);
    assert_eq!(
        token.name().unwrap().to_vec(),
        [
            asset_code.to_vec(),
            b":".to_vec(),
            test.admin_key.public.to_bytes().to_vec()
        ]
        .concat()
        .to_vec()
    );

    let user = TestSigner::account(&account_id, vec![&test.user_key]);

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        10_000_000
    );
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        10_000_000
    );
    assert_eq!(token.nonce(user.get_identifier(&test.host)).unwrap(), 0);
}

#[test]
fn test_classic_asset4_smart_init() {
    test_classic_asset_init(&[0, 'a' as u8, 'b' as u8, 255]);
}

#[test]
fn test_classic_asset12_smart_init() {
    test_classic_asset_init(&[255, 0, 0, 127, b'a', b'b', b'c', 1, 2, 3, 4, 5]);
}

#[test]
fn test_direct_transfer() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(token.balance(user_2.get_identifier(&test.host)).unwrap(), 0);

    // Transfer some balance from user 1 to user 2.
    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            9_999_999,
        )
        .unwrap();
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        90_000_001
    );
    assert_eq!(
        token.balance(user_2.get_identifier(&test.host)).unwrap(),
        9_999_999
    );

    // Can't transfer more than the balance from user 2.
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user_2,
                    token.nonce(user_2.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    10_000_000,
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // Transfer some balance back from user 2 to user 1.
    token
        .xfer(
            &user_2,
            token.nonce(user_2.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            999_999,
        )
        .unwrap();
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        91_000_000
    );
    assert_eq!(
        token.balance(user_2.get_identifier(&test.host)).unwrap(),
        9_000_000
    );
}

#[test]
fn test_transfer_with_allowance() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    let user_3 = TestSigner::Ed25519(&test.user_key_3);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        100_000_000
    );
    assert_eq!(token.balance(user_2.get_identifier(&test.host)).unwrap(), 0);
    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap(),
        0
    );

    // Allow 10_000_000 units of token to be transferred from user by user 3.
    token
        .incr_allow(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_3.get_identifier(&test.host),
            10_000_000,
        )
        .unwrap();

    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap(),
        10_000_000
    );

    // Transfer 5_000_000 of allowance to user 2.
    token
        .xfer_from(
            &user_3,
            token.nonce(user_3.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            user_2.get_identifier(&test.host),
            6_000_000,
        )
        .unwrap();
    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        94_000_000
    );
    assert_eq!(
        token.balance(user_2.get_identifier(&test.host)).unwrap(),
        6_000_000
    );
    assert_eq!(token.balance(user_3.get_identifier(&test.host)).unwrap(), 0);
    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap(),
        4_000_000
    );

    // Can't transfer more than remaining allowance.
    assert_eq!(
        to_contract_err(
            token
                .xfer_from(
                    &user_3,
                    token.nonce(user_3.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    user_3.get_identifier(&test.host),
                    4_000_001,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );

    // Decrease allow by more than what's left. This will set the allowance to 0
    token
        .decr_allow(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_3.get_identifier(&test.host),
            10_000_000,
        )
        .unwrap();

    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap(),
        0
    );

    token
        .incr_allow(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_3.get_identifier(&test.host),
            4_000_000,
        )
        .unwrap();

    // Transfer the remaining allowance to user 3.
    token
        .xfer_from(
            &user_3,
            token.nonce(user_3.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            user_3.get_identifier(&test.host),
            4_000_000,
        )
        .unwrap();

    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        90_000_000
    );
    assert_eq!(
        token.balance(user_2.get_identifier(&test.host)).unwrap(),
        6_000_000
    );
    assert_eq!(
        token.balance(user_3.get_identifier(&test.host)).unwrap(),
        4_000_000
    );
    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap(),
        0
    );

    // Can't transfer anything at all now.
    assert_eq!(
        to_contract_err(
            token
                .xfer_from(
                    &user_3,
                    token.nonce(user_3.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    user_3.get_identifier(&test.host),
                    1,
                )
                .err()
                .unwrap()
        ),
        ContractError::AllowanceError
    );
}

#[test]
fn test_freeze_and_unfreeze() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            200_000_000,
        )
        .unwrap();

    assert!(!token.is_frozen(user.get_identifier(&test.host)).unwrap());

    // Freeze the balance of `user`.
    token
        .freeze(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
        )
        .unwrap();

    assert!(token.is_frozen(user.get_identifier(&test.host)).unwrap());
    // Make sure neither outgoing nor incoming balance transfers are possible.
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user_2.get_identifier(&test.host),
                    1
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceFrozenError
    );
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user_2,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    1
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceFrozenError
    );

    // Unfreeze the balance of `user`.
    token
        .unfreeze(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
        )
        .unwrap();

    assert!(!token.is_frozen(user.get_identifier(&test.host)).unwrap());
    // Make sure balance transfers are possible now.
    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            1,
        )
        .unwrap();
    token
        .xfer(
            &user_2,
            token.nonce(user_2.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            1,
        )
        .unwrap();
}

#[test]
fn test_clawback() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();

    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        100_000_000
    );

    token
        .clawback(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            40_000_000,
        )
        .unwrap();

    assert_eq!(
        token.balance(user.get_identifier(&test.host)).unwrap(),
        60_000_000
    );

    // Can't clawback more than the balance
    assert_eq!(
        to_contract_err(
            token
                .clawback(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    60_000_001,
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // clawback everything else
    token
        .clawback(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            60_000_000,
        )
        .unwrap();
    assert_eq!(token.balance(user.get_identifier(&test.host)).unwrap(), 0);
}

#[test]
fn test_set_admin() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);
    let new_admin = TestSigner::Ed25519(&test.user_key);

    // Give admin rights to the new admin.
    token
        .set_admin(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
        )
        .unwrap();

    // Make sure admin functions are unavailable to the old admin.
    assert_eq!(
        to_contract_err(
            token
                .set_admin(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    new_admin.get_identifier(&test.host),
                )
                .err()
                .unwrap()
        ),
        ContractError::UnauthorizedError
    );
    assert_eq!(
        to_contract_err(
            token
                .mint(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    new_admin.get_identifier(&test.host),
                    1
                )
                .err()
                .unwrap()
        ),
        ContractError::UnauthorizedError
    );
    assert_eq!(
        to_contract_err(
            token
                .clawback(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    new_admin.get_identifier(&test.host),
                    1
                )
                .err()
                .unwrap()
        ),
        ContractError::UnauthorizedError
    );
    assert_eq!(
        to_contract_err(
            token
                .freeze(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    new_admin.get_identifier(&test.host),
                )
                .err()
                .unwrap()
        ),
        ContractError::UnauthorizedError
    );
    assert_eq!(
        to_contract_err(
            token
                .unfreeze(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    new_admin.get_identifier(&test.host),
                )
                .err()
                .unwrap()
        ),
        ContractError::UnauthorizedError
    );

    // The admin functions are now available to the new admin.
    token
        .mint(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
            1,
        )
        .unwrap();
    token
        .clawback(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
            1,
        )
        .unwrap();
    token
        .freeze(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
        )
        .unwrap();
    token
        .unfreeze(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
        )
        .unwrap();

    // Return the admin rights to the old admin
    token
        .set_admin(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
        )
        .unwrap();
    // Make sure old admin can now perform admin operations
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
            1,
        )
        .unwrap();
}

#[test]
fn test_account_spendable_balance() {
    let test = TokenTest::setup();
    let token = TestToken::new_from_asset(&test.host, Asset::Native);
    let user_acc = signer_to_account_id(&test.host, &test.user_key);
    let user_id = Identifier::Account(user_acc.clone());

    test.create_classic_account(
        &user_acc,
        vec![(&test.user_key, 100)],
        100_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    assert_eq!(token.balance(user_id.clone()).unwrap(), 100_000_000);
    // base reserve = 5_000_000
    // signer + account = 3 base reserves
    assert_eq!(token.spendable(user_id.clone()).unwrap(), 85_000_000);
}

#[test]
fn test_trustline_auth() {
    let test = TokenTest::setup();
    // the admin is the issuer_key
    let admin_acc = signer_to_account_id(&test.host, &test.issuer_key);
    let user_acc = signer_to_account_id(&test.host, &test.user_key);

    test.create_classic_account(
        &admin_acc,
        vec![(&test.admin_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    test.create_classic_account(
        &user_acc,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let admin_id = Identifier::Account(admin_acc.clone());
    let user_id = Identifier::Account(user_acc.clone());

    let acc_invoker = TestSigner::AccountInvoker;
    let token = test.default_token_with_admin_id(admin_id.clone());

    // create a trustline for user_acc so the issuer can mint into it
    test.create_classic_trustline(
        &user_acc,
        &admin_acc,
        &test.asset_code,
        0,
        10000,
        TrustLineFlags::AuthorizedFlag as u32,
        Some((0, 0)),
    );

    //mint to user_id
    test.run_from_account(admin_acc.clone(), || {
        token.mint(&acc_invoker, 0, user_id.clone(), 1000)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 1000);

    // transfer 1 back to the issuer (which gets burned)
    test.run_from_account(user_acc.clone(), || {
        token.xfer(&acc_invoker, 0, admin_id.clone(), 1)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 999);
    assert_eq!(token.balance(admin_id.clone()).unwrap(), i64::MAX.into());

    // try to deauthorize trustline, but fail because RevocableFlag is not set on the issuer
    assert_eq!(
        to_contract_err(
            test.run_from_account(admin_acc.clone(), || {
                token.freeze(&acc_invoker, 0, user_id.clone())
            })
            .err()
            .unwrap()
        ),
        ContractError::OperationNotSupportedError
    );

    // Add RevocableFlag to the issuer
    test.create_classic_account(
        &admin_acc,
        vec![(&test.admin_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        AccountFlags::RevocableFlag as u32,
    );

    // trustline should be deauthorized now.
    test.run_from_account(admin_acc.clone(), || {
        token.freeze(&acc_invoker, 0, user_id.clone())
    })
    .unwrap();

    // transfer should fail from deauthorized trustline
    assert_eq!(
        to_contract_err(
            test.run_from_account(user_acc.clone(), || {
                token.xfer(&acc_invoker, 0, admin_id.clone(), 1)
            })
            .err()
            .unwrap()
        ),
        ContractError::BalanceFrozenError
    );

    // mint should also fail for the same reason
    assert_eq!(
        to_contract_err(
            test.run_from_account(admin_acc.clone(), || {
                token.mint(&acc_invoker, 0, user_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::BalanceFrozenError
    );

    // Now authorize trustline
    test.run_from_account(admin_acc.clone(), || {
        token.unfreeze(&acc_invoker, 0, user_id.clone())
    })
    .unwrap();

    test.run_from_account(user_acc.clone(), || {
        token.incr_allow(&acc_invoker, 0, admin_id.clone(), 500)
    })
    .unwrap();

    test.run_from_account(admin_acc.clone(), || {
        token.xfer_from(&acc_invoker, 0, user_id.clone(), admin_id, 500)
    })
    .unwrap();

    test.run_from_account(admin_acc.clone(), || {
        token.mint(&acc_invoker, 0, user_id.clone(), 1)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 500);

    // try to clawback
    assert_eq!(
        to_contract_err(
            test.run_from_account(admin_acc.clone(), || {
                token.clawback(&acc_invoker, 0, user_id.clone(), 10)
            })
            .err()
            .unwrap()
        ),
        ContractError::BalanceError
    );

    // set TrustlineClawbackEnabledFlag on trustline
    // Also add selling liabilities to test spendable balance
    test.create_classic_trustline(
        &user_acc,
        &admin_acc,
        &test.asset_code,
        500,
        10000,
        TrustLineFlags::TrustlineClawbackEnabledFlag as u32,
        Some((0, 10)),
    );

    test.run_from_account(admin_acc.clone(), || {
        token.clawback(&acc_invoker, 0, user_id.clone(), 10)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 490);
    assert_eq!(token.spendable(user_id.clone()).unwrap(), 480);
}

#[test]
fn test_account_invoker_auth_with_issuer_admin() {
    let test = TokenTest::setup();
    let admin_acc = signer_to_account_id(&test.host, &test.issuer_key);
    let user_acc = signer_to_account_id(&test.host, &test.user_key);

    test.create_classic_account(
        &admin_acc,
        vec![(&test.admin_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );
    test.create_classic_account(
        &user_acc,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let admin_id = Identifier::Account(admin_acc.clone());
    let user_id = Identifier::Account(user_acc.clone());

    let acc_invoker = TestSigner::AccountInvoker;
    let token = test.default_token_with_admin_id(admin_id.clone());

    // create a trustline for user_acc so the issuer can mint into it
    test.create_classic_trustline(
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
        token.mint(&acc_invoker, 0, user_id.clone(), 1000)
    })
    .unwrap();

    // Non-zero nonce is not allowed for invoker.
    assert_eq!(
        to_contract_err(
            test.run_from_account(admin_acc.clone(), || {
                token.mint(&acc_invoker, 1, user_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::NonceError
    );

    // Make another succesful call with 0 nonce.
    test.run_from_account(admin_acc.clone(), || {
        token.mint(&acc_invoker, 0, admin_id.clone(), 2000)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 1000);
    assert_eq!(token.balance(admin_id.clone()).unwrap(), i64::MAX.into());

    // // User invoker can't perform admin operation.
    // test.host.set_source_account(user_acc.clone());
    assert_eq!(
        to_contract_err(
            test.run_from_account(user_acc.clone(), || {
                token.mint(&acc_invoker, 0, user_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::UnauthorizedError
    );

    // Perform transfers based on the invoker id.
    test.run_from_account(user_acc.clone(), || {
        token.xfer(&acc_invoker, 0, admin_id.clone(), 500)
    })
    .unwrap();

    test.run_from_account(admin_acc.clone(), || {
        token.xfer(&acc_invoker, 0, user_id.clone(), 800)
    })
    .unwrap();

    assert_eq!(token.balance(user_id.clone()).unwrap(), 1300);
    assert_eq!(token.balance(admin_id.clone()).unwrap(), i64::MAX.into());

    // Contract invoker can't perform unauthorized admin operation.
    assert_eq!(
        to_contract_err(
            test.run_from_contract(&generate_bytes(&test.host), || {
                token.mint(&TestSigner::ContractInvoker, 0, user_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::UnauthorizedError
    );
}

#[test]
fn test_contract_invoker_auth() {
    let test = TokenTest::setup();
    let contract_invoker = TestSigner::ContractInvoker;

    let admin_contract_id_bytes = generate_bytes(&test.host);
    let user_contract_id_bytes = generate_bytes(&test.host);
    let admin_contract_id = Identifier::Contract(admin_contract_id_bytes.clone());
    let user_contract_id = Identifier::Contract(user_contract_id_bytes.clone());

    let token = test.default_token_with_admin_id(admin_contract_id.clone());

    test.run_from_contract(&admin_contract_id_bytes, || {
        token.mint(&contract_invoker, 0, user_contract_id.clone(), 1000)
    })
    .unwrap();

    // Non-zero nonce is not allowed for invoker.
    assert_eq!(
        to_contract_err(
            test.run_from_contract(&admin_contract_id_bytes, || {
                token.mint(&contract_invoker, 1, user_contract_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::NonceError
    );

    // Make another succesful call with 0 nonce.
    test.run_from_contract(&admin_contract_id_bytes, || {
        token.mint(&contract_invoker, 0, admin_contract_id.clone(), 2000)
    })
    .unwrap();

    assert_eq!(token.balance(user_contract_id.clone()).unwrap(), 1000);
    assert_eq!(token.balance(admin_contract_id.clone()).unwrap(), 2000);

    // User contract invoker can't perform admin operation.
    assert_eq!(
        to_contract_err(
            test.run_from_contract(&user_contract_id_bytes, || {
                token.mint(&contract_invoker, 0, user_contract_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::UnauthorizedError
    );

    // Perform transfers based on the invoker id.
    test.run_from_contract(&user_contract_id_bytes, || {
        token.xfer(&contract_invoker, 0, admin_contract_id.clone(), 500)
    })
    .unwrap();

    test.run_from_contract(&admin_contract_id_bytes, || {
        token.xfer(&contract_invoker, 0, user_contract_id.clone(), 800)
    })
    .unwrap();

    assert_eq!(token.balance(user_contract_id.clone()).unwrap(), 1300);
    assert_eq!(token.balance(admin_contract_id.clone()).unwrap(), 1700);

    // Account invoker can't perform unauthorized admin operation.
    let acc_invoker = TestSigner::AccountInvoker;
    assert_eq!(
        to_contract_err(
            test.run_from_account(signer_to_account_id(&test.host, &test.admin_key), || {
                token.mint(&acc_invoker, 0, user_contract_id.clone(), 1000)
            })
            .err()
            .unwrap()
        ),
        ContractError::UnauthorizedError
    );
}

#[test]
fn test_auth_rejected_with_incorrect_nonce() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);
    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);

    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();

    // Bump user's nonce and approve some amount to cover xfer_from below.
    token
        .incr_allow(&user, 0, user_2.get_identifier(&test.host), 1000)
        .unwrap();

    assert_eq!(
        to_contract_err(
            token
                .xfer(&user, 2, user_2.get_identifier(&test.host), 1000)
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );

    assert_eq!(
        to_contract_err(
            token
                .incr_allow(&user, 2, user_2.get_identifier(&test.host), 1000)
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );
    assert_eq!(
        to_contract_err(
            token
                .xfer_from(
                    &user_2,
                    1,
                    user.get_identifier(&test.host),
                    user_2.get_identifier(&test.host),
                    100
                )
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );
    assert_eq!(
        to_contract_err(
            token
                .mint(&admin, 2, user.get_identifier(&test.host), 10_000_000,)
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );
    assert_eq!(
        to_contract_err(
            token
                .clawback(&admin, 2, user.get_identifier(&test.host), 10_000_000,)
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );
    assert_eq!(
        to_contract_err(
            token
                .set_admin(&admin, 2, user.get_identifier(&test.host))
                .err()
                .unwrap()
        ),
        ContractError::NonceError
    );
}

#[test]
fn test_auth_rejected_with_incorrect_signer() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);
    let user = TestSigner::Ed25519(&test.user_key);

    let nonce = 0;
    let amount = 1000;
    let user_signature = sign_args(
        &test.host,
        &user,
        "mint",
        &token.id,
        host_vec![
            &test.host,
            admin.get_identifier(&test.host),
            nonce.clone(),
            user.get_identifier(&test.host),
            amount.clone(),
        ],
    );
    // Replace public key in the user signature to imitate admin signature.
    let signature = Signature::Ed25519(Ed25519Signature {
        public_key: match admin.get_identifier(&test.host) {
            Identifier::Ed25519(id) => id,
            _ => unreachable!(),
        },
        signature: match user_signature {
            Signature::Ed25519(signature) => signature.signature,
            _ => unreachable!(),
        },
    });

    assert!(test
        .host
        .call(
            token.id.clone().into(),
            Symbol::from_str("mint").into(),
            host_vec![
                &test.host,
                signature,
                nonce,
                user.get_identifier(&test.host),
                amount
            ]
            .into(),
        )
        .is_err());
}

#[test]
fn test_auth_rejected_for_incorrect_function_name() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);
    let user = TestSigner::Ed25519(&test.user_key);

    let nonce = 0;
    let amount = 1000;
    let signature = sign_args(
        &test.host,
        &admin,
        "clawback",
        &token.id,
        host_vec![
            &test.host,
            admin.get_identifier(&test.host),
            nonce.clone(),
            user.get_identifier(&test.host),
            amount.clone(),
        ],
    );

    assert!(test
        .host
        .call(
            token.id.clone().into(),
            Symbol::from_str("mint").into(),
            host_vec![
                &test.host,
                signature,
                nonce,
                user.get_identifier(&test.host),
                amount
            ]
            .into(),
        )
        .is_err());
}

#[test]
fn test_auth_rejected_for_incorrect_function_args() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);
    let user = TestSigner::Ed25519(&test.user_key);

    let nonce = 0;
    let signature = sign_args(
        &test.host,
        &admin,
        "mint",
        &token.id,
        host_vec![
            &test.host,
            admin.get_identifier(&test.host),
            nonce.clone(),
            user.get_identifier(&test.host),
            1000,
        ],
    );

    assert!(test
        .host
        .call(
            token.id.clone().into(),
            Symbol::from_str("mint").into(),
            host_vec![
                &test.host,
                signature,
                nonce,
                user.get_identifier(&test.host),
                // call with 1000000 amount instead of 1000 that was signed.
                1_000_000,
            ]
            .into(),
        )
        .is_err());
}

#[test]
fn test_classic_account_multisig_auth() {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    test.create_classic_account(
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

    let account_ident = Identifier::Account(account_id.clone());
    let token = TestToken::new_from_asset(&test.host, Asset::Native);

    // Success: account weight (60) + 40 = 100
    token
        .xfer(
            &TestSigner::account(&account_id, vec![&test.user_key, &test.user_key_3]),
            token.nonce(account_ident.clone()).unwrap(),
            account_ident.clone(),
            100,
        )
        .unwrap();

    // Success: 1 high weight signer (u32::MAX)
    token
        .xfer(
            &TestSigner::account(&account_id, vec![&test.user_key_2]),
            token.nonce(account_ident.clone()).unwrap(),
            account_ident.clone(),
            100,
        )
        .unwrap();

    // Success: 60 + 59 > 100, no account signature
    token
        .xfer(
            &TestSigner::account(&account_id, vec![&test.user_key_3, &test.user_key_4]),
            token.nonce(account_ident.clone()).unwrap(),
            account_ident.clone(),
            100,
        )
        .unwrap();

    // Success: 40 + 60 + 59 > 100
    token
        .xfer(
            &TestSigner::account(
                &account_id,
                vec![&test.user_key, &test.user_key_3, &test.user_key_4],
            ),
            token.nonce(account_ident.clone()).unwrap(),
            account_ident.clone(),
            100,
        )
        .unwrap();

    // Success: all signers
    token
        .xfer(
            &TestSigner::account(
                &account_id,
                vec![
                    &test.user_key,
                    &test.user_key_2,
                    &test.user_key_3,
                    &test.user_key_4,
                ],
            ),
            token.nonce(account_ident.clone()).unwrap(),
            account_ident.clone(),
            100,
        )
        .unwrap();

    // Failure: only account weight (40)
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(&account_id, vec![&test.user_key]),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: 40 + 59 < 100
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(&account_id, vec![&test.user_key, &test.user_key_4]),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: 60 < 100, duplicate signatures
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(&account_id, vec![&test.user_key_3, &test.user_key_3]),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: 60 + 59 > 100, duplicate signatures
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(
                        &account_id,
                        vec![&test.user_key_3, &test.user_key_4, &test.user_key_3],
                    ),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: 60 < 100 and incorrect signer
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(&account_id, vec![&test.user_key_3, &test.admin_key],),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: 60 + 59 > 100, but have incorrect signer
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(
                        &account_id,
                        vec![&test.user_key_3, &test.user_key_4, &test.admin_key],
                    ),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: too many signatures (even though weight would be enough after
    // deduplication).
    let mut too_many_sigs = vec![];
    for _ in 0..21 {
        too_many_sigs.push(&test.user_key_2);
    }
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::account(&account_id, too_many_sigs,),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident.clone(),
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );

    // Failure: out of order signers
    let mut out_of_order_signers = vec![
        &test.user_key,
        &test.user_key_2,
        &test.user_key_3,
        &test.user_key_4,
    ];
    out_of_order_signers.sort_by_key(|k| k.public.as_bytes());
    out_of_order_signers.swap(1, 2);
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &TestSigner::Account(AccountSigner {
                        account_id: account_id,
                        signers: out_of_order_signers,
                    }),
                    token.nonce(account_ident.clone()).unwrap(),
                    account_ident,
                    100,
                )
                .err()
                .unwrap()
        ),
        ContractError::AuthenticationError
    );
}

#[test]
fn test_negative_amounts_are_not_allowed() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            100_000_000,
        )
        .unwrap();

    assert_eq!(
        to_contract_err(
            token
                .mint(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            token
                .clawback(
                    &admin,
                    token.nonce(admin.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user_2.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            token
                .incr_allow(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user_2.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    assert_eq!(
        to_contract_err(
            token
                .decr_allow(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user_2.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );

    // Approve some balance before doing the negative xfer_from.
    token
        .incr_allow(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            10_000,
        )
        .unwrap();

    assert_eq!(
        to_contract_err(
            token
                .xfer_from(
                    &user_2,
                    token.nonce(user_2.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    user_2.get_identifier(&test.host),
                    -1,
                )
                .err()
                .unwrap()
        ),
        ContractError::NegativeAmountError
    );
}

fn test_native_token_classic_balance_boundaries(
    test: &TokenTest,
    user: &TestSigner,
    account_id: &AccountId,
    init_balance: i64,
    expected_min_balance: i64,
    expected_max_balance: i64,
) {
    let token = TestToken::new_from_asset(&test.host, Asset::Native);

    let new_balance_key = generate_keypair();
    let new_balance_acc = signer_to_account_id(&test.host, &new_balance_key);
    let new_balance_signer = TestSigner::account(&new_balance_acc, vec![&new_balance_key]);
    test.create_classic_account(
        &new_balance_acc,
        vec![(&new_balance_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    // Try to do xfer that would leave balance lower than min.
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    new_balance_signer.get_identifier(&test.host),
                    (init_balance - expected_min_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // now transfer spendable balance
    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            new_balance_signer.get_identifier(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    // now transfer back
    token
        .xfer(
            &new_balance_signer,
            token
                .nonce(new_balance_signer.get_identifier(&test.host))
                .unwrap(),
            user.get_identifier(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    // Create an account with balance close to i64::MAX and transfer it
    // to the account being tested. That's not a realistic scenario
    // given limited XLM supply, but that's the only way to
    // cover max_balance.
    let large_balance_key = generate_keypair();
    let large_balance_acc = signer_to_account_id(&test.host, &large_balance_key);
    let large_balance_signer = TestSigner::account(&large_balance_acc, vec![&large_balance_key]);
    test.create_classic_account(
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
            token
                .xfer(
                    &large_balance_signer,
                    token
                        .nonce(large_balance_signer.get_identifier(&test.host))
                        .unwrap(),
                    user.get_identifier(&test.host),
                    (expected_max_balance - init_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // ...but transferring exactly up to expected_max_balance should be
    // possible.
    token
        .xfer(
            &large_balance_signer,
            token
                .nonce(large_balance_signer.get_identifier(&test.host))
                .unwrap(),
            user.get_identifier(&test.host),
            (expected_max_balance - init_balance).into(),
        )
        .unwrap();

    assert_eq!(test.get_native_balance(account_id), expected_max_balance);
}

#[test]
fn test_native_token_classic_balance_boundaries_simple() {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    // Account with no liabilities/sponsorships.
    test.create_classic_account(
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
    test_native_token_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        100_000_000,
        35_000_000,
        i64::MAX,
    );
}

#[test]
fn test_native_token_classic_balance_boundaries_with_liabilities() {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
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
    test_native_token_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        1_000_000_000,
        550_000_000,
        i64::MAX - 300_000_000,
    );
}

#[test]
fn test_native_token_classic_balance_boundaries_with_sponsorships() {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
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
    test_native_token_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        100_000_000,
        50_000_000,
        i64::MAX,
    );
}

#[test]
fn test_native_token_classic_balance_boundaries_with_sponsorships_and_liabilities() {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
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
    test_native_token_classic_balance_boundaries(
        &test,
        &user,
        &account_id,
        1_000_000_000,
        550_000_000,
        i64::MAX - 300_000_000,
    );
}

#[test]
fn test_native_token_classic_balance_boundaries_with_large_values() {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        i64::MAX - i64::MAX / 4,
        u32::MAX,
        [1, 0, 0, 0],
        Some((i64::MAX / 4, i64::MAX / 5)),
        Some((0, u32::MAX)),
        0,
    );
    test_native_token_classic_balance_boundaries(
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
    init_balance: i64,
    expected_min_balance: i64,
    expected_max_balance: i64,
    // (buying, selling) liabilities
    liabilities: Option<(i64, i64)>,
    limit: i64,
) {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let account_id2 = signer_to_account_id(&test.host, &test.user_key_2);
    let user2 = TestSigner::account(&account_id2, vec![&test.user_key_2]);
    test.create_classic_account(
        &account_id2,
        vec![(&test.user_key_2, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let issuer_id = signer_to_account_id(&test.host, &test.admin_key);
    let issuer = TestSigner::account(&issuer_id, vec![&test.admin_key]);
    test.create_classic_account(
        &issuer_id,
        vec![(&test.admin_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let trustline_key = test.create_classic_trustline(
        &account_id,
        &issuer_id,
        &[255; 12],
        init_balance,
        limit,
        TrustLineFlags::AuthorizedFlag as u32,
        liabilities,
    );

    let trustline_key2 = test.create_classic_trustline(
        &account_id2,
        &issuer_id,
        &[255; 12],
        0,
        limit,
        TrustLineFlags::AuthorizedFlag as u32,
        Some((0, 0)),
    );

    let token = TestToken::new_from_asset(
        &test.host,
        Asset::CreditAlphanum12(AlphaNum12 {
            asset_code: AssetCode12([255; 12]),
            issuer: AccountId(PublicKey::PublicKeyTypeEd25519(
                test.host.to_u256_from_account(&issuer_id).unwrap(),
            )),
        }),
    );
    // Try to do transfer that would leave balance lower than min.
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    (init_balance - expected_min_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // Send spendable balance
    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user2.get_identifier(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        expected_min_balance
    );

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key2),
        init_balance - expected_min_balance
    );

    // Send back
    token
        .xfer(
            &user2,
            token.nonce(user2.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            (init_balance - expected_min_balance).into(),
        )
        .unwrap();

    // Mint a balancethat would exceed
    // expected_max_balance shouldn't be possible.
    assert_eq!(
        to_contract_err(
            token
                .mint(
                    &issuer,
                    token.nonce(issuer.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    (expected_max_balance - init_balance + 1).into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceError
    );

    // ...but transferring exactly up to expected_max_balance should be
    // possible.
    token
        .mint(
            &issuer,
            token.nonce(issuer.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            (expected_max_balance - init_balance).into(),
        )
        .unwrap();

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        expected_max_balance
    );
    assert_eq!(test.get_classic_trustline_balance(&trustline_key2), 0);
}

#[test]
fn test_asset_token_classic_balance_boundaries_simple() {
    test_wrapped_asset_classic_balance_boundaries(100_000_000, 0, i64::MAX, None, i64::MAX);
}

#[test]
fn test_asset_token_classic_balance_boundaries_with_trustline_limit() {
    test_wrapped_asset_classic_balance_boundaries(100_000_000, 0, 200_000_000, None, 200_000_000);
}

#[test]
fn test_asset_token_classic_balance_boundaries_with_liabilities() {
    test_wrapped_asset_classic_balance_boundaries(
        100_000_000,
        300_000,
        i64::MAX - 500_000,
        Some((500_000, 300_000)),
        i64::MAX,
    );
}

#[test]
fn test_asset_token_classic_balance_boundaries_with_limit_and_liabilities() {
    test_wrapped_asset_classic_balance_boundaries(
        100_000_000,
        300_000,
        150_000_000, /* = 200_000_000 (limit) - 50_000_000 (buying liabilities) */
        Some((50_000_000, 300_000)),
        200_000_000,
    );
}

#[test]
fn test_asset_token_classic_balance_boundaries_large_values() {
    test_wrapped_asset_classic_balance_boundaries(
        i64::MAX - i64::MAX / 5,
        i64::MAX / 4,
        i64::MAX - i64::MAX / 5,
        Some((i64::MAX / 5, i64::MAX / 4)),
        i64::MAX,
    );
}

#[test]
fn test_classic_transfers_not_possible_for_unauthorized_asset() {
    let test = TokenTest::setup();
    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let user = TestSigner::account(&account_id, vec![&test.user_key]);
    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        0,
        [1, 0, 0, 0],
        None,
        None,
        0,
    );

    let issuer_id = signer_to_account_id(&test.host, &test.admin_key);

    let trustline_key = test.create_classic_trustline(
        &account_id,
        &issuer_id,
        &[255; 4],
        100_000_000,
        i64::MAX,
        TrustLineFlags::AuthorizedFlag as u32,
        None,
    );

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        100_000_000
    );

    let token = TestToken::new_from_asset(
        &test.host,
        Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4([255; 4]),
            issuer: AccountId(PublicKey::PublicKeyTypeEd25519(
                test.host.to_u256_from_account(&issuer_id).unwrap(),
            )),
        }),
    );

    // Authorized to xfer
    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            1.into(),
        )
        .unwrap();

    // Override the trustline authorization flag.
    let trustline_key = test.create_classic_trustline(
        &account_id,
        &issuer_id,
        &[255; 4],
        100_000_000,
        i64::MAX,
        0,
        None,
    );

    // No longer authorized
    assert_eq!(
        to_contract_err(
            token
                .xfer(
                    &user,
                    token.nonce(user.get_identifier(&test.host)).unwrap(),
                    user.get_identifier(&test.host),
                    1.into(),
                )
                .err()
                .unwrap()
        ),
        ContractError::BalanceFrozenError
    );

    // Trustline balance stays the same.
    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        100_000_000
    );
}
