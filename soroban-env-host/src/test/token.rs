use std::{convert::TryInto, rc::Rc};

use crate::{
    budget::Budget,
    native_contract::{
        base_types::BigInt,
        testutils::{generate_keypair, signer_to_account_id, signer_to_id_bytes, TestSigner},
        token::{public_types::TokenMetadata, test_token::TestToken},
    },
    storage::{test_storage::MockSnapshotSource, Storage},
    Host, LedgerInfo,
};
use ed25519_dalek::Keypair;
use soroban_env_common::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
    AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountId, AlphaNum12, AlphaNum4, Asset,
    AssetCode12, AssetCode4, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey, Liabilities,
    SequenceNumber, SignerKey, Thresholds, TrustLineEntry, TrustLineEntryExt, TrustLineEntryV1,
    TrustLineEntryV1Ext, TrustLineFlags,
};
use soroban_env_common::{EnvBase, TryFromVal};

use crate::native_contract::base_types::{Bytes, BytesN};

struct TokenTest {
    host: Host,
    admin_key: Keypair,
    user_key: Keypair,
    user_key_2: Keypair,
    user_key_3: Keypair,
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
            base_reserve: 500_000,
        });
        Self {
            host,
            admin_key: generate_keypair(),
            user_key: generate_keypair(),
            user_key_2: generate_keypair(),
            user_key_3: generate_keypair(),
        }
    }

    fn default_smart_token(&self, admin: &TestSigner) -> TestToken {
        let token = TestToken::new(&self.host);
        token
            .init(
                admin.get_identifier(&self.host),
                TokenMetadata {
                    name: self.convert_bytes(b"abcd"),
                    symbol: self.convert_bytes(b"123xyz"),
                    decimals: 8,
                },
            )
            .unwrap();
        token
    }

    fn get_native_balance(&self, account_id: &AccountId) -> i64 {
        let account = self.host.load_account(account_id.clone()).unwrap();
        account.balance
    }

    fn get_classic_trustline_balance(&self, key: &LedgerKey) -> i64 {
        self.host
            .with_mut_storage(|s| match s.get(key).unwrap().data {
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
            flags: 0,
            home_domain: Default::default(),
            thresholds: Thresholds(thresholds),
            signers: acc_signers.try_into().unwrap(),
            ext,
        };

        self.host
            .add_ledger_entry(
                key,
                LedgerEntry {
                    last_modified_ledger_seq: 0,
                    data: LedgerEntryData::Account(acc_entry),
                    ext: LedgerEntryExt::V0,
                },
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
        authorized: bool,
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
            flags: if authorized {
                TrustLineFlags::AuthorizedFlag as u32
            } else {
                0
            },
            ext,
        };

        self.host
            .add_ledger_entry(
                key.clone(),
                LedgerEntry {
                    last_modified_ledger_seq: 0,
                    data: LedgerEntryData::Trustline(trustline_entry),
                    ext: LedgerEntryExt::V0,
                },
            )
            .unwrap();

        key
    }

    fn convert_bytes(&self, bytes: &[u8]) -> Bytes {
        Bytes::try_from_val(&self.host, self.host.bytes_new_from_slice(bytes).unwrap()).unwrap()
    }
}

#[test]
fn test_smart_token_init_and_balance() {
    let test = TokenTest::setup();
    let token = TestToken::new(&test.host);
    let admin = TestSigner::Ed25519(&test.admin_key);
    token
        .init(
            admin.get_identifier(&test.host),
            TokenMetadata {
                name: test.convert_bytes(&[0, 0, b'a']),
                symbol: test.convert_bytes(&[255, 123, 0, b'a']),
                decimals: 0xffffffff,
            },
        )
        .unwrap();

    assert_eq!(token.name().unwrap().to_vec(), vec![0, 0, b'a']);
    assert_eq!(token.symbol().unwrap().to_vec(), vec![255, 123, 0, b'a']);
    assert_eq!(token.decimals().unwrap(), 0xffffffff);

    let user = TestSigner::Ed25519(&test.user_key);

    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );
    assert_eq!(
        token
            .nonce(admin.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );

    token
        .mint(
            &admin,
            BigInt::from_u64(&test.host, 0).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 10_000_000).unwrap(),
        )
        .unwrap();

    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        10_000_000
    );
    assert_eq!(
        token
            .nonce(admin.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        1
    );
}

#[test]
fn test_native_token_smart_roundtrip() {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [100, 100, 100, 100],
        None,
        None,
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

    assert_eq!(test.get_native_balance(&account_id), 10_000_000);
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );

    token
        .import(&user, BigInt::from_u64(&test.host, 0).unwrap(), 1_000_000)
        .unwrap();

    assert_eq!(test.get_native_balance(&account_id), 9_000_000);
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        1_000_000
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        1
    );

    token
        .export(&user, BigInt::from_u64(&test.host, 1).unwrap(), 500_000)
        .unwrap();

    assert_eq!(test.get_native_balance(&account_id), 9_500_000);
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        500_000
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        2
    );
}

fn test_classic_asset_roundtrip(asset_code: &[u8]) {
    let test = TokenTest::setup();

    let account_id = signer_to_account_id(&test.host, &test.user_key);
    let issuer_id = signer_to_account_id(&test.host, &test.admin_key);

    test.create_classic_account(
        &account_id,
        vec![(&test.user_key, 100)],
        10_000_000,
        1,
        [100, 100, 100, 100],
        None,
        None,
    );

    let trustline_key = test.create_classic_trustline(
        &account_id,
        &issuer_id,
        asset_code,
        10_000_000,
        100_000_000,
        true,
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
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );

    token
        .import(&user, BigInt::from_u64(&test.host, 0).unwrap(), 1_000_000)
        .unwrap();

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        9_000_000
    );
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        1_000_000
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        1
    );

    token
        .export(&user, BigInt::from_u64(&test.host, 1).unwrap(), 500_000)
        .unwrap();

    assert_eq!(
        test.get_classic_trustline_balance(&trustline_key),
        9_500_000
    );
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        500_000
    );
    assert_eq!(
        token
            .nonce(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        2
    );
}

#[test]
fn test_classic_asset4_smart_roundtrip() {
    test_classic_asset_roundtrip(&[0, 'a' as u8, 'b' as u8, 255]);
}

#[test]
fn test_classic_asset12_smart_roundtrip() {
    test_classic_asset_roundtrip(&[255, 0, 0, 127, b'a', b'b', b'c', 1, 2, 3, 4, 5]);
}

#[test]
fn test_direct_transfer() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_smart_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 100_000_000).unwrap(),
        )
        .unwrap();
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        100_000_000
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );

    token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 9_999_999).unwrap(),
        )
        .unwrap();
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        90_000_001
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        9_999_999
    );

    token
        .xfer(
            &user_2,
            token.nonce(user_2.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 999_999).unwrap(),
        )
        .unwrap();
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        91_000_000
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        9_000_000
    );
}

#[test]
fn test_transfer_with_allowance() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_smart_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    let user_3 = TestSigner::Ed25519(&test.user_key_3);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 100_000_000).unwrap(),
        )
        .unwrap();
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        100_000_000
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );
    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap()
            .to_i64(),
        0
    );

    token
        .approve(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_3.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 10_000_000).unwrap(),
        )
        .unwrap();

    assert_eq!(
        token
            .allowance(
                user.get_identifier(&test.host),
                user_3.get_identifier(&test.host)
            )
            .unwrap()
            .to_i64(),
        10_000_000
    );

    token
        .xfer_from(
            &user_3,
            token.nonce(user_3.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            user_2.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 5_000_000).unwrap(),
        )
        .unwrap();
    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        95_000_000
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        5_000_000
    );
    assert_eq!(
        token
            .balance(user_3.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        0
    );

    token
        .xfer_from(
            &user_3,
            token.nonce(user_3.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            user_3.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 3_000_000).unwrap(),
        )
        .unwrap();

    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        92_000_000
    );
    assert_eq!(
        token
            .balance(user_2.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        5_000_000
    );
    assert_eq!(
        token
            .balance(user_3.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        3_000_000
    );
}

#[test]
fn test_freeze_and_unfreeze() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_smart_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    let user_2 = TestSigner::Ed25519(&test.user_key_2);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 100_000_000).unwrap(),
        )
        .unwrap();
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 200_000_000).unwrap(),
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
    // TODO: find a better way to check for the error being ContractError
    assert!(token
        .xfer(
            &user,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user_2.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap()
        )
        .is_err());
    assert!(token
        .xfer(
            &user_2,
            token.nonce(user.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap()
        )
        .is_err());

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
            BigInt::from_u64(&test.host, 1).unwrap(),
        )
        .unwrap();
    token
        .xfer(
            &user_2,
            token.nonce(user_2.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap(),
        )
        .unwrap();
}

#[test]
fn test_burn() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_smart_token(&admin);

    let user = TestSigner::Ed25519(&test.user_key);
    token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 100_000_000).unwrap(),
        )
        .unwrap();

    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        100_000_000
    );

    token
        .burn(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            user.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 40_000_000).unwrap(),
        )
        .unwrap();

    assert_eq!(
        token
            .balance(user.get_identifier(&test.host))
            .unwrap()
            .to_i64(),
        60_000_000
    );
}

#[test]
fn test_set_admin() {
    let test = TokenTest::setup();
    let admin = TestSigner::Ed25519(&test.admin_key);
    let token = test.default_smart_token(&admin);
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
    assert!(token
        .set_admin(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
        )
        .is_err());
    assert!(token
        .mint(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap()
        )
        .is_err());
    assert!(token
        .burn(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap()
        )
        .is_err());
    assert!(token
        .freeze(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
        )
        .is_err());
    assert!(token
        .unfreeze(
            &admin,
            token.nonce(admin.get_identifier(&test.host)).unwrap(),
            new_admin.get_identifier(&test.host),
        )
        .is_err());

    // The admin functions are now available to the new admin.
    token
        .mint(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap(),
        )
        .unwrap();
    token
        .burn(
            &new_admin,
            token.nonce(new_admin.get_identifier(&test.host)).unwrap(),
            admin.get_identifier(&test.host),
            BigInt::from_u64(&test.host, 1).unwrap(),
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
            BigInt::from_u64(&test.host, 1).unwrap(),
        )
        .unwrap();
}
