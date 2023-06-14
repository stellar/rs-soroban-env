use crate::auth::{AuthorizedFunction, ContractFunction};
use crate::{Host, LedgerInfo};
use ed25519_dalek::{Keypair, Signer};
use rand::thread_rng;
use soroban_env_common::xdr::{
    AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
    AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountId, Hash, HashIdPreimage,
    HashIdPreimageSorobanAuthorization, LedgerEntryData, LedgerKey, Liabilities, PublicKey,
    ScAddress, ScSymbol, ScVec, SequenceNumber, SignerKey, SorobanAddressCredentials,
    SorobanAuthorizationEntry, SorobanAuthorizedContractFunction, SorobanAuthorizedFunction,
    SorobanAuthorizedInvocation, SorobanCredentials, Thresholds, Uint256,
};
use soroban_env_common::{EnvBase, Symbol, TryFromVal};

use crate::native_contract::base_types::BytesN;

pub(crate) use crate::native_contract::base_types::Vec as HostVec;

use super::account_contract::AccountEd25519Signature;
use super::base_types::Address;

#[macro_export]
macro_rules! host_vec {
    ($host:expr $(,)?) => {
        HostVec::new($host).unwrap()
    };
    ($host:expr, $($x:expr),+ $(,)?) => {
        HostVec::from_slice($host, &[$($x.try_into_val($host).unwrap()),+]).unwrap()
    };
}

pub(crate) fn generate_keypair() -> Keypair {
    Keypair::generate(&mut thread_rng())
}

pub(crate) fn keypair_to_account_id(key: &Keypair) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
        key.public.to_bytes(),
    )))
}

pub(crate) fn account_to_address(host: &Host, account_id: AccountId) -> Address {
    Address::try_from_val(
        host,
        &host
            .add_host_object(ScAddress::Account(account_id))
            .unwrap(),
    )
    .unwrap()
}

pub(crate) fn contract_id_to_address(host: &Host, contract_id: [u8; 32]) -> Address {
    Address::try_from_val(
        host,
        &host
            .add_host_object(ScAddress::Contract(Hash(contract_id)))
            .unwrap(),
    )
    .unwrap()
}

pub(crate) enum TestSigner<'a> {
    AccountInvoker(AccountId),
    ContractInvoker(Hash),
    Account(AccountSigner<'a>),
    #[allow(dead_code)]
    AccountContract(AccountContractSigner<'a>),
}

pub(crate) struct AccountContractSigner<'a> {
    pub(crate) address: Address,
    #[allow(clippy::type_complexity)]
    pub(crate) sign: Box<dyn Fn(&[u8]) -> HostVec + 'a>,
}

pub(crate) struct AccountSigner<'a> {
    pub(crate) account_id: AccountId,
    pub(crate) signers: Vec<&'a Keypair>,
}

impl<'a> TestSigner<'a> {
    pub(crate) fn account(kp: &'a Keypair) -> Self {
        TestSigner::Account(AccountSigner {
            account_id: keypair_to_account_id(kp),
            signers: vec![kp],
        })
    }

    pub(crate) fn account_with_multisig(
        account_id: &AccountId,
        mut signers: Vec<&'a Keypair>,
    ) -> Self {
        signers.sort_by_key(|k| k.public.as_bytes());
        TestSigner::Account(AccountSigner {
            account_id: account_id.clone(),
            signers,
        })
    }

    pub(crate) fn account_id(&self) -> AccountId {
        match self {
            TestSigner::AccountInvoker(acc_id) => acc_id.clone(),
            TestSigner::Account(AccountSigner {
                account_id,
                signers: _,
            }) => account_id.clone(),
            TestSigner::AccountContract(_) => panic!("not supported"),
            TestSigner::ContractInvoker(_) => panic!("not supported"),
        }
    }

    fn sign(&self, host: &Host, payload: &[u8]) -> ScVec {
        let signature_args = match self {
            TestSigner::AccountInvoker(_) => host_vec![host],
            TestSigner::Account(account_signer) => {
                let mut signatures = HostVec::new(&host).unwrap();
                for key in &account_signer.signers {
                    signatures
                        .push(&sign_payload_for_account(host, key, payload))
                        .unwrap();
                }
                signatures
            }
            TestSigner::AccountContract(signer) => (signer.sign)(payload),
            TestSigner::ContractInvoker(_) => host_vec![host],
        };
        host.call_args_to_scvec(signature_args.into()).unwrap()
    }

    pub(crate) fn address(&self, host: &Host) -> Address {
        let sc_address = match self {
            TestSigner::AccountInvoker(acc_id) => ScAddress::Account(acc_id.clone()),
            TestSigner::Account(acc) => ScAddress::Account(acc.account_id.clone()),
            TestSigner::AccountContract(signer) => signer.address.to_sc_address().unwrap(),
            TestSigner::ContractInvoker(contract_id) => ScAddress::Contract(contract_id.clone()),
        };
        Address::try_from_val(host, &host.add_host_object(sc_address).unwrap()).unwrap()
    }
}

pub(crate) fn authorize_single_invocation_with_nonce(
    host: &Host,
    signer: &TestSigner,
    contract_address: &Address,
    function_name: &str,
    args: HostVec,
    nonce: Option<u64>,
) {
    let sc_address = signer.address(host).to_sc_address().unwrap();
    let mut credentials = match signer {
        TestSigner::AccountInvoker(_) => SorobanCredentials::SourceAccount,
        TestSigner::Account(_) | TestSigner::AccountContract(_) => {
            SorobanCredentials::Address(SorobanAddressCredentials {
                address: sc_address,
                nonce: nonce.unwrap(),
                signature_args: Default::default(),
            })
        }
        TestSigner::ContractInvoker(_) => {
            // Nothing need to be authorized for contract invoker here.
            return;
        }
    };

    let root_invocation = SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(SorobanAuthorizedContractFunction {
            contract_address: contract_address.to_sc_address().unwrap(),
            function_name: ScSymbol(function_name.try_into().unwrap()),
            args: host.call_args_to_scvec(args.into()).unwrap(),
        }),
        sub_invocations: Default::default(),
    };

    if let SorobanCredentials::Address(address_credentials) = &mut credentials {
        let signature_payload_preimage =
            HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                network_id: host
                    .with_ledger_info(|li: &LedgerInfo| Ok(li.network_id))
                    .unwrap()
                    .try_into()
                    .unwrap(),
                invocation: root_invocation.clone(),
                nonce: address_credentials.nonce,
            });
        let signature_payload = host.metered_hash_xdr(&signature_payload_preimage).unwrap();
        address_credentials.signature_args = signer.sign(host, &signature_payload);
    }
    let auth_entry = SorobanAuthorizationEntry {
        credentials,
        root_invocation,
    };

    host.set_authorization_entries(vec![auth_entry]).unwrap();
}

pub(crate) fn authorize_single_invocation(
    host: &Host,
    signer: &TestSigner,
    contract_address: &Address,
    function_name: &str,
    args: HostVec,
) {
    let nonce = match signer {
        TestSigner::AccountInvoker(_) => None,
        TestSigner::Account(_) | TestSigner::AccountContract(_) => Some(
            host.read_nonce(
                signer.address(host).to_sc_address().unwrap(),
                &AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: contract_address.clone().into(),
                    function_name: Symbol::try_from_small_str("").unwrap(),
                    args: Default::default(),
                }),
            )
            .unwrap(),
        ),
        TestSigner::ContractInvoker(_) => {
            return;
        }
    };
    authorize_single_invocation_with_nonce(
        host,
        signer,
        contract_address,
        function_name,
        args,
        nonce,
    );
}

pub(crate) fn sign_payload_for_account(
    host: &Host,
    signer: &Keypair,
    payload: &[u8],
) -> AccountEd25519Signature {
    AccountEd25519Signature {
        public_key: BytesN::<32>::try_from_val(
            host,
            &host
                .bytes_new_from_slice(&signer.public.to_bytes())
                .unwrap(),
        )
        .unwrap(),
        signature: BytesN::<64>::try_from_val(
            host,
            &host
                .bytes_new_from_slice(&signer.sign(payload).to_bytes())
                .unwrap(),
        )
        .unwrap(),
    }
}

#[allow(dead_code)]
pub(crate) fn sign_payload_for_ed25519(
    host: &Host,
    signer: &Keypair,
    payload: &[u8],
) -> BytesN<64> {
    BytesN::<64>::try_from_val(
        host,
        &host
            .bytes_new_from_slice(&signer.sign(payload).to_bytes())
            .unwrap(),
    )
    .unwrap()
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn create_account(
    host: &Host,
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
    let key = host.to_account_key(account_id.clone());
    let account_id = match key.as_ref() {
        LedgerKey::Account(acc) => acc.account_id.clone(),
        _ => unreachable!(),
    };
    let mut acc_signers = vec![];
    for (signer, weight) in signers {
        acc_signers.push(soroban_env_common::xdr::Signer {
            key: SignerKey::Ed25519(Uint256(signer.public.to_bytes())),
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
        balance,
        seq_num: SequenceNumber(0),
        num_sub_entries,
        inflation_dest: None,
        flags,
        home_domain: Default::default(),
        thresholds: Thresholds(thresholds),
        signers: acc_signers.try_into().unwrap(),
        ext,
    };

    host.add_ledger_entry(
        &key,
        &Host::ledger_entry_from_data(LedgerEntryData::Account(acc_entry)),
    )
    .unwrap();
}
