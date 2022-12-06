use crate::{auth::HostAccount, test::util::generate_bytes_array, Host, HostError, LedgerInfo};
use ed25519_dalek::{Keypair, Signer};
use rand::thread_rng;
use soroban_env_common::xdr::{
    self, AccountId, AuthorizedInvocation, ContractInvocation, Hash, HashIdPreimage,
    HashIdPreimageContractAuth, PublicKey, ScAccount, ScAccountId, ScAddress, ScObject, ScVal,
    ScVec,
};
use soroban_env_common::{EnvBase, RawVal, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Account, BytesN};

pub(crate) use crate::native_contract::base_types::Vec as HostVec;

use super::account_contract::{AccountEd25519Signature, Signature};

impl HostVec {
    pub(crate) fn from_array(host: &Host, vals: &[RawVal]) -> Result<Self, HostError> {
        let mut res = HostVec::new(host)?;
        for val in vals {
            res.push_raw(val.clone())?;
        }
        Ok(res)
    }
}

#[macro_export]
macro_rules! host_vec {
    ($host:expr $(,)?) => {
        HostVec::new($host).unwrap()
    };
    ($host:expr, $($x:expr),+ $(,)?) => {
        HostVec::from_array($host, &[$($x.try_into_val($host).unwrap()),+]).unwrap()
    };
}

pub(crate) fn generate_keypair() -> Keypair {
    Keypair::generate(&mut thread_rng())
}

pub(crate) fn generate_bytes(host: &Host) -> BytesN<32> {
    BytesN::<32>::try_from_val(
        host,
        host.bytes_new_from_slice(&generate_bytes_array()).unwrap(),
    )
    .unwrap()
}

pub(crate) fn signer_to_id_bytes(host: &Host, key: &Keypair) -> BytesN<32> {
    BytesN::<32>::try_from_val(
        host,
        host.bytes_new_from_slice(&key.public.to_bytes()).unwrap(),
    )
    .unwrap()
}

pub(crate) fn signer_to_account_id(host: &Host, key: &Keypair) -> AccountId {
    let account_id_bytes = signer_to_id_bytes(host, key);
    AccountId(PublicKey::PublicKeyTypeEd25519(
        host.to_u256(account_id_bytes.into()).unwrap(),
    ))
}

pub(crate) enum TestSigner<'a> {
    ContractInvoker(Hash),
    ClassicAccountInvoker,
    Ed25519(&'a Keypair),
    Account(AccountSigner<'a>),
}

pub(crate) struct AccountSigner<'a> {
    pub(crate) account_id: AccountId,
    pub(crate) signers: Vec<&'a Keypair>,
}

impl<'a> TestSigner<'a> {
    pub(crate) fn account(account_id: &AccountId, mut signers: Vec<&'a Keypair>) -> Self {
        signers.sort_by_key(|k| k.public.as_bytes());
        TestSigner::Account(AccountSigner {
            account_id: account_id.clone(),
            signers,
        })
    }

    fn sign(&self, host: &Host, payload: &[u8]) -> ScVec {
        let signature_args = match self {
            TestSigner::ClassicAccountInvoker => host_vec![host],
            TestSigner::Ed25519(signer) => {
                host_vec![
                    host,
                    Signature::Ed25519(sign_payload_for_ed25519(host, signer, payload))
                ]
            }
            TestSigner::Account(account_signer) => {
                let mut signatures = HostVec::new(&host).unwrap();
                for key in &account_signer.signers {
                    signatures
                        .push(sign_payload_for_account(host, key, payload))
                        .unwrap();
                }
                host_vec![host, Signature::Account(signatures)]
            }
            TestSigner::ContractInvoker(_) => unreachable!(),
        };
        host.call_args_to_scvec(signature_args.into()).unwrap()
    }

    fn get_account_id(&self) -> ScAccountId {
        match self {
            TestSigner::Ed25519(kp) => {
                ScAccountId::BuiltinEd25519(xdr::Hash(kp.public.to_bytes().try_into().unwrap()))
            }
            TestSigner::Account(acc) => ScAccountId::BuiltinClassicAccount(acc.account_id.clone()),
            TestSigner::ClassicAccountInvoker => ScAccountId::BuiltinInvoker,
            TestSigner::ContractInvoker(_) => unreachable!(),
        }
    }

    pub(crate) fn get_address(&self) -> ScAddress {
        match self {
            TestSigner::ClassicAccountInvoker => panic!("not supported"),
            TestSigner::Ed25519(kp) => ScAddress::Ed25519(kp.public.to_bytes().try_into().unwrap()),
            TestSigner::Account(acc) => ScAddress::ClassicAccount(acc.account_id.clone()),
            TestSigner::ContractInvoker(hash) => ScAddress::Contract(hash.clone()),
        }
    }
}

pub(crate) struct AccountAuthBuilder<'a, 'b> {
    host: &'a Host,
    signer: &'a TestSigner<'b>,
    invocations: Vec<AuthorizedInvocation>,
}

impl<'a, 'b> AccountAuthBuilder<'a, 'b> {
    pub(crate) fn new(host: &'a Host, signer: &'a TestSigner<'b>) -> Self {
        Self {
            host,
            signer,
            invocations: vec![],
        }
    }

    pub(crate) fn add_invocation(
        &mut self,
        contract_id: &BytesN<32>,
        function_name: &str,
        args: HostVec,
    ) -> &mut Self {
        let nonce = match self.signer {
            TestSigner::ClassicAccountInvoker => None,
            TestSigner::Ed25519(_) | TestSigner::Account(_) => Some(
                self.host
                    .read_nonce(
                        &Hash(contract_id.to_vec().clone().try_into().unwrap()),
                        &self.signer.get_address(),
                    )
                    .unwrap(),
            ),
            TestSigner::ContractInvoker(_) => {
                return self;
            }
        };
        let invocation = AuthorizedInvocation {
            call_stack: [ContractInvocation {
                contract_id: contract_id.to_vec().try_into().unwrap(),
                function_name: function_name.try_into().unwrap(),
            }]
            .try_into()
            .unwrap(),
            top_args: self.host.call_args_to_scvec(args.into()).unwrap(),
            nonce,
        };
        self.invocations.push(invocation);

        self
    }

    pub(crate) fn build(&mut self) -> Account {
        let host_acc = match self.signer {
            TestSigner::ContractInvoker(id) => self
                .host
                .add_host_object(HostAccount::InvokerContract(id.clone()))
                .unwrap()
                .into(),
            _ => {
                let account_id = self.signer.get_account_id();
                let signature_payload_preimage =
                    HashIdPreimage::ContractAuth(HashIdPreimageContractAuth {
                        network_passphrase: self
                            .host
                            .with_ledger_info(|li: &LedgerInfo| Ok(li.network_passphrase.clone()))
                            .unwrap()
                            .try_into()
                            .unwrap(),
                        invocations: self.invocations.clone().try_into().unwrap(),
                    });
                let signature_payload = self
                    .host
                    .metered_hash_xdr(&signature_payload_preimage)
                    .unwrap();
                let signature_args = self.signer.sign(self.host, &signature_payload);
                let account = ScAccount {
                    account_id,
                    invocations: self.invocations.clone().try_into().unwrap(),
                    signature_args,
                };
                let sc_obj = ScVal::Object(Some(ScObject::Account(account)));
                self.host.to_host_val(&sc_obj).unwrap()
            }
        };
        Account::try_from_val(self.host, host_acc.val).unwrap()
    }
}

fn sign_payload_for_account(
    host: &Host,
    signer: &Keypair,
    payload: &[u8],
) -> AccountEd25519Signature {
    AccountEd25519Signature {
        public_key: BytesN::<32>::try_from_val(
            host,
            host.bytes_new_from_slice(&signer.public.to_bytes())
                .unwrap(),
        )
        .unwrap(),
        signature: BytesN::<64>::try_from_val(
            host,
            host.bytes_new_from_slice(&signer.sign(payload).to_bytes())
                .unwrap(),
        )
        .unwrap(),
    }
}

fn sign_payload_for_ed25519(host: &Host, signer: &Keypair, payload: &[u8]) -> BytesN<64> {
    BytesN::<64>::try_from_val(
        host,
        host.bytes_new_from_slice(&signer.sign(payload).to_bytes())
            .unwrap(),
    )
    .unwrap()
}
