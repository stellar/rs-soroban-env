use crate::{
    native_contract::token::public_types::{
        AccountSignatures, Ed25519Signature, Signature, SignaturePayload, SignaturePayloadV0,
    },
    Host, HostError,
};
use ed25519_dalek::{Keypair, Signer};
use rand::{thread_rng, RngCore};
use soroban_env_common::{
    xdr::{AccountId, PublicKey, Uint256},
    CheckedEnv,
};
use soroban_env_common::{EnvBase, RawVal, Symbol, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Bytes, BytesN};

use crate::native_contract::token::public_types::{self, Identifier};
pub(crate) use public_types::Vec as HostVec;

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
    let mut bytes: [u8; 32] = Default::default();
    thread_rng().fill_bytes(&mut bytes);
    BytesN::<32>::try_from_val(host, host.bytes_new_from_slice(&bytes).unwrap()).unwrap()
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
    ContractInvoker,
    AccountInvoker,
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

    pub(crate) fn get_identifier(&self, host: &Host) -> Identifier {
        match self {
            TestSigner::ContractInvoker => {
                // Use stub id to make this work in wrapped contract calls.
                // The id shouldn't be used anywhere else.
                Identifier::Contract(BytesN::<32>::from_slice(host, &[0; 32]).unwrap())
            }
            TestSigner::AccountInvoker => {
                // Use stub id to make this work in wrapped contract calls.
                // The id shouldn't be used anywhere else.
                Identifier::Account(AccountId(PublicKey::PublicKeyTypeEd25519(Uint256([0; 32]))))
            }
            TestSigner::Ed25519(key) => Identifier::Ed25519(signer_to_id_bytes(host, key)),
            TestSigner::Account(acc_signer) => Identifier::Account(acc_signer.account_id.clone()),
        }
    }
}

pub(crate) fn sign_args(
    host: &Host,
    signer: &TestSigner,
    fn_name: &str,
    contract_id: &BytesN<32>,
    args: HostVec,
) -> Signature {
    let msg = SignaturePayload::V0(SignaturePayloadV0 {
        name: Symbol::from_str(fn_name),
        contract: contract_id.clone(),
        network: Bytes::try_from_val(host, host.get_ledger_network_passphrase().unwrap()).unwrap(),
        args,
    });
    let msg_bin = host
        .serialize_to_bytes(msg.try_into_val(host).unwrap())
        .unwrap();
    let msg_bytes = Bytes::try_from_val(host, msg_bin).unwrap().to_vec();
    let payload = &msg_bytes[..];

    match signer {
        TestSigner::ContractInvoker => Signature::Invoker,
        TestSigner::AccountInvoker => Signature::Invoker,
        TestSigner::Ed25519(key) => Signature::Ed25519(sign_payload(host, key, payload)),
        TestSigner::Account(account_signer) => Signature::Account(AccountSignatures {
            account_id: account_signer.account_id.clone(),
            signatures: {
                let mut signatures = HostVec::new(&host).unwrap();
                for key in &account_signer.signers {
                    signatures.push(sign_payload(host, key, payload)).unwrap();
                }
                signatures
            },
        }),
    }
}

fn sign_payload(host: &Host, signer: &Keypair, payload: &[u8]) -> Ed25519Signature {
    Ed25519Signature {
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
