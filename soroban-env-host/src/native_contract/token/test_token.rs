use crate::{
    host_vec,
    native_contract::{
        base_types::i128,
        testutils::{AccountAuthBuilder, HostVec, TestSigner},
        token::public_types::TokenMetadata,
    },
    test::util::{generate_account_id, generate_bytes_array},
    Host, HostError,
};
use soroban_env_common::{
    xdr::{
        Asset, ContractId, CreateContractArgs, HostFunction, ScAddress, ScContractCode, Uint256,
    },
    CheckedEnv, RawVal,
};
use soroban_env_common::{Symbol, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Bytes, BytesN};

pub(crate) struct TestToken<'a> {
    pub(crate) id: BytesN<32>,
    host: &'a Host,
}

impl<'a> TestToken<'a> {
    pub(crate) fn new(host: &'a Host) -> Self {
        host.set_source_account(generate_account_id());
        let id_obj: RawVal = host
            .invoke_function(HostFunction::CreateContract(CreateContractArgs {
                contract_id: ContractId::SourceAccount(Uint256(generate_bytes_array())),
                source: ScContractCode::Token,
            }))
            .unwrap()
            .try_into_val(host)
            .unwrap();
        host.remove_source_account();
        Self {
            id: BytesN::<32>::try_from_val(host, id_obj).unwrap(),
            host,
        }
    }

    pub(crate) fn new_from_asset(host: &'a Host, asset: Asset) -> Self {
        let id_obj: RawVal = host
            .invoke_function(HostFunction::CreateContract(CreateContractArgs {
                contract_id: ContractId::Asset(asset),
                source: ScContractCode::Token,
            }))
            .unwrap()
            .try_into_val(host)
            .unwrap();
        Self {
            id: BytesN::<32>::try_from_val(host, id_obj).unwrap(),
            host,
        }
    }

    pub(crate) fn init(&self, admin: ScAddress, metadata: TokenMetadata) -> Result<(), HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("init").into(),
                host_vec![self.host, admin, metadata].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn allowance(
        &self,
        from: ScAddress,
        spender: ScAddress,
    ) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("allowance").into(),
                host_vec![self.host, from, spender].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn approve(
        &self,
        from: &TestSigner,
        spender: ScAddress,
        amount: i128,
    ) -> Result<(), HostError> {
        let from_acc = AccountAuthBuilder::new(self.host, from)
            .add_invocation(
                &self.id,
                "approve",
                host_vec![self.host, spender.clone(), amount.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("approve").into(),
                host_vec![self.host, from_acc, spender, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn balance(&self, id: ScAddress) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("balance").into(),
                host_vec![self.host, id].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn xfer(
        &self,
        from: &TestSigner,
        to: ScAddress,
        amount: i128,
    ) -> Result<(), HostError> {
        let from_acc = AccountAuthBuilder::new(self.host, from)
            .add_invocation(
                &self.id,
                "xfer",
                host_vec![self.host, to.clone(), amount.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("xfer").into(),
                host_vec![self.host, from_acc, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn xfer_from(
        &self,
        spender: &TestSigner,
        from: ScAddress,
        to: ScAddress,
        amount: i128,
    ) -> Result<(), HostError> {
        let spender_acc = AccountAuthBuilder::new(self.host, spender)
            .add_invocation(
                &self.id,
                "xfer_from",
                host_vec![self.host, from.clone(), to.clone(), amount.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("xfer_from").into(),
                host_vec![self.host, spender_acc, from, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn freeze(&self, admin: &TestSigner, id: ScAddress) -> Result<(), HostError> {
        let admin_acc = AccountAuthBuilder::new(self.host, admin)
            .add_invocation(&self.id, "freeze", host_vec![self.host, id.clone()])
            .build();
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("freeze").into(),
                host_vec![self.host, admin_acc, id].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn unfreeze(&self, admin: &TestSigner, id: ScAddress) -> Result<(), HostError> {
        let admin_acc = AccountAuthBuilder::new(self.host, admin)
            .add_invocation(&self.id, "unfreeze", host_vec![self.host, id.clone()])
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("unfreeze").into(),
                host_vec![self.host, admin_acc, id].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn is_frozen(&self, id: ScAddress) -> Result<bool, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("is_frozen").into(),
                host_vec![self.host, id].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn mint(
        &self,
        admin: &TestSigner,
        to: ScAddress,
        amount: i128,
    ) -> Result<(), HostError> {
        let admin_acc = AccountAuthBuilder::new(self.host, admin)
            .add_invocation(
                &self.id,
                "mint",
                host_vec![self.host, to.clone(), amount.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("mint").into(),
                host_vec![self.host, admin_acc, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn burn(
        &self,
        admin: &TestSigner,
        from: ScAddress,
        amount: i128,
    ) -> Result<(), HostError> {
        let admin_acc = AccountAuthBuilder::new(self.host, admin)
            .add_invocation(
                &self.id,
                "burn",
                host_vec![self.host, from.clone(), amount.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("burn").into(),
                host_vec![self.host, admin_acc, from, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn set_admin(
        &self,
        admin: &TestSigner,
        new_admin: ScAddress,
    ) -> Result<(), HostError> {
        let admin_acc = AccountAuthBuilder::new(self.host, admin)
            .add_invocation(
                &self.id,
                "set_admin",
                host_vec![self.host, new_admin.clone()],
            )
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("set_admin").into(),
                host_vec![self.host, admin_acc, new_admin].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn decimals(&self) -> Result<u32, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("decimals").into(),
                host_vec![self.host].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn name(&self) -> Result<Bytes, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("name").into(),
                host_vec![self.host].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn symbol(&self) -> Result<Bytes, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("symbol").into(),
                host_vec![self.host].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn import(&self, from: &TestSigner, amount: i64) -> Result<(), HostError> {
        let from_acc = AccountAuthBuilder::new(self.host, from)
            .add_invocation(&self.id, "import", host_vec![self.host, amount])
            .build();
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("import").into(),
                host_vec![self.host, from_acc, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn export(&self, from: &TestSigner, amount: i64) -> Result<(), HostError> {
        let from_acc = AccountAuthBuilder::new(self.host, from)
            .add_invocation(&self.id, "export", host_vec![self.host, amount])
            .build();

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("export").into(),
                host_vec![self.host, from_acc, amount].into(),
            )?
            .try_into()?)
    }
}
