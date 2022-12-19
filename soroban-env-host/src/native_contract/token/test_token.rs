use crate::{
    host_vec,
    native_contract::testutils::{sign_args, HostVec, TestSigner},
    Host, HostError,
};
use soroban_env_common::{
    xdr::{Asset, ContractId, CreateContractArgs, HostFunction, ScContractCode},
    CheckedEnv, RawVal,
};
use soroban_env_common::{Symbol, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Bytes, BytesN};

use crate::native_contract::token::public_types::Identifier;

pub(crate) struct TestToken<'a> {
    pub(crate) id: BytesN<32>,
    host: &'a Host,
}

impl<'a> TestToken<'a> {
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

    pub(crate) fn nonce(&self, id: Identifier) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("nonce").into(),
                host_vec![self.host, id].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn allowance(
        &self,
        from: Identifier,
        spender: Identifier,
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

    pub(crate) fn incr_allow(
        &self,
        from: &TestSigner,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            from,
            "incr_allow",
            &self.id,
            host_vec![
                self.host,
                from.get_identifier(self.host),
                nonce.clone(),
                spender.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("incr_allow").into(),
                host_vec![self.host, signature, nonce, spender, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn decr_allow(
        &self,
        from: &TestSigner,
        nonce: i128,
        spender: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            from,
            "decr_allow",
            &self.id,
            host_vec![
                self.host,
                from.get_identifier(self.host),
                nonce.clone(),
                spender.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("decr_allow").into(),
                host_vec![self.host, signature, nonce, spender, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn balance(&self, id: Identifier) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("balance").into(),
                host_vec![self.host, id].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn spendable(&self, id: Identifier) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("spendable").into(),
                host_vec![self.host, id].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn xfer(
        &self,
        from: &TestSigner,
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            from,
            "xfer",
            &self.id,
            host_vec![
                self.host,
                from.get_identifier(self.host),
                nonce.clone(),
                to.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("xfer").into(),
                host_vec![self.host, signature, nonce, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn xfer_from(
        &self,
        spender: &TestSigner,
        nonce: i128,
        from: Identifier,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            spender,
            "xfer_from",
            &self.id,
            host_vec![
                self.host,
                spender.get_identifier(self.host),
                nonce.clone(),
                from.clone(),
                to.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("xfer_from").into(),
                host_vec![self.host, signature, nonce, from, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn freeze(
        &self,
        admin: &TestSigner,
        nonce: i128,
        id: Identifier,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            admin,
            "freeze",
            &self.id,
            host_vec![
                self.host,
                admin.get_identifier(self.host),
                nonce.clone(),
                id.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("freeze").into(),
                host_vec![self.host, signature, nonce, id].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn unfreeze(
        &self,
        admin: &TestSigner,
        nonce: i128,
        id: Identifier,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            admin,
            "unfreeze",
            &self.id,
            host_vec![
                self.host,
                admin.get_identifier(self.host),
                nonce.clone(),
                id.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("unfreeze").into(),
                host_vec![self.host, signature, nonce, id].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn is_frozen(&self, id: Identifier) -> Result<bool, HostError> {
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
        nonce: i128,
        to: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            admin,
            "mint",
            &self.id,
            host_vec![
                self.host,
                admin.get_identifier(self.host),
                nonce.clone(),
                to.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("mint").into(),
                host_vec![self.host, signature, nonce, to, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn burn(
        &self,
        admin: &TestSigner,
        nonce: i128,
        from: Identifier,
        amount: i128,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            admin,
            "burn",
            &self.id,
            host_vec![
                self.host,
                admin.get_identifier(self.host),
                nonce.clone(),
                from.clone(),
                amount.clone()
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("burn").into(),
                host_vec![self.host, signature, nonce, from, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn set_admin(
        &self,
        admin: &TestSigner,
        nonce: i128,
        new_admin: Identifier,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            admin,
            "set_admin",
            &self.id,
            host_vec![
                self.host,
                admin.get_identifier(self.host),
                nonce.clone(),
                new_admin.clone(),
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("set_admin").into(),
                host_vec![self.host, signature, nonce, new_admin].into(),
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
}
