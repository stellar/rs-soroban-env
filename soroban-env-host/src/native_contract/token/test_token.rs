use crate::{
    host_vec,
    native_contract::{
        base_types::BigInt,
        testutils::{sign_args, HostVec, TestSigner},
        token::public_types::TokenMetadata,
    },
    Host, HostError,
};
use soroban_env_common::{
    xdr::{Asset, ScContractCode, WriteXdr},
    CheckedEnv,
};
use soroban_env_common::{EnvBase, Symbol, TryFromVal, TryIntoVal};

use crate::native_contract::base_types::{Bytes, BytesN};

use crate::native_contract::token::public_types::Identifier;

use crate::native_contract::testutils::generate_bytes;

pub(crate) struct TestToken<'a> {
    pub(crate) id: BytesN<32>,
    host: &'a Host,
}

impl<'a> TestToken<'a> {
    pub(crate) fn new(host: &'a Host) -> Self {
        let id = generate_bytes(host);
        host.create_contract_with_id(ScContractCode::Token, id.clone().into())
            .unwrap();
        Self { id, host }
    }

    pub(crate) fn new_from_asset(host: &'a Host, asset: Asset) -> Self {
        let id = BytesN::<32>::try_from_val(
            host,
            host.create_token_from_asset(
                host.bytes_new_from_slice(&asset.to_xdr().unwrap()).unwrap(),
            )
            .unwrap(),
        )
        .unwrap();
        Self { id, host }
    }

    pub(crate) fn init(&self, admin: Identifier, metadata: TokenMetadata) -> Result<(), HostError> {
        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("init").into(),
                host_vec![self.host, admin, metadata].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn nonce(&self, id: Identifier) -> Result<BigInt, HostError> {
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
    ) -> Result<BigInt, HostError> {
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
        nonce: BigInt,
        spender: Identifier,
        amount: BigInt,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            from,
            "approve",
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
                Symbol::from_str("approve").into(),
                host_vec![self.host, signature, nonce, spender, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn balance(&self, id: Identifier) -> Result<BigInt, HostError> {
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
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
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
        nonce: BigInt,
        from: Identifier,
        to: Identifier,
        amount: BigInt,
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
        nonce: BigInt,
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
        nonce: BigInt,
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
        nonce: BigInt,
        to: Identifier,
        amount: BigInt,
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
        nonce: BigInt,
        from: Identifier,
        amount: BigInt,
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
        nonce: BigInt,
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

    pub(crate) fn import(
        &self,
        id: &TestSigner,
        nonce: BigInt,
        amount: i64,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            id,
            "import",
            &self.id,
            host_vec![
                self.host,
                id.get_identifier(self.host),
                nonce.clone(),
                amount
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("import").into(),
                host_vec![self.host, signature, nonce, amount].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn export(
        &self,
        id: &TestSigner,
        nonce: BigInt,
        amount: i64,
    ) -> Result<(), HostError> {
        let signature = sign_args(
            self.host,
            id,
            "export",
            &self.id,
            host_vec![
                self.host,
                id.get_identifier(self.host),
                nonce.clone(),
                amount
            ],
        );

        Ok(self
            .host
            .call(
                self.id.clone().into(),
                Symbol::from_str("export").into(),
                host_vec![self.host, signature, nonce, amount].into(),
            )?
            .try_into()?)
    }
}
