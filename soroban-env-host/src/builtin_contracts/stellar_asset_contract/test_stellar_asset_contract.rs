use crate::{
    builtin_contracts::{
        base_types::Address,
        testutils::{authorize_single_invocation, HostVec, TestSigner},
    },
    host_vec, Host, HostError,
};
use soroban_env_common::{
    xdr::{Asset, Limited, Limits, WriteXdr},
    Env,
};
use soroban_env_common::{Symbol, TryFromVal, TryIntoVal};

use crate::builtin_contracts::base_types::{Bytes, String};

pub(crate) struct TestStellarAssetContract<'a> {
    pub(crate) address: Address,
    host: &'a Host,
}

impl<'a> TestStellarAssetContract<'a> {
    pub(crate) fn new_from_asset(host: &'a Host, asset: Asset) -> Self {
        let mut asset_bytes_vec = Limited::new(vec![], Limits::default());
        asset.write_xdr(&mut asset_bytes_vec).unwrap();
        let address_obj = host
            .create_asset_contract(
                Bytes::from_slice(host, &asset_bytes_vec.inner.as_slice())
                    .unwrap()
                    .into(),
            )
            .unwrap();
        Self {
            address: Address::try_from_val(host, &address_obj).unwrap(),
            host,
        }
    }

    pub(crate) fn allowance(&self, from: Address, spender: Address) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"allowance")?,
                host_vec![self.host, from, spender].into(),
            )?
            .try_into_val(self.host)?)
    }

    fn call_with_single_signer(
        &self,
        signer: &TestSigner,
        function_name: &str,
        args: HostVec,
    ) -> Result<(), HostError> {
        authorize_single_invocation(
            self.host,
            signer,
            &self.address,
            function_name,
            args.clone(),
        );
        Ok(self
            .host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &function_name)?,
                args.into(),
            )?
            .try_into()?)
    }

    pub(crate) fn approve(
        &self,
        from: &TestSigner,
        spender: Address,
        amount: i128,
        live_until_ledger: u32,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(
            from,
            "approve",
            host_vec![
                self.host,
                from.address(self.host),
                spender,
                amount,
                live_until_ledger
            ],
        )
    }

    pub(crate) fn balance(&self, addr: Address) -> Result<i128, HostError> {
        Ok(self
            .host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"balance")?,
                host_vec![self.host, addr].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn authorized(&self, addr: Address) -> Result<bool, HostError> {
        Ok(self
            .host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"authorized")?,
                host_vec![self.host, addr].into(),
            )?
            .try_into_val(self.host)?)
    }

    pub(crate) fn transfer(
        &self,
        from: &TestSigner,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(
            from,
            "transfer",
            host_vec![self.host, from.address(self.host), to, amount],
        )
    }

    pub(crate) fn transfer_from(
        &self,
        spender: &TestSigner,
        from: Address,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(
            spender,
            "transfer_from",
            host_vec![self.host, spender.address(self.host), from, to, amount],
        )
    }

    pub(crate) fn burn(&self, from: &TestSigner, amount: i128) -> Result<(), HostError> {
        self.call_with_single_signer(
            from,
            "burn",
            host_vec![self.host, from.address(self.host), amount],
        )
    }

    pub(crate) fn burn_from(
        &self,
        spender: &TestSigner,
        from: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(
            spender,
            "burn_from",
            host_vec![self.host, spender.address(self.host), from, amount],
        )
    }

    pub(crate) fn set_authorized(
        &self,
        admin: &TestSigner,
        addr: Address,
        authorize: bool,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(
            admin,
            "set_authorized",
            host_vec![self.host, addr, authorize],
        )
    }

    pub(crate) fn mint(
        &self,
        admin: &TestSigner,
        to: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(admin, "mint", host_vec![self.host, to, amount])
    }

    pub(crate) fn clawback(
        &self,
        admin: &TestSigner,
        from: Address,
        amount: i128,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(admin, "clawback", host_vec![self.host, from, amount])
    }

    pub(crate) fn set_admin(
        &self,
        admin: &TestSigner,
        new_admin: Address,
    ) -> Result<(), HostError> {
        self.call_with_single_signer(admin, "set_admin", host_vec![self.host, new_admin])
    }

    pub(crate) fn admin(&self) -> Result<Address, HostError> {
        self.host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"admin")?,
                host_vec![self.host].into(),
            )?
            .try_into_val(self.host)
    }

    pub(crate) fn decimals(&self) -> Result<u32, HostError> {
        Ok(self
            .host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"decimals")?,
                host_vec![self.host].into(),
            )?
            .try_into()?)
    }

    pub(crate) fn name(&self) -> Result<String, HostError> {
        self.host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"name")?,
                host_vec![self.host].into(),
            )?
            .try_into_val(self.host)
    }

    pub(crate) fn symbol(&self) -> Result<String, HostError> {
        self.host
            .call(
                self.address.clone().into(),
                Symbol::try_from_val(self.host, &"symbol")?,
                host_vec![self.host].into(),
            )?
            .try_into_val(self.host)
    }
}
