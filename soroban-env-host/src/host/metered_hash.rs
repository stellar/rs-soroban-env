use crate::{
    budget::Budget,
    events::{
        EventError, InternalContractEvent, InternalDiagnosticArg, InternalDiagnosticEvent,
        InternalEvent,
    },
    host_object::HostObject,
    num::{I256, U256},
    xdr::{
        AccountEntry, AccountEntryExt, AccountEntryExtensionV1, AccountEntryExtensionV1Ext,
        AccountEntryExtensionV2, AccountEntryExtensionV2Ext, AccountEntryExtensionV3, AccountId,
        AlphaNum12, AlphaNum4, Asset, AssetCode12, AssetCode4, BytesM, ContractCodeEntry,
        ContractCostType, ContractDataDurability, ContractDataEntry, ContractEventType,
        ContractExecutable, ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgs,
        Duration, ExtensionPoint, HostFunctionType, Int128Parts, Int256Parts, LedgerEntry,
        LedgerEntryData, LedgerEntryExt, LedgerEntryExtensionV1, LedgerEntryExtensionV1Ext,
        LedgerKey, LedgerKeyAccount, LedgerKeyContractCode, LedgerKeyContractData,
        LedgerKeyTrustLine, Liabilities, PublicKey, ScAddress, ScBytes, ScContractInstance,
        ScError, ScErrorCode, ScErrorType, ScMap, ScMapEntry, ScNonceKey, ScString, ScSymbol,
        ScVal, ScVec, SequenceNumber, Signer, SignerKey, SignerKeyEd25519SignedPayload,
        SponsorshipDescriptor, StringM, Thresholds, TimePoint, TrustLineAsset, TrustLineEntry,
        TrustLineEntryExt, TrustLineEntryExtensionV2, TrustLineEntryExtensionV2Ext,
        TrustLineEntryV1, TrustLineEntryV1Ext, UInt128Parts, UInt256Parts, Uint256,
    },
    AddressObject, BytesObject, Error, HostError, Object, Symbol, Val, VecObject,
};
use std::{
    collections::BTreeMap,
    hash::{Hash, Hasher},
    mem::Discriminant,
    rc::Rc,
};

// Technically we should be metering the cost of the hash function used, but
// this codepath is used only for charging the costs of tracing against the
// shadow budget, and we do not want to add a cost type to the protocol just
// for this purpose (it's not protocol-visible at all).
//
// In practice, Rust's default hasher is SIP-1-3 which is of a similar order
// of magnitude as a ChaCha20 round, so this is a reasonable approximation.
// It's also fine if we overcharge here, since again this is only used to
// ensure that if the hashing code is ever called _outside_ the shadow budget
// it's not a free operation / DoS vector.
const HASH_COST_TYPE: ContractCostType = ContractCostType::ChaCha20DrawBytes;

/// MeteredHash is a variant of the Hash trait that is both fallible and charges
/// a budget for the hashing operation. It currently overestimates the cost of
/// hashing by charging [`ContractCostType::ComputeSha256Hash`] but it is only
/// intended to be used for observations and trace diagnostics, all made under
/// the shadow budget, so this overestimate is harmless. It also treats as
/// errors any attempts to hash values that can't occur in the host, such as the
/// non-representable [`LedgerEntry`] and [`LedgerKey`]` variants.
pub(crate) trait MeteredHash {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError>;
    fn metered_hash_slice<H: Hasher>(
        slice: &[Self],
        state: &mut H,
        budget: &Budget,
    ) -> Result<(), HostError>
    where
        Self: Sized,
    {
        slice.len().metered_hash(state, budget)?;
        for e in slice {
            e.metered_hash(state, budget)?;
        }
        Ok(())
    }
    fn hash_discriminant<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError>
    where
        Self: Sized,
    {
        budget.charge(
            HASH_COST_TYPE,
            Some(std::mem::size_of::<Discriminant<Self>>() as u64),
        )?;
        std::mem::discriminant(self).hash(state);
        Ok(())
    }
}

macro_rules! impl_metered_hash_for_int {
    ($t:ty, $method:ident) => {
        impl MeteredHash for $t {
            fn metered_hash<H: Hasher>(
                &self,
                state: &mut H,
                budget: &Budget,
            ) -> Result<(), HostError> {
                budget.charge(HASH_COST_TYPE, Some(std::mem::size_of::<$t>() as u64))?;
                state.$method(*self);
                Ok(())
            }
            fn metered_hash_slice<H: Hasher>(
                slice: &[Self],
                state: &mut H,
                budget: &Budget,
            ) -> Result<(), HostError>
            where
                Self: Sized,
            {
                slice.len().metered_hash(state, budget)?;
                // Copy of the pattern in the stdlib Hash::hash_slice method.
                let newlen = std::mem::size_of_val(slice);
                let ptr = slice.as_ptr() as *const u8;
                budget.charge(HASH_COST_TYPE, Some(newlen as u64))?;
                state.write(unsafe { std::slice::from_raw_parts(ptr, newlen) });
                Ok(())
            }
        }
    };
}

impl_metered_hash_for_int!(u8, write_u8);
impl_metered_hash_for_int!(u16, write_u16);
impl_metered_hash_for_int!(u32, write_u32);
impl_metered_hash_for_int!(u64, write_u64);
impl_metered_hash_for_int!(u128, write_u128);
impl_metered_hash_for_int!(usize, write_usize);
impl_metered_hash_for_int!(i8, write_i8);
impl_metered_hash_for_int!(i16, write_i16);
impl_metered_hash_for_int!(i32, write_i32);
impl_metered_hash_for_int!(i64, write_i64);
impl_metered_hash_for_int!(i128, write_i128);
impl_metered_hash_for_int!(isize, write_isize);

impl MeteredHash for char {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        budget.charge(HASH_COST_TYPE, Some(std::mem::size_of::<char>() as u64))?;
        state.write_u32(*self as u32);
        Ok(())
    }
}

impl MeteredHash for bool {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        budget.charge(HASH_COST_TYPE, Some(std::mem::size_of::<bool>() as u64))?;
        state.write_u8(*self as u8);
        Ok(())
    }
}

impl MeteredHash for str {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        // Copy what the unstable / builtin Hash::write_str method does, which is write the
        // bytes and then write 0xff as a domain separator.
        budget.charge(HASH_COST_TYPE, Some(self.len().saturating_add(1) as u64))?;
        state.write(self.as_bytes());
        state.write_u8(0xff);
        Ok(())
    }
}

impl<const N: usize, T: MeteredHash> MeteredHash for [T; N] {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        T::metered_hash_slice(self.as_slice(), state, budget)
    }
}

impl<T> MeteredHash for Vec<T>
where
    T: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        T::metered_hash_slice(self.as_slice(), state, budget)
    }
}

impl<T> MeteredHash for Rc<T>
where
    T: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.as_ref().metered_hash(state, budget)
    }
}

impl<T> MeteredHash for Box<T>
where
    T: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.as_ref().metered_hash(state, budget)
    }
}

impl<K, V> MeteredHash for BTreeMap<K, V>
where
    K: MeteredHash,
    V: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.len().metered_hash(state, budget)?;
        for e in self {
            e.metered_hash(state, budget)?;
        }
        Ok(())
    }
}

impl<T> MeteredHash for Option<T>
where
    T: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        if let Some(x) = self {
            x.metered_hash(state, budget)?;
        }
        Ok(())
    }
}

impl<T, U> MeteredHash for (T, U)
where
    T: MeteredHash,
    U: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)?;
        self.1.metered_hash(state, budget)
    }
}

impl<'a, T> MeteredHash for &'a T
where
    T: MeteredHash,
{
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        <T as MeteredHash>::metered_hash(*self, state, budget)
    }
}

impl MeteredHash for HostObject {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            HostObject::Vec(v) => v.metered_hash(state, budget),
            HostObject::Map(v) => v.metered_hash(state, budget),
            HostObject::U64(v) => v.metered_hash(state, budget),
            HostObject::I64(v) => v.metered_hash(state, budget),
            HostObject::TimePoint(v) => v.metered_hash(state, budget),
            HostObject::Duration(v) => v.metered_hash(state, budget),
            HostObject::U128(v) => v.metered_hash(state, budget),
            HostObject::I128(v) => v.metered_hash(state, budget),
            HostObject::U256(v) => v.metered_hash(state, budget),
            HostObject::I256(v) => v.metered_hash(state, budget),
            HostObject::Bytes(v) => v.metered_hash(state, budget),
            HostObject::String(v) => v.metered_hash(state, budget),
            HostObject::Symbol(v) => v.metered_hash(state, budget),
            HostObject::Address(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for U256 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for I256 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for Val {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.get_payload().metered_hash(state, budget)
    }
}

macro_rules! impl_metered_hash_for_val_wrapper {
    ($t:ty) => {
        impl MeteredHash for $t {
            fn metered_hash<H: Hasher>(
                &self,
                state: &mut H,
                budget: &Budget,
            ) -> Result<(), HostError> {
                self.to_val().metered_hash(state, budget)
            }
        }
    };
}

impl_metered_hash_for_val_wrapper!(Object);
impl_metered_hash_for_val_wrapper!(Symbol);
impl_metered_hash_for_val_wrapper!(AddressObject);
impl_metered_hash_for_val_wrapper!(BytesObject);
impl_metered_hash_for_val_wrapper!(VecObject);
impl_metered_hash_for_val_wrapper!(Error);

impl MeteredHash for InternalEvent {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        match self {
            InternalEvent::Contract(c) => {
                c.type_.metered_hash(state, budget)?;
                match c.contract_id {
                    Some(cid) => cid.to_val().get_payload().metered_hash(state, budget)?,
                    None => 0.metered_hash(state, budget)?,
                }
                c.data.get_payload().metered_hash(state, budget)?;
                c.topics.to_val().get_payload().metered_hash(state, budget)
            }
            // These are not included in the hash because they not supposed to
            // be observable, and we only have hashing to support
            // test-observation.
            InternalEvent::Diagnostic(_) => Ok(()),
        }
    }
}

impl MeteredHash for InternalContractEvent {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.type_.metered_hash(state, budget)?;
        self.contract_id.metered_hash(state, budget)?;
        self.topics.metered_hash(state, budget)?;
        self.data.metered_hash(state, budget)
    }
}

impl MeteredHash for InternalDiagnosticEvent {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.contract_id.metered_hash(state, budget)?;
        self.topics.metered_hash(state, budget)?;
        self.args.metered_hash(state, budget)
    }
}

impl MeteredHash for InternalDiagnosticArg {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            InternalDiagnosticArg::HostVal(v) => v.metered_hash(state, budget),
            InternalDiagnosticArg::XdrVal(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for ContractEventType {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)
    }
}

impl MeteredHash for EventError {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        // We hard-coded this in the hash impl, rather than using derive(Hash)
        // or hash_discriminant.
        match self {
            EventError::FromFailedCall => 0.metered_hash(state, budget),
            EventError::FromSuccessfulCall => 1.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for HostFunctionType {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)
    }
}

impl MeteredHash for LedgerKey {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            LedgerKey::Account(v) => v.metered_hash(state, budget),
            LedgerKey::Trustline(v) => v.metered_hash(state, budget),
            LedgerKey::ContractData(v) => v.metered_hash(state, budget),
            LedgerKey::ContractCode(v) => v.metered_hash(state, budget),
            LedgerKey::Offer(_)
            | LedgerKey::Data(_)
            | LedgerKey::ClaimableBalance(_)
            | LedgerKey::LiquidityPool(_)
            | LedgerKey::ConfigSetting(_)
            | LedgerKey::Ttl(_) => Err((ScErrorType::Value, ScErrorCode::InvalidInput).into()),
        }
    }
}

impl MeteredHash for LedgerKeyAccount {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.account_id.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerKeyTrustLine {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.account_id.metered_hash(state, budget)?;
        self.asset.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerKeyContractCode {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerKeyContractData {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.contract.metered_hash(state, budget)?;
        self.key.metered_hash(state, budget)?;
        self.durability.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.last_modified_ledger_seq.metered_hash(state, budget)?;
        self.data.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerEntryExt {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            LedgerEntryExt::V0 => Ok(()),
            LedgerEntryExt::V1(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for LedgerEntryExtensionV1 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.sponsoring_id.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for LedgerEntryExtensionV1Ext {
    fn metered_hash<H: Hasher>(&self, _state: &mut H, _budget: &Budget) -> Result<(), HostError> {
        Ok(())
    }
}

impl MeteredHash for LedgerEntryData {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            LedgerEntryData::Account(v) => v.metered_hash(state, budget),
            LedgerEntryData::Trustline(v) => v.metered_hash(state, budget),
            LedgerEntryData::ContractData(v) => v.metered_hash(state, budget),
            LedgerEntryData::ContractCode(v) => v.metered_hash(state, budget),

            LedgerEntryData::Offer(_)
            | LedgerEntryData::Data(_)
            | LedgerEntryData::ClaimableBalance(_)
            | LedgerEntryData::LiquidityPool(_)
            | LedgerEntryData::ConfigSetting(_)
            | LedgerEntryData::Ttl(_) => {
                Err((ScErrorType::Value, ScErrorCode::InvalidInput).into())
            }
        }
    }
}

impl MeteredHash for AccountEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.account_id.metered_hash(state, budget)?;
        self.balance.metered_hash(state, budget)?;
        self.seq_num.metered_hash(state, budget)?;
        self.num_sub_entries.metered_hash(state, budget)?;
        self.inflation_dest.metered_hash(state, budget)?;
        self.flags.metered_hash(state, budget)?;
        self.home_domain.metered_hash(state, budget)?;
        self.thresholds.metered_hash(state, budget)?;
        self.signers.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for Signer {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.key.metered_hash(state, budget)?;
        self.weight.metered_hash(state, budget)
    }
}

impl MeteredHash for SignerKey {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            SignerKey::Ed25519(v) => v.metered_hash(state, budget),
            SignerKey::PreAuthTx(v) => v.metered_hash(state, budget),
            SignerKey::HashX(v) => v.metered_hash(state, budget),
            SignerKey::Ed25519SignedPayload(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for SignerKeyEd25519SignedPayload {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.ed25519.metered_hash(state, budget)?;
        self.payload.metered_hash(state, budget)
    }
}

impl MeteredHash for AccountEntryExt {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            AccountEntryExt::V0 => Ok(()),
            AccountEntryExt::V1(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for AccountEntryExtensionV1 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.liabilities.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for AccountEntryExtensionV1Ext {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            AccountEntryExtensionV1Ext::V0 => Ok(()),
            AccountEntryExtensionV1Ext::V2(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for AccountEntryExtensionV2 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.num_sponsored.metered_hash(state, budget)?;
        self.num_sponsoring.metered_hash(state, budget)?;
        self.signer_sponsoring_i_ds.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for SponsorshipDescriptor {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for AccountEntryExtensionV2Ext {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            AccountEntryExtensionV2Ext::V0 => Ok(()),
            AccountEntryExtensionV2Ext::V3(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for AccountEntryExtensionV3 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.ext.metered_hash(state, budget)?;
        self.seq_ledger.metered_hash(state, budget)?;
        self.seq_time.metered_hash(state, budget)
    }
}

impl MeteredHash for TimePoint {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for Duration {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for SequenceNumber {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for Thresholds {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        u8::metered_hash_slice(self.as_slice(), state, budget)
    }
}

impl MeteredHash for TrustLineEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.account_id.metered_hash(state, budget)?;
        self.asset.metered_hash(state, budget)?;
        self.balance.metered_hash(state, budget)?;
        self.limit.metered_hash(state, budget)?;
        self.flags.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}
impl MeteredHash for TrustLineEntryExt {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            TrustLineEntryExt::V0 => Ok(()),
            TrustLineEntryExt::V1(v) => v.metered_hash(state, budget),
        }
    }
}
impl MeteredHash for TrustLineEntryV1 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.liabilities.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for Liabilities {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.buying.metered_hash(state, budget)?;
        self.selling.metered_hash(state, budget)
    }
}

impl MeteredHash for TrustLineEntryV1Ext {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            TrustLineEntryV1Ext::V0 => Ok(()),
            TrustLineEntryV1Ext::V2(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for TrustLineEntryExtensionV2 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.liquidity_pool_use_count.metered_hash(state, budget)?;
        self.ext.metered_hash(state, budget)
    }
}

impl MeteredHash for TrustLineEntryExtensionV2Ext {
    fn metered_hash<H: Hasher>(&self, _state: &mut H, _budget: &Budget) -> Result<(), HostError> {
        Ok(())
    }
}

impl MeteredHash for TrustLineAsset {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            TrustLineAsset::Native => Ok(()),
            TrustLineAsset::CreditAlphanum4(v) => v.metered_hash(state, budget),
            TrustLineAsset::CreditAlphanum12(v) => v.metered_hash(state, budget),
            TrustLineAsset::PoolShare(_) => {
                Err((ScErrorType::Value, ScErrorCode::InvalidInput).into())
            }
        }
    }
}

impl MeteredHash for AlphaNum4 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.asset_code.metered_hash(state, budget)?;
        self.issuer.metered_hash(state, budget)
    }
}

impl MeteredHash for AlphaNum12 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.asset_code.metered_hash(state, budget)?;
        self.issuer.metered_hash(state, budget)
    }
}

impl MeteredHash for AssetCode12 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        u8::metered_hash_slice(self.as_slice(), state, budget)
    }
}
impl MeteredHash for AssetCode4 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        u8::metered_hash_slice(self.as_slice(), state, budget)
    }
}

impl MeteredHash for ContractCodeEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.ext.metered_hash(state, budget)?;
        self.hash.metered_hash(state, budget)?;
        self.code.metered_hash(state, budget)
    }
}

impl MeteredHash for ContractDataEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.ext.metered_hash(state, budget)?;
        self.contract.metered_hash(state, budget)?;
        self.key.metered_hash(state, budget)?;
        self.durability.metered_hash(state, budget)?;
        self.val.metered_hash(state, budget)
    }
}

impl MeteredHash for ContractDataDurability {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)
    }
}

impl MeteredHash for ExtensionPoint {
    fn metered_hash<H: Hasher>(&self, _state: &mut H, _budget: &Budget) -> Result<(), HostError> {
        Ok(())
    }
}

impl MeteredHash for BytesM {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        u8::metered_hash_slice(self.as_slice(), state, budget)
    }
}

impl MeteredHash for StringM {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        u8::metered_hash_slice(self.as_slice(), state, budget)
    }
}

#[allow(unused_variables)]
impl MeteredHash for ScVal {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            ScVal::Bool(v) => v.metered_hash(state, budget),
            ScVal::Void => Ok(()),
            ScVal::Error(v) => v.metered_hash(state, budget),
            ScVal::U32(v) => v.metered_hash(state, budget),
            ScVal::I32(v) => v.metered_hash(state, budget),
            ScVal::U64(v) => v.metered_hash(state, budget),
            ScVal::I64(v) => v.metered_hash(state, budget),
            ScVal::Timepoint(v) => v.0.metered_hash(state, budget),
            ScVal::Duration(v) => v.0.metered_hash(state, budget),
            ScVal::U128(v) => v.metered_hash(state, budget),
            ScVal::I128(v) => v.metered_hash(state, budget),
            ScVal::U256(v) => v.metered_hash(state, budget),
            ScVal::I256(v) => v.metered_hash(state, budget),
            ScVal::Bytes(v) => v.metered_hash(state, budget),
            ScVal::String(v) => v.metered_hash(state, budget),
            ScVal::Symbol(v) => v.metered_hash(state, budget),
            ScVal::Vec(v) => v.metered_hash(state, budget),
            ScVal::Map(v) => v.metered_hash(state, budget),
            ScVal::Address(v) => v.metered_hash(state, budget),
            ScVal::LedgerKeyContractInstance => Ok(()),
            ScVal::LedgerKeyNonce(v) => v.metered_hash(state, budget),
            ScVal::ContractInstance(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for ScBytes {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}
impl MeteredHash for ScString {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for ScSymbol {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for ScErrorCode {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)
    }
}

impl MeteredHash for ScError {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            ScError::Contract(x) => x.metered_hash(state, budget),
            ScError::WasmVm(x) => x.metered_hash(state, budget),
            ScError::Context(x) => x.metered_hash(state, budget),
            ScError::Storage(x) => x.metered_hash(state, budget),
            ScError::Object(x) => x.metered_hash(state, budget),
            ScError::Crypto(x) => x.metered_hash(state, budget),
            ScError::Events(x) => x.metered_hash(state, budget),
            ScError::Budget(x) => x.metered_hash(state, budget),
            ScError::Value(x) => x.metered_hash(state, budget),
            ScError::Auth(x) => x.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for UInt128Parts {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hi.metered_hash(state, budget)?;
        self.lo.metered_hash(state, budget)
    }
}

impl MeteredHash for Int128Parts {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hi.metered_hash(state, budget)?;
        self.lo.metered_hash(state, budget)
    }
}

impl MeteredHash for UInt256Parts {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hi_hi.metered_hash(state, budget)?;
        self.hi_lo.metered_hash(state, budget)?;
        self.lo_hi.metered_hash(state, budget)?;
        self.lo_lo.metered_hash(state, budget)
    }
}

impl MeteredHash for Int256Parts {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hi_hi.metered_hash(state, budget)?;
        self.hi_lo.metered_hash(state, budget)?;
        self.lo_hi.metered_hash(state, budget)?;
        self.lo_lo.metered_hash(state, budget)
    }
}

impl MeteredHash for ScVec {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for ScMapEntry {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.key.metered_hash(state, budget)?;
        self.val.metered_hash(state, budget)
    }
}

impl MeteredHash for ScMap {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for crate::xdr::Hash {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for Uint256 {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for PublicKey {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        // There's only one variant, so the standard hashing algorithm apparently skips
        // the discriminant. We'll do the same.
        let PublicKey::PublicKeyTypeEd25519(u256) = self;
        u256.metered_hash(state, budget)
    }
}

impl MeteredHash for AccountId {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.0.metered_hash(state, budget)
    }
}

impl MeteredHash for ScAddress {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            ScAddress::Account(aid) => aid.metered_hash(state, budget),
            ScAddress::Contract(hash) => hash.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for ScNonceKey {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.nonce.metered_hash(state, budget)
    }
}

impl MeteredHash for ScContractInstance {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.executable.metered_hash(state, budget)?;
        self.storage.metered_hash(state, budget)
    }
}

impl MeteredHash for ContractExecutable {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            ContractExecutable::Wasm(hash) => hash.metered_hash(state, budget),
            ContractExecutable::StellarAsset => Ok(()),
        }
    }
}

impl MeteredHash for CreateContractArgs {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.contract_id_preimage.metered_hash(state, budget)?;
        self.executable.metered_hash(state, budget)
    }
}

impl MeteredHash for ContractIdPreimage {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            ContractIdPreimage::Address(v) => v.metered_hash(state, budget),
            ContractIdPreimage::Asset(v) => v.metered_hash(state, budget),
        }
    }
}
impl MeteredHash for Asset {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.hash_discriminant(state, budget)?;
        match self {
            Asset::Native => Ok(()),
            Asset::CreditAlphanum4(v) => v.metered_hash(state, budget),
            Asset::CreditAlphanum12(v) => v.metered_hash(state, budget),
        }
    }
}

impl MeteredHash for ContractIdPreimageFromAddress {
    fn metered_hash<H: Hasher>(&self, state: &mut H, budget: &Budget) -> Result<(), HostError> {
        self.address.metered_hash(state, budget)?;
        self.salt.metered_hash(state, budget)
    }
}

#[cfg(all(test, feature = "testutils"))]
mod test {
    use super::*;
    use crate::Host;
    use arbitrary::{Arbitrary, Unstructured};
    use rand::RngCore;

    fn check_metered_hash_equal_to_std_hash<T: Hash + MeteredHash + core::fmt::Debug + ?Sized>(
        x: &T,
        n: &mut usize,
    ) {
        let mut std_hasher = std::collections::hash_map::DefaultHasher::new();
        x.hash(&mut std_hasher);
        let std_hash = std_hasher.finish();

        let mut metered_hasher = std::collections::hash_map::DefaultHasher::new();
        let budget = Budget::default();
        budget.reset_unlimited().unwrap();

        // Ignore errors, they just mean "unrepresentable" (they have a different hash though)
        if let Ok(()) = x.metered_hash(&mut metered_hasher, &budget) {
            let metered_hash = metered_hasher.finish();

            if std_hash != metered_hash {
                dbg!(x);
            }
            assert_eq!(std_hash, metered_hash);
            *n += 1;
        }
    }

    fn check_random_hash_identities_for<
        T: MeteredHash + std::hash::Hash + for<'a> Arbitrary<'a> + core::fmt::Debug + ?Sized,
    >() {
        const ITERATIONS: u32 = 50_000;
        let host = Host::test_host();
        host.budget_ref().reset_unlimited().unwrap();
        let mut n: usize = 0;
        for _ in 0..ITERATIONS {
            let mut data = vec![0u8; 50000];
            host.with_test_prng(|rng| {
                rng.fill_bytes(data.as_mut_slice());
                Ok(())
            })
            .unwrap();

            let x = T::arbitrary(&mut Unstructured::new(data.as_slice())).unwrap();
            check_metered_hash_equal_to_std_hash(&x, &mut n);
        }
        println!(
            "checked {} random hash identities for {}",
            n,
            std::any::type_name::<T>()
        )
    }

    #[test]
    #[ignore = "too expensive to run regularly"]
    fn test_random_hash_identities() {
        check_random_hash_identities_for::<ScVal>();
        check_random_hash_identities_for::<LedgerKey>();
        check_random_hash_identities_for::<LedgerEntry>();
    }

    #[test]
    fn test_fixed_hash_identities() {
        let mut n: usize = 0;
        check_metered_hash_equal_to_std_hash(&true, &mut n);
        check_metered_hash_equal_to_std_hash(&false, &mut n);
        check_metered_hash_equal_to_std_hash(&0u8, &mut n);
        check_metered_hash_equal_to_std_hash(&0u16, &mut n);
        check_metered_hash_equal_to_std_hash(&0u32, &mut n);
        check_metered_hash_equal_to_std_hash(&0u64, &mut n);
        check_metered_hash_equal_to_std_hash(&0u128, &mut n);
        check_metered_hash_equal_to_std_hash(&0usize, &mut n);
        check_metered_hash_equal_to_std_hash(&0i8, &mut n);
        check_metered_hash_equal_to_std_hash(&0i16, &mut n);
        check_metered_hash_equal_to_std_hash(&0i32, &mut n);
        check_metered_hash_equal_to_std_hash(&0i64, &mut n);
        check_metered_hash_equal_to_std_hash(&0i128, &mut n);
        check_metered_hash_equal_to_std_hash(&0isize, &mut n);
        check_metered_hash_equal_to_std_hash(&'a', &mut n);
        check_metered_hash_equal_to_std_hash("abc", &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0u8), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0u16), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0u32), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0u64), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0u128), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0usize), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0i8), &mut n);
        check_metered_hash_equal_to_std_hash(&Some(0i16), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::Bool(true), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::Bool(false), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::Void, &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::U32(0), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::I32(0), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::U64(0), &mut n);
        check_metered_hash_equal_to_std_hash(&ScVal::I64(123459), &mut n);
        check_metered_hash_equal_to_std_hash(
            &ScVal::Bytes(vec![1, 2, 3, 4, 5, 6].try_into().unwrap()),
            &mut n,
        );
        check_metered_hash_equal_to_std_hash(
            &vec![HostObject::Bytes(ScBytes(
                vec![1, 2, 3, 4, 5, 6].try_into().unwrap(),
            ))],
            &mut n,
        );
        check_metered_hash_equal_to_std_hash(
            &vec![HostObject::Bytes(ScBytes(vec![].try_into().unwrap()))],
            &mut n,
        );
        check_metered_hash_equal_to_std_hash(
            &vec![HostObject::Vec(
                crate::MeteredVector::from_vec(vec![crate::U32Val::from(3).to_val()]).unwrap(),
            )],
            &mut n,
        );
        check_metered_hash_equal_to_std_hash(&crate::xdr::Hash([1u8; 32]), &mut n);
        check_metered_hash_equal_to_std_hash(&crate::xdr::Uint256([1u8; 32]), &mut n);
        println!("checked {} fixed hash identities", n)
    }
}
