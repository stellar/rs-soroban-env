use crate::{
    auth::{
        AccountAuthorizationTracker, AccountAuthorizationTrackerSnapshot, AuthorizedInvocation,
        AuthorizedInvocationSnapshot, ContractInvocation, InvokerContractAuthorizationTracker,
    },
    events::{EventError, HostEvent, InternalContractEvent, InternalEvent},
    host::{frame::Context, Events},
    host_object::HostObject,
    native_contract::base_types::Address,
    storage::AccessType,
    xdr::{
        AccountEntry, AccountId, Asset, BytesM, ClaimableBalanceEntry, ConfigSettingEntry,
        ContractCodeEntry, ContractDataDurability, ContractEvent, ContractExecutable,
        ContractIdPreimage, CreateContractArgs, DataEntry, Duration, ExtensionPoint, Hash,
        LedgerEntry, LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyClaimableBalance,
        LedgerKeyConfigSetting, LedgerKeyContractCode, LedgerKeyData, LedgerKeyLiquidityPool,
        LedgerKeyOffer, LedgerKeyTrustLine, LiquidityPoolEntry, OfferEntry, PublicKey, ScAddress,
        ScBytes, ScContractInstance, ScMap, ScMapEntry, ScNonceKey, ScString, ScSymbol, ScVal,
        ScVec, SorobanAuthorizationEntry, SorobanAuthorizedInvocation, StringM, TimePoint,
        TrustLineAsset, TrustLineEntry, TtlEntry, Uint256, SCSYMBOL_LIMIT,
    },
    AddressObject, Bool, BytesObject, DurationObject, DurationSmall, DurationVal, Error, HostError,
    I128Object, I128Small, I128Val, I256Object, I256Small, I256Val, I32Val, I64Object, I64Small,
    I64Val, MapObject, Object, ScValObject, StringObject, Symbol, SymbolObject, SymbolSmall,
    SymbolSmallIter, SymbolStr, TimepointObject, TimepointSmall, TimepointVal, U128Object,
    U128Small, U128Val, U256Object, U256Small, U256Val, U32Val, U64Object, U64Small, U64Val, Val,
    VecObject, Void, I256, U256,
};
use std::rc::Rc;
use wasmi::Value;

// Declared size (bytes) of a single element. This value determines the metering input for clone
// and comparison. It should be the upperbound (across various compilations and platforms) of the
// actual type's size. Implementer of the trait needs to decide this value based on Rust's
// guideline on type layout: https://doc.rust-lang.org/reference/type-layout.html
pub trait DeclaredSizeForMetering {
    const DECLARED_SIZE: u64;
}

macro_rules! impl_declared_size_type {
    ($t:ty, $size:expr) => {
        impl DeclaredSizeForMetering for $t {
            const DECLARED_SIZE: u64 = $size;
        }
    };
}

// Primitive types
impl_declared_size_type!(bool, 1);
impl_declared_size_type!(u8, 1);
impl_declared_size_type!(u32, 4);
impl_declared_size_type!(i32, 4);
impl_declared_size_type!(u64, 8);
impl_declared_size_type!(i64, 8);
impl_declared_size_type!(u128, 16);
impl_declared_size_type!(i128, 16);
impl_declared_size_type!(usize, 8);
// Val-wrapping types
impl_declared_size_type!(Val, 8);
impl_declared_size_type!(Void, 8);
impl_declared_size_type!(Bool, 8);
impl_declared_size_type!(VecObject, 8);
impl_declared_size_type!(MapObject, 8);
impl_declared_size_type!(AddressObject, 8);
impl_declared_size_type!(BytesObject, 8);
impl_declared_size_type!(U32Val, 8);
impl_declared_size_type!(I32Val, 8);
impl_declared_size_type!(U64Val, 8);
impl_declared_size_type!(U64Small, 8);
impl_declared_size_type!(U64Object, 8);
impl_declared_size_type!(I64Val, 8);
impl_declared_size_type!(I64Small, 8);
impl_declared_size_type!(I64Object, 8);
impl_declared_size_type!(TimepointVal, 8);
impl_declared_size_type!(TimepointSmall, 8);
impl_declared_size_type!(TimepointObject, 8);
impl_declared_size_type!(DurationVal, 8);
impl_declared_size_type!(DurationSmall, 8);
impl_declared_size_type!(DurationObject, 8);
impl_declared_size_type!(U128Val, 8);
impl_declared_size_type!(U128Small, 8);
impl_declared_size_type!(U128Object, 8);
impl_declared_size_type!(I128Val, 8);
impl_declared_size_type!(I128Small, 8);
impl_declared_size_type!(I128Object, 8);
impl_declared_size_type!(U256Val, 8);
impl_declared_size_type!(U256Small, 8);
impl_declared_size_type!(U256Object, 8);
impl_declared_size_type!(I256Val, 8);
impl_declared_size_type!(I256Small, 8);
impl_declared_size_type!(I256Object, 8);
impl_declared_size_type!(Object, 8);
impl_declared_size_type!(Error, 8);
impl_declared_size_type!(StringObject, 8);
impl_declared_size_type!(Symbol, 8);
impl_declared_size_type!(SymbolSmall, 8);
impl_declared_size_type!(SymbolObject, 8);
impl_declared_size_type!(Value, 16);
// other common types
impl_declared_size_type!(SymbolStr, SCSYMBOL_LIMIT);
impl_declared_size_type!(SymbolSmallIter, 8);
impl_declared_size_type!(U256, 32);
impl_declared_size_type!(I256, 32);
impl_declared_size_type!(HostObject, 48);
impl_declared_size_type!(HostError, 16);
impl_declared_size_type!(Context, 512);
impl_declared_size_type!(Address, 16);
// xdr types
impl_declared_size_type!(TimePoint, 8);
impl_declared_size_type!(Duration, 8);
impl_declared_size_type!(ScVal, 64);
impl_declared_size_type!(ScValObject, 64);
impl_declared_size_type!(ScMapEntry, 128);
impl_declared_size_type!(ScVec, 24);
impl_declared_size_type!(ScMap, 24);
impl_declared_size_type!(Hash, 32);
impl_declared_size_type!(Uint256, 32);
impl_declared_size_type!(ContractExecutable, 33);
impl_declared_size_type!(AccountId, 32);
impl_declared_size_type!(ScAddress, 33);
impl_declared_size_type!(ScNonceKey, 33);
impl_declared_size_type!(PublicKey, 32);
impl_declared_size_type!(TrustLineAsset, 45);
impl_declared_size_type!(LedgerKeyAccount, 32);
impl_declared_size_type!(LedgerKeyTrustLine, 77);
impl_declared_size_type!(LedgerKeyOffer, 40);
impl_declared_size_type!(LedgerKeyData, 56);
impl_declared_size_type!(LedgerKeyClaimableBalance, 32);
impl_declared_size_type!(LedgerKeyLiquidityPool, 32);
impl_declared_size_type!(LedgerKeyContractCode, 36);
impl_declared_size_type!(LedgerKeyConfigSetting, 4);
impl_declared_size_type!(LedgerEntryExt, 33);
impl_declared_size_type!(AccountEntry, 216);
impl_declared_size_type!(TrustLineEntry, 128);
impl_declared_size_type!(OfferEntry, 152);
impl_declared_size_type!(DataEntry, 80);
impl_declared_size_type!(ClaimableBalanceEntry, 120);
impl_declared_size_type!(LiquidityPoolEntry, 160);
impl_declared_size_type!(ContractCodeEntry, 64);
impl_declared_size_type!(ConfigSettingEntry, 96);
impl_declared_size_type!(TtlEntry, 36);
impl_declared_size_type!(LedgerKey, 120);
impl_declared_size_type!(LedgerEntry, 256);
impl_declared_size_type!(AccessType, 1);
impl_declared_size_type!(InternalContractEvent, 40);
impl_declared_size_type!(ContractEvent, 128);
impl_declared_size_type!(HostEvent, 136);
impl_declared_size_type!(Events, 24);
impl_declared_size_type!(InternalEvent, 40);
impl_declared_size_type!(EventError, 1);
impl_declared_size_type!(ScBytes, 24);
impl_declared_size_type!(ScString, 24);
impl_declared_size_type!(ScSymbol, 24);
impl_declared_size_type!(CreateContractArgs, 98);
impl_declared_size_type!(ContractIdPreimage, 65);
impl_declared_size_type!(ContractDataDurability, 4);
impl_declared_size_type!(ExtensionPoint, 0);
impl_declared_size_type!(ScContractInstance, 64);
impl_declared_size_type!(SorobanAuthorizationEntry, 240);
impl_declared_size_type!(SorobanAuthorizedInvocation, 128);
impl_declared_size_type!(AuthorizedInvocation, 136);
impl_declared_size_type!(AuthorizedInvocationSnapshot, 32);
impl_declared_size_type!(AccountAuthorizationTracker, 232);
impl_declared_size_type!(InvokerContractAuthorizationTracker, 192);
impl_declared_size_type!(AccountAuthorizationTrackerSnapshot, 40);
impl_declared_size_type!(ContractInvocation, 16);
impl_declared_size_type!(Asset, 45);

// composite types

// Rc is an exception, nothing is being cloned. We approximate ref counter bump with the cost of
// cloning 16 bytes.
impl<T> DeclaredSizeForMetering for Rc<T> {
    const DECLARED_SIZE: u64 = 16;
}

// Cloning a slice only clones the reference without deep cloning its contents.
impl<T> DeclaredSizeForMetering for &[T] {
    const DECLARED_SIZE: u64 = 16;
}

impl<K, V> DeclaredSizeForMetering for (K, V)
where
    K: DeclaredSizeForMetering,
    V: DeclaredSizeForMetering,
{
    // Their sum plus an 8 bytes of max possible alignment-mismatch overhead. If we need to handle
    // types with larger alignment mismatches, we need to increase the overhead.
    const DECLARED_SIZE: u64 = <K as DeclaredSizeForMetering>::DECLARED_SIZE
        .saturating_add(<V as DeclaredSizeForMetering>::DECLARED_SIZE)
        .saturating_add(8);
}

impl<C: DeclaredSizeForMetering, const N: usize> DeclaredSizeForMetering for [C; N] {
    // no additional alignment needed since `C::DECLARED_SIZE` is at least `size_of::<C>`
    // which is already multiple of the alignment
    const DECLARED_SIZE: u64 = C::DECLARED_SIZE.saturating_mul(N as u64);
}

impl<const C: u32> DeclaredSizeForMetering for BytesM<C> {
    const DECLARED_SIZE: u64 = 24;
}

impl<const C: u32> DeclaredSizeForMetering for StringM<C> {
    const DECLARED_SIZE: u64 = 24;
}

impl<C> DeclaredSizeForMetering for Vec<C> {
    const DECLARED_SIZE: u64 = 24;
}

impl<C> DeclaredSizeForMetering for Box<C> {
    const DECLARED_SIZE: u64 = 8;
}

impl<C: DeclaredSizeForMetering> DeclaredSizeForMetering for Option<C> {
    // Size of C plus an 8 byte alignment overhead. If we need to handle types with larger
    // alignment size, we need to increase the overhead.
    const DECLARED_SIZE: u64 = C::DECLARED_SIZE.saturating_add(8);
}

impl<C: DeclaredSizeForMetering, E: DeclaredSizeForMetering> DeclaredSizeForMetering
    for Result<C, E>
{
    const DECLARED_SIZE: u64 = C::DECLARED_SIZE + E::DECLARED_SIZE;
}

mod test {
    #[allow(unused)]
    use super::*;

    // This section is for outputting the actual size of types. They are for informational use.
    // They might become outdated due to Rust type changes, and numbers may differ between
    // platforms. Run `UPDATE_EXPECT=true cargo test` to update this.
    #[test]
    fn test_expected_size() {
        use expect_test::expect;
        use std::mem::size_of;

        // primitive types
        expect!["1"].assert_eq(size_of::<bool>().to_string().as_str());
        expect!["1"].assert_eq(size_of::<u8>().to_string().as_str());
        expect!["4"].assert_eq(size_of::<u32>().to_string().as_str());
        expect!["4"].assert_eq(size_of::<i32>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<u64>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<i64>().to_string().as_str());
        expect!["16"].assert_eq(size_of::<u128>().to_string().as_str());
        expect!["16"].assert_eq(size_of::<i128>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<usize>().to_string().as_str());
        // Val-wrapping types
        expect!["8"].assert_eq(size_of::<Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Void>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Bool>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<VecObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<MapObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<AddressObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<BytesObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U32Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I32Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U64Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U64Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U64Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I64Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I64Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I64Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<TimepointVal>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<TimepointSmall>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<TimepointObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<DurationVal>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<DurationSmall>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<DurationObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U128Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U128Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U128Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I128Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I128Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I128Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U256Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U256Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<U256Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I256Val>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I256Small>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<I256Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Object>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Error>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<StringObject>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Symbol>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<SymbolSmall>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<SymbolObject>().to_string().as_str());
        expect!["16"].assert_eq(size_of::<Value>().to_string().as_str());
        // other common types
        expect!["32"].assert_eq(size_of::<SymbolStr>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<SymbolSmallIter>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<U256>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<I256>().to_string().as_str());
        #[cfg(target_arch = "x86_64")]
        expect!["40"].assert_eq(size_of::<HostObject>().to_string().as_str());
        #[cfg(target_arch = "aarch64")]
        expect!["48"].assert_eq(size_of::<HostObject>().to_string().as_str());
        expect!["16"].assert_eq(size_of::<HostError>().to_string().as_str());
        #[cfg(target_arch = "x86_64")]
        expect!["512"].assert_eq(size_of::<Context>().to_string().as_str());
        #[cfg(target_arch = "aarch64")]
        expect!["496"].assert_eq(size_of::<Context>().to_string().as_str());
        expect!["16"].assert_eq(size_of::<Address>().to_string().as_str());
        // xdr types
        expect!["8"].assert_eq(size_of::<TimePoint>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Duration>().to_string().as_str());
        expect!["64"].assert_eq(size_of::<ScVal>().to_string().as_str());
        expect!["64"].assert_eq(size_of::<ScValObject>().to_string().as_str());
        expect!["128"].assert_eq(size_of::<ScMapEntry>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<ScVec>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<ScMap>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<Hash>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<Uint256>().to_string().as_str());
        expect!["33"].assert_eq(size_of::<ContractExecutable>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<AccountId>().to_string().as_str());
        expect!["33"].assert_eq(size_of::<ScAddress>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<ScNonceKey>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<PublicKey>().to_string().as_str());
        expect!["45"].assert_eq(size_of::<TrustLineAsset>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<LedgerKeyAccount>().to_string().as_str());
        expect!["77"].assert_eq(size_of::<LedgerKeyTrustLine>().to_string().as_str());
        expect!["40"].assert_eq(size_of::<LedgerKeyOffer>().to_string().as_str());
        expect!["56"].assert_eq(size_of::<LedgerKeyData>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<LedgerKeyClaimableBalance>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<LedgerKeyLiquidityPool>().to_string().as_str());
        expect!["32"].assert_eq(size_of::<LedgerKeyContractCode>().to_string().as_str());
        expect!["4"].assert_eq(size_of::<LedgerKeyConfigSetting>().to_string().as_str());
        expect!["33"].assert_eq(size_of::<LedgerEntryExt>().to_string().as_str());
        expect!["216"].assert_eq(size_of::<AccountEntry>().to_string().as_str());
        expect!["128"].assert_eq(size_of::<TrustLineEntry>().to_string().as_str());
        expect!["152"].assert_eq(size_of::<OfferEntry>().to_string().as_str());
        expect!["80"].assert_eq(size_of::<DataEntry>().to_string().as_str());
        expect!["120"].assert_eq(size_of::<ClaimableBalanceEntry>().to_string().as_str());
        expect!["160"].assert_eq(size_of::<LiquidityPoolEntry>().to_string().as_str());
        expect!["56"].assert_eq(size_of::<ContractCodeEntry>().to_string().as_str());
        expect!["96"].assert_eq(size_of::<ConfigSettingEntry>().to_string().as_str());
        expect!["36"].assert_eq(size_of::<TtlEntry>().to_string().as_str());
        expect!["112"].assert_eq(size_of::<LedgerKey>().to_string().as_str());
        expect!["256"].assert_eq(size_of::<LedgerEntry>().to_string().as_str());
        expect!["1"].assert_eq(size_of::<AccessType>().to_string().as_str());
        expect!["40"].assert_eq(size_of::<InternalContractEvent>().to_string().as_str());
        expect!["128"].assert_eq(size_of::<ContractEvent>().to_string().as_str());
        expect!["136"].assert_eq(size_of::<HostEvent>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<Events>().to_string().as_str());
        expect!["40"].assert_eq(size_of::<InternalEvent>().to_string().as_str());
        expect!["1"].assert_eq(size_of::<EventError>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<ScBytes>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<ScString>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<ScSymbol>().to_string().as_str());
        expect!["98"].assert_eq(size_of::<CreateContractArgs>().to_string().as_str());
        expect!["65"].assert_eq(size_of::<ContractIdPreimage>().to_string().as_str());
        expect!["4"].assert_eq(size_of::<ContractDataDurability>().to_string().as_str());
        expect!["0"].assert_eq(size_of::<ExtensionPoint>().to_string().as_str());
        expect!["128"].assert_eq(
            size_of::<SorobanAuthorizedInvocation>()
                .to_string()
                .as_str(),
        );
        expect!["136"].assert_eq(size_of::<AuthorizedInvocation>().to_string().as_str());
        expect!["32"].assert_eq(
            size_of::<AuthorizedInvocationSnapshot>()
                .to_string()
                .as_str(),
        );
        expect!["232"].assert_eq(
            size_of::<AccountAuthorizationTracker>()
                .to_string()
                .as_str(),
        );
        expect!["192"].assert_eq(
            size_of::<InvokerContractAuthorizationTracker>()
                .to_string()
                .as_str(),
        );
        expect!["40"].assert_eq(
            size_of::<AccountAuthorizationTrackerSnapshot>()
                .to_string()
                .as_str(),
        );
        expect!["16"].assert_eq(size_of::<ContractInvocation>().to_string().as_str());
        expect!["45"].assert_eq(size_of::<Asset>().to_string().as_str());
        // composite types
        expect!["16"].assert_eq(size_of::<&[ScVal]>().to_string().as_str());
        expect!["72"].assert_eq(size_of::<(Val, ScVal)>().to_string().as_str());
        expect!["320"].assert_eq(size_of::<[ScVal; 5]>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<BytesM<10000>>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<StringM<10000>>().to_string().as_str());
        expect!["24"].assert_eq(size_of::<Vec<ScVal>>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Box<ScVal>>().to_string().as_str());
        expect!["8"].assert_eq(size_of::<Rc<ScVal>>().to_string().as_str());
        expect!["64"].assert_eq(size_of::<Option<ScVal>>().to_string().as_str());
        expect!["64"].assert_eq(size_of::<ScContractInstance>().to_string().as_str());
    }

    // This is the actual test.
    // For any new type that impls `DeclaredSizeForMetering`, a new test case needs to be
    // added below.
    #[test]
    fn test_declared_size() {
        use more_asserts as ma;
        use std::mem::size_of;

        macro_rules! assert_mem_size_le_declared_size {
            ($t:ty) => {
                ma::assert_le!(
                    size_of::<$t>() as u64,
                    <$t as DeclaredSizeForMetering>::DECLARED_SIZE
                );
            };
        }
        // primitive types
        assert_mem_size_le_declared_size!(bool);
        assert_mem_size_le_declared_size!(u8);
        assert_mem_size_le_declared_size!(u32);
        assert_mem_size_le_declared_size!(i32);
        assert_mem_size_le_declared_size!(u64);
        assert_mem_size_le_declared_size!(i64);
        assert_mem_size_le_declared_size!(u128);
        assert_mem_size_le_declared_size!(i128);
        assert_mem_size_le_declared_size!(usize);
        // Val-wrapping types
        assert_mem_size_le_declared_size!(Val);
        assert_mem_size_le_declared_size!(Void);
        assert_mem_size_le_declared_size!(Bool);
        assert_mem_size_le_declared_size!(VecObject);
        assert_mem_size_le_declared_size!(MapObject);
        assert_mem_size_le_declared_size!(AddressObject);
        assert_mem_size_le_declared_size!(BytesObject);
        assert_mem_size_le_declared_size!(U32Val);
        assert_mem_size_le_declared_size!(I32Val);
        assert_mem_size_le_declared_size!(U64Val);
        assert_mem_size_le_declared_size!(U64Small);
        assert_mem_size_le_declared_size!(U64Object);
        assert_mem_size_le_declared_size!(I64Val);
        assert_mem_size_le_declared_size!(I64Small);
        assert_mem_size_le_declared_size!(I64Object);
        assert_mem_size_le_declared_size!(TimepointVal);
        assert_mem_size_le_declared_size!(TimepointSmall);
        assert_mem_size_le_declared_size!(TimepointObject);
        assert_mem_size_le_declared_size!(DurationVal);
        assert_mem_size_le_declared_size!(DurationSmall);
        assert_mem_size_le_declared_size!(DurationObject);
        assert_mem_size_le_declared_size!(U128Val);
        assert_mem_size_le_declared_size!(U128Small);
        assert_mem_size_le_declared_size!(U128Object);
        assert_mem_size_le_declared_size!(I128Val);
        assert_mem_size_le_declared_size!(I128Small);
        assert_mem_size_le_declared_size!(I128Object);
        assert_mem_size_le_declared_size!(U256Val);
        assert_mem_size_le_declared_size!(U256Small);
        assert_mem_size_le_declared_size!(U256Object);
        assert_mem_size_le_declared_size!(I256Val);
        assert_mem_size_le_declared_size!(I256Small);
        assert_mem_size_le_declared_size!(I256Object);
        assert_mem_size_le_declared_size!(Object);
        assert_mem_size_le_declared_size!(Error);
        assert_mem_size_le_declared_size!(StringObject);
        assert_mem_size_le_declared_size!(Symbol);
        assert_mem_size_le_declared_size!(SymbolSmall);
        assert_mem_size_le_declared_size!(SymbolObject);
        assert_mem_size_le_declared_size!(Value);
        // other common types
        assert_mem_size_le_declared_size!(SymbolStr);
        assert_mem_size_le_declared_size!(SymbolSmallIter);
        assert_mem_size_le_declared_size!(U256);
        assert_mem_size_le_declared_size!(I256);
        assert_mem_size_le_declared_size!(HostObject);
        assert_mem_size_le_declared_size!(HostError);
        assert_mem_size_le_declared_size!(Context);
        assert_mem_size_le_declared_size!(Address);
        // xdr types
        assert_mem_size_le_declared_size!(TimePoint);
        assert_mem_size_le_declared_size!(Duration);
        assert_mem_size_le_declared_size!(ScVal);
        assert_mem_size_le_declared_size!(ScValObject);
        assert_mem_size_le_declared_size!(ScMapEntry);
        assert_mem_size_le_declared_size!(ScVec);
        assert_mem_size_le_declared_size!(ScMap);
        assert_mem_size_le_declared_size!(Hash);
        assert_mem_size_le_declared_size!(Uint256);
        assert_mem_size_le_declared_size!(ContractExecutable);
        assert_mem_size_le_declared_size!(AccountId);
        assert_mem_size_le_declared_size!(ScAddress);
        assert_mem_size_le_declared_size!(ScNonceKey);
        assert_mem_size_le_declared_size!(PublicKey);
        assert_mem_size_le_declared_size!(TrustLineAsset);
        assert_mem_size_le_declared_size!(LedgerKeyAccount);
        assert_mem_size_le_declared_size!(LedgerKeyTrustLine);
        assert_mem_size_le_declared_size!(LedgerKeyOffer);
        assert_mem_size_le_declared_size!(LedgerKeyData);
        assert_mem_size_le_declared_size!(LedgerKeyClaimableBalance);
        assert_mem_size_le_declared_size!(LedgerKeyLiquidityPool);
        assert_mem_size_le_declared_size!(LedgerKeyContractCode);
        assert_mem_size_le_declared_size!(LedgerKeyConfigSetting);
        assert_mem_size_le_declared_size!(LedgerEntryExt);
        assert_mem_size_le_declared_size!(AccountEntry);
        assert_mem_size_le_declared_size!(TrustLineEntry);
        assert_mem_size_le_declared_size!(OfferEntry);
        assert_mem_size_le_declared_size!(DataEntry);
        assert_mem_size_le_declared_size!(ClaimableBalanceEntry);
        assert_mem_size_le_declared_size!(LiquidityPoolEntry);
        assert_mem_size_le_declared_size!(ContractCodeEntry);
        assert_mem_size_le_declared_size!(ConfigSettingEntry);
        assert_mem_size_le_declared_size!(TtlEntry);
        assert_mem_size_le_declared_size!(LedgerKey);
        assert_mem_size_le_declared_size!(LedgerEntry);
        assert_mem_size_le_declared_size!(AccessType);
        assert_mem_size_le_declared_size!(InternalContractEvent);
        assert_mem_size_le_declared_size!(ContractEvent);
        assert_mem_size_le_declared_size!(HostEvent);
        assert_mem_size_le_declared_size!(Events);
        assert_mem_size_le_declared_size!(InternalEvent);
        assert_mem_size_le_declared_size!(ScBytes);
        assert_mem_size_le_declared_size!(ScString);
        assert_mem_size_le_declared_size!(ScSymbol);
        assert_mem_size_le_declared_size!(CreateContractArgs);
        assert_mem_size_le_declared_size!(ContractDataDurability);
        assert_mem_size_le_declared_size!(ExtensionPoint);
        assert_mem_size_le_declared_size!(SorobanAuthorizedInvocation);
        assert_mem_size_le_declared_size!(SorobanAuthorizationEntry);
        assert_mem_size_le_declared_size!(AuthorizedInvocation);
        assert_mem_size_le_declared_size!(AuthorizedInvocationSnapshot);
        assert_mem_size_le_declared_size!(AccountAuthorizationTracker);
        assert_mem_size_le_declared_size!(InvokerContractAuthorizationTracker);
        assert_mem_size_le_declared_size!(AccountAuthorizationTrackerSnapshot);
        assert_mem_size_le_declared_size!(ContractInvocation);
        assert_mem_size_le_declared_size!(Asset);
        // composite types
        assert_mem_size_le_declared_size!(&[ScVal]);
        assert_mem_size_le_declared_size!((Val, ScVal));
        assert_mem_size_le_declared_size!([ScVal; 5]);
        assert_mem_size_le_declared_size!(BytesM<10000>);
        assert_mem_size_le_declared_size!(StringM<10000>);
        assert_mem_size_le_declared_size!(Vec<ScVal>);
        assert_mem_size_le_declared_size!(Box<ScVal>);
        assert_mem_size_le_declared_size!(Option<ScVal>);
        assert_mem_size_le_declared_size!(Rc<ScVal>);
    }
}
