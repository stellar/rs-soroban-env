use crate::{
    num::{i256_from_pieces, i256_into_pieces, u256_from_pieces, u256_into_pieces},
    ConversionError, DurationSmall, DurationVal, Env, I128Small, I256Small, I64Small,
    TimepointSmall, TimepointVal, U128Small, U256Small, U64Small, U64Val, Val, I256, U256,
};
use core::fmt::Debug;
use stellar_xdr::int128_helpers;

#[cfg(feature = "std")]
use crate::{
    num, object::ScValObjRef, val::ValConvert, Error, Object, ScValObject, SymbolSmall, Tag,
};
#[cfg(feature = "std")]
use stellar_xdr::{
    Duration, Int128Parts, Int256Parts, ScVal, TimePoint, UInt128Parts, UInt256Parts,
};

/// General trait representing a the ability of some object to perform a
/// (possibly unsuccessful) conversion between two other types.
pub trait Convert<F, T> {
    type Error: Debug;
    fn convert(&self, f: F) -> Result<T, Self::Error>;
}

pub trait TryIntoVal<E: Env, V> {
    type Error: Debug;
    fn try_into_val(&self, env: &E) -> Result<V, Self::Error>;
}

pub trait TryFromVal<E: Env, V: ?Sized>: Sized {
    type Error: Debug;
    fn try_from_val(env: &E, v: &V) -> Result<Self, Self::Error>;
}

// Blanket-impl uses of TryIntoVal to TryFromVal, so that we
// only ever have to impl TryFromVal.
impl<E: Env, T, U> TryIntoVal<E, T> for U
where
    T: TryFromVal<E, U>,
{
    type Error = T::Error;

    fn try_into_val(&self, env: &E) -> Result<T, Self::Error> {
        T::try_from_val(env, self)
    }
}

// i64 conversions

impl<E: Env> TryFromVal<E, Val> for i64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = I64Small::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.obj_to_i64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, i64> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &i64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I64Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env.obj_from_i64(v).map_err(|_| ConversionError)?.to_val())
        }
    }
}

// u64 conversions

impl<E: Env> TryFromVal<E, Val> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = U64Small::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.obj_to_u64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U64Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env.obj_from_u64(v).map_err(|_| ConversionError)?.to_val())
        }
    }
}

impl<E: Env> TryFromVal<E, U64Val> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &U64Val) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = U64Small::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.obj_to_u64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for U64Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U64Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env.obj_from_u64(v).map_err(|_| ConversionError)?.into())
        }
    }
}

// {Timepoint, Duration} <-> u64 conversions

impl<E: Env> TryFromVal<E, TimepointVal> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &TimepointVal) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = TimepointSmall::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.timepoint_obj_to_u64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for TimepointVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = TimepointSmall::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .timepoint_obj_from_u64(v)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

impl<E: Env> TryFromVal<E, DurationVal> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &DurationVal) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = DurationSmall::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.duration_obj_to_u64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for DurationVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = DurationSmall::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .duration_obj_from_u64(v)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// i128 conversions

impl<E: Env> TryFromVal<E, Val> for i128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &Val) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I128Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let hi = env.obj_to_i128_hi64(obj).map_err(|_| ConversionError)?;
            let lo = env.obj_to_i128_lo64(obj).map_err(|_| ConversionError)?;
            Ok(int128_helpers::i128_from_pieces(hi, lo))
        }
    }
}
impl<E: Env> TryFromVal<E, i128> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &i128) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I128Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .obj_from_i128_pieces(int128_helpers::i128_hi(v), int128_helpers::i128_lo(v))
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// u128 conversions

impl<E: Env> TryFromVal<E, Val> for u128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &Val) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U128Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let hi = env.obj_to_u128_hi64(obj).map_err(|_| ConversionError)?;
            let lo = env.obj_to_u128_lo64(obj).map_err(|_| ConversionError)?;
            Ok(int128_helpers::u128_from_pieces(hi, lo))
        }
    }
}
impl<E: Env> TryFromVal<E, u128> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u128) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U128Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .obj_from_u128_pieces(int128_helpers::u128_hi(v), int128_helpers::u128_lo(v))
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// i256 conversions
impl<E: Env> TryFromVal<E, Val> for I256 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &Val) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I256Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let hi_hi = env.obj_to_i256_hi_hi(obj).map_err(|_| ConversionError)?;
            let hi_lo = env.obj_to_i256_hi_lo(obj).map_err(|_| ConversionError)?;
            let lo_hi = env.obj_to_i256_lo_hi(obj).map_err(|_| ConversionError)?;
            let lo_lo = env.obj_to_i256_lo_lo(obj).map_err(|_| ConversionError)?;
            Ok(i256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
        }
    }
}
impl<E: Env> TryFromVal<E, I256> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &I256) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I256Small::try_from(v) {
            Ok(so.into())
        } else {
            let (hi_hi, hi_lo, lo_hi, lo_lo) = i256_into_pieces(v);
            Ok(env
                .obj_from_i256_pieces(hi_hi, hi_lo, lo_hi, lo_lo)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// u256 conversions
impl<E: Env> TryFromVal<E, Val> for U256 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &Val) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U256Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let hi_hi = env.obj_to_u256_hi_hi(obj).map_err(|_| ConversionError)?;
            let hi_lo = env.obj_to_u256_hi_lo(obj).map_err(|_| ConversionError)?;
            let lo_hi = env.obj_to_u256_lo_hi(obj).map_err(|_| ConversionError)?;
            let lo_lo = env.obj_to_u256_lo_lo(obj).map_err(|_| ConversionError)?;
            Ok(u256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
        }
    }
}
impl<E: Env> TryFromVal<E, U256> for Val {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &U256) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U256Small::try_from(v) {
            Ok(so.into())
        } else {
            let (hi_hi, hi_lo, lo_hi, lo_lo) = u256_into_pieces(v);
            Ok(env
                .obj_from_u256_pieces(hi_hi, hi_lo, lo_hi, lo_lo)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// ScVal conversions (that require Object conversions)

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, Val> for ScVal
where
    ScValObject: TryFromVal<E, Object>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &Val) -> Result<Self, ConversionError> {
        if let Ok(object) = Object::try_from(val) {
            // FIXME: it's not really great to be dropping the error from the other
            // TryFromVal here, we should really switch to taking errors from E.
            let scvo: ScValObject = object.try_into_val(env).map_err(|_| ConversionError)?;
            return Ok(scvo.into());
        }
        let val = *val;
        match val.get_tag() {
            Tag::False => Ok(ScVal::Bool(false)),
            Tag::True => Ok(ScVal::Bool(true)),
            Tag::Void => Ok(ScVal::Void),
            Tag::Error => {
                let status: Error = unsafe { <Error as ValConvert>::unchecked_from_val(val) };
                Ok(status.try_into()?)
            }
            Tag::U32Val => Ok(ScVal::U32(val.get_major())),
            Tag::I32Val => Ok(ScVal::I32(val.get_major() as i32)),
            Tag::U64Small => Ok(ScVal::U64(val.get_body())),
            Tag::I64Small => Ok(ScVal::I64(val.get_signed_body())),
            Tag::TimepointSmall => Ok(ScVal::Timepoint(TimePoint(val.get_body()))),
            Tag::DurationSmall => Ok(ScVal::Duration(Duration(val.get_body()))),
            Tag::U128Small => Ok(ScVal::U128(UInt128Parts {
                hi: 0,
                lo: val.get_body(),
            })),
            Tag::I128Small => {
                let body = val.get_signed_body() as i128;
                Ok(ScVal::I128(Int128Parts {
                    hi: (body >> 64) as i64,
                    lo: body as u64,
                }))
            }
            Tag::U256Small => Ok(ScVal::U256(UInt256Parts {
                hi_hi: 0,
                hi_lo: 0,
                lo_hi: 0,
                lo_lo: val.get_body(),
            })),
            Tag::I256Small => {
                let body = val.get_signed_body() as i128;
                let (hi_hi, hi_lo, lo_hi, lo_lo) = i256_into_pieces(I256::from(body));
                Ok(ScVal::I256(Int256Parts {
                    hi_hi,
                    hi_lo,
                    lo_hi,
                    lo_lo,
                }))
            }
            Tag::SymbolSmall => {
                let sym: SymbolSmall =
                    unsafe { <SymbolSmall as ValConvert>::unchecked_from_val(val) };
                let str: String = sym.into_iter().collect();
                Ok(ScVal::Symbol(crate::xdr::ScSymbol(
                    str.as_bytes().try_into()?,
                )))
            }

            Tag::LedgerKeyContractInstance => Ok(ScVal::LedgerKeyContractInstance),

            Tag::StorageType => Ok(ScVal::StorageType((val.get_major() as i32).try_into()?)),

            Tag::U64Object
            | Tag::I64Object
            | Tag::TimepointObject
            | Tag::DurationObject
            | Tag::U128Object
            | Tag::I128Object
            | Tag::U256Object
            | Tag::I256Object
            | Tag::BytesObject
            | Tag::StringObject
            | Tag::SymbolObject
            | Tag::VecObject
            | Tag::MapObject
            | Tag::AddressObject => unreachable!(),
            Tag::SmallCodeUpperBound
            | Tag::ObjectCodeLowerBound
            | Tag::ObjectCodeUpperBound
            | Tag::Bad => Err(ConversionError),
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, ScVal> for Val
where
    Object: for<'a> TryFromVal<E, ScValObjRef<'a>, Error = ConversionError>,
{
    type Error = ConversionError;
    fn try_from_val(env: &E, val: &ScVal) -> Result<Val, Self::Error> {
        if let Some(scvo) = ScValObjRef::classify(val) {
            let obj = Object::try_from_val(env, &scvo)?;
            return Ok(obj.into());
        }

        Ok(match val {
            ScVal::Bool(b) => Val::from_bool(*b).into(),
            ScVal::Void => Val::from_void().into(),
            ScVal::Error(e) => e.into(),
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::U64(u) => {
                assert!(num::is_small_u64(*u));
                unsafe { Val::from_body_and_tag(*u, Tag::U64Small) }
            }
            ScVal::I64(i) => {
                assert!(num::is_small_i64(*i));
                unsafe { Val::from_body_and_tag(*i as u64, Tag::I64Small) }
            }
            ScVal::Timepoint(TimePoint(u)) => {
                assert!(num::is_small_u64(*u));
                unsafe { Val::from_body_and_tag(*u, Tag::TimepointSmall) }
            }
            ScVal::Duration(Duration(u)) => {
                assert!(num::is_small_u64(*u));
                unsafe { Val::from_body_and_tag(*u, Tag::DurationSmall) }
            }
            ScVal::U128(u) => {
                let u: u128 = u.into();
                assert!(num::is_small_u128(u));
                unsafe { Val::from_body_and_tag(u as u64, Tag::U128Small) }
            }
            ScVal::I128(i) => {
                let i: i128 = i.into();
                assert!(num::is_small_i128(i));
                unsafe { Val::from_body_and_tag((i as i64) as u64, Tag::I128Small) }
            }
            ScVal::U256(u) => {
                assert!(num::is_small_u256_parts(u));
                unsafe { Val::from_body_and_tag(u.lo_lo, Tag::U256Small) }
            }
            ScVal::I256(i) => {
                assert!(num::is_small_i256_parts(i));
                unsafe { Val::from_body_and_tag(i.lo_lo, Tag::I256Small) }
            }
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(ConversionError),
                };
                SymbolSmall::try_from_str(ss)?.into()
            }
            ScVal::LedgerKeyContractInstance => unsafe {
                Val::from_body_and_tag(0, Tag::LedgerKeyContractInstance)
            },
            ScVal::StorageType(_storage_type) => unsafe {
                let major: i32 = (*_storage_type).into();
                Val::from_major_minor_and_tag(major as u32, 0, Tag::StorageType)
            },

            ScVal::Bytes(_)
            | ScVal::String(_)
            | ScVal::Vec(_)
            | ScVal::Map(_)
            | ScVal::Address(_)
            | ScVal::LedgerKeyNonce(_)
            | ScVal::ContractInstance(_) => unreachable!(),
        })
    }
}
