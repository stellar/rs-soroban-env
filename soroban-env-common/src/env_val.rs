use crate::{ConversionError, Env, I128Small, I64Small, RawVal, U128Small, U64Small};
use core::fmt::Debug;

#[cfg(feature = "std")]
use crate::{
    num, object::ScValObjRef, Object, RawValConvertible, ScValObject, Status, SymbolSmall, Tag,
    I256, U256,
};
#[cfg(feature = "std")]
use stellar_xdr::{Duration, Int128Parts, ScVal, TimePoint};

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

// FIXME: maybe return to conversion errors and improve logging / revive
// this code path.
#[allow(dead_code)]
pub(crate) fn log_err_convert<T>(env: &impl Env, val: &impl AsRef<RawVal>) {
    // Logging here is best-effort; ignore failures (they only arise if we're
    // out of gas or something otherwise-unrecoverable).
    let _ = env.log_static_fmt_val_static_str(
        "can't convert {} to {}",
        *val.as_ref(),
        core::any::type_name::<T>(),
    );
}

// i64 conversions

impl<E: Env> TryFromVal<E, RawVal> for i64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = I64Small::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.obj_to_i64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, i64> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &i64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I64Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env.obj_from_i64(v).map_err(|_| ConversionError)?.to_raw())
        }
    }
}

// u64 conversions

impl<E: Env> TryFromVal<E, RawVal> for u64 {
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
        let val = *val;
        if let Ok(so) = U64Small::try_from(val) {
            Ok(so.into())
        } else {
            let obj = val.try_into()?;
            Ok(env.obj_to_u64(obj).map_err(|_| ConversionError)?)
        }
    }
}

impl<E: Env> TryFromVal<E, u64> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u64) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U64Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env.obj_from_u64(v).map_err(|_| ConversionError)?.to_raw())
        }
    }
}

// i128 conversions

impl<E: Env> TryFromVal<E, RawVal> for i128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &RawVal) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I128Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let lo = env.obj_to_i128_lo64(obj).map_err(|_| ConversionError)?;
            let hi = env.obj_to_i128_hi64(obj).map_err(|_| ConversionError)?;
            let u: u128 = (lo as u128) | ((hi as u128) << 64);
            Ok(u as i128)
        }
    }
}
impl<E: Env> TryFromVal<E, i128> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &i128) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = I128Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .obj_from_i128_pieces(v as u64, (v as u128 >> 64) as u64)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// u128 conversions

impl<E: Env> TryFromVal<E, RawVal> for u128 {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &RawVal) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U128Small::try_from(v) {
            Ok(so.into())
        } else {
            let obj = v.try_into()?;
            let lo = env.obj_to_u128_lo64(obj).map_err(|_| ConversionError)?;
            let hi = env.obj_to_u128_hi64(obj).map_err(|_| ConversionError)?;
            let u: u128 = (lo as u128) | ((hi as u128) << 64);
            Ok(u)
        }
    }
}
impl<E: Env> TryFromVal<E, u128> for RawVal {
    type Error = ConversionError;

    fn try_from_val(env: &E, v: &u128) -> Result<Self, Self::Error> {
        let v = *v;
        if let Ok(so) = U128Small::try_from(v) {
            Ok(so.into())
        } else {
            Ok(env
                .obj_from_u128_pieces(v as u64, (v >> 64) as u64)
                .map_err(|_| ConversionError)?
                .into())
        }
    }
}

// TODO: need some {iu}256 conversions, once we have host functions.

// ScVal conversions (that require Object conversions)

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, RawVal> for ScVal
where
    ScValObject: TryFromVal<E, Object>,
{
    type Error = ConversionError;

    fn try_from_val(env: &E, val: &RawVal) -> Result<Self, Self::Error> {
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
            Tag::Status => {
                let status: Status =
                    unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                Ok(status.try_into()?)
            }
            Tag::U32Val => Ok(ScVal::U32(val.get_major())),
            Tag::I32Val => Ok(ScVal::I32(val.get_major() as i32)),
            Tag::U64Small => Ok(ScVal::U64(val.get_body())),
            Tag::I64Small => Ok(ScVal::I64(val.get_signed_body())),
            Tag::TimepointSmall => Ok(ScVal::Timepoint(TimePoint(val.get_body()))),
            Tag::DurationSmall => Ok(ScVal::Duration(Duration(val.get_body()))),
            Tag::U128Small => Ok(ScVal::U128(Int128Parts {
                hi: 0,
                lo: val.get_body(),
            })),
            Tag::I128Small => Ok(ScVal::I128(Int128Parts {
                hi: 0,
                lo: val.get_signed_body() as u64,
            })),
            Tag::U256Small => todo!(),
            Tag::I256Small => todo!(),
            Tag::SymbolSmall => {
                let sym: SymbolSmall =
                    unsafe { <SymbolSmall as RawValConvertible>::unchecked_from_val(val) };
                let str: String = sym.into_iter().collect();
                Ok(ScVal::Symbol(crate::xdr::ScSymbol(
                    str.as_bytes().try_into()?,
                )))
            }

            Tag::LedgerKeyContractExecutable => Ok(ScVal::LedgerKeyContractExecutable),
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
            | Tag::ContractExecutableObject
            | Tag::AddressObject
            | Tag::LedgerKeyNonceObject => unreachable!(),
            Tag::SmallCodeUpperBound
            | Tag::ObjectCodeLowerBound
            | Tag::ObjectCodeUpperBound
            | Tag::Bad => Err(ConversionError),
        }
    }
}

#[cfg(feature = "std")]
impl<E: Env> TryFromVal<E, ScVal> for RawVal
where
    Object: for<'a> TryFromVal<E, ScValObjRef<'a>, Error = ConversionError>,
{
    type Error = ConversionError;
    fn try_from_val(env: &E, val: &ScVal) -> Result<RawVal, Self::Error> {
        if let Some(scvo) = ScValObjRef::classify(val) {
            let obj = Object::try_from_val(env, &scvo)?;
            return Ok(obj.into());
        }

        Ok(match val {
            ScVal::Bool(b) => RawVal::from_bool(*b).into(),
            ScVal::Void => RawVal::from_void().into(),
            ScVal::Status(st) => st.into(),
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::U64(u) => {
                assert!(num::is_small_u64(*u));
                unsafe { RawVal::from_body_and_tag(*u, Tag::U64Small) }
            }
            ScVal::I64(i) => {
                assert!(num::is_small_i64(*i));
                unsafe { RawVal::from_body_and_tag(*i as u64, Tag::I64Small) }
            }
            ScVal::Timepoint(TimePoint(u)) => {
                assert!(num::is_small_u64(*u));
                unsafe { RawVal::from_body_and_tag(*u, Tag::TimepointSmall) }
            }
            ScVal::Duration(Duration(u)) => {
                assert!(num::is_small_u64(*u));
                unsafe { RawVal::from_body_and_tag(*u, Tag::DurationSmall) }
            }
            ScVal::U128(u) => {
                let u: u128 = u.into();
                assert!(num::is_small_u128(u));
                unsafe { RawVal::from_body_and_tag(u as u64, Tag::U128Small) }
            }
            ScVal::I128(i) => {
                let i: i128 = i.into();
                assert!(num::is_small_i128(i));
                unsafe { RawVal::from_body_and_tag((i as i64) as u64, Tag::I128Small) }
            }
            ScVal::U256(u) => {
                let u: U256 = U256::from_be_bytes(u.0.clone());
                assert!(num::is_small_u256(&u));
                unsafe { RawVal::from_body_and_tag(u.as_u64(), Tag::U256Small) }
            }
            ScVal::I256(i) => {
                let i: I256 = I256::from_be_bytes(i.0.clone());
                assert!(num::is_small_i256(&i));
                unsafe { RawVal::from_body_and_tag(i.as_i64() as u64, Tag::I256Small) }
            }
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(ConversionError),
                };
                SymbolSmall::try_from_str(ss)?.into()
            }
            ScVal::LedgerKeyContractExecutable => unsafe {
                RawVal::from_body_and_tag(0, Tag::LedgerKeyContractExecutable)
            },

            ScVal::Bytes(_)
            | ScVal::String(_)
            | ScVal::Vec(_)
            | ScVal::Map(_)
            | ScVal::ContractExecutable(_)
            | ScVal::Address(_)
            | ScVal::LedgerKeyNonce(_) => unreachable!(),
        })
    }
}
