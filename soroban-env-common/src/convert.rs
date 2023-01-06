use crate::{EnvBase, Object, RawVal, RawValConvertible};
use core::borrow::Borrow;
use stellar_xdr::ScObjectType;

#[cfg(feature = "std")]
use crate::{BitSet, Static, Status, Symbol, Tag};
#[cfg(feature = "std")]
use stellar_xdr::{ScObject, ScStatic, ScStatus, ScVal, StringM};

/// General trait representing the capacity to convert `Object` handles to and
/// from other types, as well as report errors associated with conversion.
///
/// This trait exists for two reasons:
///
///   1. To define a `ConvertFrom<ScVal> for RawVal` in this crate (which we
///      must do by orphan rules, because `ConvertFrom` and `RawVal` are defined
///      here) while deferring part of its logic (object conversion) to the
///      downstream crate that defines `Host`, which will impl
///      `Convert<ScObject>`.
///
///   2. To abstract over differences in fallibility (and the associated error
///      type) in object-conversion methods on `Env` and `CheckedEnv`. Both of
///      these types provide object conversion service but `Env` panics or halts
///      the VM on any internal failure (eg. a bad object handle or running out
///      of gas) whereas `CheckedEnv` returns an error in all cases. Since
///      general `ConvertFrom` conversion may have guest-side validity reasons
///      for failing, the _general_ interface to conversion is fallible, and the
///      `CheckedEnv` impl of `Convert` routes more cases into that fallible
///      error path than the `Env` impl.
///
///      (In earlier versions of this code there was also a trait
///      `FromVal`/`IntoVal` that provided "infallible" conversions via the
///      environment, but these were only usable in the fairly narrow case of
///      conversions that were (a) `Env`/`Guest`-based so could escalate
///      internal errors to a crash-or-panic, (b) environment-requiring and not
///      directly convertable with `From`/`TryFrom` and (c) lacked any
///      guest-side well-formedness checking. In practice this meant 4
///      conversions: the injections from {ui}{64,128} to `Object` handles. By
///      removing `FromVal`/`IntoVal` we gain a lot of simplicity in the
///      implementation, and any context that did those injections can just
///      `.unwrap()` the result to get the same effect they got before.)

pub trait ConvertObject<T>: EnvBase {
    fn object_len(&self, obj: Object) -> Result<usize, Self::Error> {
        Err(self.err_convert_value::<T>(obj.to_raw()))
    }

    fn to_object(&self, _t: impl Borrow<T>) -> Result<Object, Self::Error> {
        Err(self.err_convert_type::<T, Object>())
    }

    fn from_object(&self, obj: Object) -> Result<T, Self::Error> {
        Err(self.err_convert_value::<T>(obj.to_raw()))
    }
}

pub trait ConvertFrom<E: EnvBase, T>: Sized {
    fn convert_from(e: &E, t: impl Borrow<T>) -> Result<Self, E::Error>;
}

/// As with the design in the Rust stdlib's Into type, the ConvertInto
/// trait is defined as a convenience form with a blanket impl that
/// calls into the corresponding ConvertFrom impl.
pub trait ConvertInto<E: EnvBase, T>: Sized {
    fn convert_into(&self, e: &E) -> Result<T, E::Error>;
}

impl<E, T, U> ConvertInto<E, U> for T
where
    E: EnvBase,
    U: ConvertFrom<E, T>,
{
    fn convert_into(&self, e: &E) -> Result<U, E::Error> {
        U::convert_from(e, self)
    }
}

// We do _not_ blanket-impl on U: TryFrom<T>, as as doing so would produce an
// overlap-check failure if we use generics elsewhere (as we wish to, in option
// or result). Instead we include impls of the (few) env-less 'TryFrom' upgrades
// we wish to support on RawVal over in the raw_val module.
/* impl<E:EnvBase,T:Clone,U> ConvertFrom<E,C> for U
where
    U: TryFrom<T>
{
    fn convert_from(e: &E, t: impl Borrow<T>) -> Result<Self, E::Error> {
        U::try_from(t.borrow().clone()).map_err(|_| e.err_convert_type::<T,U>())
    }
}
 */

// i64 conversions
impl<E: EnvBase> ConvertFrom<E, i64> for RawVal
where
    E: ConvertObject<i64>,
{
    fn convert_from(e: &E, t: impl Borrow<i64>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if t >= 0 {
            Ok(unsafe { RawVal::unchecked_from_u63(t) })
        } else {
            e.to_object(t).map(|obj| obj.to_raw())
        }
    }
}

impl<E: EnvBase> ConvertFrom<E, RawVal> for i64
where
    E: ConvertObject<i64>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if t.is_u63() {
            Ok(unsafe { t.unchecked_as_u63() })
        } else if Object::val_is_obj_type(t, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            e.from_object(obj)
        } else {
            Err(e.err_convert_value::<i64>(t))
        }
    }
}

// u64 conversions
impl<E: EnvBase> ConvertFrom<E, u64> for RawVal
where
    E: ConvertObject<u64>,
{
    fn convert_from(e: &E, t: impl Borrow<u64>) -> Result<Self, E::Error> {
        e.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<E: EnvBase> ConvertFrom<E, RawVal> for u64
where
    E: ConvertObject<u64>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::U64) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            e.from_object(obj)
        } else {
            Err(e.err_convert_value::<u64>(t))
        }
    }
}

// i128 conversions
impl<E: EnvBase> ConvertFrom<E, i128> for RawVal
where
    E: ConvertObject<i128>,
{
    fn convert_from(e: &E, t: impl Borrow<i128>) -> Result<Self, E::Error> {
        e.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<E: EnvBase> ConvertFrom<E, RawVal> for i128
where
    E: ConvertObject<i128>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::I128) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            e.from_object(obj)
        } else {
            Err(e.err_convert_value::<i128>(t))
        }
    }
}

// u128 conversions
impl<E: EnvBase> ConvertFrom<E, u128> for RawVal
where
    E: ConvertObject<u128>,
{
    fn convert_from(e: &E, t: impl Borrow<u128>) -> Result<Self, E::Error> {
        e.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<E: EnvBase> ConvertFrom<E, RawVal> for u128
where
    E: ConvertObject<u128>,
{
    fn convert_from(e: &E, t: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::U128) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            e.from_object(obj)
        } else {
            Err(e.err_convert_value::<u128>(t))
        }
    }
}

#[cfg(feature = "std")]
impl<E: EnvBase> ConvertFrom<E, RawVal> for ScVal
where
    E: ConvertObject<ScObject>,
{
    fn convert_from(e: &E, val: impl Borrow<RawVal>) -> Result<Self, E::Error> {
        let val: RawVal = *val.borrow();
        if val.is_u63() {
            Ok(ScVal::U63(unsafe { val.unchecked_as_u63() }))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe {
                    <u32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::I32(unsafe {
                    <i32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    let tag_static =
                        unsafe { <Static as RawValConvertible>::unchecked_from_val(val) };
                    if tag_static.is_type(ScStatic::True) {
                        Ok(ScVal::Static(ScStatic::True))
                    } else if tag_static.is_type(ScStatic::False) {
                        Ok(ScVal::Static(ScStatic::False))
                    } else if tag_static.is_type(ScStatic::Void) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else if tag_static.is_type(ScStatic::LedgerKeyContractCode) {
                        Ok(ScVal::Static(ScStatic::LedgerKeyContractCode))
                    } else {
                        Err(e.err_convert_value::<ScVal>(val))
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    e.from_object(ob).map(|scob| ScVal::Object(Some(scob)))
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    let bytes: StringM<10> = str
                        .as_bytes()
                        .try_into()
                        .map_err(|_| e.err_convert_value::<StringM<10>>(val))?;
                    Ok(ScVal::Symbol(bytes))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    let scstatus: ScStatus = status
                        .try_into()
                        .map_err(|_| e.err_convert_value::<ScStatus>(val))?;
                    Ok(ScVal::Status(scstatus))
                }
                Tag::Reserved => Err(e.err_convert_value::<ScVal>(val)),
            }
        }
    }
}

#[cfg(feature = "std")]
impl<E: EnvBase> ConvertFrom<E, ScVal> for RawVal
where
    E: ConvertObject<ScObject>,
{
    fn convert_from(e: &E, val: impl Borrow<ScVal>) -> Result<Self, E::Error> {
        match val.borrow() {
            ScVal::U63(i) => {
                if *i >= 0 {
                    Ok(unsafe { RawVal::unchecked_from_u63(*i) })
                } else {
                    Err(e.err_convert_type::<i64, RawVal>())
                }
            }
            ScVal::U32(u) => Ok((*u).into()),
            ScVal::I32(i) => Ok((*i).into()),
            ScVal::Static(ScStatic::Void) => Ok(RawVal::from_void()),
            ScVal::Static(ScStatic::True) => Ok(RawVal::from_bool(true)),
            ScVal::Static(ScStatic::False) => Ok(RawVal::from_bool(false)),
            ScVal::Static(other) => Ok(RawVal::from_other_static(*other)),
            ScVal::Object(None) => Err(e.err_convert_type::<ScVal, RawVal>()),
            ScVal::Object(Some(scob)) => e.to_object(scob).map(|obj| obj.to_raw()),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(e.err_convert_type::<StringM<10>, &str>()),
                };
                Ok(Symbol::try_from_str(ss)
                    .map_err(|_| e.err_convert_type::<&str, Symbol>())?
                    .into())
            }
            ScVal::Bitset(i) => Ok(BitSet::try_from_u64(*i)
                .map_err(|_| e.err_convert_type::<u64, BitSet>())?
                .into()),
            ScVal::Status(st) => Ok(st.into()),
        }
    }
}
