use stellar_xdr::{ScObjectType, ScHostValErrorCode};
use crate::{Object, EnvBase, Status, RawVal, RawValConvertible};
use core::borrow::Borrow;

#[cfg(feature = "std")]
use stellar_xdr::{ScStatic, ScVal, StringM, ScStatus};
#[cfg(feature = "std")]
use crate::{BitSet, Static, Symbol, Tag};

/// General trait representing the capacity to convert `Object` handles to and
/// from other types, as well as report errors associated with conversion.
///
/// This trait exists for two reasons:
/// 
///   1. To define a `ConvertFrom<ScVal> for RawVal` in this crate (which we
///      must do by orphan rules, because `ConvertFrom` and `RawVal` are defined
///      here) while deferring part of its logic (object conversion) to the
///      downstream crate that defines `Host`, which will impl
///      `EnvConvert<ScObject>`.
///
///   2. To abstract over differences in fallibility (and the associated error
///      type) in object-conversion methods on `Env` and `CheckedEnv`. Both of
///      these types provide object conversion service but `Env` panics or halts
///      the VM on any internal failure (eg. a bad object handle or running out
///      of gas) whereas `CheckedEnv` returns an error in all cases. Since
///      general `ConvertFrom` conversion may have guest-side validity reasons
///      for failing, the _general_ interface to conversion is fallible, and the
///      `CheckedEnv` impl of `EnvConvert` routes more cases into that fallible
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

pub trait EnvConvertError: EnvBase {

    type Error: From<Status>;

    /// Generate an error of the correct error type without attempting
    /// conversion; this is used by clients that may wish to generate an error
    /// before even attempting a conversion, or by impls that wish to
    /// use simple, default error reporting.
    fn val_cvt_err<T>(&self, val: RawVal) -> Self::Error {
        // Logging here is best-effort; ignore failures (they only arise if
        // we're out of gas or something otherwise-unrecoverable).
        let _ = self.log_static_fmt_val_static_str(
            "can't convert {} to {}",
            val,
            core::any::type_name::<T>());
            self.cvt_err()
    }

    fn ty_cvt_err<T,U>(&self) -> Self::Error {
        let _ = self.log_static_fmt_general(
            "can't convert {} to {}",
            &[],
            &[core::any::type_name::<T>(),
              core::any::type_name::<U>()]);
            self.cvt_err()
    }

    fn cvt_err(&self) -> Self::Error {
        Self::Error::from(ScHostValErrorCode::UnexpectedValType.into())
    }
}

pub trait EnvConvertObject<T> : EnvConvertError {
    fn to_object(&self, t: impl Borrow<T>) -> Result<Object, Self::Error> {
        Err(self.ty_cvt_err::<T,Object>())
    }
    fn from_object(&self, obj: Object) -> Result<T, Self::Error> {
        Err(self.val_cvt_err::<T>(obj.to_raw()))
    }
}


/// ConvertHelpers<T,U> is a "join point" on the trait graph that asserts a type
/// is both a `ConvertHelper<T>` _and_ a `ConvertHelper<U>`. The one that gets
/// used will be chosen by the call site as appropriate.
///
/// The trait exists to allow us to impl `ConvertFrom<T> for U` (rather than
/// `ConvertInto`) in all concrete cases: `ConvertFrom<T>` has a requirement in
/// its method for a `ConvertHelper<T>` or a `ConvertHelper<Self>` depending on
/// which direction makes sense (whether the `ConvertFrom` is projecting into or
/// out of an object handle). Since we can't know ahead of time which direction
/// it's converting, we simply arrange to always have both directions.
///
/// Why do we want users to define only `ConvertFrom<T>`? Because whichever side
/// users define, we want to have a blanket definition that points to the other
/// automatically; but we _also_ want a blanket on `ConvertFrom` that extends
/// `TryFrom`, and coherence rules require only one unconstrained blanket for a
/// trait.
pub trait EnvConvert<T,U> :
    EnvConvertObject<T> +
    EnvConvertObject<U>
{}

impl<T,U,X> EnvConvert<T,U> for X
where
    X: EnvConvertObject<T>,
    X: EnvConvertObject<U>
{} 

pub trait ConvertFrom<T,C:EnvConvert<T,Self>>: Sized {
    fn convert_from(t: impl Borrow<T>, c: &C) -> Result<Self, C::Error>;
}

/// As with the design in the Rust stdlib's Into type, the ConvertInto
/// trait is defined as a convenience form with a blanket impl that
/// calls into the corresponding ConvertFrom impl.
pub trait ConvertInto<T,C:EnvConvert<T,Self>>: Sized {
    fn convert_into(&self, c: &C) -> Result<T, C::Error>;
}

impl<T, U, C> ConvertInto<U,C> for T
where
    U: ConvertFrom<T,C>,
    C: EnvConvert<T,U>
{
    fn convert_into(&self, c: &C) -> Result<U, C::Error> {
        U::convert_from(self, c)
    }
}


/// Blanket impl `ConvertFrom` fallible assisted conversion for any types
/// implementing the Rust standard fallible conversion types
/// `TryFrom`/`TryInto`. Since `TryInto`/`TryFrom` themselves have a blanket
/// impl on types implementing `From`/`Into`, this extends those types as well.
impl<C,T:Clone,U> ConvertFrom<T,C> for U
where
    U: TryFrom<T>,
    C: EnvConvert<T,U>
{
    fn convert_from(t: impl Borrow<T>, c: &C) -> Result<Self, C::Error> {
        U::try_from(t.borrow().clone()).map_err(|_| c.ty_cvt_err::<T,U>())
    }
}

// i64 conversions
impl<C> ConvertFrom<i64,C> for RawVal
where C: EnvConvert<i64,RawVal>
{
    fn convert_from(t: impl Borrow<i64>, c: &C) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if t >= 0 {
            Ok(unsafe { RawVal::unchecked_from_u63(t) })
        } else {
            c.to_object(t).map(|obj| obj.to_raw())
        }
    }
}

impl<C> ConvertFrom<RawVal,C> for i64
where C: EnvConvert<RawVal,i64>
{
    fn convert_from(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if t.is_u63() {
            Ok(unsafe { t.unchecked_as_u63() })
        } else if Object::val_is_obj_type(t, ScObjectType::I64) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            c.from_object(obj)
        } else{
            Err(c.val_cvt_err::<i64>(t))
        }
    }
}


// u64 conversions
impl<C> ConvertFrom<u64, C> for RawVal 
where C:EnvConvert<u64,RawVal>
{
    fn convert_from(t: impl Borrow<u64>, c: &C) -> Result<Self, C::Error> {
        c.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<C> ConvertFrom<RawVal, C> for u64 
where C:EnvConvert<RawVal,u64>
{
    fn convert_from(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::U64){
            let obj = unsafe { Object::unchecked_from_val(t) };
            c.from_object(obj)
        } else{
            Err(c.val_cvt_err::<u64>(t))
        }
    }
}

// i128 conversions
impl<C> ConvertFrom<i128, C> for RawVal 
where C:EnvConvert<i128,RawVal>
{
    fn convert_from(t: impl Borrow<i128>, c: &C) -> Result<Self, C::Error> {
        c.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<C> ConvertFrom<RawVal, C> for i128 
where C:EnvConvert<RawVal,i128> {
    fn convert_from(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::I128) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            c.from_object(obj)
        } else {
            Err(c.val_cvt_err::<i128>(t))
        }
    }
}

// u128 conversions
impl<C> ConvertFrom<u128, C> for RawVal
where C:EnvConvert<u128,RawVal>
{
    fn convert_from(t: impl Borrow<u128>, c: &C) -> Result<Self, C::Error> {
        c.to_object(t).map(|obj| obj.to_raw())
    }
}

impl<C> ConvertFrom<RawVal, C> for u128 
where C:EnvConvert<RawVal,u128>
{
    fn convert_from(t: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
        let t = *t.borrow();
        if Object::val_is_obj_type(t, ScObjectType::U128) {
            let obj = unsafe { Object::unchecked_from_val(t) };
            c.from_object(obj)
        } else {
            Err(c.val_cvt_err::<u128>(t))
        }
    }
}

// We define a pair of `Converter<RawVal,ScVal>` impls for `Env` and
// `CheckedEnv` that both depend on impls of `Converter<Object,ScObject>`
// which are provided in downstream crates.

#[cfg(feature = "std")]
impl<C> ConvertFrom<RawVal, C> for ScVal 
where C:EnvConvert<RawVal,Self> {
    fn convert_from(val: impl Borrow<RawVal>, c: &C) -> Result<Self, C::Error> {
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
                        Err(c.val_cvt_err::<ScVal>(val))
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    c.from_object(ob)
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    let bytes: StringM<10> = str.as_bytes().try_into().map_err(|_|c.val_cvt_err::<StringM<10>>(val))?;
                    Ok(ScVal::Symbol(bytes))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    let scstatus: ScStatus = status.try_into().map_err(|_| c.val_cvt_err::<ScStatus>(val))?;
                    Ok(ScVal::Status(scstatus))
                }
                Tag::Reserved => Err(c.val_cvt_err::<ScVal>(val)),
            }
        }
    }
}


#[cfg(feature = "std")]
impl<C> ConvertFrom<ScVal, C> for RawVal
where C:EnvConvert<ScVal,Self>
{
    fn convert_from(val: impl Borrow<ScVal>, c: &C) -> Result<Self, C::Error> {
        match val.borrow() {
            ScVal::U63(i) => {
                if *i >= 0 {
                    Ok(unsafe { RawVal::unchecked_from_u63(*i) })
                } else {
                    Err(c.ty_cvt_err::<i64,RawVal>())
                }
            }
            ScVal::U32(u) => Ok((*u).into()),
            ScVal::I32(i) => Ok((*i).into()),
            ScVal::Static(ScStatic::Void) => Ok(RawVal::from_void()),
            ScVal::Static(ScStatic::True) => Ok(RawVal::from_bool(true)),
            ScVal::Static(ScStatic::False) => Ok(RawVal::from_bool(false)),
            ScVal::Static(other) => Ok(RawVal::from_other_static(*other)),
            ScVal::Object(None) => Err(c.ty_cvt_err::<ScVal,RawVal>()),
            ScVal::Object(Some(scob)) => c.to_object(val).map(|obj| obj.to_raw()),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(c.ty_cvt_err::<StringM<10>,&str>()),
                };
                Ok(Symbol::try_from_str(ss).map_err(|_| c.ty_cvt_err::<&str,Symbol>())?.into())
            }
            ScVal::Bitset(i) => Ok(BitSet::try_from_u64(*i).map_err(|_| c.ty_cvt_err::<u64,BitSet>())?.into()),
            ScVal::Status(st) => Ok(st.into()),
        }
    }
}
