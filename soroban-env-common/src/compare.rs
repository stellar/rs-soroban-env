#[cfg(feature = "std")]
use std::rc::Rc;

use crate::{
    val::ValConvert,
    xdr::{ScErrorCode, ScErrorType},
    Env, Error, Tag, Val,
};
use core::cmp::Ordering;

/// General trait representing the ability to compare two values of some type.
/// Similar to `core::cmp::Cmp` but with two key differences: the comparison is
/// fallible, and is provided by some external type implementing `Compare`
/// rather than the compared type itself.
///
/// This trait exists to support comparing `Val`s with help from the
/// environment in which object handles are defined, and allowing for the
/// possibility of erroneous inputs like invalid object handles, as well as the
/// possibility of running over budget during the comparison. It is also used
/// in other places where comparison work has to be budgeted, such as inside
/// the host's storage maps.
pub trait Compare<T> {
    type Error;
    fn compare(&self, a: &T, b: &T) -> Result<Ordering, Self::Error>;
}

impl<T, C: Compare<T>> Compare<&T> for C {
    type Error = C::Error;

    fn compare(&self, a: &&T, b: &&T) -> Result<Ordering, Self::Error> {
        <C as Compare<T>>::compare(self, *a, *b)
    }
}

impl<T, C: Compare<T>> Compare<Option<T>> for C {
    type Error = C::Error;

    fn compare(&self, a: &Option<T>, b: &Option<T>) -> Result<Ordering, Self::Error> {
        match (a, b) {
            (Some(a), Some(b)) => <C as Compare<T>>::compare(self, a, b),
            (None, None) => Ok(Ordering::Equal),
            (None, Some(_)) => Ok(Ordering::Less),
            (Some(_), None) => Ok(Ordering::Greater),
        }
    }
}

macro_rules! impl_compare_for_tuple {
    ( $($idx:tt $T:ident),+) => {
        impl<$($T,)+ E, C> Compare<($($T,)+)> for C
        where
            $(C: Compare<$T, Error = E>,)+
        {
            type Error = E;

            fn compare(&self, a: &($($T,)+), b: &($($T,)+)) -> Result<Ordering, Self::Error> {
                $(
                    match <C as Compare<$T>>::compare(self, &a.$idx, &b.$idx)? {
                        unequal @ (Ordering::Less | Ordering::Greater) => return Ok(unequal),
                        _ => ()
                    }
                )*
                Ok(Ordering::Equal)
            }
        }
    };
}

impl_compare_for_tuple!(0 T, 1 U);
impl_compare_for_tuple!(0 T, 1 U, 2 V);
impl_compare_for_tuple!(0 T, 1 U, 2 V, 3 W);
impl_compare_for_tuple!(0 T, 1 U, 2 V, 3 W, 4 X);

#[cfg(feature = "std")]
impl<T, C: Compare<T>> Compare<Vec<T>> for C {
    type Error = C::Error;

    fn compare(&self, a: &Vec<T>, b: &Vec<T>) -> Result<Ordering, Self::Error> {
        let mut i = 0;
        loop {
            match (a.get(i), b.get(i)) {
                (None, None) => return Ok(Ordering::Equal),
                (None, Some(_)) => return Ok(Ordering::Less),
                (Some(_), None) => return Ok(Ordering::Greater),
                (Some(a), Some(b)) => match <C as Compare<T>>::compare(self, a, b)? {
                    Ordering::Equal => i += 1,
                    unequal => return Ok(unequal),
                },
            }
        }
    }
}

#[cfg(feature = "std")]
impl<T, C: Compare<T>> Compare<Box<T>> for C {
    type Error = C::Error;

    fn compare(&self, a: &Box<T>, b: &Box<T>) -> Result<Ordering, Self::Error> {
        <Self as Compare<T>>::compare(self, a, b)
    }
}

#[cfg(feature = "std")]
impl<T, C: Compare<T>> Compare<Rc<T>> for C {
    type Error = C::Error;

    fn compare(&self, a: &Rc<T>, b: &Rc<T>) -> Result<Ordering, Self::Error> {
        <Self as Compare<T>>::compare(self, a, b)
    }
}

// Apparently we can't do a blanket T:Ord impl because there are Ord derivations
// that also go through &T and Option<T> that conflict with our impls above
// (patches welcome from someone who understands trait-system workarounds
// better). But we can list out any concrete Ord instances we want to support
// here.

macro_rules! delegate_compare_to_wrapper {
    ($T:ident,$A:ident,$B:ident,$SELF:ident) => {{
        let a = unsafe { crate::$T::unchecked_from_val(*$A) };
        let b = unsafe { crate::$T::unchecked_from_val(*$B) };
        $SELF.compare(&a, &b)
    }};
}

#[allow(clippy::comparison_chain)]
impl<E: Env> Compare<Val> for E {
    type Error = E::Error;

    fn compare(&self, a: &Val, b: &Val) -> Result<Ordering, Self::Error> {
        if a.get_payload() == b.get_payload() {
            // Fast-path exactly-equal values.
            return Ok(Ordering::Equal);
        }
        if a.is_object() || b.is_object() {
            // Delegate any object-comparing to environment.
            let v = self.obj_cmp(*a, *b)?;
            return if v == 0 {
                Ok(Ordering::Equal)
            } else if v < 0 {
                Ok(Ordering::Less)
            } else {
                Ok(Ordering::Greater)
            };
        }
        let a_tag = a.get_tag();
        let b_tag = b.get_tag();
        if a_tag < b_tag {
            Ok(Ordering::Less)
        } else if a_tag > b_tag {
            Ok(Ordering::Greater)
        } else {
            // Tags are equal so we only have to switch on one.
            match a_tag {
                Tag::False => Ok(Ordering::Equal),
                Tag::True => Ok(Ordering::Equal),
                Tag::Void => Ok(Ordering::Equal),

                Tag::Error => delegate_compare_to_wrapper!(Error, a, b, self),

                Tag::U32Val => delegate_compare_to_wrapper!(U32Val, a, b, self),
                Tag::I32Val => delegate_compare_to_wrapper!(I32Val, a, b, self),

                Tag::U64Small => delegate_compare_to_wrapper!(U64Small, a, b, self),
                Tag::I64Small => delegate_compare_to_wrapper!(I64Small, a, b, self),

                Tag::TimepointSmall => delegate_compare_to_wrapper!(TimepointSmall, a, b, self),
                Tag::DurationSmall => delegate_compare_to_wrapper!(DurationSmall, a, b, self),

                Tag::U128Small => delegate_compare_to_wrapper!(U128Small, a, b, self),
                Tag::I128Small => delegate_compare_to_wrapper!(I128Small, a, b, self),

                Tag::U256Small => delegate_compare_to_wrapper!(U256Small, a, b, self),
                Tag::I256Small => delegate_compare_to_wrapper!(I256Small, a, b, self),

                Tag::SymbolSmall => delegate_compare_to_wrapper!(SymbolSmall, a, b, self),

                Tag::SmallCodeUpperBound => Ok(Ordering::Equal),
                Tag::ObjectCodeLowerBound => Ok(Ordering::Equal),

                // None of the object cases should be reachable, they
                // should all have been handled by the is_object() branch
                // above.
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
                | Tag::AddressObject => Err(self.error_from_error_val(Error::from_type_and_code(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                ))),

                Tag::ObjectCodeUpperBound => Ok(Ordering::Equal),
                Tag::Bad => Ok(Ordering::Equal),
            }
        }
    }
}
