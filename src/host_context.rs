use im_rc::{OrdMap, Vector};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::rc::{Rc, Weak};

use crate::Val;
pub(crate) struct ValInContext {
    ctx: Weak<HostContext>,
    val: Val,
}

enum HostObject {
    Box(ValInContext),
    Vec(Vector<ValInContext>),
    Map(OrdMap<ValInContext, ValInContext>),
    U64(u64),
    I64(i64),
    Str(String),
    Blob(Vec<u8>),
    BigInt(BigInt),
    BigRat(BigRational),
}

pub struct HostContext {
    objects: Vec<HostObject>,
}

impl HostContext {
    pub(crate) fn associate(self: Rc<Self>, val: Val) -> ValInContext {
        let ctx = Rc::downgrade(&self);
        ValInContext { ctx, val }
    }
}
