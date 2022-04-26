#![allow(unused_variables)]

use im_rc::{OrdMap, Vector};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::cmp::Ordering;
use std::rc::{Rc, Weak};

use crate::val::Tag;
use crate::{require, BitSet, Host, Object, Status, Symbol, Val, ValType};

#[derive(Clone)]
pub struct ValInContext {
    ctx: Weak<HostContext>,
    val: Val,
}

impl ValInContext {
    pub fn get_context(&self) -> Rc<HostContext> {
        self.ctx
            .upgrade()
            .expect("ValInContext.get_context() on expired context")
    }
    pub fn check_same_context(&self, other: &Self) -> Rc<HostContext> {
        let self_ctx = self.get_context();
        let other_ctx = other.get_context();
        require(Rc::ptr_eq(&self_ctx, &other_ctx));
        self_ctx
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HostObject {
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

impl Eq for ValInContext {}

impl PartialEq for ValInContext {
    fn eq(&self, other: &Self) -> bool {
        let ctx = self.check_same_context(other);
        if self.val.get_payload() == other.val.get_payload() {
            // Fast path: bit-identical vals.
            true
        } else if self.val.get_tag() != Tag::Object || other.val.get_tag() != Tag::Object {
            // Other fast path: non-identical non-objects, must be non-equal.
            false
        } else {
            // Slow path: deep object comparison.
            let self_hobj = unsafe { ctx.unchecked_host_obj_from_val(self.val) };
            let other_hobj = unsafe { ctx.unchecked_host_obj_from_val(other.val) };
            *self_hobj == *other_hobj
        }
    }
}

impl PartialOrd for ValInContext {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ValInContext {
    fn cmp(&self, other: &Self) -> Ordering {
        let ctx = self.check_same_context(other);
        let self_tag = self.val.get_tag();
        let other_tag = other.val.get_tag();
        if self_tag < other_tag {
            Ordering::Less
        } else if self_tag > other_tag {
            Ordering::Greater
        } else {
            // Tags are equal so we only have to switch on one.
            match self_tag {
                Tag::U32 => {
                    let a = unsafe { <u32 as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <u32 as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::I32 => {
                    let a = unsafe { <i32 as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <i32 as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Static => self.val.get_body().cmp(&other.val.get_body()),
                Tag::Object => {
                    let a = unsafe { ctx.unchecked_host_obj_from_val(self.val) };
                    let b = unsafe { ctx.unchecked_host_obj_from_val(other.val) };
                    a.cmp(b)
                }
                Tag::Symbol => {
                    let a = unsafe { <Symbol as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <Symbol as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::BitSet => {
                    let a = unsafe { <BitSet as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <BitSet as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Status => {
                    let a = unsafe { <Status as ValType>::unchecked_from_val(self.val) };
                    let b = unsafe { <Status as ValType>::unchecked_from_val(other.val) };
                    a.cmp(&b)
                }
                Tag::Reserved => self.val.get_payload().cmp(&other.val.get_payload()),
            }
        }
    }
}

#[derive(Default)]
pub struct HostContext {
    objects: Vec<HostObject>,
}

impl HostContext {
    unsafe fn unchecked_host_obj_from_val(&self, val: Val) -> &HostObject {
        &self.objects[<Object as ValType>::unchecked_from_val(val).get_handle() as usize]
    }

    pub fn associate(self: Rc<Self>, val: Val) -> ValInContext {
        let ctx = Rc::downgrade(&self);
        ValInContext { ctx, val }
    }
}

impl Host for HostContext {
    fn as_mut_any(&mut self) -> &mut dyn core::any::Any {
        todo!()
    }

    fn log_value(&mut self, v: Val) -> Val {
        todo!()
    }

    fn get_last_operation_result(&mut self) -> Object {
        todo!()
    }

    fn obj_from_u64(&mut self, u: u64) -> Object {
        todo!()
    }

    fn obj_to_u64(&mut self, u: Object) -> u64 {
        todo!()
    }

    fn obj_from_i64(&mut self, i: i64) -> Object {
        todo!()
    }

    fn obj_to_i64(&mut self, i: Object) -> i64 {
        todo!()
    }

    fn map_new(&mut self) -> Object {
        todo!()
    }

    fn map_put(&mut self, m: Object, k: Val, v: Val) -> Object {
        todo!()
    }

    fn map_get(&mut self, m: Object, k: Val) -> Val {
        todo!()
    }

    fn map_del(&mut self, m: Object, k: Val) -> Object {
        todo!()
    }

    fn map_len(&mut self, m: Object) -> Val {
        todo!()
    }

    fn map_keys(&mut self, m: Object) -> Object {
        todo!()
    }

    fn map_has(&mut self, m: Object, k: Val) -> Val {
        todo!()
    }

    fn vec_new(&mut self) -> Object {
        todo!()
    }

    fn vec_put(&mut self, v: Object, i: Val, x: Val) -> Object {
        todo!()
    }

    fn vec_get(&mut self, v: Object, i: Val) -> Val {
        todo!()
    }

    fn vec_del(&mut self, v: Object, i: Val) -> Object {
        todo!()
    }

    fn vec_len(&mut self, v: Object) -> Val {
        todo!()
    }

    fn vec_push(&mut self, v: Object, x: Val) -> Object {
        todo!()
    }

    fn vec_pop(&mut self, v: Object) -> Object {
        todo!()
    }

    fn vec_take(&mut self, v: Object, n: Val) -> Object {
        todo!()
    }

    fn vec_drop(&mut self, v: Object, n: Val) -> Object {
        todo!()
    }

    fn vec_front(&mut self, v: Object) -> Val {
        todo!()
    }

    fn vec_back(&mut self, v: Object) -> Val {
        todo!()
    }

    fn vec_insert(&mut self, v: Object, i: Val, n: Val) -> Object {
        todo!()
    }

    fn vec_append(&mut self, v1: Object, v2: Object) -> Object {
        todo!()
    }

    fn pay(&mut self, src: Object, dst: Object, asset: Object, amount: Val) -> Val {
        todo!()
    }

    fn account_balance(&mut self, acc: Object) -> Val {
        todo!()
    }

    fn account_trust_line(&mut self, acc: Object, asset: Object) -> Object {
        todo!()
    }

    fn trust_line_balance(&mut self, tl: Object) -> Val {
        todo!()
    }

    fn get_contract_data(&mut self, k: Val) -> Val {
        todo!()
    }

    fn put_contract_data(&mut self, k: Val, v: Val) -> Val {
        todo!()
    }

    fn has_contract_data(&mut self, k: Val) -> Val {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::HostContext;
    use crate::Host;

    #[test]
    fn i64_roundtrip() {
        let mut host = HostContext::default();
        let i = 12345_i64;
        let v = host.val_from(i);
        let j = host.val_into::<i64>(v);
        assert_eq!(i, j);
    }
}
