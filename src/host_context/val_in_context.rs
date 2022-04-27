use super::HostContextImpl;
use crate::{require, val::Tag, Val};
use crate::{BitSet, Status, Symbol, ValType};
use std::cmp::Ordering;
use std::rc::{Rc, Weak};

#[derive(Clone)]
pub struct ValInContext {
    pub(crate) ctx: Weak<HostContextImpl>,
    pub(crate) val: Val,
}

impl ValInContext {
    pub(crate) fn get_context(&self) -> Rc<HostContextImpl> {
        self.ctx
            .upgrade()
            .expect("ValInContext.get_context() on expired context")
    }
    pub(crate) fn check_same_context(&self, other: &Self) -> Rc<HostContextImpl> {
        let self_ctx = self.get_context();
        let other_ctx = other.get_context();
        require(Rc::ptr_eq(&self_ctx, &other_ctx));
        self_ctx
    }
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
            unsafe {
                ctx.unchecked_visit_val_obj(self.val, |a| {
                    ctx.unchecked_visit_val_obj(other.val, |b| a.eq(&b))
                })
            }
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
                Tag::Object => unsafe {
                    ctx.unchecked_visit_val_obj(self.val, |a| {
                        ctx.unchecked_visit_val_obj(other.val, |b| a.cmp(&b))
                    })
                },
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
