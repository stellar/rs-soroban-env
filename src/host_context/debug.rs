use std::fmt::Debug;

use crate::{Object, Status, Symbol, Val, ValType};

use super::ValInContext;

impl Debug for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Symbol")
            .field(&self.into_iter().collect::<String>())
            .finish()
    }
}

impl Debug for Val {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_u63() {
            f.debug_struct("Val")
                .field("u63", &unsafe { self.unchecked_as_u63() })
                .finish()
        } else if self.is::<Object>() {
            f.debug_struct("Val")
                .field("obj", &self.get_major())
                .field("ty", &self.get_minor())
                .finish()
        } else if self.is::<Status>() {
            f.debug_struct("Val")
                .field("code", &self.get_major())
                .field("ty", &self.get_minor())
                .finish()
        } else if self.is::<Symbol>() {
            f.debug_struct("Val")
                .field("symbol", &unsafe {
                    <Symbol as ValType>::unchecked_from_val(*self)
                })
                .finish()
        } else if self.is::<()>() {
            f.debug_struct("Val").field("void", &()).finish()
        } else if self.is::<bool>() {
            f.debug_struct("Val")
                .field("bool", &unsafe {
                    <bool as ValType>::unchecked_from_val(*self)
                })
                .finish()
        } else if self.is::<u32>() {
            f.debug_struct("Val")
                .field("u32", &unsafe {
                    <u32 as ValType>::unchecked_from_val(*self)
                })
                .finish()
        } else if self.is::<i32>() {
            f.debug_struct("Val")
                .field("i32", &unsafe {
                    <i32 as ValType>::unchecked_from_val(*self)
                })
                .finish()
        } else {
            f.debug_struct("Val")
                .field("payload", &self.get_payload())
                .finish()
        }
    }
}

impl Debug for ValInContext {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ValInContext")
            .field("ctx", &self.ctx.as_ptr())
            .field("val", &self.val)
            .finish()
    }
}
