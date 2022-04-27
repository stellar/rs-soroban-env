#![allow(unused_variables)]

use core::cell::RefCell;
use im_rc::{OrdMap, Vector};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::cmp::Ordering;
use std::rc::{Rc, Weak};
use stellar_xdr::{ScObject, ScObjectType, ScStatic, ScVal};

use crate::val::Tag;
use crate::{require, BitSet, Host, Object, Status, Symbol, Val, ValType};

mod debug;

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
    // TODO: waiting for Ord, PartialOrd on these
    //LedgerKey(LedgerKey),
    //Operation(Operation),
    //OperationResult(OperationResult),
    //Transaction(Transaction),
    //Asset(Asset),
    //Price(Price),
    //AccountID(AccountID)
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

#[derive(Default)]
pub struct HostContext {
    objects: RefCell<Vec<HostObject>>,
}

impl HostContext {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: Val, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.objects.borrow();
        let index = <Object as ValType>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }

    pub fn associate(self: &Rc<Self>, val: Val) -> ValInContext {
        let ctx = Rc::downgrade(&self);
        ValInContext { ctx, val }
    }

    pub fn to_host_val(self: &Rc<Self>, v: &ScVal) -> Result<Val, ()> {
        match v {
            ScVal::ScvU63(u) => {
                if u.0 <= (i64::MAX as u64) {
                    Ok(unsafe { Val::unchecked_from_u63(u.0 as i64) })
                } else {
                    Err(())
                }
            }
            ScVal::ScvU32(u) => Ok(u.0.into()),
            ScVal::ScvI32(i) => Ok(i.0.into()),
            ScVal::ScvStatic(ScStatic::ScsVoid) => Ok(Val::from_void()),
            ScVal::ScvStatic(ScStatic::ScsTrue) => Ok(Val::from_bool(true)),
            ScVal::ScvStatic(ScStatic::ScsFalse) => Ok(Val::from_bool(false)),
            ScVal::ScvObject(None) => Err(()),
            ScVal::ScvObject(Some(ob)) => Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::ScvSymbol(_) => todo!(),
            ScVal::ScvBitset(_) => todo!(),
            ScVal::ScvStatus(_) => todo!(),
        }
    }

    pub fn to_host_obj(self: &Rc<Self>, ob: &ScObject) -> Result<Object, ()> {
        let (ty, hobj) = match ob {
            ScObject::ScoBox(b) => (
                ScObjectType::ScoBox,
                HostObject::Box(self.associate(self.to_host_val(b)?)),
            ),
            ScObject::ScoVec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    vv.push_back(self.associate(self.to_host_val(e)?))
                }
                (ScObjectType::ScoVec, HostObject::Vec(vv))
            }
            ScObject::ScoMap(m) => {
                let mut mm = OrdMap::new();
                for pair in m.0.iter() {
                    let k = self.associate(self.to_host_val(&pair.key)?);
                    let v = self.associate(self.to_host_val(&pair.val)?);
                    mm.insert(k, v);
                }
                (ScObjectType::ScoMap, HostObject::Map(mm))
            }
            ScObject::ScoU64(u) => (ScObjectType::ScoU64, HostObject::U64(u.0)),
            ScObject::ScoI64(i) => (ScObjectType::ScoU64, HostObject::I64(i.0)),
            ScObject::ScoString(s) => {
                let ss = match String::from_utf8(s.clone()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(()),
                };
                (ScObjectType::ScoU64, HostObject::Str(ss))
            }
            ScObject::ScoBinary(b) => (ScObjectType::ScoBinary, HostObject::Blob(b.clone())),

            ScObject::ScoBigint(_) => todo!(),
            ScObject::ScoBigrat(_) => todo!(),

            ScObject::ScoLedgerkey(None) => return Err(()),
            ScObject::ScoLedgerkey(Some(lk)) => todo!(),

            ScObject::ScoOperation(None) => return Err(()),
            ScObject::ScoOperation(Some(op)) => todo!(),

            ScObject::ScoOperationResult(_) => todo!(),
            ScObject::ScoTransaction(_) => todo!(),
            ScObject::ScoAsset(_) => todo!(),
            ScObject::ScoPrice(_) => todo!(),
            ScObject::ScoAccountid(_) => todo!(),
        };

        let handle = self.objects.borrow().len();
        if handle > u32::MAX as usize {
            return Err(());
        }
        self.objects.borrow_mut().push(hobj);
        Ok(Object::from_type_and_handle(ty, handle as u32))
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
    use std::rc::Rc;
    use stellar_xdr::{ScObject, ScObjectType, ScVal, ScVec, Uint32};

    use super::HostContext;
    use crate::{or_abort::OrAbort, Host, Object};

    #[test]
    fn i64_roundtrip() {
        let mut host = HostContext::default();
        let i = 12345_i64;
        let v = host.val_from(i);
        let j = host.val_into::<i64>(v);
        assert_eq!(i, j);
    }

    #[test]
    fn vec_host_objs() {
        let host = Rc::new(HostContext::default());
        let scvec0: ScVec = ScVec(vec![ScVal::ScvU32(Uint32(1))]);
        let scvec1: ScVec = ScVec(vec![ScVal::ScvU32(Uint32(1))]);
        let scobj0: ScObject = ScObject::ScoVec(scvec0);
        let scobj1: ScObject = ScObject::ScoVec(scvec1);
        let scval0 = ScVal::ScvObject(Some(Box::new(scobj0)));
        let scval1 = ScVal::ScvObject(Some(Box::new(scobj1)));
        let val0 = host.to_host_val(&scval0).or_abort();
        let val1 = host.to_host_val(&scval1).or_abort();
        assert!(val0.is::<Object>());
        assert!(val1.is::<Object>());
        let obj0: Object = val0.try_into().or_abort();
        let obj1: Object = val1.try_into().or_abort();
        assert_eq!(obj0.get_handle(), 0);
        assert_eq!(obj1.get_handle(), 1);
        assert!(obj0.is_type(ScObjectType::ScoVec));
        assert!(obj1.is_type(ScObjectType::ScoVec));
        // Check that we got 2 distinct Vec objects
        assert_ne!(val0.get_payload(), val1.get_payload());
        // But also that they compare deep-equal.
        assert_eq!(host.associate(val0), host.associate(val1));
    }
}
