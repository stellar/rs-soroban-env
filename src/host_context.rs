#![allow(unused_variables)]

use core::cell::RefCell;
use im_rc::{OrdMap, Vector};

use std::rc::Rc;
use stellar_xdr::{ScMap, ScMapEntry, ScObject, ScStatic, ScStatus, ScStatusType, ScVal, ScVec};

use crate::{val::Tag, BitSet, Host, Object, Status, Symbol, Val, ValType};

mod debug;
mod host_object;
mod val_in_context;

pub use host_object::HostObject;
use host_object::HostObjectType;

use val_in_context::ValInContext;

#[derive(Default)]
pub(crate) struct HostContextImpl {
    objects: RefCell<Vec<HostObject>>,
}

impl HostContextImpl {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: Val, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.objects.borrow();
        let index = <Object as ValType>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }
}

#[derive(Default)]
pub struct HostContext(Rc<HostContextImpl>);

impl HostContext {
    pub fn associate(&mut self, val: Val) -> ValInContext {
        let ctx = Rc::downgrade(&self.0);
        ValInContext { ctx, val }
    }

    pub fn from_host_val(&self, val: Val) -> Result<ScVal, ()> {
        if val.is_u63() {
            Ok(ScVal::ScvU63(unsafe { val.unchecked_as_u63() } as u64))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::ScvU32(unsafe {
                    <u32 as ValType>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::ScvI32(unsafe {
                    <i32 as ValType>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    if let Some(b) = <bool as ValType>::try_convert(val) {
                        if b {
                            Ok(ScVal::ScvStatic(ScStatic::ScsTrue))
                        } else {
                            Ok(ScVal::ScvStatic(ScStatic::ScsFalse))
                        }
                    } else if <() as ValType>::is_val_type(val) {
                        Ok(ScVal::ScvStatic(ScStatic::ScsVoid))
                    } else {
                        Err(())
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as ValType>::unchecked_from_val(val);
                    let scob = self.from_host_obj(ob)?;
                    Ok(ScVal::ScvObject(Some(Box::new(scob))))
                },
                Tag::Symbol => {
                    let sym: Symbol = unsafe { <Symbol as ValType>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::ScvSymbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::ScvBitset(val.get_payload())),
                Tag::Status => {
                    let status: Status = unsafe { <Status as ValType>::unchecked_from_val(val) };
                    if status.is_ok() {
                        Ok(ScVal::ScvStatus(ScStatus::SstOk))
                    } else if status.is_type(ScStatusType::SstUnknownError) {
                        Ok(ScVal::ScvStatus(ScStatus::SstUnknownError(
                            status.get_code(),
                        )))
                    } else {
                        Err(())
                    }
                }
                Tag::Reserved => Err(()),
            }
        }
    }

    pub fn to_host_val(&mut self, v: &ScVal) -> Result<Val, ()> {
        match v {
            ScVal::ScvU63(u) => {
                if *u <= (i64::MAX as u64) {
                    Ok(unsafe { Val::unchecked_from_u63(*u as i64) })
                } else {
                    Err(())
                }
            }
            ScVal::ScvU32(u) => Ok((*u).into()),
            ScVal::ScvI32(i) => Ok((*i).into()),
            ScVal::ScvStatic(ScStatic::ScsVoid) => Ok(Val::from_void()),
            ScVal::ScvStatic(ScStatic::ScsTrue) => Ok(Val::from_bool(true)),
            ScVal::ScvStatic(ScStatic::ScsFalse) => Ok(Val::from_bool(false)),
            ScVal::ScvObject(None) => Err(()),
            ScVal::ScvObject(Some(ob)) => Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::ScvSymbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(()),
                };
                Ok(Symbol::try_from_str(ss)?.into())
            }
            ScVal::ScvBitset(i) => Ok(BitSet::try_from_u64(*i)?.into()),
            ScVal::ScvStatus(st) => {
                let status = match st {
                    ScStatus::SstOk => Status::from_type_and_code(ScStatusType::SstOk, 0),
                    ScStatus::SstUnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::SstUnknownError, *e)
                    }
                };
                Ok(status.into())
            }
        }
    }

    pub fn from_host_obj(&self, ob: Object) -> Result<ScObject, ()> {
        unsafe {
            self.0.unchecked_visit_val_obj(ob.into(), |ob| match ob {
                None => Err(()),
                Some(ho) => match ho {
                    HostObject::Box(v) => Ok(ScObject::ScoBox(self.from_host_val(v.val)?)),
                    HostObject::Vec(vv) => {
                        let mut sv = Vec::new();
                        for e in vv.iter() {
                            sv.push(self.from_host_val(e.val)?);
                        }
                        Ok(ScObject::ScoVec(ScVec(sv.try_into()?)))
                    }
                    HostObject::Map(mm) => {
                        let mut mv = Vec::new();
                        for (k, v) in mm.iter() {
                            let key = self.from_host_val(k.val)?;
                            let val = self.from_host_val(v.val)?;
                            mv.push(ScMapEntry { key, val });
                        }
                        Ok(ScObject::ScoMap(ScMap(mv.try_into()?)))
                    }
                    HostObject::U64(u) => Ok(ScObject::ScoU64(*u)),
                    HostObject::I64(i) => Ok(ScObject::ScoI64(*i)),
                    HostObject::Str(s) => Ok(ScObject::ScoString(s.as_bytes().try_into()?)),
                    HostObject::Bin(b) => Ok(ScObject::ScoBinary(b.clone().try_into()?)),
                    HostObject::BigInt(_) => todo!(),
                    HostObject::BigRat(_) => todo!(),
                    HostObject::LedgerKey(_) => todo!(),
                    HostObject::Operation(_) => todo!(),
                    HostObject::OperationResult(_) => todo!(),
                    HostObject::Transaction(_) => todo!(),
                    HostObject::Asset(_) => todo!(),
                    HostObject::Price(_) => todo!(),
                    HostObject::AccountID(_) => todo!(),
                },
            })
        }
    }

    pub fn to_host_obj(&mut self, ob: &ScObject) -> Result<Object, ()> {
        match ob {
            ScObject::ScoBox(b) => {
                let hv = self.to_host_val(b)?;
                let vic = self.associate(hv);
                self.add_host_object(vic)
            }
            ScObject::ScoVec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    let v = self.to_host_val(e)?;
                    vv.push_back(self.associate(v))
                }
                self.add_host_object(vv)
            }
            ScObject::ScoMap(m) => {
                let mut mm = OrdMap::new();
                for pair in m.0.iter() {
                    let kv = self.to_host_val(&pair.key)?;
                    let vv = self.to_host_val(&pair.val)?;
                    let k = self.associate(kv);
                    let v = self.associate(vv);
                    mm.insert(k, v);
                }
                self.add_host_object(mm)
            }
            ScObject::ScoU64(u) => self.add_host_object(*u),
            ScObject::ScoI64(i) => self.add_host_object(*i),
            ScObject::ScoString(s) => {
                let ss = match String::from_utf8(s.clone().into()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(()),
                };
                self.add_host_object(ss)
            }
            ScObject::ScoBinary(b) => self.add_host_object::<Vec<u8>>(b.clone().into()),

            ScObject::ScoBigint(_) => todo!(),
            ScObject::ScoBigrat(_) => todo!(),

            ScObject::ScoLedgerkey(None) => Err(()),
            ScObject::ScoLedgerkey(Some(lk)) => self.add_host_object(lk.clone()),

            ScObject::ScoOperation(None) => Err(()),
            ScObject::ScoOperation(Some(op)) => self.add_host_object(op.clone()),

            ScObject::ScoOperationResult(None) => Err(()),
            ScObject::ScoOperationResult(Some(o)) => self.add_host_object(o.clone()),

            ScObject::ScoTransaction(None) => Err(()),
            ScObject::ScoTransaction(Some(t)) => self.add_host_object(t.clone()),

            ScObject::ScoAsset(a) => self.add_host_object(a.clone()),
            ScObject::ScoPrice(p) => self.add_host_object(p.clone()),
            ScObject::ScoAccountid(a) => self.add_host_object(a.clone()),
        }
    }

    pub fn add_host_object<HOT: HostObjectType>(&mut self, hot: HOT) -> Result<Object, ()> {
        let handle = self.0.objects.borrow().len();
        if handle > u32::MAX as usize {
            return Err(());
        }
        self.0.objects.borrow_mut().push(HOT::inject(hot));
        Ok(Object::from_type_and_handle(HOT::get_type(), handle as u32))
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

    fn obj_to_u64(&self, u: Object) -> u64 {
        todo!()
    }

    fn obj_from_i64(&mut self, i: i64) -> Object {
        todo!()
    }

    fn obj_to_i64(&self, i: Object) -> i64 {
        todo!()
    }

    fn map_new(&mut self) -> Object {
        self.add_host_object(OrdMap::new()).unwrap()
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
    use stellar_xdr::{ScObject, ScObjectType, ScVal, ScVec};

    use super::HostContext;
    use crate::{or_abort::OrAbort, Host, HostConvertable, Object};

    #[test]
    fn i64_roundtrip() {
        let mut host = HostContext::default();
        let i = 12345_i64;
        let v = i.into_val(&mut host);
        let j = i64::try_from_val(v, &mut host).or_abort();
        assert_eq!(i, j);
    }

    #[test]
    fn vec_host_objs() -> Result<(), ()> {
        let mut host = HostContext::default();
        let scvec0: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
        let scvec1: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
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
        Ok(())
    }

    #[test]
    fn vec_host_fn() {
        let mut host = HostContext::default();
        let m = host.map_new();
        assert!(m.is_type(ScObjectType::ScoMap));
    }
}
