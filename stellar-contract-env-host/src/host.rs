#![allow(unused_variables)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use im_rc::{OrdMap, Vector};

use super::xdr::{ScMap, ScMapEntry, ScObject, ScStatic, ScStatus, ScStatusType, ScVal, ScVec};
use std::rc::{Rc, Weak};

use super::{
    BitSet, Env, EnvValType, HostMap, HostObject, HostObjectType, HostVal, Object, RawObj, Status,
    Symbol, Tag, Val, ValType,
};

#[derive(Default, Clone)]
struct HostImpl {
    objects: RefCell<Vec<HostObject>>,
}

// WeakHost is a newtype on Weak<HostImpl> so we can impl Env for it below.
#[derive(Clone)]
pub struct WeakHost(Weak<HostImpl>);

impl From<&Host> for WeakHost {
    fn from(h: &Host) -> Self {
        WeakHost(Rc::downgrade(&h.0))
    }
}

impl Debug for WeakHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WeakHost({:x})", self.0.as_ptr() as usize)
    }
}

impl WeakHost {
    fn get_host(&self) -> Host {
        Host(self.0.upgrade().expect("WeakHost upgrade"))
    }
}

// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Default, Clone)]
pub struct Host(Rc<HostImpl>);

impl From<&WeakHost> for Host {
    fn from(w: &WeakHost) -> Self {
        w.get_host()
    }
}

impl Debug for Host {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RcHost({:x})", Rc::<HostImpl>::as_ptr(&self.0) as usize)
    }
}

impl Host {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: Val, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.0.objects.borrow();
        let index = <RawObj as ValType>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }
}

impl Host {
    pub fn associate(&self, val: Val) -> HostVal {
        let env = WeakHost(Rc::downgrade(&self.0));
        HostVal { env, val }
    }

    pub fn env_val_from<V: EnvValType>(&self, v: V) -> HostVal {
        let env: WeakHost = self.into();
        v.into_env_val(env)
    }

    pub fn from_host_val(&self, val: Val) -> Result<ScVal, ()> {
        if val.is_positive_i64() {
            Ok(ScVal::ScvU63(
                unsafe { val.unchecked_as_positive_i64() } as u64
            ))
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
                    let ob = <RawObj as ValType>::unchecked_from_val(val);
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

    pub fn to_host_val(&mut self, v: &ScVal) -> Result<HostVal, ()> {
        let ok = match v {
            ScVal::ScvU63(u) => {
                if *u <= (i64::MAX as u64) {
                    unsafe { Val::unchecked_from_positive_i64(*u as i64) }
                } else {
                    return Err(());
                }
            }
            ScVal::ScvU32(u) => (*u).into(),
            ScVal::ScvI32(i) => (*i).into(),
            ScVal::ScvStatic(ScStatic::ScsVoid) => Val::from_void(),
            ScVal::ScvStatic(ScStatic::ScsTrue) => Val::from_bool(true),
            ScVal::ScvStatic(ScStatic::ScsFalse) => Val::from_bool(false),
            ScVal::ScvObject(None) => return Err(()),
            ScVal::ScvObject(Some(ob)) => return Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::ScvSymbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(()),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::ScvBitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::ScvStatus(st) => {
                let status = match st {
                    ScStatus::SstOk => Status::from_type_and_code(ScStatusType::SstOk, 0),
                    ScStatus::SstUnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::SstUnknownError, *e)
                    }
                };
                status.into()
            }
        };
        Ok(self.associate(ok))
    }

    pub fn from_host_obj(&self, ob: RawObj) -> Result<ScObject, ()> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| match ob {
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
                self.add_host_object(hv)
            }
            ScObject::ScoVec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    vv.push_back(self.to_host_val(e)?)
                }
                self.add_host_object(vv)
            }
            ScObject::ScoMap(m) => {
                let mut mm = OrdMap::new();
                for pair in m.0.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
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
        let env = WeakHost(Rc::downgrade(&self.0));
        Ok(Object::from_type_and_handle(
            HOT::get_type(),
            handle as u32,
            env,
        ))
    }
}

impl Env for Host {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }

    fn obj_cmp(&self, a: Val, b: Val) -> i64 {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| self.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    }

    fn log_value(&mut self, v: Val) -> Val {
        todo!()
    }

    fn get_last_operation_result(&mut self) -> Val {
        todo!()
    }

    fn obj_from_u64(&mut self, u: u64) -> Val {
        todo!()
    }

    fn obj_to_u64(&mut self, u: Val) -> u64 {
        todo!()
    }

    fn obj_from_i64(&mut self, i: i64) -> Val {
        todo!()
    }

    fn obj_to_i64(&mut self, i: Val) -> i64 {
        todo!()
    }

    fn map_new(&mut self) -> Val {
        self.add_host_object(HostMap::new())
            .expect("map_new")
            .into()
    }

    fn map_put(&mut self, m: Val, k: Val, v: Val) -> Val {
        todo!()
    }

    fn map_get(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn map_del(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn map_len(&mut self, m: Val) -> Val {
        todo!()
    }

    fn map_keys(&mut self, m: Val) -> Val {
        todo!()
    }

    fn map_has(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn vec_new(&mut self) -> Val {
        todo!()
    }

    fn vec_put(&mut self, v: Val, i: Val, x: Val) -> Val {
        todo!()
    }

    fn vec_get(&mut self, v: Val, i: Val) -> Val {
        todo!()
    }

    fn vec_del(&mut self, v: Val, i: Val) -> Val {
        todo!()
    }

    fn vec_len(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_push(&mut self, v: Val, x: Val) -> Val {
        todo!()
    }

    fn vec_pop(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_take(&mut self, v: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_drop(&mut self, v: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_front(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_back(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_insert(&mut self, v: Val, i: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_append(&mut self, v1: Val, v2: Val) -> Val {
        todo!()
    }

    fn pay(&mut self, src: Val, dst: Val, asset: Val, amount: Val) -> Val {
        todo!()
    }

    fn account_balance(&mut self, acc: Val) -> Val {
        todo!()
    }

    fn account_trust_line(&mut self, acc: Val, asset: Val) -> Val {
        todo!()
    }

    fn trust_line_balance(&mut self, tl: Val) -> Val {
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

impl Env for WeakHost {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        self.get_host().check_same_env(&other.get_host())
    }

    fn obj_cmp(&self, a: Val, b: Val) -> i64 {
        let h = self.get_host();
        let res = unsafe {
            h.unchecked_visit_val_obj(a, |ao| h.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    }

    fn log_value(&mut self, v: Val) -> Val {
        todo!()
    }

    fn get_last_operation_result(&mut self) -> Val {
        todo!()
    }

    fn obj_from_u64(&mut self, u: u64) -> Val {
        todo!()
    }

    fn obj_to_u64(&mut self, u: Val) -> u64 {
        todo!()
    }

    fn obj_from_i64(&mut self, i: i64) -> Val {
        todo!()
    }

    fn obj_to_i64(&mut self, i: Val) -> i64 {
        todo!()
    }

    fn map_new(&mut self) -> Val {
        self.get_host()
            .add_host_object(HostMap::new())
            .expect("map_new")
            .into()
    }

    fn map_put(&mut self, m: Val, k: Val, v: Val) -> Val {
        todo!()
    }

    fn map_get(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn map_del(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn map_len(&mut self, m: Val) -> Val {
        todo!()
    }

    fn map_keys(&mut self, m: Val) -> Val {
        todo!()
    }

    fn map_has(&mut self, m: Val, k: Val) -> Val {
        todo!()
    }

    fn vec_new(&mut self) -> Val {
        todo!()
    }

    fn vec_put(&mut self, v: Val, i: Val, x: Val) -> Val {
        todo!()
    }

    fn vec_get(&mut self, v: Val, i: Val) -> Val {
        todo!()
    }

    fn vec_del(&mut self, v: Val, i: Val) -> Val {
        todo!()
    }

    fn vec_len(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_push(&mut self, v: Val, x: Val) -> Val {
        todo!()
    }

    fn vec_pop(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_take(&mut self, v: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_drop(&mut self, v: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_front(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_back(&mut self, v: Val) -> Val {
        todo!()
    }

    fn vec_insert(&mut self, v: Val, i: Val, n: Val) -> Val {
        todo!()
    }

    fn vec_append(&mut self, v1: Val, v2: Val) -> Val {
        todo!()
    }

    fn pay(&mut self, src: Val, dst: Val, asset: Val, amount: Val) -> Val {
        todo!()
    }

    fn account_balance(&mut self, acc: Val) -> Val {
        todo!()
    }

    fn account_trust_line(&mut self, acc: Val, asset: Val) -> Val {
        todo!()
    }

    fn trust_line_balance(&mut self, tl: Val) -> Val {
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

    use crate::xdr::{ScObject, ScObjectType, ScVal, ScVec};

    use crate::{Env, EnvValType, Host, OrAbort, RawObj, WeakHost};

    #[test]
    fn i64_roundtrip() {
        let host = Host::default();
        let i = 12345_i64;
        let v = host.env_val_from(i);
        let j = <i64 as EnvValType>::try_from_env_val(v).unwrap();
        assert_eq!(i, j);
    }

    #[test]
    fn vec_host_objs() -> Result<(), ()> {
        let mut host = Host::default();
        let scvec0: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
        let scvec1: ScVec = ScVec(vec![ScVal::ScvU32(1)].try_into()?);
        let scobj0: ScObject = ScObject::ScoVec(scvec0);
        let scobj1: ScObject = ScObject::ScoVec(scvec1);
        let scval0 = ScVal::ScvObject(Some(Box::new(scobj0)));
        let scval1 = ScVal::ScvObject(Some(Box::new(scobj1)));
        let val0 = host.to_host_val(&scval0).or_abort();
        let val1 = host.to_host_val(&scval1).or_abort();
        assert!(val0.val.is::<RawObj>());
        assert!(val1.val.is::<RawObj>());
        let obj0: RawObj = val0.val.try_into().or_abort();
        let obj1: RawObj = val1.val.try_into().or_abort();
        assert_eq!(obj0.get_handle(), 0);
        assert_eq!(obj1.get_handle(), 1);
        assert!(obj0.is_type(ScObjectType::ScoVec));
        assert!(obj1.is_type(ScObjectType::ScoVec));
        // Check that we got 2 distinct Vec objects
        assert_ne!(val0.val.get_payload(), val1.val.get_payload());
        // But also that they compare deep-equal.
        assert_eq!(val0, val1);
        Ok(())
    }

    #[test]
    fn vec_host_fn() {
        let host = Host::default();
        let mut weak: WeakHost = (&host).into();
        let m = weak.map_new();
        assert!(RawObj::val_is_obj_type(m, ScObjectType::ScoMap));
    }
}
