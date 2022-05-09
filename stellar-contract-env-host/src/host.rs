#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::{Debug, Display};
use im_rc::{OrdMap, Vector};

use crate::weak_host::WeakHost;

use super::xdr::{ScMap, ScMapEntry, ScObject, ScStatic, ScStatus, ScStatusType, ScVal, ScVec};
use std::rc::Rc;

use crate::host_object::{HostMap, HostObj, HostObject, HostObjectType, HostVal, HostVec};
use crate::{
    BitSet, Env, EnvBase, EnvObj, EnvValType, RawObj, RawVal, RawValType, Status, Symbol, Tag,
};

#[derive(Debug)]
pub enum Error {
    General,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "host::Error")
    }
}

#[derive(Default, Clone)]
pub(crate) struct HostImpl {
    objects: RefCell<Vec<HostObject>>,
}

// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Default, Clone)]
pub struct Host(pub(crate) Rc<HostImpl>);

impl Debug for Host {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Host({:x})", Rc::<HostImpl>::as_ptr(&self.0) as usize)
    }
}

impl Host {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: RawVal, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.0.objects.borrow();
        let index = <RawObj as RawValType>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }
}

impl Host {
    pub(crate) fn get_weak(&self) -> WeakHost {
        WeakHost(Rc::downgrade(&self.0))
    }

    pub(crate) fn associate_raw_val(&self, val: RawVal) -> HostVal {
        let env = self.get_weak();
        HostVal { env, val }
    }

    pub(crate) fn associate_env_val_type<V: EnvValType>(&self, v: V) -> HostVal {
        let env = self.get_weak();
        v.into_env_val(&env)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, ()> {
        if val.is_positive_i64() {
            Ok(ScVal::ScvU63(
                unsafe { val.unchecked_as_positive_i64() } as u64
            ))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::ScvU32(unsafe {
                    <u32 as RawValType>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::ScvI32(unsafe {
                    <i32 as RawValType>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    if let Some(b) = <bool as RawValType>::try_convert(val) {
                        if b {
                            Ok(ScVal::ScvStatic(ScStatic::ScsTrue))
                        } else {
                            Ok(ScVal::ScvStatic(ScStatic::ScsFalse))
                        }
                    } else if <() as RawValType>::is_val_type(val) {
                        Ok(ScVal::ScvStatic(ScStatic::ScsVoid))
                    } else {
                        Err(())
                    }
                }
                Tag::Object => unsafe {
                    let ob = <RawObj as RawValType>::unchecked_from_val(val);
                    let scob = self.from_host_obj(ob)?;
                    Ok(ScVal::ScvObject(Some(Box::new(scob))))
                },
                Tag::Symbol => {
                    let sym: Symbol = unsafe { <Symbol as RawValType>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::ScvSymbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::ScvBitset(val.get_payload())),
                Tag::Status => {
                    let status: Status = unsafe { <Status as RawValType>::unchecked_from_val(val) };
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

    pub(crate) fn to_host_val(&mut self, v: &ScVal) -> Result<HostVal, ()> {
        let ok = match v {
            ScVal::ScvU63(u) => {
                if *u <= (i64::MAX as u64) {
                    unsafe { RawVal::unchecked_from_positive_i64(*u as i64) }
                } else {
                    return Err(());
                }
            }
            ScVal::ScvU32(u) => (*u).into(),
            ScVal::ScvI32(i) => (*i).into(),
            ScVal::ScvStatic(ScStatic::ScsVoid) => RawVal::from_void(),
            ScVal::ScvStatic(ScStatic::ScsTrue) => RawVal::from_bool(true),
            ScVal::ScvStatic(ScStatic::ScsFalse) => RawVal::from_bool(false),
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
        Ok(self.associate_raw_val(ok))
    }

    pub(crate) fn from_host_obj(&self, ob: RawObj) -> Result<ScObject, ()> {
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

    pub(crate) fn to_host_obj(&mut self, ob: &ScObject) -> Result<HostObj, ()> {
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

    pub(crate) fn add_host_object<HOT: HostObjectType>(&self, hot: HOT) -> Result<HostObj, ()> {
        let handle = self.0.objects.borrow().len();
        if handle > u32::MAX as usize {
            return Err(());
        }
        self.0.objects.borrow_mut().push(HOT::inject(hot));
        let env = WeakHost(Rc::downgrade(&self.0));
        Ok(EnvObj::<WeakHost>::from_type_and_handle(
            HOT::get_type(),
            handle as u32,
            env,
        ))
    }
}

impl EnvBase for Host {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }
}

impl Env for Host {
    fn log_value(&self, v: RawVal) -> RawVal {
        todo!()
    }

    fn get_last_operation_result(&self) -> RawVal {
        todo!()
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> i64 {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| self.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }
    }

    fn obj_from_u64(&self, u: u64) -> RawVal {
        self.add_host_object(u).expect("obj_from_u64").into()
    }

    fn obj_to_u64(&self, u: RawVal) -> u64 {
        todo!()
    }

    fn obj_from_i64(&self, i: i64) -> RawVal {
        self.add_host_object(i).expect("obj_from_i64").into()
    }

    fn obj_to_i64(&self, i: RawVal) -> i64 {
        todo!()
    }

    fn map_new(&self) -> RawVal {
        self.add_host_object(HostMap::new())
            .expect("map_new")
            .into()
    }

    fn map_put(&self, m: RawVal, k: RawVal, v: RawVal) -> RawVal {
        todo!()
    }

    fn map_get(&self, m: RawVal, k: RawVal) -> RawVal {
        todo!()
    }

    fn map_del(&self, m: RawVal, k: RawVal) -> RawVal {
        todo!()
    }

    fn map_len(&self, m: RawVal) -> RawVal {
        todo!()
    }

    fn map_keys(&self, m: RawVal) -> RawVal {
        todo!()
    }

    fn map_has(&self, m: RawVal, k: RawVal) -> RawVal {
        todo!()
    }

    fn vec_new(&self) -> RawVal {
        self.add_host_object(HostVec::new())
            .expect("vec_new")
            .into()
    }

    fn vec_put(&self, v: RawVal, i: RawVal, x: RawVal) -> RawVal {
        todo!()
    }

    fn vec_get(&self, v: RawVal, i: RawVal) -> RawVal {
        todo!()
    }

    fn vec_del(&self, v: RawVal, i: RawVal) -> RawVal {
        todo!()
    }

    fn vec_len(&self, v: RawVal) -> RawVal {
        let len = unsafe {
            self.unchecked_visit_val_obj(v, move |ho| {
                if let Some(HostObject::Vec(vec)) = ho {
                    vec.len()
                } else {
                    panic!("bad or nonexistent host object ref")
                }
            })
        };
        u32::try_from(len)
            .expect("vec length exceeds u32 max")
            .into()
    }

    fn vec_push(&self, v: RawVal, x: RawVal) -> RawVal {
        let x = self.associate_raw_val(x);
        let vnew = unsafe {
            self.unchecked_visit_val_obj(v, move |ho| {
                if let Some(HostObject::Vec(vec)) = ho {
                    let mut vnew = vec.clone();
                    vnew.push_back(x);
                    vnew
                } else {
                    panic!("bad or nonexistent host object ref")
                }
            })
        };
        self.add_host_object(vnew).expect("vec_push").into()
    }

    fn vec_pop(&self, v: RawVal) -> RawVal {
        todo!()
    }

    fn vec_take(&self, v: RawVal, n: RawVal) -> RawVal {
        todo!()
    }

    fn vec_drop(&self, v: RawVal, n: RawVal) -> RawVal {
        todo!()
    }

    fn vec_front(&self, v: RawVal) -> RawVal {
        todo!()
    }

    fn vec_back(&self, v: RawVal) -> RawVal {
        todo!()
    }

    fn vec_insert(&self, v: RawVal, i: RawVal, n: RawVal) -> RawVal {
        todo!()
    }

    fn vec_append(&self, v1: RawVal, v2: RawVal) -> RawVal {
        todo!()
    }

    fn get_current_ledger_num(&self) -> RawVal {
        todo!()
    }

    fn get_current_ledger_close_time(&self) -> RawVal {
        todo!()
    }

    fn pay(&self, src: RawVal, dst: RawVal, asset: RawVal, amount: RawVal) -> RawVal {
        todo!()
    }

    fn put_contract_data(&self, k: RawVal, v: RawVal) -> RawVal {
        todo!()
    }

    fn has_contract_data(&self, k: RawVal) -> RawVal {
        todo!()
    }

    fn get_contract_data(&self, k: RawVal) -> RawVal {
        todo!()
    }

    fn del_contract_data(&self, k: RawVal) -> RawVal {
        todo!()
    }

    fn account_balance(&self, acc: RawVal) -> RawVal {
        todo!()
    }

    fn account_trust_line(&self, acc: RawVal, asset: RawVal) -> RawVal {
        todo!()
    }

    fn trust_line_balance(&self, tl: RawVal) -> RawVal {
        todo!()
    }

    fn call0(&self, contract: RawVal, func: RawVal) -> RawVal {
        todo!()
    }

    fn call1(&self, contract: RawVal, func: RawVal, a: RawVal) -> RawVal {
        todo!()
    }

    fn call2(&self, contract: RawVal, func: RawVal, a: RawVal, b: RawVal) -> RawVal {
        todo!()
    }

    fn call3(&self, contract: RawVal, func: RawVal, a: RawVal, b: RawVal, c: RawVal) -> RawVal {
        todo!()
    }

    fn call4(
        &self,
        contract: RawVal,
        func: RawVal,
        a: RawVal,
        b: RawVal,
        c: RawVal,
        d: RawVal,
    ) -> RawVal {
        todo!()
    }

    fn bigint_from_u64(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_add(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_sub(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_mul(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_div(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_rem(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_and(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_or(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_xor(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_shl(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_shr(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_cmp(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_is_zero(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_neg(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_not(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_gcd(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_lcm(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_pow(&self, x: RawVal, y: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_pow_mod(&self, p: RawVal, q: RawVal, m: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_sqrt(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_bits(&self, x: RawVal) -> RawVal {
        todo!()
    }

    fn bigint_to_u64(&self, x: RawVal) -> u64 {
        todo!()
    }

    fn bigint_to_i64(&self, x: RawVal) -> i64 {
        todo!()
    }

    fn bigint_from_i64(&self, x: i64) -> RawVal {
        todo!()
    }
}
