#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use im_rc::{OrdMap, Vector};
use std::num::TryFromIntError;
use stellar_contract_env_common::xdr::ScVal;
use stellar_contract_env_common::{ObjectXdrConverter, TryFromVal, TryIntoVal};

use crate::weak_host::WeakHost;

use crate::xdr;
use crate::xdr::{ScMap, ScMapEntry, ScObject, ScVec};
use std::rc::Rc;

use crate::host_object::{HostMap, HostObj, HostObject, HostObjectType, HostVal, HostVec};
use crate::CheckedEnv;
use crate::{
    BitSetError, EnvBase, IntoEnvVal, Object, RawVal, RawValConvertible, SymbolError, Val,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum HostError {
    #[error("general host error: {0}")]
    General(&'static str),
    #[error("XDR error")]
    XDRError(xdr::Error),
}

impl From<xdr::Error> for HostError {
    fn from(x: xdr::Error) -> Self {
        HostError::XDRError(x)
    }
}

impl From<TryFromIntError> for HostError {
    fn from(_: TryFromIntError) -> Self {
        HostError::General("number out of range of u32")
    }
}

impl From<SymbolError> for HostError {
    fn from(_: SymbolError) -> Self {
        HostError::General("symbol error")
    }
}

impl From<BitSetError> for HostError {
    fn from(_: BitSetError) -> Self {
        HostError::General("bitset error")
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

impl ObjectXdrConverter for Host {
    fn from_xdr_obj(&self, ob: Object) -> Result<ScObject, ()> {
        self.from_host_obj(ob).map_err(|_| ())
    }

    fn to_xdr_obj(&self, ob: &ScObject) -> Result<Object, ()> {
        self.to_host_obj(ob).map_err(|_| ()).map(|ob| ob.val)
    }
}

impl Host {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: RawVal, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.0.objects.borrow();
        let index = <Object as RawValConvertible>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }

    fn visit_obj<HOT: HostObjectType, F, U>(&self, obj: Object, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&HOT) -> Result<U, HostError>,
    {
        unsafe {
            self.unchecked_visit_val_obj(obj.into(), |hopt| match hopt {
                None => Err(HostError::General("unknown object reference")),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(HostError::General("unexpected host object type")),
                    Some(hot) => f(hot),
                },
            })
        }
    }

    fn reassociate_val(hv: &mut HostVal, weak: WeakHost) {
        hv.env = weak;
    }

    pub(crate) fn get_weak(&self) -> WeakHost {
        WeakHost(Rc::downgrade(&self.0))
    }

    pub(crate) fn associate_raw_val(&self, val: RawVal) -> HostVal {
        let env = self.get_weak();
        HostVal { env, val }
    }

    pub(crate) fn associate_env_val_type<V: Val, CVT: IntoEnvVal<WeakHost, RawVal>>(
        &self,
        v: CVT,
    ) -> HostVal {
        let env = self.get_weak();
        v.into_env_val(&env)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        ScVal::try_from_val(self, val)
            .map_err(|_| HostError::General("converting from RawVal to ScVal"))
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<HostVal, HostError> {
        let rv = v
            .try_into_val(self)
            .map_err(|_| HostError::General("converting from ScVal to RawVal"))?;
        Ok(self.associate_raw_val(rv))
    }

    pub(crate) fn from_host_obj(&self, ob: Object) -> Result<ScObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| match ob {
                None => Err(HostError::General("object not found")),
                Some(ho) => match ho {
                    HostObject::Box(v) => Ok(ScObject::Box(self.from_host_val(v.val)?)),
                    HostObject::Vec(vv) => {
                        let mut sv = Vec::new();
                        for e in vv.iter() {
                            sv.push(self.from_host_val(e.val)?);
                        }
                        Ok(ScObject::Vec(ScVec(sv.try_into()?)))
                    }
                    HostObject::Map(mm) => {
                        let mut mv = Vec::new();
                        for (k, v) in mm.iter() {
                            let key = self.from_host_val(k.val)?;
                            let val = self.from_host_val(v.val)?;
                            mv.push(ScMapEntry { key, val });
                        }
                        Ok(ScObject::Map(ScMap(mv.try_into()?)))
                    }
                    HostObject::U64(u) => Ok(ScObject::U64(*u)),
                    HostObject::I64(i) => Ok(ScObject::I64(*i)),
                    HostObject::Str(s) => Ok(ScObject::String(s.as_bytes().try_into()?)),
                    HostObject::Bin(b) => Ok(ScObject::Binary(b.clone().try_into()?)),
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

    pub(crate) fn to_host_obj(&self, ob: &ScObject) -> Result<HostObj, HostError> {
        match ob {
            ScObject::Box(b) => {
                let hv = self.to_host_val(b)?;
                self.add_host_object(hv)
            }
            ScObject::Vec(v) => {
                let mut vv = Vector::new();
                for e in v.0.iter() {
                    vv.push_back(self.to_host_val(e)?)
                }
                self.add_host_object(vv)
            }
            ScObject::Map(m) => {
                let mut mm = OrdMap::new();
                for pair in m.0.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.insert(k, v);
                }
                self.add_host_object(mm)
            }
            ScObject::U64(u) => self.add_host_object(*u),
            ScObject::I64(i) => self.add_host_object(*i),
            ScObject::String(s) => {
                let ss = match String::from_utf8(s.clone().into()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(HostError::General("non-UTF-8 in ScoString")),
                };
                self.add_host_object(ss)
            }
            ScObject::Binary(b) => self.add_host_object::<Vec<u8>>(b.clone().into()),

            ScObject::Bigint(_) => todo!(),
            ScObject::Bigrat(_) => todo!(),

            ScObject::Ledgerkey(None) => Err(HostError::General("missing ScoLedgerKey")),
            ScObject::Ledgerkey(Some(lk)) => self.add_host_object(lk.clone()),

            ScObject::Operation(None) => Err(HostError::General("missing ScoOperation")),
            ScObject::Operation(Some(op)) => self.add_host_object(op.clone()),

            ScObject::OperationResult(None) => {
                Err(HostError::General("missing ScoOperationResult"))
            }
            ScObject::OperationResult(Some(o)) => self.add_host_object(o.clone()),

            ScObject::Transaction(None) => Err(HostError::General("missing ScoTransaction")),
            ScObject::Transaction(Some(t)) => self.add_host_object(t.clone()),

            ScObject::Asset(a) => self.add_host_object(a.clone()),
            ScObject::Price(p) => self.add_host_object(p.clone()),
            ScObject::Accountid(a) => self.add_host_object(a.clone()),
        }
    }

    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<HostObj, HostError> {
        let handle = self.0.objects.borrow().len();
        if handle > u32::MAX as usize {
            return Err(HostError::General("object handle exceeds u32::MAX"));
        }
        self.0.objects.borrow_mut().push(HOT::inject(hot));
        let env = WeakHost(Rc::downgrade(&self.0));
        let v = Object::from_type_and_handle(HOT::get_type(), handle as u32);
        Ok(v.into_env_val(&env))
    }
}

impl EnvBase for Host {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }

    fn deep_clone(&self) -> Self {
        // Step 1: naive deep-clone the HostImpl. At this point some of the
        // objects in new_host may have WeakHost refs to the old host.
        let new_host = Host(Rc::new((*self.0).clone()));

        // Step 2: adjust all the objects that have internal WeakHost refs
        // to point to a weakhost associated with the new host. There are
        // only a few of these.
        let new_weak = new_host.get_weak();
        for hobj in new_host.0.objects.borrow_mut().iter_mut() {
            match hobj {
                HostObject::Box(v) => v.env = new_weak.clone(),
                HostObject::Vec(vs) => {
                    vs.iter_mut().for_each(|v| v.env = new_weak.clone());
                }
                HostObject::Map(m) => {
                    *m = HostMap::from_iter(m.clone().into_iter().map(|(mut k, mut v)| {
                        k.env = new_weak.clone();
                        v.env = new_weak.clone();
                        (k, v)
                    }))
                }
                _ => (),
            }
        }
        new_host
    }
}

impl CheckedEnv for Host {
    type Error = HostError;

    fn log_value(&self, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_last_operation_result(&self) -> Result<RawVal, HostError> {
        todo!()
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> Result<i64, HostError> {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| self.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
    }

    fn obj_from_u64(&self, u: u64) -> Result<Object, HostError> {
        Ok(self.add_host_object(u)?.into())
    }

    fn obj_to_u64(&self, obj: Object) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &u64| Ok(*u))
    }

    fn obj_from_i64(&self, i: i64) -> Result<Object, HostError> {
        Ok(self.add_host_object(i)?.into())
    }

    fn obj_to_i64(&self, obj: Object) -> Result<i64, HostError> {
        self.visit_obj(obj, |i: &i64| Ok(*i))
    }

    fn map_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(HostMap::new())?.into())
    }

    fn map_put(&self, m: Object, k: RawVal, v: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn map_get(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_del(&self, m: Object, k: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn map_len(&self, m: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_keys(&self, m: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn map_has(&self, m: Object, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_new(&self) -> Result<Object, HostError> {
        Ok(self.add_host_object(HostVec::new())?.into())
    }

    fn vec_put(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_get(&self, v: Object, i: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_del(&self, v: Object, i: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_len(&self, v: Object) -> Result<RawVal, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        Ok(u32::try_from(len)?.into())
    }

    fn vec_push(&self, v: Object, x: RawVal) -> Result<Object, HostError> {
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let mut vnew = hv.clone();
            vnew.push_back(x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop(&self, v: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_take(&self, v: Object, n: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_drop(&self, v: Object, n: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_front(&self, v: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_back(&self, v: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_insert(&self, v: Object, i: RawVal, x: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn vec_append(&self, v1: Object, v2: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn get_current_ledger_num(&self) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_current_ledger_close_time(&self) -> Result<RawVal, HostError> {
        todo!()
    }

    fn pay(
        &self,
        src: RawVal,
        dst: RawVal,
        asset: RawVal,
        amt: RawVal,
    ) -> Result<RawVal, HostError> {
        todo!()
    }

    fn put_contract_data(&self, k: RawVal, v: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn has_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn get_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn del_contract_data(&self, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn account_balance(&self, acct: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn account_trust_line(&self, acct: RawVal, asset: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn trust_line_balance(&self, tl: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn call0(&self, contract: RawVal, func: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn call1(&self, contract: RawVal, func: RawVal, a: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn call2(
        &self,
        contract: RawVal,
        func: RawVal,
        a: RawVal,
        b: RawVal,
    ) -> Result<RawVal, HostError> {
        todo!()
    }

    fn call3(
        &self,
        contract: RawVal,
        func: RawVal,
        a: RawVal,
        b: RawVal,
        c: RawVal,
    ) -> Result<RawVal, HostError> {
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
    ) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_from_u64(&self, x: u64) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_add(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_sub(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_mul(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_div(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_rem(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_and(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_or(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_xor(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_shl(&self, x: Object, y: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_shr(&self, x: Object, y: RawVal) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_cmp(&self, x: Object, y: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_is_zero(&self, x: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_neg(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_not(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_gcd(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_lcm(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_pow(&self, x: Object, y: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_pow_mod(&self, p: Object, q: Object, m: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_sqrt(&self, x: Object) -> Result<Object, HostError> {
        todo!()
    }

    fn bigint_bits(&self, x: Object) -> Result<RawVal, HostError> {
        todo!()
    }

    fn bigint_to_u64(&self, x: Object) -> Result<u64, HostError> {
        todo!()
    }

    fn bigint_to_i64(&self, x: Object) -> Result<i64, HostError> {
        todo!()
    }

    fn bigint_from_i64(&self, x: i64) -> Result<Object, HostError> {
        todo!()
    }
}
