#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use im_rc::{OrdMap, Vector};
use std::num::TryFromIntError;

use crate::weak_host::WeakHost;

use crate::xdr;
use crate::xdr::{ScMap, ScMapEntry, ScObject, ScStatic, ScStatus, ScStatusType, ScVal, ScVec};
use std::rc::Rc;

use crate::host_object::{HostMap, HostObj, HostObject, HostObjectType, HostVal, HostVec};
use crate::CheckedEnv;
use crate::{
    BitSet, BitSetError, EnvBase, EnvObj, EnvValType, RawObj, RawVal, RawValType, Status, Symbol,
    SymbolError, Tag,
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

impl Host {
    unsafe fn unchecked_visit_val_obj<F, U>(&self, val: RawVal, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.0.objects.borrow();
        let index = <RawObj as RawValType>::unchecked_from_val(val).get_handle() as usize;
        f(r.get(index))
    }

    fn visit_obj<HOT: HostObjectType, F, U>(&self, obj: RawObj, f: F) -> Result<U, HostError>
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
}

impl Host {
    pub(crate) fn get_weak(&self) -> WeakHost {
        WeakHost(Rc::downgrade(&self.0))
    }

    pub(crate) fn associate_raw_val(&self, val: RawVal) -> HostVal {
        let env = self.get_weak();
        HostVal { env, val }
    }

    pub(crate) fn associate_env_val_type<V: EnvValType<WeakHost>>(&self, v: V) -> HostVal {
        let env = self.get_weak();
        v.into_env_val(&env)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
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
                        Err(HostError::General("unknown Tag::Static case"))
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
                        Err(HostError::General("unknown Tag::Status case"))
                    }
                }
                Tag::Reserved => Err(HostError::General("Tag::Reserved value")),
            }
        }
    }

    pub(crate) fn to_host_val(&mut self, v: &ScVal) -> Result<HostVal, HostError> {
        let ok = match v {
            ScVal::ScvU63(u) => {
                if *u <= (i64::MAX as u64) {
                    unsafe { RawVal::unchecked_from_positive_i64(*u as i64) }
                } else {
                    return Err(HostError::General("ScvU63 > i64::MAX"));
                }
            }
            ScVal::ScvU32(u) => (*u).into(),
            ScVal::ScvI32(i) => (*i).into(),
            ScVal::ScvStatic(ScStatic::ScsVoid) => RawVal::from_void(),
            ScVal::ScvStatic(ScStatic::ScsTrue) => RawVal::from_bool(true),
            ScVal::ScvStatic(ScStatic::ScsFalse) => RawVal::from_bool(false),
            ScVal::ScvObject(None) => return Err(HostError::General("missing expected ScvObject")),
            ScVal::ScvObject(Some(ob)) => return Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::ScvSymbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(HostError::General("non-UTF-8 in symbol")),
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

    pub(crate) fn from_host_obj(&self, ob: RawObj) -> Result<ScObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| match ob {
                None => Err(HostError::General("object not found")),
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

    pub(crate) fn to_host_obj(&mut self, ob: &ScObject) -> Result<HostObj, HostError> {
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
                    Err(_) => return Err(HostError::General("non-UTF-8 in ScoString")),
                };
                self.add_host_object(ss)
            }
            ScObject::ScoBinary(b) => self.add_host_object::<Vec<u8>>(b.clone().into()),

            ScObject::ScoBigint(_) => todo!(),
            ScObject::ScoBigrat(_) => todo!(),

            ScObject::ScoLedgerkey(None) => Err(HostError::General("missing ScoLedgerKey")),
            ScObject::ScoLedgerkey(Some(lk)) => self.add_host_object(lk.clone()),

            ScObject::ScoOperation(None) => Err(HostError::General("missing ScoOperation")),
            ScObject::ScoOperation(Some(op)) => self.add_host_object(op.clone()),

            ScObject::ScoOperationResult(None) => {
                Err(HostError::General("missing ScoOperationResult"))
            }
            ScObject::ScoOperationResult(Some(o)) => self.add_host_object(o.clone()),

            ScObject::ScoTransaction(None) => Err(HostError::General("missing ScoTransaction")),
            ScObject::ScoTransaction(Some(t)) => self.add_host_object(t.clone()),

            ScObject::ScoAsset(a) => self.add_host_object(a.clone()),
            ScObject::ScoPrice(p) => self.add_host_object(p.clone()),
            ScObject::ScoAccountid(a) => self.add_host_object(a.clone()),
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

    fn obj_from_u64(&self, u: u64) -> Result<RawObj, HostError> {
        Ok(self.add_host_object(u)?.into())
    }

    fn obj_to_u64(&self, v: RawVal) -> Result<u64, HostError> {
        todo!()
    }

    fn obj_from_i64(&self, i: i64) -> Result<RawObj, HostError> {
        Ok(self.add_host_object(i)?.into())
    }

    fn obj_to_i64(&self, v: RawVal) -> Result<i64, HostError> {
        todo!()
    }

    fn map_new(&self) -> Result<RawObj, HostError> {
        Ok(self.add_host_object(HostMap::new())?.into())
    }

    fn map_put(&self, m: RawObj, k: RawVal, v: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn map_get(&self, m: RawObj, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_del(&self, m: RawObj, k: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn map_len(&self, m: RawObj) -> Result<RawVal, HostError> {
        todo!()
    }

    fn map_keys(&self, m: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn map_has(&self, m: RawObj, k: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_new(&self) -> Result<RawObj, HostError> {
        Ok(self.add_host_object(HostVec::new())?.into())
    }

    fn vec_put(&self, v: RawObj, i: RawVal, x: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_get(&self, v: RawObj, i: RawVal) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_del(&self, v: RawObj, i: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_len(&self, v: RawObj) -> Result<RawVal, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        Ok(u32::try_from(len)?.into())
    }

    fn vec_push(&self, v: RawObj, x: RawVal) -> Result<RawObj, HostError> {
        let x = self.associate_raw_val(x);
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let mut vnew = hv.clone();
            vnew.push_back(x);
            Ok(vnew)
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop(&self, v: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_take(&self, v: RawObj, n: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_drop(&self, v: RawObj, n: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_front(&self, v: RawObj) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_back(&self, v: RawObj) -> Result<RawVal, HostError> {
        todo!()
    }

    fn vec_insert(&self, v: RawObj, i: RawVal, x: RawVal) -> Result<RawObj, HostError> {
        todo!()
    }

    fn vec_append(&self, v1: RawObj, v2: RawObj) -> Result<RawObj, HostError> {
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

    fn bigint_from_u64(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_add(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_sub(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_mul(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_div(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_rem(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_and(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_or(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_xor(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_shl(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_shr(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_cmp(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_is_zero(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_neg(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_not(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_gcd(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_lcm(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_pow(&self, x: RawObj, y: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_pow_mod(&self, p: RawObj, q: RawObj, m: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_sqrt(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_bits(&self, x: RawObj) -> Result<RawObj, HostError> {
        todo!()
    }

    fn bigint_to_u64(&self, x: RawObj) -> Result<u64, HostError> {
        todo!()
    }

    fn bigint_to_i64(&self, x: RawObj) -> Result<i64, HostError> {
        todo!()
    }

    fn bigint_from_i64(&self, x: i64) -> Result<RawObj, HostError> {
        todo!()
    }
}
