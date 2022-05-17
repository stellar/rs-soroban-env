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

use crate::host_object::{HostObj, HostObject, HostObjectType, HostVal};
use crate::CheckedEnv;
use crate::{
    BitSet, BitSetError, EnvBase, EnvValConvertible, IntoEnvVal, Object, RawVal, RawValConvertible,
    Status, Symbol, SymbolError, Tag, Val,
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
    unsafe fn unchecked_visit_val_obj<F, U>(&self, obj: Object, f: F) -> U
    where
        F: FnOnce(Option<&HostObject>) -> U,
    {
        let r = self.0.objects.borrow();
        let index = obj.get_handle() as usize;
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
}

impl Host {
    pub(crate) fn get_weak(&self) -> WeakHost {
        WeakHost(Rc::downgrade(&self.0))
    }

    pub(crate) fn associate_raw_val(&self, val: RawVal) -> HostVal {
        let env = self.get_weak();
        HostVal { env, val }
    }

    pub(crate) fn associate_env_val_type<V: Val, CVT: EnvValConvertible<WeakHost, RawVal>>(
        &self,
        v: CVT,
    ) -> HostVal {
        let env = self.get_weak();
        v.into_env_val(&env)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        if val.is_positive_i64() {
            Ok(ScVal::PosI64(
                unsafe { val.unchecked_as_positive_i64() }
            ))
        } else {
            match val.get_tag() {
                Tag::U32 => Ok(ScVal::U32(unsafe {
                    <u32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::I32 => Ok(ScVal::I32(unsafe {
                    <i32 as RawValConvertible>::unchecked_from_val(val)
                })),
                Tag::Static => {
                    if let Some(b) = <bool as RawValConvertible>::try_convert(val) {
                        if b {
                            Ok(ScVal::Static(ScStatic::True))
                        } else {
                            Ok(ScVal::Static(ScStatic::False))
                        }
                    } else if <() as RawValConvertible>::is_val_type(val) {
                        Ok(ScVal::Static(ScStatic::Void))
                    } else {
                        Err(HostError::General("unknown Tag::Static case"))
                    }
                }
                Tag::Object => unsafe {
                    let ob = <Object as RawValConvertible>::unchecked_from_val(val);
                    let scob = self.from_host_obj(ob)?;
                    Ok(ScVal::Object(Some(Box::new(scob))))
                },
                Tag::Symbol => {
                    let sym: Symbol =
                        unsafe { <Symbol as RawValConvertible>::unchecked_from_val(val) };
                    let str: String = sym.into_iter().collect();
                    Ok(ScVal::Symbol(str.as_bytes().try_into()?))
                }
                Tag::BitSet => Ok(ScVal::Bitset(val.get_payload())),
                Tag::Status => {
                    let status: Status =
                        unsafe { <Status as RawValConvertible>::unchecked_from_val(val) };
                    if status.is_ok() {
                        Ok(ScVal::Status(ScStatus::Ok))
                    } else if status.is_type(ScStatusType::UnknownError) {
                        Ok(ScVal::Status(ScStatus::UnknownError(
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
            ScVal::PosI64(u) => {
                if *u >= 0 {
                    unsafe { RawVal::unchecked_from_positive_i64(*u as i64) }
                } else {
                    return Err(HostError::General("PosI64 < 0"));
                }
            }
            ScVal::U32(u) => (*u).into(),
            ScVal::I32(i) => (*i).into(),
            ScVal::Static(ScStatic::Void) => RawVal::from_void(),
            ScVal::Static(ScStatic::True) => RawVal::from_bool(true),
            ScVal::Static(ScStatic::False) => RawVal::from_bool(false),
            ScVal::Object(None) => return Err(HostError::General("missing expected ScvObject")),
            ScVal::Object(Some(ob)) => return Ok(self.to_host_obj(&*ob)?.into()),
            ScVal::Symbol(bytes) => {
                let ss = match std::str::from_utf8(bytes.as_slice()) {
                    Ok(ss) => ss,
                    Err(_) => return Err(HostError::General("non-UTF-8 in symbol")),
                };
                Symbol::try_from_str(ss)?.into()
            }
            ScVal::Bitset(i) => BitSet::try_from_u64(*i)?.into(),
            ScVal::Status(st) => {
                let status = match st {
                    ScStatus::Ok => Status::from_type_and_code(ScStatusType::Ok, 0),
                    ScStatus::UnknownError(e) => {
                        Status::from_type_and_code(ScStatusType::UnknownError, *e)
                    }
                };
                status.into()
            }
        };
        Ok(self.associate_raw_val(ok))
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
                    HostObject::Bin(b) => Ok(ScObject::Binary(b.clone().try_into()?)),
                },
            })
        }
    }

    pub(crate) fn to_host_obj(&mut self, ob: &ScObject) -> Result<HostObj, HostError> {
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
            ScObject::Binary(b) => self.add_host_object::<Vec<u8>>(b.clone().into()),
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
}

impl CheckedEnv for Host {
    type Error = HostError;

    fn obj_cmp(&self, a: Object, b: Object) -> Result<i64, HostError> {
        let res = unsafe {
            self.unchecked_visit_val_obj(a, |ao| self.unchecked_visit_val_obj(b, |bo| ao.cmp(&bo)))
        };
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
    }
}
