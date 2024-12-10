use super::{TraceEvent, TraceState};
use crate::{host::Frame, xdr::ContractExecutable, Symbol, SymbolObject, SymbolSmall, Val};
use core::fmt::{Debug, Display};

impl Debug for TraceEvent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceEvent::Begin => write!(f, "TraceEvent::Begin"),
            TraceEvent::PushCtx(..) => write!(f, "TraceEvent::PushCtx(...)"),
            TraceEvent::PopCtx(..) => write!(f, "TraceEvent::PopCtx(...)"),
            TraceEvent::EnvCall(hostfn, args) => {
                write!(f, "TraceEvent::EnvCall({:?}, {:?})", hostfn, args)
            }
            TraceEvent::EnvRet(hostfn, Ok(ok)) => {
                write!(f, "TraceEvent::EnvRet({:?}, Ok({:?}))", hostfn, ok)
            }
            TraceEvent::EnvRet(hostfn, Err(err)) => {
                write!(f, "TraceEvent::EnvRet({:?}, Err({:?}))", hostfn, err)
            }
            TraceEvent::End => write!(f, "TraceEvent::End"),
        }
    }
}

enum ShortHashOrStaticStr {
    ShortHash(u32),
    StaticStr(&'static str),
}

impl From<&'static str> for ShortHashOrStaticStr {
    fn from(s: &'static str) -> Self {
        Self::StaticStr(s)
    }
}

impl From<&crate::xdr::Hash> for ShortHashOrStaticStr {
    fn from(h: &crate::xdr::Hash) -> Self {
        if let Ok(bytes4) = <[u8; 4]>::try_from(&h.0[0..4]) {
            Self::ShortHash(u32::from_be_bytes(bytes4))
        } else {
            Self::StaticStr("")
        }
    }
}

impl From<&ContractExecutable> for ShortHashOrStaticStr {
    fn from(exec: &ContractExecutable) -> Self {
        match exec {
            ContractExecutable::Wasm(hash) => hash.into(),
            ContractExecutable::StellarAsset => "SAC".into(),
        }
    }
}

impl Display for ShortHashOrStaticStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShortHashOrStaticStr::ShortHash(hash) => write!(f, "{:4.4x}", hash),
            ShortHashOrStaticStr::StaticStr(s) => write!(f, "{}", s),
        }
    }
}

struct FrameId {
    ty: &'static str,
    id: ShortHashOrStaticStr,
    sym: Option<Symbol>,
}

impl Display for FrameId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.ty, self.id)?;
        if let Some(s) = &self.sym {
            write!(f, ":")?;
            if let Ok(small) = SymbolSmall::try_from(*s) {
                for i in small.into_iter() {
                    write!(f, "{}", i)?;
                }
            } else if let Ok(obj) = SymbolObject::try_from(*s) {
                write!(f, "sym#{}", obj.get_handle())?
            } else {
                write!(f, "{:?}", s)?
            }
        }
        Ok(())
    }
}

impl TraceEvent<'_> {
    pub fn is_begin(&self) -> bool {
        match self {
            TraceEvent::Begin => true,
            _ => false,
        }
    }

    pub fn is_end(&self) -> bool {
        match self {
            TraceEvent::End => true,
            _ => false,
        }
    }

    fn frame_id_and_args(frame: &Frame) -> (FrameId, &[Val]) {
        match frame {
            Frame::ContractVM {
                fn_name,
                args,
                instance,
                ..
            } => (
                FrameId {
                    ty: "VM",
                    id: (&instance.executable).into(),
                    sym: Some(*fn_name),
                },
                &args,
            ),
            // TODO: this isn't ideal, we should capture the args in Frame::HostFunction
            Frame::HostFunction(ty) => (
                FrameId {
                    ty: ty.name(),
                    id: "".into(),
                    sym: None,
                },
                &[],
            ),
            Frame::StellarAssetContract(id, fn_name, args, _) => (
                FrameId {
                    ty: "SAC",
                    id: id.into(),
                    sym: Some(*fn_name),
                },
                &args,
            ),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (
                FrameId {
                    ty: "TEST",
                    id: (&tc.id).into(),
                    sym: Some(tc.func),
                },
                &tc.args,
            ),
        }
    }
}

impl Display for TraceEvent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraceEvent::PushCtx(ctx) => {
                let (frameid, args) = Self::frame_id_and_args(&ctx.frame);
                write!(f, "push {frameid}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                write!(f, ")")
            }
            TraceEvent::PopCtx(ctx, Ok(ok)) => {
                let (frameid, _) = Self::frame_id_and_args(&ctx.frame);
                write!(f, "pop {frameid} -> Ok({ok:?})")
            }
            TraceEvent::PopCtx(ctx, Err(err)) => {
                let (frameid, _) = Self::frame_id_and_args(&ctx.frame);
                write!(f, "pop {frameid} -> Err({:?})", err.error)
            }
            TraceEvent::EnvCall(hostfn, args) => {
                write!(f, "call {hostfn}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                write!(f, ")")
            }
            TraceEvent::EnvRet(hostfn, Ok(ok)) => {
                write!(f, "ret {hostfn} -> Ok({ok:?})")
            }
            TraceEvent::EnvRet(hostfn, Err(err)) => {
                write!(f, "ret {hostfn} -> Err({:?})", err.error)
            }
            TraceEvent::Begin => write!(f, "begin"),
            TraceEvent::End => write!(f, "end"),
        }
    }
}

struct RenderHash(u64);
impl Display for RenderHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            write!(f, "-")
        } else {
            // Truncate the hash to u32, to fit on a line.
            write!(f, "{:4.4x}", self.0 as u32)
        }
    }
}

struct RenderSizeAndHash(usize, u64);
impl Display for RenderSizeAndHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            write!(f, "-")
        } else {
            write!(f, "{}@{}", self.0, RenderHash(self.1))
        }
    }
}

impl Display for TraceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "cpu:{}, mem:{}, prngs:{}/{}, objs:{}/{}, vm:{}/{}, evt:{}, store:{}/{}, foot:{}, stk:{}, auth:{}/{}",
            self.cpu_insns,
            self.mem_bytes,
            RenderHash(self.local_prng_hash),
            RenderHash(self.base_prng_hash),
            RenderSizeAndHash(self.local_objs_size, self.local_objs_hash),
            RenderSizeAndHash(self.global_objs_size, self.global_objs_hash),
            RenderSizeAndHash(self.vm_mem_size, self.vm_mem_hash),
            RenderSizeAndHash(self.vm_exports_size, self.vm_exports_hash),
            RenderSizeAndHash(self.events_size, self.events_hash),
            RenderSizeAndHash(self.instance_storage_size, self.instance_storage_hash),
            RenderSizeAndHash(self.ledger_storage_size, self.ledger_storage_hash),
            RenderSizeAndHash(self.storage_footprint_size, self.storage_footprint_hash),
            RenderSizeAndHash(self.context_stack_size, self.context_stack_hash),
            RenderSizeAndHash(self.auth_stack_size, self.auth_stack_hash),
            RenderSizeAndHash(self.auth_trackers_size, self.auth_trackers_hash),
        )
    }
}
