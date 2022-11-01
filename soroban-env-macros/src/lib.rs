extern crate proc_macro;

use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    parse::Parse,
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{Comma, Semi},
    Expr, FnArg, ForeignItemFn, Ident, LitInt, LitStr, ReturnType, Stmt, Token, Type,
};

use stellar_xdr::{ScEnvMetaEntry, WriteXdr};

struct MetaInput {
    pub interface_version: u64,
}

impl Parse for MetaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(MetaInput {
            interface_version: {
                assert_eq!(input.parse::<Ident>()?, "interface_version");
                input.parse::<Token![:]>()?;
                let v = input.parse::<LitInt>()?.base10_parse()?;
                input.parse::<Token![,]>()?;
                v
            },
        })
    }
}

struct MetaConstsOutput {
    pub input: MetaInput,
}

impl MetaConstsOutput {
    pub fn to_meta_entries(&self) -> Vec<ScEnvMetaEntry> {
        vec![ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(
            self.input.interface_version,
        )]
    }
}

impl ToTokens for MetaConstsOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // Build params for expressing the interface version.
        let interface_version = self.input.interface_version;

        // Build params for expressing the meta xdr.
        let meta_xdr = self
            .to_meta_entries()
            .into_iter()
            .map(|entry| entry.to_xdr())
            .collect::<Result<Vec<Vec<u8>>, stellar_xdr::Error>>()
            .unwrap()
            .concat();
        let meta_xdr_len = meta_xdr.len();
        let meta_xdr_lit = proc_macro2::Literal::byte_string(meta_xdr.as_slice());

        // Output.
        tokens.extend(quote! {
            pub const INTERFACE_VERSION: u64 = #interface_version;
            pub const XDR: [u8; #meta_xdr_len] = *#meta_xdr_lit;
        });
    }
}

#[proc_macro]
pub fn generate_env_meta_consts(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let meta_input = parse_macro_input!(input as MetaInput);
    let meta_consts_output = MetaConstsOutput { input: meta_input };
    quote! { #meta_consts_output }.into()
}

fn ident_is_abi_v160_type(id: &Ident) -> bool {
    id == "RawVal"
}

fn ident_is_abi_v128_type(id: &Ident) -> bool {
    id == "Symbol" || id == "BitSet" || id == "u128" || id == "i128"
}

fn ident_is_abi_direct_type(id: &Ident) -> bool {
    id == "Object"
        || id == "Status"
        || id == "Static"
        || id == "u64"
        || id == "i64"
        || id == "u32"
        || id == "i32"
}

/// Returns Some(id) if the provided TokenStream is a single TokenTree::Ident,
/// recurring through any TokenTree::Group(g) with Delimiter::None.
fn token_stream_sole_ident(ts: TokenStream) -> Option<Ident> {
    let mut ret = None;
    for tt in ts {
        match (ret, tt) {
            (None, TokenTree::Ident(id)) => ret = Some(id),
            (None, TokenTree::Group(group)) if group.delimiter() == Delimiter::None => {
                ret = token_stream_sole_ident(group.stream())
            }
            _ => {
                ret = None;
                break;
            }
        }
    }
    ret
}

fn ty_is_abi_v160_type(ty: &Type) -> bool {
    if let Some(id) = token_stream_sole_ident(ty.to_token_stream()) {
        return ident_is_abi_v160_type(&id);
    }
    false
}

fn ty_is_abi_v128_type(ty: &Type) -> bool {
    if let Some(id) = token_stream_sole_ident(ty.to_token_stream()) {
        return ident_is_abi_v128_type(&id);
    }
    false
}

fn ty_is_abi_indirect_return_type(ty: &Type) -> bool {
    ty_is_abi_v128_type(ty) || ty_is_abi_v160_type(ty)
}

fn ty_is_abi_direct_type(ty: &Type) -> bool {
    if let Some(id) = token_stream_sole_ident(ty.to_token_stream()) {
        return ident_is_abi_direct_type(&id);
    }
    false
}

fn arg_abi_v160_pat_ident_ty(a: &FnArg) -> Option<(Ident, Type)> {
    match a {
        FnArg::Typed(t) if ty_is_abi_v160_type(&*t.ty) => {
            if let Some(pat_id) = token_stream_sole_ident(t.pat.to_token_stream()) {
                return Some((pat_id, (*t.ty).clone()));
            }
        }
        _ => (),
    }
    None
}

fn arg_abi_v128_pat_ident_ty(a: &FnArg) -> Option<(Ident, Type)> {
    match a {
        FnArg::Typed(t) if ty_is_abi_v128_type(&*t.ty) => {
            if let Some(pat_id) = token_stream_sole_ident(t.pat.to_token_stream()) {
                return Some((pat_id, (*t.ty).clone()));
            }
        }
        _ => (),
    }
    None
}

fn arg_abi_direct_pat_ident(a: &FnArg) -> Option<(Ident, Type)> {
    match a {
        FnArg::Typed(t) if ty_is_abi_direct_type(&*t.ty) => {
            if let Some(pat_id) = token_stream_sole_ident(t.pat.to_token_stream()) {
                return Some((pat_id, (*t.ty).clone()));
            }
        }
        _ => (),
    }
    None
}

enum ArgExplosion {
    V160 {
        a1_arg: FnArg,
        a2_arg: FnArg,
        a3_arg: FnArg,
        in_id: Ident,
        in_ty: Type,
        a1_id: Ident,
        a2_id: Ident,
        a3_id: Ident,
    },
    V128 {
        a1_arg: FnArg,
        a2_arg: FnArg,
        in_id: Ident,
        in_ty: Type,
        a1_id: Ident,
        a2_id: Ident,
    },
    Direct {
        in_arg: FnArg,
        in_id: Ident,
        in_ty: Type,
    },
}

fn explode_fnarg(arg: &FnArg) -> ArgExplosion {
    if let Some((in_id, in_ty)) = arg_abi_v160_pat_ident_ty(arg) {
        let a1_id = Ident::new(&format!("{}_1", in_id), Span::call_site());
        let a2_id = Ident::new(&format!("{}_2", in_id), Span::call_site());
        let a3_id = Ident::new(&format!("{}_3", in_id), Span::call_site());
        let a1_arg: FnArg = parse_quote! { #a1_id:u32 };
        let a2_arg: FnArg = parse_quote! { #a2_id:u64 };
        let a3_arg: FnArg = parse_quote! { #a3_id:u64 };
        ArgExplosion::V160 {
            in_id,
            in_ty,
            a1_id,
            a2_id,
            a3_id,
            a1_arg,
            a2_arg,
            a3_arg,
        }
    } else if let Some((in_id, in_ty)) = arg_abi_v128_pat_ident_ty(arg) {
        let a1_id = Ident::new(&format!("{}_1", in_id), Span::call_site());
        let a2_id = Ident::new(&format!("{}_2", in_id), Span::call_site());
        let a1_arg: FnArg = parse_quote! { #a1_id:u64 };
        let a2_arg: FnArg = parse_quote! { #a2_id:u64 };
        ArgExplosion::V128 {
            in_id,
            in_ty,
            a1_id,
            a2_id,
            a1_arg,
            a2_arg,
        }
    } else if let Some((in_id, in_ty)) = arg_abi_direct_pat_ident(arg) {
        ArgExplosion::Direct {
            in_arg: arg.clone(),
            in_id,
            in_ty,
        }
    } else {
        panic!("unrecognized host function arg pattern or type")
    }
}

enum ArgExprExplosion {
    V160 {
        prefix: Stmt,
        e1: Expr,
        e2: Expr,
        e3: Expr,
    },
    V128 {
        prefix: Stmt,
        e1: Expr,
        e2: Expr,
    },
    Direct(Expr),
}

fn explode_fnarg_to_exprs(arg: &FnArg) -> ArgExprExplosion {
    match explode_fnarg(arg) {
        ArgExplosion::V160 {
            in_id,
            a1_id,
            a2_id,
            a3_id,
            ..
        } => {
            let prefix: Stmt = parse_quote! { let (#a1_id, #a2_id, #a3_id) = <_ as abi::V160>::v160_explode(#in_id); };
            let e1: Expr = parse_quote! { #a1_id };
            let e2: Expr = parse_quote! { #a2_id };
            let e3: Expr = parse_quote! { #a3_id };
            ArgExprExplosion::V160 { prefix, e1, e2, e3 }
        }
        ArgExplosion::V128 {
            in_id,
            a1_id,
            a2_id,
            ..
        } => {
            let prefix: Stmt =
                parse_quote! { let (#a1_id, #a2_id) = <_ as abi::V128>::v128_explode(#in_id); };
            let e1: Expr = parse_quote! { #a1_id };
            let e2: Expr = parse_quote! { #a2_id };
            ArgExprExplosion::V128 { prefix, e1, e2 }
        }
        ArgExplosion::Direct { in_id, .. } => {
            let e: Expr = parse_quote! { #in_id };
            ArgExprExplosion::Direct(e)
        }
    }
}

struct HostFnDecl {
    mod_id: Ident,
    link_name: String,
    fn_decl: ForeignItemFn,
}

impl Parse for HostFnDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mod_id: Ident = input.parse::<Ident>()?;
        input.parse::<Comma>()?;
        let link_name: String = input.parse::<LitStr>()?.value();
        input.parse::<Comma>()?;
        let fn_decl: ForeignItemFn = input.parse::<ForeignItemFn>()?;
        Ok(HostFnDecl {
            mod_id,
            link_name,
            fn_decl,
        })
    }
}

struct ExplodedExternHostFnDecl(HostFnDecl);

impl ToTokens for ExplodedExternHostFnDecl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fdecl = &self.0.fn_decl;
        let mut args: Vec<FnArg> = Vec::new();
        let mut ret = fdecl.sig.output.clone();
        match &fdecl.sig.output {
            ReturnType::Type(_, r) if ty_is_abi_indirect_return_type(r) => {
                ret = ReturnType::Default;
                let retptr: FnArg = parse_quote! { retptr:u32 };
                args.push(retptr);
            }
            _ => (),
        }
        for arg in fdecl.sig.inputs.iter() {
            match explode_fnarg(arg) {
                ArgExplosion::V160 {
                    a1_arg,
                    a2_arg,
                    a3_arg,
                    ..
                } => {
                    args.push(a1_arg);
                    args.push(a2_arg);
                    args.push(a3_arg);
                }
                ArgExplosion::V128 { a1_arg, a2_arg, .. } => {
                    args.push(a1_arg);
                    args.push(a2_arg);
                }
                ArgExplosion::Direct { in_arg, .. } => {
                    args.push(in_arg);
                }
            }
        }
        let args: Punctuated<FnArg, Comma> = args.into_iter().collect();
        let id = &fdecl.sig.ident;
        tokens.extend(quote! {pub(crate) fn #id(#args) #ret;})
    }
}

#[proc_macro]
pub fn decl_exploded_extern_fn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let host_fn_decl = parse_macro_input!(input as HostFnDecl);
    let link_name = host_fn_decl.link_name.clone();
    let exploded = ExplodedExternHostFnDecl(host_fn_decl);
    quote! {
        #[cfg_attr(target_family = "wasm", link_name = #link_name)]
        #exploded
    }
    .into()
}

struct ExplodedHostFnArgs(HostFnDecl);

impl ToTokens for ExplodedHostFnArgs {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fdecl = &self.0.fn_decl;
        let mut call_args: Vec<Expr> = Vec::new();
        let use_retval = match &fdecl.sig.output {
            ReturnType::Type(_, r) if ty_is_abi_indirect_return_type(r) => {
                tokens.extend(quote! {use core::mem::MaybeUninit;});
                tokens.extend(quote! {let mut retval: MaybeUninit<#r> = MaybeUninit::uninit();});
                let retptr: Expr = parse_quote! { retval.as_mut_ptr() as u32 };
                call_args.push(retptr);
                true
            }
            _ => false,
        };
        for arg in fdecl.sig.inputs.iter() {
            match explode_fnarg_to_exprs(arg) {
                ArgExprExplosion::V160 { prefix, e1, e2, e3 } => {
                    tokens.extend(prefix.into_token_stream());
                    call_args.push(e1);
                    call_args.push(e2);
                    call_args.push(e3);
                }
                ArgExprExplosion::V128 { prefix, e1, e2 } => {
                    tokens.extend(prefix.into_token_stream());
                    call_args.push(e1);
                    call_args.push(e2);
                }
                ArgExprExplosion::Direct(a) => {
                    call_args.push(a);
                }
            }
        }
        let args: Punctuated<Expr, Comma> = call_args.into_iter().collect();
        let mod_id = &self.0.mod_id;
        let id = &fdecl.sig.ident;
        if use_retval {
            // Used a maybe-init retval, return its assume_init() return.
            tokens.extend(quote! { crate::guest::#mod_id::#id(#args); retval.assume_init() });
        } else {
            // No retval, return whatever the function returns
            tokens.extend(quote! { crate::guest::#mod_id::#id(#args) });
        }
    }
}

#[proc_macro]
pub fn explode_args(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let host_fn_decl = parse_macro_input!(input as HostFnDecl);
    let exploded = ExplodedHostFnArgs(host_fn_decl);
    quote! { #exploded }.into()
}

struct DispatchFunction(HostFnDecl);

impl ToTokens for DispatchFunction {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fdecl = &self.0.fn_decl;

        let mut in_args: Vec<FnArg> = Vec::new();
        in_args.push(parse_quote! {caller: wasmi::Caller<Host>});

        let (ret_ok, ret_ty): (Expr, Type) = match &fdecl.sig.output {
            ReturnType::Type(_, ret_ty) => {
                if ty_is_abi_indirect_return_type(ret_ty) {
                    in_args.push(parse_quote! {retptr:u32});
                    let ret_ok: Expr = parse_quote! {
                        {
                            host.write_abi_buf_at(ok, &mut vmcaller, retptr).map_err(|_| UnexpectedSignature)?;
                            Ok(())
                        }
                    };
                    let ret_ty: Type = parse_quote! {Result<(),Trap>};
                    (ret_ok, ret_ty)
                } else {
                    let ret_ok: Expr = parse_quote! {Ok((<#ret_ty as abi::Direct>::to_repr(ok),))};
                    let ret_ty: Type =
                        parse_quote! {Result<(<#ret_ty as abi::Direct>::Repr,),Trap>};
                    (ret_ok, ret_ty)
                }
            }
            _ => (parse_quote! {Ok(())}, parse_quote! {Result<(),Trap>}),
        };

        let mut implode_stmts: Vec<Stmt> = Vec::new();

        let mut call_args: Vec<Expr> = Vec::new();
        call_args.push(parse_quote! {&mut vmcaller});

        for arg in fdecl.sig.inputs.iter() {
            match explode_fnarg(arg) {
                ArgExplosion::V160 {
                    in_id,
                    in_ty,
                    a1_id,
                    a2_id,
                    a3_id,
                    a1_arg,
                    a2_arg,
                    a3_arg,
                    ..
                } => {
                    in_args.push(a1_arg);
                    in_args.push(a2_arg);
                    in_args.push(a3_arg);
                    implode_stmts.push(
                        parse_quote! { let #in_id: #in_ty = <#in_ty as abi::V160>::v160_implode(#a1_id, #a2_id, #a3_id); },
                    );
                    call_args
                        .push(parse_quote! {#in_id.try_into().map_err(|_| UnexpectedSignature)?})
                }
                ArgExplosion::V128 {
                    in_id,
                    in_ty,
                    a1_id,
                    a2_id,
                    a1_arg,
                    a2_arg,
                    ..
                } => {
                    in_args.push(a1_arg);
                    in_args.push(a2_arg);
                    implode_stmts.push(
                        parse_quote! { let #in_id: #in_ty = <#in_ty as abi::V128>::v128_implode(#a1_id, #a2_id); },
                    );
                    call_args
                        .push(parse_quote! {#in_id.try_into().map_err(|_| UnexpectedSignature)?})
                }
                ArgExplosion::Direct { in_id, in_ty, .. } => {
                    let arg: FnArg = parse_quote! { #in_id: <#in_ty as abi::Direct>::Repr };
                    in_args.push(arg);
                    call_args.push(parse_quote! { <#in_ty as abi::Direct>::from_repr(#in_id) })
                }
            }
        }
        let in_args: Punctuated<FnArg, Comma> = in_args.into_iter().collect();
        let implode_stmts: Punctuated<Stmt, Semi> = implode_stmts.into_iter().collect();
        let call_args: Punctuated<Expr, Comma> = call_args.into_iter().collect();
        let id = &fdecl.sig.ident;
        tokens.extend(quote! {
            pub(crate) fn #id(#in_args) -> #ret_ty {
                let host = caller.host_data().clone();
                host.charge_budget(CostType::HostFunction, 1)?;
                let mut vmcaller = VmCaller(Some(caller));
                #implode_stmts
                let res: Result<_, HostError> = host.#id(#call_args);
                match res {
                    Err(hosterr) => Err(escalate_status(host, hosterr.status)),
                    Ok(ok) => #ret_ok
                }
            }
        })
    }
}

#[proc_macro]
pub fn dispatch_function(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let host_fn_decl = parse_macro_input!(input as HostFnDecl);
    let imploded = DispatchFunction(host_fn_decl);
    quote! { #imploded }.into()
}
