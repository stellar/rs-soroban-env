use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::{BTreeMap, BTreeSet};
use syn::{Error, LitStr};

use crate::{Arg, Function};

// This module is a somewhat gnarly metaprogram that generates a set of
// "expression enums" and associated impls of the `Emit` trait for each possible
// type and function call in the env.json environment interface definition. It's
// very tightly connected to its sole user, the synth-wasm crate (where `Emit`
// is defined to drive `FuncEmitter`). Unfortunately there are a lot of special
// cases to make it work well.

const CONST_SUFFIX: &str = "_const";
const EXISTING_OR_SUB_EXPR: &str = "existing_or_sub_expr";

const SUBTYPES: &[(&str, &str)] = &[
    ("Val", "Bool"),
    ("Val", "Void"),
    ("Val", "I32Val"),
    ("Val", "U32Val"),
    ("Val", "Error"),
    ("Val", "Symbol"),
    ("Symbol", "SymbolObject"),
    ("Val", "U64Val"),
    ("U64Val", "U64Object"),
    ("Val", "I64Val"),
    ("I64Val", "I64Object"),
    ("Val", "U128Val"),
    ("U128Val", "U128Object"),
    ("Val", "I128Val"),
    ("I128Val", "I128Object"),
    ("Val", "U256Val"),
    ("U256Val", "U256Object"),
    ("Val", "I256Val"),
    ("I256Val", "I256Object"),
    ("Val", "TimepointVal"),
    ("TimepointVal", "TimepointObject"),
    ("Val", "DurationVal"),
    ("DurationVal", "DurationObject"),
    ("Val", "MapObject"),
    ("Val", "VecObject"),
    ("Val", "BytesObject"),
    ("Val", "StringObject"),
    ("Val", "AddressObject"),
];

fn expr_enum_name(ty: &String) -> Ident {
    format_ident!("Expr{}", ty)
}

fn type_const_expr_type(ty: &str) -> Option<String> {
    match ty {
        "Bool" => Some("bool".to_string()),
        "I32Val" => Some("i32".to_string()),
        "U32Val" => Some("u32".to_string()),
        "i64" => Some("i64".to_string()),
        "u64" => Some("u64".to_string()),
        "Void" => Some("Void".to_string()),
        "Error" => Some("Error".to_string()),
        "StorageType" => Some("StorageType".to_string()),
        "Symbol" => Some("Symbol".to_string()),
        _ => None,
    }
}

impl crate::Function {
    fn synthesize_existing_or_sub_expr_function(ty: &str) -> Self {
        let mut f = Function {
            name: EXISTING_OR_SUB_EXPR.to_string(),
            ..Default::default()
        };
        f.args.push(crate::Arg {
            name: "index".to_string(),
            r#type: "u8".to_string(),
        });
        f.args.push(crate::Arg {
            name: "x".to_string(),
            r#type: ty.to_string(),
        });
        f
    }

    fn synthesize_const_expr_function(ty: &str) -> Option<Self> {
        if let Some(const_ty) = type_const_expr_type(ty) {
            let mut f = Function {
                name: format!("{}{}", ty, CONST_SUFFIX),
                ..Default::default()
            };
            f.args.push(crate::Arg {
                name: "x".to_string(),
                r#type: const_ty,
            });
            Some(f)
        } else {
            None
        }
    }

    // These are crude, but work for our purposes
    fn is_const_expr_fn(&self) -> bool {
        self.name.ends_with(CONST_SUFFIX) && self.args.len() == 1
    }

    fn is_existing_or_sub_fn(&self) -> bool {
        self.name == EXISTING_OR_SUB_EXPR && self.args.len() == 2
    }
}

pub fn generate(file_lit: LitStr) -> Result<TokenStream, Error> {
    let root: crate::Root = crate::load_env_file(file_lit)?;

    // First we collect all the types used anywhere in the env.json file as well
    // as all the functions, grouped by return type.
    let mut all_tys: BTreeSet<String> = BTreeSet::new();
    let mut all_subty_fns: BTreeSet<String> = BTreeSet::new();
    let mut fns_by_type = BTreeMap::<String, Vec<crate::Function>>::new();

    for m in root.modules.iter() {
        for f in m.functions.iter() {
            all_tys.insert(f.r#return.clone());
            for arg in f.args.iter() {
                all_tys.insert(arg.r#type.clone());
            }
            fns_by_type
                .entry(f.r#return.clone())
                .or_default()
                .push(f.clone())
        }
    }

    // Several types have expression-subtype relationships, for example
    // ExprSymbolObject is a subtype of ExprSymbol or ExprMapObject is a subtype
    // of ExprVal. We encode these as extra pseudo-function cases in each
    // supertype.
    for (sup, sub) in SUBTYPES {
        let subty_fn_name = format!("sub_{}", sub);
        all_subty_fns.insert(subty_fn_name.clone());
        let mut f = Function {
            name: subty_fn_name,
            ..Default::default()
        };
        f.args.push(Arg {
            name: "sub".to_string(),
            r#type: sub.to_string(),
        });
        fns_by_type.entry(sup.to_string()).or_default().push(f);
        all_tys.insert(sup.to_string());
        all_tys.insert(sub.to_string());
    }

    // Next for each type foo used anywhere we either synthesize a
    // foo_const(some_const_ty) function or require there be _some_
    // function that returns a foo
    for ty in all_tys.iter() {
        let ty_fns = fns_by_type.entry(ty.clone()).or_default();
        ty_fns.push(Function::synthesize_existing_or_sub_expr_function(
            ty.as_str(),
        ));
        if let Some(f) = Function::synthesize_const_expr_function(ty.as_str()) {
            ty_fns.push(f)
        } else if ty_fns.is_empty() {
            panic!("no way to synthesize {}", ty)
        }
    }

    // Now we declare an enum ExprFoo for each type Foo in the input, with one case for
    // every known function that can produce a Foo.
    let enums = fns_by_type.iter().map(|(ty, fns)| {
        let enum_name = expr_enum_name(ty);
        let cases = fns.iter().map(|f| {
            let case_name = format_ident!("{}", f.name);
            let args: Vec<_> = f
                .args
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    if f.is_const_expr_fn() || (f.is_existing_or_sub_fn() && i == 0) {
                        // The const and existing-or-sub-fn index nodes just contain
                        // their constant or index args directly.
                        let arg = format_ident!("{}", a.r#type);
                        quote! { #arg }
                    } else {
                        // Interior nodes contain their Foo args as Box<ExprFoo>
                        let arg_expr = expr_enum_name(&a.r#type);
                        quote! { Box<#arg_expr> }
                    }
                })
                .collect();
            quote! {
                #case_name(#(#args),*)
            }
        });
        quote! {
            #[derive(Arbitrary, Clone, Debug)]
            pub enum #enum_name {
                #(#cases),*
            }
        }
    });

    let impls = fns_by_type.iter().map(|(ty, fns)| {
        let enum_name = expr_enum_name(ty);
        let emit_cases = fns.iter().map(|f| {
            let case_name = format_ident!("{}", f.name);
            let enum_name_2 = enum_name.clone();
            let arg_names: Vec<_> = f.args.iter().map(|a| format_ident!("{}", a.name)).collect();

            if f.is_const_expr_fn() {
                // Constant-expression nodes just form a constant type (eg. U32Val) and then
                // call .into() to turn it into a constant Operand (eg. Operand::Const64).
                let ty2 = format_ident!("{}", ty.clone());
                let arg0 = arg_names[0].clone();
                quote! {
                    #enum_name_2::#case_name(#(#arg_names),*) => {
                        #ty2::from(#arg0.clone()).into()
                    }
                }
            } else if f.is_existing_or_sub_fn() {
                // "existing_or_sub_expr" nodes take a u8 index and pass
                // it to a lookup function on the FuncEmitter, to find an
                // existing Operand with the associated tag, falling back to
                // calling their sub-expr node when there is nothing found.
                let arg0 = arg_names[0].clone();
                let arg1 = arg_names[1].clone();
                quote! {
                    #enum_name_2::#case_name(#(#arg_names),*) => {
                        _func_emitter.maybe_choose_local(#ty, *#arg0).map(|x| x.into())
                        .unwrap_or_else(|| #arg1.emit(_func_emitter))
                    }

                }
            } else if all_subty_fns.contains(&f.name) {
                // Subtype-wrapping "functions" (cases in the enum) just pass
                // through to the inner type.
                quote! {
                    #enum_name_2::#case_name(x) => x.emit(_func_emitter),
                }
            } else {
                // Non-leaf functions involve making a subcall to each argument sub-expr, and
                // then calling the named function with those subcall results.
                let arg_names_2 = arg_names.clone();
                let subcalls: Vec<_> = f
                    .args
                    .iter()
                    .map(|a| {
                        let arg_expr = format_ident!("{}", a.name);
                        quote! { let #arg_expr = #arg_expr.emit(_func_emitter); }
                    })
                    .collect();
                quote! {
                    #enum_name_2::#case_name(#(#arg_names),*) => {
                        #(#subcalls)*
                        _func_emitter.#case_name(#(#arg_names_2),*);
                        _func_emitter.alloc_and_store_local(#ty).into()
                    }
                }
            }
        });
        let num_locals_cases = fns.iter().map(|f| {
            let case_name = format_ident!("{}", f.name);
            let arg_names: Vec<_> = if f.is_const_expr_fn() {
                vec![format_ident!("_")]
            } else {
                f.args.iter().map(|a| format_ident!("{}", a.name)).collect()
            };
            let subcalls: Vec<_> = if f.is_const_expr_fn() {
                vec![quote! { 0 }]
            } else if f.is_existing_or_sub_fn() {
                let arg1 = arg_names[1].clone();
                vec![quote! { #arg1.num_locals() }]
            } else if all_subty_fns.contains(&f.name) {
                // Subtype-wrapping "functions" (cases in the enum) just pass
                // through to the inner type.
                let arg0 = arg_names[0].clone();
                vec![quote! {
                    #arg0.num_locals()
                }]
            } else {
                f.args
                    .iter()
                    .map(|a| {
                        let arg_expr = format_ident!("{}", a.name);
                        quote! { #arg_expr.num_locals() + }
                    })
                    .chain(std::iter::once(quote! { 1 }))
                    .collect()
            };
            let enum_name_2 = enum_name.clone();
            quote! {
                #enum_name_2::#case_name(#(#arg_names),*) => {
                    #(#subcalls)*
                }
            }
        });
        quote! {
            impl crate::Emit for #enum_name {
                fn emit(&self, _func_emitter: &mut crate::FuncEmitter) -> crate::Operand {
                    match self {
                        #(#emit_cases)*
                    }
                }
                fn num_locals(&self) -> u32 {
                    match self {
                        #(#num_locals_cases)*
                    }
                }
            }
        }
    });

    Ok(quote! {
        use arbitrary::Arbitrary;
        use soroban_env_common::{StorageType, xdr::ScError, Void, Bool, U32Val, I32Val, Error, Symbol};
        #(#enums)*
        #(#impls)*
    })
}
