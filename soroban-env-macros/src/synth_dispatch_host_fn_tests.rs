use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Error, LitStr};

use crate::{Function, LEDGER_PROTOCOL_VERSION};

use std::collections::{BTreeMap, BTreeSet};

const SUBTYPES: &[(&str, &str)] = &[
    ("Val", "Bool"),
    ("Val", "Void"),
    ("Val", "Error"),
    ("Val", "U32Val"),
    ("Val", "I32Val"),
    ("Val", "U64Small"),
    ("Val", "I64Small"),
    ("Val", "TimepointSmall"),
    ("Val", "DurationSmall"),
    ("Val", "U128Small"),
    ("Val", "I128Small"),
    ("Val", "U256Small"),
    ("Val", "I256Small"),
    ("Val", "SymbolSmall"),
    ("Val", "U64Object"),
    ("Val", "I64Object"),
    ("Val", "TimepointObject"),
    ("Val", "DurationObject"),
    ("Val", "U128Object"),
    ("Val", "I128Object"),
    ("Val", "U256Object"),
    ("Val", "I256Object"),
    ("Val", "BytesObject"),
    ("Val", "StringObject"),
    ("Val", "SymbolObject"),
    ("Val", "VecObject"),
    ("Val", "MapObject"),
    ("Val", "AddressObject"),
    ("Val", "Symbol"),
    ("Val", "U64Val"),
    ("Val", "U128Val"),
    ("Val", "I128Val"),
    ("Val", "U256Val"),
    ("Val", "I256Val"),
    ("Val", "DurationVal"),
    ("Val", "TimepointVal"),
    ("Symbol", "SymbolSmall"),
    ("U64Val", "U64Small"),
    ("U128Val", "U128Small"),
    ("I128Val", "I128Small"),
    ("U256Val", "U256Small"),
    ("I256Val", "I256Small"),
    ("DurationVal", "DurationSmall"),
    ("TimepointVal", "TimepointSmall"),
    ("Symbol", "SymbolObject"),
    ("U64Val", "U64Object"),
    ("U128Val", "U128Object"),
    ("I128Val", "I128Object"),
    ("U256Val", "U256Object"),
    ("I256Val", "I256Object"),
    ("DurationVal", "DurationObject"),
    ("TimepointVal", "TimepointObject"),
    // Unfortunately we can only test types that are convertable to `Val` due to the call interface.
    // Ideally we should be able to jam these bits directly into a `Val` (or a call interface taking
    // `Wasmi::Value`) and provide them as call args. But this will require some work.
    // ("Val", "StorageType"),
    // ("u64", "Val"),
    // ("i64", "Val"),
    // ("Val", "SmallCodeUpperBound "),
    // ("Val", "ObjectCodeLowerBound "),
    // ("Val", "ObjectCodeUpperBound "),
    // ("Val", "Bad "),
];

fn dfs(edges: &BTreeMap<String, BTreeSet<String>>, ty: &String, set: &mut BTreeSet<String>) {
    if let Some(subtypes) = edges.get(ty) {
        for st in subtypes.iter() {
            set.insert(st.clone());
            dfs(edges, st, set);
        }
    }
}

fn check_function_protocol_is_in_range(func: &Function) -> bool {
    let min_supported_proto_is_too_new = func
        .min_supported_protocol
        .is_some_and(|v| v > LEDGER_PROTOCOL_VERSION);
    let max_supported_proto_is_too_old = func
        .max_supported_protocol
        .is_some_and(|v| v < LEDGER_PROTOCOL_VERSION);
    !min_supported_proto_is_too_new && !max_supported_proto_is_too_old
}

// This requires the input to be a valid signature
const SPECIAL_CASES: [&str; 1] = ["recover_key_ecdsa_secp256k1"];

fn generate_invalid_obj_call_for_special_cases() -> TokenStream {
    let fn_name = "recover_key_ecdsa_secp256k1";
    let wasm_module = format_ident!("wasm_module_calling_{}", fn_name);
    let impls = (0..2).map(|i| {
        let args = if i == 0 {
            quote! {
                // copied from test
                let sig: Vec<u8> = hex::FromHex::from_hex(b"90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc93").unwrap();
                let sig = host.add_host_object(ScBytes(sig.try_into().unwrap())).unwrap();
                let args = HostVec::from_vec(
                    vec![
                        BytesObject::test_val(), // valid object type with invalid handle 123
                        sig.to_val(), // valid object type + a valid object handle + valid signature
                        U32Val::test_val_with_initial_value(0_i64)
                    ]
                )?;
            }
        } else {
            quote! {
                let args = HostVec::from_vec(
                    vec![
                        BytesObject::test_object(&host).to_val(), // valid object type and handle
                        BytesObject::test_val(), // valid object type with invalid handle 123
                        U32Val::test_val_with_initial_value(1)
                    ]
                )?;
            }
        };
        (i, args)
    }).map(|(i, args)| {
        let fn_ident = format_ident!("invalid_object_handle_{}_arg_{}", fn_name, i as u32);
        quote! {
            #[test]
            fn #fn_ident() -> Result<(), HostError> {
                let wasm = #wasm_module();
                let host = observe_host!(Host::test_host_with_recording_footprint());
                host.as_budget().reset_unlimited()?;
                let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

                #args
                let args = host.add_host_object(args)?;

                let res = host.call(
                    contract_id_obj,
                    Symbol::try_from_small_str("test")?,
                    args,
                );
                assert!(HostError::result_matches_err(
                    res,
                    (ScErrorType::Value, ScErrorCode::InvalidInput)
                ));

                Ok(())
            }
        }
    });

    quote! {
        #(#impls)*
    }
}

pub fn generate_wasm_module_calling_host_functions(file_lit: LitStr) -> Result<TokenStream, Error> {
    let root: crate::Root = crate::load_env_file(file_lit)?;
    let impls = root
        .modules
        .iter()
        .flat_map(|m| {
            m.functions
                .clone()
                .into_iter()
                .map(move |f| (m.export.clone(), f))
        })
        .map(|(mod_export, hf)| {
            let fn_export = hf.export;
            let arity = hf.args.len() as u32;
            let wasm_module = format_ident!("wasm_module_calling_{}", hf.name);
            quote! {
                // define the wasms
                fn #wasm_module() -> Vec<u8> {
                    let mut me = ModEmitter::default_with_test_protocol();
                    let f0 = me.import_func(#mod_export, #fn_export, Arity(#arity));
                    let mut fe = me.func(Arity(#arity), 0);
                    for i in 0..#arity {
                        fe.push(Operand::Local(LocalRef(i)));
                    }
                    fe.call_func(f0);
                    fe.drop();
                    fe.push(Symbol::try_from_small_str("pass").unwrap());
                    fe.finish_and_export("test").finish()
                }
            }
        });

    Ok(quote! {
        #(#impls)*
    })
}

pub fn generate_hostfn_call_with_wrong_types(file_lit: LitStr) -> Result<TokenStream, Error> {
    let root: crate::Root = crate::load_env_file(file_lit)?;

    // We first build the type relationship graph from the hardcoded values.
    // The goal is to figure out all the ancestors and descentents of a type.
    let mut edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut reverse_edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut all_test_candidate_types: BTreeSet<String> = BTreeSet::new();
    for (ty, subty) in SUBTYPES.iter() {
        edges
            .entry(ty.to_string())
            .or_default()
            .insert(subty.to_string());
        reverse_edges
            .entry(subty.to_string())
            .or_default()
            .insert(ty.to_string());
        all_test_candidate_types.insert(ty.to_string());
        all_test_candidate_types.insert(subty.to_string());
    }

    let mut children_of_type: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (ty, _) in edges.iter() {
        let children = children_of_type.entry(ty.clone()).or_default();
        dfs(&edges, ty, children);
    }

    let mut parents_of_type: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (ty, _) in reverse_edges.iter() {
        let parents = parents_of_type.entry(ty.clone()).or_default();
        dfs(&reverse_edges, ty, parents);
    }

    // Collect all the types appeared as an argument in a host function
    // and map it to the host function and position it appears in.
    let mut type_to_fn_arg: BTreeMap<String, Vec<(Function, usize)>> = BTreeMap::new();
    for m in root.modules.clone() {
        for f in m.functions.clone() {
            // checks if the current ledger protocol version falls between
            // supported protocol versions of this function
            if check_function_protocol_is_in_range(&f) {
                for (i, a) in f.args.iter().enumerate() {
                    type_to_fn_arg
                        .entry(a.r#type.clone())
                        .or_default()
                        .push((f.clone(), i));
                }
            }
        }
    }

    // Because of the combination of "all-host-fns * all-args-of-a-fn * all-the-wrong-types-of-that-arg" is very large,
    // we go through each appeared type, and choose one hostfn -- `target_fn` to fun the test with.
    let test_impls = type_to_fn_arg
        .iter()
        .filter(|entry| all_test_candidate_types.contains(entry.0))
        .map(|entry| {
            let target_type = entry.0;
            // we just always pick the first function, instead of a random one
            let (target_fn, target_arg_pos) = entry.1.first().unwrap();
            let wasm_module = format_ident!("wasm_module_calling_{}", &target_fn.name);
            let test_wrong_arg_type =
                format_ident!("dispatch_with_wrong_arg_type_{}", &target_fn.name);
            let target_min_vers = target_fn.min_supported_protocol.unwrap_or(0);
            let target_max_vers = target_fn
                .max_supported_protocol
                .unwrap_or(LEDGER_PROTOCOL_VERSION);

            // There are three types of possibilities between two types "target" and "input":
            // 1. Compatible -- the target type is a parent (say `Val`), then passing in any child type value is fine.
            // 2. Maybe compatible -- the target value is a child (say `SymbolSmall`), then passing in a parent type (e.g. `Symbol`) maybe compatible
            // 3. Incompatible -- if the target and input types are neither 1 or 2, e.g `VecObject` and `MapObject`.
            // For simplicity, our tests cover scenario 3.
            let calls = all_test_candidate_types
                .iter()
                .filter(|&t| {
                    !(t == target_type
                        || children_of_type
                            .get(t)
                            .is_some_and(|s| s.contains(target_type))
                        || parents_of_type
                            .get(t)
                            .is_some_and(|s| s.contains(target_type)))
                })
                .map(|t| {
                    let input_args = target_fn.args.iter().enumerate().map(|(i, a)| {
                        if i == *target_arg_pos {
                            let wrong_ty = format_ident!("{}", t);
                            quote! {
                                #wrong_ty::test_val()
                            }
                        } else {
                            let rest_ty = format_ident!("{}", a.r#type);
                            quote! {
                                #rest_ty::test_val()
                            }
                        }
                    });

                    quote! {
                        {
                            let args = HostVec::from_vec(vec![ #(#input_args),*])?;
                            let args = host.add_host_object(args)?;
                            let res = host.call(
                                contract_id_obj,
                                Symbol::try_from_small_str("test")?,
                                args,
                            );
                            assert!(HostError::result_matches_err(
                                res,
                                (ScErrorType::Value, ScErrorCode::InvalidInput)
                            ));
                        }
                    }
                });

            quote! {
                #[test]
                fn #test_wrong_arg_type() -> Result<(), HostError> {
                    let wasm = #wasm_module();
                    let host = observe_host!(Host::test_host_with_recording_footprint());
                    host.as_budget().reset_unlimited()?;
                    let proto = host.get_ledger_protocol_version()?;
                    if proto < #target_min_vers || proto > #target_max_vers {
                        return Ok(());
                    }
                    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
                    #(#calls)*
                    Ok(())
                }
            }
        });

    Ok(quote! {
        #(#test_impls)*
    })
}

pub fn generate_hostfn_call_with_invalid_obj_handles(
    file_lit: LitStr,
) -> Result<TokenStream, Error> {
    let root: crate::Root = crate::load_env_file(file_lit)?;

    let mut all_test_candidate_types: BTreeSet<String> = BTreeSet::new();
    for (ty, subty) in SUBTYPES.iter() {
        all_test_candidate_types.insert(ty.to_string());
        all_test_candidate_types.insert(subty.to_string());
    }
    let special_case_fns: BTreeSet<String> = SPECIAL_CASES.iter().map(|s| s.to_string()).collect();

    let test_impls = root
        .modules
        .iter()
        .flat_map(|m| m.functions.clone().into_iter())
        .filter(check_function_protocol_is_in_range)
        .flat_map(|f| {
            f.args
                .clone()
                .into_iter()
                .enumerate()
                .map(move |(i, a)| ((f.clone(), f.args.clone()), (i, a)))
        })
        .filter(|((f, _), (_, arg))| {
            !special_case_fns.contains(&f.name) && arg.r#type.ends_with("Object")
        })
        .map(|((func, args), (pos, _))| {
            let wasm_module = format_ident!("wasm_module_calling_{}", func.name);
            let fn_ident = format_ident!("invalid_object_handle_{}_arg_{}", func.name, pos);
            let target_min_vers = func.min_supported_protocol.unwrap_or(0);
            let target_max_vers = func
                .max_supported_protocol
                .unwrap_or(LEDGER_PROTOCOL_VERSION);
            let args = args.iter().enumerate().map(|(i, a)| {
                let ty_ident = format_ident!("{}", a.r#type);
                // if an arg is Object type, but it is not our test target, we generate an valid handle for it
                if a.r#type.ends_with("Object") && i != pos {
                    // we must handle some special cases, e.g. a valid signature must be 64 bytes
                    if func.name == "verify_sig_ed25519" && i == 2 {
                        quote! {
                            #ty_ident::test_object_with_initial_length(&host, 64).to_val()
                        }
                    } else {
                        quote! {
                            #ty_ident::test_object(&host).to_val()
                        }
                    }
                } else {
                    quote! {
                        #ty_ident::test_val()
                    }
                }
            });

            quote! {
                #[test]
                fn #fn_ident() -> Result<(), HostError> {
                    let wasm = #wasm_module();
                    let host = observe_host!(Host::test_host_with_recording_footprint());
                    host.as_budget().reset_unlimited()?;
                    let proto = host.get_ledger_protocol_version()?;
                    if proto < #target_min_vers || proto > #target_max_vers {
                        return Ok(());
                    }
                    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

                    let args = HostVec::from_vec(vec![ #(#args),*])?;
                    let args = host.add_host_object(args)?;
                    let res = host.call(
                        contract_id_obj,
                        Symbol::try_from_small_str("test")?,
                        args,
                    );
                    assert!(HostError::result_matches_err(
                        res,
                        (ScErrorType::Value, ScErrorCode::InvalidInput)
                    ));

                    Ok(())
                }
            }
        });

    // manually handle the special case functions
    let bl_impls = generate_invalid_obj_call_for_special_cases();

    Ok(quote! {
        #(#test_impls)*
        #bl_impls
    })
}
