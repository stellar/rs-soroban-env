use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeSet;
use syn::{Error, LitStr};

// This file generates tests for various linear memory host functions. There are
// three different types of test, depending on the form of input arguments that
// a host function take (a host function may contain multiple forms):
//
// a). "lm_pos + len (bytes)" where either the initial `lm_pos` or the range can
// be out of bound. Involved host functions:
//   - "bytes_new_from_linear_memory"
//   - "string_new_from_linear_memory"
//   - "symbol_new_from_linear_memory"
//   - "bytes_copy_from_linear_memory"
//   - "bytes_copy_to_linear_memory"
//   - "string_copy_to_linear_memory"
//   - "symbol_copy_to_linear_memory"
//
// b). "val_pos + len (number of Vals)", where either the initial `val_pos` or
// the range can go out of bound. Here `Val` must also be 8 bytes forming a
// valid `env::Val`. Involved host functions:
//   - "map_new_from_linear_memory"
//   - "vec_new_from_linear_memory"
//   - "map_unpack_to_linear_memory"
//   - "vec_unpack_to_linear_memory"
//
// c) An array of keys where each key is a 8-byte slice consist of 4-byte ptr +
// 4-byte len, where either the ptr or len can go out of range. Involved host
// functions:
//   - "map_new_from_linear_memory"
//   - "symbol_index_in_linear_memory"
//   - "map_unpack_to_linear_memory"
//
//
// The tests require setting up a wasm VM with pre-loaded linear memory for each
// host function, where various inputs can be tested in combination. VM linear
// memory used in the tests is one-page length and is divided into the sections
// illustrated below (these are purely choices made to faciliate testing, and
// has nothing to do with wasm specs):
//
// |----------------------------------- Linear Memory (0x10000 / 65536 bytes) ---------------------------------------|
// |                                                                                                                 |
// | KEYS_SECTION       | BAD_PTR_KEY | BAD_LEN_KEY | ............. | DS2           | DS1           | DS0            |
// | (0x0 - 0xFF)       | (0x100)     | (0x108)     |               |(0xFD00-0xFDFF)|(0xFE00-0xFEFF)|(0xFF00-0xFFFF) |
// |                                                                                                                 |
// |--------------------|-------------|-------------|---------------|---------------|---------------|----------------|
//
// where D0S, D1S, D2S = DATA_SECTION_0_START, DATA_SECTION_1_START, DATA_SECTION_2_START.

const LM_START: u64 = 0;
const LM_LENGTH: u64 = 0x10_000;
const DATA_SECTION_0_START: u64 = 0x10_000 - 0x100;
const DATA_SECTION_1_START: u64 = DATA_SECTION_0_START - 0x100;
const DATA_SECTION_2_START: u64 = DATA_SECTION_1_START - 0x100;
const DATA_SECTION_LEN: u64 = 0x100;

const KEYS_SECTION_START: u64 = 0;
const BAD_PTR_KEY_START: u64 = 0x100;
const BAD_LEN_KEY_START: u64 = 0x100 + 8;

const SYMBOL_LEN_LIMIT: u64 = 32;

const SLICE_CASES: [&str; 3] = [
    "map_new_from_linear_memory",
    "symbol_index_in_linear_memory",
    "map_unpack_to_linear_memory",
];

const VAL_CASES: [&str; 4] = [
    "map_new_from_linear_memory",
    "vec_new_from_linear_memory",
    "map_unpack_to_linear_memory",
    "vec_unpack_to_linear_memory",
];

const BYTE_CASES: [&str; 7] = [
    "bytes_new_from_linear_memory",
    "string_new_from_linear_memory",
    "symbol_new_from_linear_memory",
    "bytes_copy_from_linear_memory",
    "bytes_copy_to_linear_memory",
    "string_copy_to_linear_memory",
    "symbol_copy_to_linear_memory",
];

pub fn generate_wasm_module_with_preloaded_linear_memory(
    file_lit: LitStr,
) -> Result<TokenStream, Error> {
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
        .filter(|(_, f)| f.name.contains("_linear_memory"))
        .map(|(mod_export, hf)| {
            let fn_export = hf.export;
            let arity = hf.args.len() as u32;
            let wasm_module = format_ident!("wasm_module_calling_{}", hf.name);
            let hf_name_str = hf.name;
            quote! {
                // define the wasms
                fn #wasm_module() -> Vec<u8> {
                    let mut me = ModEmitter::default_with_test_protocol();
                    let f0 = me.import_func(#mod_export, #fn_export, Arity(#arity));

                    // *******************START of f1***************************
                    // Define the first guest function. It is intended for tests
                    // involving "slices" -- pointers pointing to another part
                    // of the memory. The LM is separated into two regions: the
                    // back (the last 512 bytes) region contains the actual
                    // data, and the begining region contains slices pointing to
                    // the data defined at the end.
                    let mut f1 = me.func(Arity(#arity), 0);
                    // clear all memory
                    f1.i32_const(#LM_START as i32);
                    f1.i32_const(0);
                    f1.i32_const(#LM_LENGTH as i32);
                    f1.memory_fill();
                    // prefill some linear memory region with some data for testing
                    // 1. fill the last 256 bytes with chars '11111111...zzzzzzzz'
                    let b: Vec<u8> = ('1'..='6')
                    .chain('a'..='z')
                    .flat_map(|c| std::iter::repeat(c as u8).take(8))
                    .collect();
                    for i in 0..b.len() {
                        f1.i32_const((#DATA_SECTION_0_START + i as u64) as i32);
                        f1.i64_const(b[i] as i64);
                        f1.i64_store8(0, 0);
                    }
                    // 2. fill the last 512~256 bytes with some valid vals
                    for i in 0..32u64 {
                        f1.i32_const((#DATA_SECTION_1_START + i * 8) as i32);
                        f1.i64_const(U32Val::from(i as u32 + 100).to_val().get_payload() as i64);
                        f1.i64_store(0, 0);
                    }
                    // 3. fill some slices in the beginning
                    // 3.a the first 32 words are valid slices
                    for i in 0..32u64 {
                        let ptr = #DATA_SECTION_0_START + 8 * i;
                        let len = 8u64;
                        let slice = len << 32 | ptr;
                        f1.i32_const((#KEYS_SECTION_START + i * 8) as i32);
                        f1.i64_const(slice as i64);
                        f1.i64_store(0, 0);
                    }
                    // 3.b the next word is a bad slice with oob ptr
                    {
                        let ptr = #LM_LENGTH;
                        let len = 8u64;
                        let slice = len << 32 | ptr;
                        f1.i32_const(#BAD_PTR_KEY_START as i32);
                        f1.i64_const(slice as i64);
                        f1.i64_store(0, 0);
                    }
                    // 3.c the next word is a bad slice with oob len
                    {
                        let ptr = #LM_LENGTH - 8;
                        let len = 9u64;
                        let slice = len << 32 | ptr;
                        f1.i32_const((#BAD_LEN_KEY_START) as i32);
                        f1.i64_const(slice as i64);
                        f1.i64_store(0, 0);
                    }

                    // call the target host function
                    for i in 0..#arity {
                        f1.push(Operand::Local(LocalRef(i)));
                    }
                    f1.call_func(f0);
                    me = f1.finish_and_export("loadmem1");

                    // *******************END of f1***************************

                    // *******************START of f2***************************
                    // Define the second guest function. It is intended for
                    // tests involving "Vals" -- each Val is a 8-byte data. The
                    // LM is separated into two regions: the back region
                    // contains the data (the last 768 bytes, which is further
                    // split into good Vals, bad Vals and symbols, each taking
                    // 256 bytes) and the begining region contains slices
                    // pointing to the symbol data defined at the end.
                    let mut f2 = me.func(Arity(#arity), 0);
                    // clear all memory
                    f2.i32_const(#LM_START as i32);
                    f2.i32_const(0);
                    f2.i32_const(#LM_LENGTH as i32);
                    f2.memory_fill();
                    // prefill some linear memory region with some data for testing
                    // 1. fill the last 256 bytes with good vals
                    for i in 0..32u64 {
                        f2.i32_const((#DATA_SECTION_0_START + i * 8) as i32);
                        f2.i64_const(U32Val::from(i as u32 + 100).to_val().get_payload() as i64);
                        f2.i64_store(0, 0);
                    }
                    // 2. fill the last 512~256 bytes with some bad vals
                    for i in 0..32u64 {
                        f2.i32_const((#DATA_SECTION_1_START + i * 8) as i32);
                        f2.i64_const(i64::MAX);
                        f2.i64_store(0, 0);
                    }
                    // 3. fill the last 768~512 bytes with chars '11111111...zzzzzzzz'
                    let b: Vec<u8> = ('1'..='6')
                    .chain('a'..='z')
                    .flat_map(|c| std::iter::repeat(c as u8).take(8))
                    .collect();
                    for i in 0..b.len() {
                        f2.i32_const((#DATA_SECTION_2_START + i as u64) as i32);
                        f2.i64_const(b[i] as i64);
                        f2.i64_store8(0, 0);
                    }
                    // 4. fill the slices in the beginning pointing to the keys
                    for i in 0..32u64 {
                        let ptr = #DATA_SECTION_2_START + 8 * i;
                        let len = 8u64;
                        let slice = len << 32 | ptr;
                        f2.i32_const((#KEYS_SECTION_START + i * 8) as i32);
                        f2.i64_const(slice as i64);
                        f2.i64_store(0, 0);
                    }

                    // call the target host function
                    for i in 0..#arity {
                        f2.push(Operand::Local(LocalRef(i)));
                    }
                    f2.call_func(f0);
                    me = f2.finish_and_export("loadmem2");
                    // *******************END of f2***************************

                    // *******************START of f3***************************
                    // Define the third guest function. It is intended for tests
                    // involving bytes. It just contains one region, which is
                    // the last 256 bytes of the LM filled with pre-determined
                    // value.
                    let mut f3 = me.func(Arity(#arity), 0);
                    // clear all memory
                    f3.i32_const(#LM_START as i32);
                    f3.i32_const(0);
                    f3.i32_const(#LM_LENGTH as i32);
                    f3.memory_fill();
                    // prefill the last 256 bytes with some values
                    // push in the following order: offset(d), val, length(n)
                    f3.i32_const(#DATA_SECTION_0_START as i32);
                    // Use a valid `Symbol` character for the `Symbol` tests.
                    if #hf_name_str == "symbol_new_from_linear_memory" {
                        f3.i32_const('D' as i32);
                    } else {
                        f3.i32_const(7);
                    }

                    f3.i32_const(#DATA_SECTION_LEN as i32);
                    f3.memory_fill();
                    // call the target host function
                    for i in 0..#arity {
                        f3.push(Operand::Local(LocalRef(i)));
                    }
                    f3.call_func(f0);
                    me = f3.finish_and_export("loadmem3");
                    // *******************END of f3***************************
                    me.finish()
                }
            }
        });

    Ok(quote! {
        #(#impls)*
    })
}

pub fn generate_tests_for_malformed_key_slices() -> Result<TokenStream, Error> {
    let good_impls = SLICE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_good_slices_ok", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "symbol_index_in_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        SymbolSmall::try_from_str("kkkkkkkk").unwrap().to_val(),
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(32).to_val(),
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = ('1'..='6')
                        .chain('a'..='z')
                        .map(|c| SymbolSmall::try_from_str(c.to_string().repeat(8).as_str()).unwrap().to_val())
                        .zip((100..132u32).into_iter().map(|u| U32Val::from(u).to_val()))
                        .collect();
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = match *f {
            "map_new_from_linear_memory" => quote! {
                assert!(res.is_ok());
            },
            "symbol_index_in_linear_memory" => quote! {
                assert_eq!(res?.get_payload(), U32Val::from(16).to_val().get_payload());
            },
            "map_unpack_to_linear_memory" => quote! {
                assert!(res.is_ok());
            },
            _ => panic!("malformed slices not implemented for {}", f),
        };
        (wasm_module, test_name, args, assertions)
    });

    let impls_with_bad_key_ptr = SLICE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_bad_key_ptr", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#BAD_PTR_KEY_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(1).to_val()
                    ])?;
                }
            }
            "symbol_index_in_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        SymbolSmall::try_from_str("kkkkkkkk").unwrap().to_val(),
                        U32Val::from(#BAD_PTR_KEY_START as u32).to_val(),
                        U32Val::from(1).to_val(),
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = vec![(
                        SymbolSmall::try_from_str('k'.to_string().repeat(8).as_str()).unwrap().to_val(),
                        U32Val::from(999).to_val(),
                    )];
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#BAD_PTR_KEY_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(1).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };
        (wasm_module, test_name, args, assertions)
    });

    let impls_with_bad_key_length = SLICE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_bad_key_length", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#BAD_LEN_KEY_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(1).to_val()
                    ])?;
                }
            }
            "symbol_index_in_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        SymbolSmall::try_from_str("kkkkkkkk").unwrap().to_val(),
                        U32Val::from(#BAD_LEN_KEY_START as u32).to_val(),
                        U32Val::from(1).to_val(),
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = vec![(
                        // here the symbol is 9 chars long, the last char will overflow the LM
                        SymbolSmall::try_from_str('k'.to_string().repeat(9).as_str()).unwrap().to_val(),
                        U32Val::from(999).to_val(),
                    )];
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#BAD_LEN_KEY_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                        U32Val::from(1).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls = good_impls
        .chain(impls_with_bad_key_ptr)
        .chain(impls_with_bad_key_length)
        .map(|(wasm_module, test_name, args, assertions)| {
            quote! {
                #[test]
                fn #test_name() -> Result<(), HostError> {
                    let wasm = #wasm_module();
                    let host = observe_host!(Host::test_host_with_recording_footprint());
                    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
                    // prepare args
                    #args
                    let args = host.add_host_object(args)?;

                    // make the call
                    let res = host.call(
                        contract_id_obj,
                        Symbol::try_from_small_str("loadmem1")?,
                        args,
                    );
                    # assertions
                    Ok(())
                }
            }
        });

    Ok(quote! {
        #(#impls)*
    })
}

pub fn generate_tests_for_malformed_val_data() -> Result<TokenStream, Error> {
    let good_impls = VAL_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_good_val_data_ok", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = ('1'..='6')
                        .chain('a'..='z')
                        .map(|c| SymbolSmall::try_from_str(c.to_string().repeat(8).as_str()).unwrap().to_val())
                        .zip((100..132u32).into_iter().map(|u| U32Val::from(u).to_val()))
                        .collect();
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "vec_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "vec_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<Val> = (100..132u32)
                    .into_iter()
                    .map(|u| U32Val::from(u).to_val())
                    .collect();
                let v = HostVec::from_vec(vv).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(v).unwrap().to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(res.is_ok());
        };

        (wasm_module, test_name, args, assertions)
    });

    // we skip the unpack cases because if the incoming Object contains any invalid Vals, we do not check them
    let impls_with_invalid_vals = VAL_CASES
        .iter()
        .filter(|f| *f != &"map_unpack_to_linear_memory" && *f != &"vec_unpack_to_linear_memory")
        .map(|f| {
            let wasm_module = format_ident!("wasm_module_calling_{}", f);
            let test_name = format_ident!("test_calling_{}_with_invalid_vals", f);
            let args = match *f {
                "map_new_from_linear_memory" => {
                    quote! {
                        let args = HostVec::from_vec(vec![
                            U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                            U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                            U32Val::from(32).to_val()
                        ])?;
                    }
                }
                "vec_new_from_linear_memory" => {
                    quote! {
                        let args = HostVec::from_vec(vec![
                            U32Val::from(#DATA_SECTION_1_START as u32).to_val(),
                            U32Val::from(32).to_val()
                        ])?;
                    }
                }
                _ => panic!("malformed slices not implemented for {}", f),
            };

            let assertions = quote! {
                assert!(HostError::result_matches_err(
                    res,
                    (ScErrorType::Value, ScErrorCode::InvalidInput)
                ));
            };

            (wasm_module, test_name, args, assertions)
        });

    let impls_with_invalid_pos_out_of_range = VAL_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_pos_out_of_range", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = ('1'..='6')
                        .chain('a'..='z')
                        .map(|c| SymbolSmall::try_from_str(c.to_string().repeat(8).as_str()).unwrap().to_val())
                        .zip((100..132u32).into_iter().map(|u| U32Val::from(u).to_val()))
                        .collect();
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "vec_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "vec_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<Val> = (100..132u32)
                    .into_iter()
                    .map(|u| U32Val::from(u).to_val())
                    .collect();
                let v = HostVec::from_vec(vv).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(v).unwrap().to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls_with_invalid_length_too_long = VAL_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_length_too_long", f);
        let args = match *f {
            "map_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(33).to_val()
                    ])?;
                }
            }
            "map_unpack_to_linear_memory" => {
                quote! {
                    let vv: Vec<(Val, Val)> = ('1'..='6')
                        .chain('a'..='z')
                        .map(|c| SymbolSmall::try_from_str(c.to_string().repeat(8).as_str()).unwrap().to_val())
                        .zip((100..132u32).into_iter().map(|u| U32Val::from(u).to_val()))
                        .collect();
                    let m = HostMap::from_map(vv, &host).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(m).unwrap().to_val(),
                        U32Val::from(#KEYS_SECTION_START as u32).to_val(),
                        // the only way to make val length too long working is to shift the starting point
                        U32Val::from(#DATA_SECTION_0_START as u32 + 1).to_val(),
                        U32Val::from(32).to_val()
                    ])?;
                }
            }
            "vec_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(33).to_val()
                    ])?;
                }
            }
            "vec_unpack_to_linear_memory" => {
                quote! {
                    // len = 33
                    let vv: Vec<Val> = (100..133u32)
                    .into_iter()
                    .map(|u| U32Val::from(u).to_val())
                    .collect();
                let v = HostVec::from_vec(vv).unwrap();
                    let args = HostVec::from_vec(vec![
                        host.add_host_object(v).unwrap().to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(33).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls = good_impls
        .chain(impls_with_invalid_vals)
        .chain(impls_with_invalid_pos_out_of_range)
        .chain(impls_with_invalid_length_too_long)
        .map(|(wasm_module, test_name, args, assertions)| {
            quote! {
                #[test]
                fn #test_name() -> Result<(), HostError> {
                    let wasm = #wasm_module();
                    let host = observe_host!(Host::test_host_with_recording_footprint());
                    host.enable_debug()?;
                    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
                    // prepare args
                    #args
                    let args = host.add_host_object(args)?;
                    // make the call
                    let res = host.call(
                        contract_id_obj,
                        Symbol::try_from_small_str("loadmem2")?,
                        args,
                    );
                    #assertions
                    Ok(())
                }
            }
        });

    Ok(quote! {
        #(#impls)*
    })
}

pub fn generate_tests_for_bytes() -> Result<TokenStream, Error> {
    let good_impls = BYTE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_good_inputs_ok", f);
        let args = match *f {
            "bytes_new_from_linear_memory"
            | "string_new_from_linear_memory"
            | "symbol_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            "bytes_copy_from_linear_memory" | "bytes_copy_to_linear_memory" => {
                quote! {
                    let bo = host
                        .add_host_object(ScBytes::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        bo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "string_copy_to_linear_memory" => {
                quote! {
                    let so = host
                        .add_host_object(ScString::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        so.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "symbol_copy_to_linear_memory" => {
                quote! {
                    let syo = host
                        .add_host_object(ScSymbol::try_from(vec![5; #SYMBOL_LEN_LIMIT as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        syo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(res.is_ok());
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls_with_not_enough_bytes = BYTE_CASES.iter().filter(|f| {
            BTreeSet::from([
                &"bytes_copy_to_linear_memory",
                &"string_copy_to_linear_memory",
                &"symbol_copy_to_linear_memory",
            ])
            .contains(*f)
        }).map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_not_enough_bytes", f);
        let args = match *f {
            "bytes_copy_to_linear_memory" => {
                quote! {
                    let bo = host
                        .add_host_object(ScBytes::try_from(vec![5; (#DATA_SECTION_LEN - 1) as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        bo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "string_copy_to_linear_memory" => {
                quote! {
                    let so = host
                        .add_host_object(ScString::try_from(vec![5; (#DATA_SECTION_LEN - 1) as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        so.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "symbol_copy_to_linear_memory" => {
                quote! {
                    let syo = host
                        .add_host_object(ScSymbol::try_from(vec![5; (#SYMBOL_LEN_LIMIT - 1) as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        syo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#DATA_SECTION_0_START as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(res, (ScErrorType::Object, ScErrorCode::IndexBounds)));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls_with_pos_oob = BYTE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_bytes_pos_oob", f);
        let args = match *f {
            "bytes_new_from_linear_memory"
            | "string_new_from_linear_memory"
            | "symbol_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            "bytes_copy_from_linear_memory" | "bytes_copy_to_linear_memory" => {
                quote! {
                    let bo = host
                        .add_host_object(ScBytes::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        bo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "string_copy_to_linear_memory" => {
                quote! {
                    let so = host
                        .add_host_object(ScString::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        so.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "symbol_copy_to_linear_memory" => {
                quote! {
                    let syo = host
                        .add_host_object(ScSymbol::try_from(vec![5; #SYMBOL_LEN_LIMIT as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        syo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from(#LM_LENGTH as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls_with_len_oob = BYTE_CASES.iter().map(|f| {
        let wasm_module = format_ident!("wasm_module_calling_{}", f);
        let test_name = format_ident!("test_calling_{}_with_bytes_length_oob", f);
        let args = match *f {
            "bytes_new_from_linear_memory"
            | "string_new_from_linear_memory"
            | "symbol_new_from_linear_memory" => {
                quote! {
                    let args = HostVec::from_vec(vec![
                        U32Val::from((#LM_LENGTH - #SYMBOL_LEN_LIMIT + 1) as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            "bytes_copy_from_linear_memory" | "bytes_copy_to_linear_memory" => {
                quote! {
                    let bo = host
                        .add_host_object(ScBytes::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        bo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from((#DATA_SECTION_0_START + 1) as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "string_copy_to_linear_memory" => {
                quote! {
                    let so = host
                        .add_host_object(ScString::try_from(vec![5; #DATA_SECTION_LEN as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        so.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from((#DATA_SECTION_0_START + 1) as u32).to_val(),
                        U32Val::from(#DATA_SECTION_LEN as u32).to_val()
                    ])?;
                }
            }
            "symbol_copy_to_linear_memory" => {
                quote! {
                    let syo = host
                        .add_host_object(ScSymbol::try_from(vec![5; #SYMBOL_LEN_LIMIT as usize]).unwrap())
                        .unwrap();
                    let args = HostVec::from_vec(vec![
                        syo.to_val(),
                        U32Val::from(0).to_val(),
                        U32Val::from((#LM_LENGTH - #SYMBOL_LEN_LIMIT + 1) as u32).to_val(),
                        U32Val::from(#SYMBOL_LEN_LIMIT as u32).to_val()
                    ])?;
                }
            }
            _ => panic!("malformed slices not implemented for {}", f),
        };

        let assertions = quote! {
            assert!(HostError::result_matches_err(
                res,
                (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
            ));
        };

        (wasm_module, test_name, args, assertions)
    });

    let impls = good_impls
        .chain(impls_with_not_enough_bytes)
        .chain(impls_with_pos_oob)
        .chain(impls_with_len_oob)
        .map(|(wasm_module, test_name, args, assertions)| {
            quote! {
                #[test]
                fn #test_name() -> Result<(), HostError> {
                    let wasm = #wasm_module();
                    let host = observe_host!(Host::test_host_with_recording_footprint());
                    host.enable_debug()?;
                    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
                    // prepare args
                    #args
                    let args = host.add_host_object(args)?;
                    // make the call
                    let res = host.call(
                        contract_id_obj,
                        Symbol::try_from_small_str("loadmem3")?,
                        args,
                    );
                    #assertions
                    Ok(())
                }
            }
        });

    Ok(quote! {
        #(#impls)*
    })
}
