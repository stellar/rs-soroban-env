use crate::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use soroban_env_host::{
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, Error, Host, HostError, MapObject, Symbol, SymbolStr, TryFromVal, U32Val, Val,
    VecObject,
};
use soroban_synth_wasm::{Arity, ModEmitter};

type MapEntry = (String, ModelValue);

#[derive(Arbitrary, Clone, Debug, PartialEq)]
enum ModelValue {
    U32(u32),
    Void,
}

#[derive(Debug, PartialEq)]
enum Oracle {
    ConstructionReject {
        expected_error: Error,
    },
    UnpackReject {
        expected_map: Vec<MapEntry>,
        expected_error: Error,
    },
    Success {
        expected_map: Vec<MapEntry>,
        expected_output: Vec<ModelValue>,
    },
}

#[derive(Debug)]
struct Scenario {
    construction_entries: Vec<MapEntry>,
    query: Vec<String>,
    oracle: Oracle,
}

const CHOICES: &[u8; 63] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

fn random_symbol_string<'a>(u: &mut Unstructured<'a>, len: u8) -> arbitrary::Result<String> {
    let mut sym: Vec<u8> = Vec::with_capacity(len as usize);
    for _ in 0..len {
        sym.push(*u.choose(CHOICES)?);
    }
    Ok(sym.try_into().expect("Vec<u8> contained invalid UTF-8."))
}

impl<'a> Arbitrary<'a> for Scenario {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let (mut construction_entries, mut query) = generate_valid_inputs(u)?;
        mutate(u, &mut construction_entries, &mut query)?;
        let oracle = predict(&construction_entries, &query);
        Ok(Scenario {
            construction_entries,
            query,
            oracle,
        })
    }
}

fn generate_valid_inputs(
    u: &mut Unstructured<'_>,
) -> arbitrary::Result<(Vec<MapEntry>, Vec<String>)> {
    let pool_size: u8 = u.int_in_range(0..=20)?;
    let mut keys_pool = (0..pool_size)
        .map(|_| {
            let sym_len = u.int_in_range(0..=32)?;
            random_symbol_string(u, sym_len)
        })
        .collect::<arbitrary::Result<Vec<_>>>()?;
    keys_pool.sort();
    keys_pool.dedup();

    // Independent membership gives queries both matching and absent keys,
    // while preserving the strict ordering required by the host.
    let mut construction_entries = Vec::new();
    let mut query = Vec::new();
    for key in keys_pool {
        if bool::arbitrary(u)? {
            construction_entries.push((key.clone(), ModelValue::arbitrary(u)?));
        }
        if bool::arbitrary(u)? {
            query.push(key);
        }
    }

    Ok((construction_entries, query))
}

// Select at most one mutation. Half of the choices leave the valid scenario
// unchanged. Mutations with too few entries are harmless no-ops; the model
// classifies the final inputs rather than assuming a mutation caused failure.
fn mutate(
    u: &mut Unstructured<'_>,
    entries: &mut Vec<MapEntry>,
    query: &mut Vec<String>,
) -> arbitrary::Result<()> {
    match u.int_in_range(0..=15u8)? {
        0 if entries.len() >= 2 => {
            let i = u.int_in_range(0..=entries.len() - 2)?;
            entries.swap(i, i + 1);
        }
        1 if !entries.is_empty() => {
            let i = u.int_in_range(0..=entries.len() - 1)?;
            entries.insert(i, entries[i].clone());
        }
        2 | 3 => {
            let key = if entries.is_empty() {
                entries.push((String::new(), ModelValue::arbitrary(u)?));
                &mut entries[0].0
            } else {
                let i = u.int_in_range(0..=entries.len() - 1)?;
                &mut entries[i].0
            };
            if u.arbitrary::<bool>()? {
                key.push('!');
            } else {
                *key = "a".repeat(33);
            }
        }
        4 if query.len() >= 2 => {
            let i = u.int_in_range(0..=query.len() - 2)?;
            query.swap(i, i + 1);
        }
        5 if !query.is_empty() => {
            let i = u.int_in_range(0..=query.len() - 1)?;
            query.insert(i, query[i].clone());
        }
        6 | 7 => {
            // Invalid Symbol strings are legal query keys and must miss.
            query.push(if u.arbitrary::<bool>()? {
                "!".into()
            } else {
                "a".repeat(33)
            });
            query.sort();
            query.dedup();
        }
        _ => {}
    }
    Ok(())
}

// Model the sparse operations with ordinary Rust collections, independently
// of host values and the code that observes the host's results.
fn predict(construction_entries: &[MapEntry], query: &[String]) -> Oracle {
    // Validate every construction key, including keys whose values are Void.
    let mut previous: Option<&str> = None;
    for (key, _) in construction_entries {
        if key.len() > 32 || !key.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
            return Oracle::ConstructionReject {
                expected_error: Error::from_type_and_code(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                ),
            };
        }
        if previous.is_some_and(|prev| prev >= key.as_str()) {
            return Oracle::ConstructionReject {
                expected_error: Error::from_type_and_code(
                    ScErrorType::Object,
                    ScErrorCode::InvalidInput,
                ),
            };
        }
        previous = Some(key);
    }
    let map: std::collections::BTreeMap<_, _> = construction_entries
        .iter()
        .filter(|(_, value)| *value != ModelValue::Void)
        .cloned()
        .collect();
    let expected_map = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    if query.windows(2).any(|w| w[0] >= w[1]) {
        return Oracle::UnpackReject {
            expected_map,
            expected_error: Error::from_type_and_code(
                ScErrorType::Object,
                ScErrorCode::InvalidInput,
            ),
        };
    }
    let expected_output = query
        .iter()
        .map(|key| map.get(key).cloned().unwrap_or(ModelValue::Void))
        .collect();
    Oracle::Success {
        expected_map,
        expected_output,
    }
}

fn encode_value(value: &ModelValue) -> Val {
    match value {
        ModelValue::U32(n) => (*n).into(),
        ModelValue::Void => Val::VOID.into(),
    }
}

fn decode_value(value: Val) -> ModelValue {
    if value.is_void() {
        ModelValue::Void
    } else {
        ModelValue::U32(
            u32::try_from(value).expect("host returned a value outside the model domain"),
        )
    }
}

// Enumerate the entire map independently of the sparse unpacker, so omitted
// entries and accidentally retained Void entries cannot be hidden by queries.
fn read_map(host: &Host, map: MapObject) -> Vec<MapEntry> {
    let len: u32 = host.map_len(map).unwrap().into();
    (0..len)
        .map(|i| {
            let key = host.map_key_by_pos(map, i.into()).unwrap();
            let symbol = Symbol::try_from_val(host, &key).expect("map key must be a Symbol");
            let key = SymbolStr::try_from_val(host, &symbol).unwrap().to_string();
            let val = host.map_val_by_pos(map, i.into()).unwrap();
            (key, decode_value(val))
        })
        .collect()
}

// Append key bytes followed by their little-endian (pointer, length) descriptors.
fn append_keys<'a>(memory: &mut Vec<u8>, keys: impl Iterator<Item = &'a str>) -> U32Val {
    let mut descriptors = Vec::new();
    for key in keys {
        descriptors.extend_from_slice(&(memory.len() as u32).to_le_bytes());
        descriptors.extend_from_slice(&(key.len() as u32).to_le_bytes());
        memory.extend_from_slice(key.as_bytes());
    }
    let pos = memory.len() as u32;
    memory.extend(descriptors);
    pos.into()
}

fn scenario_wasm(scenario: &Scenario) -> Vec<u8> {
    let mut memory = Vec::new();
    let keys_pos = append_keys(
        &mut memory,
        scenario
            .construction_entries
            .iter()
            .map(|(key, _)| key.as_str()),
    );
    let vals_pos = U32Val::from(memory.len() as u32);
    for (_, value) in &scenario.construction_entries {
        memory.extend_from_slice(&encode_value(value).get_payload().to_le_bytes());
    }
    let query_pos = append_keys(&mut memory, scenario.query.iter().map(String::as_str));
    let output_pos = U32Val::from(memory.len() as u32);
    for _ in &scenario.query {
        memory.extend_from_slice(&Val::from(false).get_payload().to_le_bytes());
    }

    let pages = memory.len() as u32 / 0x10_000 + 1;
    let mut module = ModEmitter::from_configs(pages, 0);
    module.define_data_segment(0, memory);
    // Named emitters re-import, so save these imports for direct calls after construct.
    let sparse_unpack = module.import_func("m", "c", Arity(4));
    let vec_from_memory = module.import_func("v", "g", Arity(2));
    let mut construct = module.func(Arity(0), 0);
    construct.sparse_map_new_from_linear_memory(
        keys_pos,
        vals_pos,
        U32Val::from(scenario.construction_entries.len() as u32),
    );
    let module = construct.finish_and_export("construct");

    let mut unpack = module.func(Arity(1), 0);
    let map = unpack.args[0].0;
    let query_len = U32Val::from(scenario.query.len() as u32);
    unpack
        .push(map)
        .push(query_pos)
        .push(output_pos)
        .push(query_len)
        .call_func(sparse_unpack)
        .drop();
    unpack
        .push(output_pos)
        .push(query_len)
        .call_func(vec_from_memory);
    unpack.finish_and_export("unpack").finish()
}

fn run_wasm_scenario(scenario: &Scenario) {
    let host = Host::test_host_with_recording_footprint();
    let contract = host.register_test_contract_wasm(&scenario_wasm(scenario));
    let constructed_map = host
        .call(
            contract,
            Symbol::try_from_small_str("construct").unwrap(),
            host.vec_new_from_slice(&[]).unwrap(),
        )
        .map(|value| MapObject::try_from(value).expect("construct must return a map"));

    check_scenario(&host, scenario, constructed_map, |map| {
        let result = host.call(
            contract,
            Symbol::try_from_small_str("unpack").unwrap(),
            host.vec_new_from_slice(&[map.into()])?,
        )?;
        let values = VecObject::try_from(result).expect("unpack must return a vector");
        let len: u32 = host.vec_len(values)?.into();
        (0..len)
            .map(|i| host.vec_get(values, i.into()).map(decode_value))
            .collect()
    });
}

/// Check sparse map semantics through Wasm linear memory against an independent
/// prediction for results and rejections.
/// Memory layouts are valid; malformed pointers and object-valued fields are
/// outside this target.
pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let mut u = Unstructured::new(data);
    let scenario = match Scenario::arbitrary(&mut u) {
        Ok(s) => s,
        Err(_) => return FuzzResult::Reject,
    };

    run_wasm_scenario(&scenario);
    FuzzResult::Ok
}

fn check_scenario(
    host: &Host,
    scenario: &Scenario,
    constructed_map: Result<MapObject, HostError>,
    unpack: impl FnOnce(MapObject) -> Result<Vec<ModelValue>, HostError>,
) {
    match &scenario.oracle {
        Oracle::ConstructionReject { expected_error } => {
            let err = constructed_map.expect_err("Host should reject construction of this map.");
            assert_eq!(err.error, *expected_error, "unexpected rejection: {err:?}");
        }

        Oracle::UnpackReject {
            expected_map,
            expected_error,
        } => {
            let map = constructed_map.expect("Host should have accepted map construction.");
            assert_eq!(&read_map(host, map), expected_map);
            let err = unpack(map).expect_err("Host should reject map unpacking");
            assert_eq!(err.error, *expected_error, "unexpected rejection: {err:?}");
        }

        Oracle::Success {
            expected_map,
            expected_output,
        } => {
            let map = constructed_map.expect("Host should have accepted map construction.");
            assert_eq!(&read_map(host, map), expected_map);
            let entries = unpack(map).expect("Host should have unpacked the requested map.");
            assert_eq!(&entries, expected_output);
        }
    }
}
