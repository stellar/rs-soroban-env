use crate::{budget::Budget, xdr, HostError};
use soroban_env_common::xdr::{
    ScSpecFunctionV0, ScSpecTypeDef, ScSpecUdtEnumCaseV0, ScSpecUdtEnumV0,
    ScSpecUdtErrorEnumCaseV0, ScSpecUdtErrorEnumV0, ScSpecUdtStructFieldV0, ScSpecUdtStructV0,
    ScSpecUdtUnionCaseV0, ScSpecUdtUnionV0,
};
use std::collections::HashMap;
use xdr::ScSpecEntry;

// This module exists to perform a type judgment on specs. Specifically it provides
// the judgment of whether one Vec<ScSpecEntry> (which is what's stapled to the front
// of any wasm contract) is a subtype of another Vec<ScSpecEntry>, such that the former
// can be safely used to update / overwrite / extend the latter.
//
// The idea here is to make it possible to evolve deployed smart contracts with new
// versions of themselves without breaking old clients. The easiest way to do this
// is to just take whatever the first interface that's uploaded is as a constraint
// for any subsequent updates: a new contract has to be a subtype of the old contract.

trait Named {
    fn get_name(&self) -> &[u8];
}

impl Named for ScSpecEntry {
    fn get_name(&self) -> &[u8] {
        match self {
            ScSpecEntry::FunctionV0(f) => f.name.as_ref(),
            ScSpecEntry::UdtStructV0(s) => s.name.as_ref(),
            ScSpecEntry::UdtUnionV0(u) => u.name.as_ref(),
            ScSpecEntry::UdtEnumV0(e) => e.name.as_ref(),
            ScSpecEntry::UdtErrorEnumV0(e) => e.name.as_ref(),
        }
    }
}

fn to_named_map<N: Named>(v: &Vec<N>) -> HashMap<&[u8], &N> {
    v.iter().map(|n| (n.get_name(), n)).collect()
}

type SpecEnv<'e> = HashMap<&'e [u8], &'e ScSpecEntry>;

struct JudgmentEnv<'env, 'budget> {
    new_env: &'env SpecEnv<'env>,
    old_env: &'env SpecEnv<'env>,
    budget: &'budget Budget,
}

pub fn judge_env_subtypes<'env, 'budget>(
    new_entries: &'env Vec<ScSpecEntry>,
    old_entries: &'env Vec<ScSpecEntry>,
    budget: &'budget Budget,
) -> Result<bool, HostError> {
    let new_env = &to_named_map(new_entries);
    let old_env = &to_named_map(old_entries);
    if new_env.len() != new_entries.len() || old_env.len() != old_entries.len() {
        return Err(xdr::ScHostFnErrorCode::InputArgsInvalid.into());
    }
    JudgmentEnv {
        new_env,
        old_env,
        budget,
    }
    .judge_env_subtypes()
}

impl<'env, 'budget> JudgmentEnv<'env, 'budget> {
    fn swap_environments(&self) -> Self {
        Self {
            new_env: self.old_env,
            old_env: self.new_env,
            budget: self.budget,
        }
    }

    fn judge_env_subtypes(&self) -> Result<bool, HostError> {
        for (old_key, old_entry) in self.old_env.iter() {
            match self.new_env.get(old_key) {
                None => return Ok(false),
                Some(new_entry) => {
                    if !self.judge_subtype(*new_entry, *old_entry)? {
                        return Ok(false);
                    }
                }
            }
        }
        Ok(true)
    }

    fn judge_supertype<T>(&self, new: &T, old: &T) -> Result<bool, HostError>
    where
        Self: JudgeSubtype<T>,
    {
        self.swap_environments().judge_subtype(old, new)
    }

    fn judge_eqtype<T>(&self, new: &T, old: &T) -> Result<bool, HostError>
    where
        Self: JudgeSubtype<T>,
    {
        Ok(self.judge_subtype(new, old)? && self.judge_supertype(new, old)?)
    }
}

trait JudgeSubtype<T> {
    /// Return Ok(true) iff `new` is a subtype of `old` according to `self`
    /// (which is presumed to contain new and old type environments)
    fn judge_subtype(&self, new: &T, old: &T) -> Result<bool, HostError>;
}

impl<'env, 'budget> JudgeSubtype<ScSpecEntry> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(&self, new: &ScSpecEntry, old: &ScSpecEntry) -> Result<bool, HostError> {
        use ScSpecEntry::*;
        match (new, old) {
            (FunctionV0(new_e), FunctionV0(old_e)) => self.judge_subtype(new_e, old_e),
            (UdtStructV0(new_e), UdtStructV0(old_e)) => self.judge_subtype(new_e, old_e),
            (UdtUnionV0(new_e), UdtUnionV0(old_e)) => self.judge_subtype(new_e, old_e),
            (UdtEnumV0(new_e), UdtEnumV0(old_e)) => self.judge_subtype(new_e, old_e),
            (UdtErrorEnumV0(new_e), UdtErrorEnumV0(old_e)) => self.judge_subtype(new_e, old_e),
            _ => Ok(false),
        }
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecFunctionV0> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(
        &self,
        new: &ScSpecFunctionV0,
        old: &ScSpecFunctionV0,
    ) -> Result<bool, HostError> {
        if new.inputs.len() != old.inputs.len() {
            return Ok(false);
        }
        for (new_in, old_in) in new.inputs.iter().zip(old.inputs.iter()) {
            if !self.judge_supertype(&new_in.type_, &old_in.type_)? {
                return Ok(false);
            }
        }
        for (new_out, old_out) in new.outputs.iter().zip(old.outputs.iter()) {
            if !self.judge_subtype(new_out, old_out)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl Named for ScSpecUdtStructFieldV0 {
    fn get_name(&self) -> &[u8] {
        self.name.as_ref()
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecUdtStructV0> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(
        &self,
        new: &ScSpecUdtStructV0,
        old: &ScSpecUdtStructV0,
    ) -> Result<bool, HostError> {
        if new.fields.len() < old.fields.len() {
            return Ok(false);
        }

        // NB: if we change to a vector-representation of structs, we need to
        // use an ordered comparison here not a named comparison.

        let old_fields = to_named_map(old.fields.as_vec());
        for new_field in new.fields.iter() {
            let Some(old_field) = old_fields.get(new_field.get_name()) else { return Ok(false) };
            if !self.judge_subtype(&new_field.type_, &old_field.type_)? {
                return Ok(false);
            }
        }

        // This is the ordered version:
        /*
        for (new_field, old_field) in new.fields.iter().zip(old.fields.iter()) {
            if !self.judge_subtype(&new_field.type_, &old_field.type_)? {
                return Ok(false)
            }
        }
         */
        Ok(true)
    }
}

impl Named for ScSpecUdtUnionCaseV0 {
    fn get_name(&self) -> &[u8] {
        match self {
            ScSpecUdtUnionCaseV0::VoidV0(v) => v.name.as_ref(),
            ScSpecUdtUnionCaseV0::TupleV0(t) => t.name.as_ref(),
        }
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecUdtUnionV0> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(
        &self,
        new: &ScSpecUdtUnionV0,
        old: &ScSpecUdtUnionV0,
    ) -> Result<bool, HostError> {
        let old_cases = to_named_map(old.cases.as_vec());
        for new_case in new.cases.iter() {
            let Some(old_case) = old_cases.get(new_case.get_name()) else { return Ok(false) };
            match (new_case, old_case) {
                (ScSpecUdtUnionCaseV0::VoidV0(_), ScSpecUdtUnionCaseV0::VoidV0(_)) => (),
                (ScSpecUdtUnionCaseV0::VoidV0(_), ScSpecUdtUnionCaseV0::TupleV0(_))
                | (ScSpecUdtUnionCaseV0::TupleV0(_), ScSpecUdtUnionCaseV0::VoidV0(_)) => {
                    return Ok(false)
                }
                (
                    ScSpecUdtUnionCaseV0::TupleV0(new_tup),
                    ScSpecUdtUnionCaseV0::TupleV0(old_tup),
                ) => {
                    if new_tup.type_.len() != old_tup.type_.len() {
                        return Ok(false);
                    }
                    for (new_elt, old_elt) in new_tup.type_.iter().zip(old_tup.type_.iter()) {
                        if !self.judge_subtype(new_elt, old_elt)? {
                            return Ok(false);
                        }
                    }
                }
            }
        }
        Ok(true)
    }
}

impl Named for ScSpecUdtEnumCaseV0 {
    fn get_name(&self) -> &[u8] {
        self.name.as_ref()
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecUdtEnumV0> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(
        &self,
        new: &ScSpecUdtEnumV0,
        old: &ScSpecUdtEnumV0,
    ) -> Result<bool, HostError> {
        let old_cases = to_named_map(old.cases.as_vec());
        for new_case in new.cases.iter() {
            let Some(old_case) = old_cases.get(new_case.get_name()) else { return Ok(false) };
            if old_case.value != new_case.value {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl Named for ScSpecUdtErrorEnumCaseV0 {
    fn get_name(&self) -> &[u8] {
        self.name.as_ref()
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecUdtErrorEnumV0> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(
        &self,
        new: &ScSpecUdtErrorEnumV0,
        old: &ScSpecUdtErrorEnumV0,
    ) -> Result<bool, HostError> {
        let old_cases = to_named_map(old.cases.as_vec());
        for new_case in new.cases.iter() {
            let Some(old_case) = old_cases.get(new_case.get_name()) else { return Ok(false) };
            if old_case.value != new_case.value {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl<'env, 'budget> JudgeSubtype<ScSpecTypeDef> for JudgmentEnv<'env, 'budget> {
    fn judge_subtype(&self, new: &ScSpecTypeDef, old: &ScSpecTypeDef) -> Result<bool, HostError> {
        use ScSpecTypeDef::*;
        match (new, old) {
            (Val, Val)
            | (Bool, Bool)
            | (Void, Void)
            | (Status, Status)
            | (U32, U32)
            | (I32, I32)
            | (U64, U64)
            | (I64, I64)
            | (Timepoint, Timepoint)
            | (Duration, Duration)
            | (U128, U128)
            | (I128, I128)
            | (U256, U256)
            | (I256, I256)
            | (Bytes, Bytes)
            | (String, String)
            | (Symbol, Symbol)
            | (Address, Address) => Ok(true),

            (Val, _)
            | (Bool, _)
            | (Void, _)
            | (Status, _)
            | (U32, _)
            | (I32, _)
            | (U64, _)
            | (I64, _)
            | (Timepoint, _)
            | (Duration, _)
            | (U128, _)
            | (I128, _)
            | (U256, _)
            | (I256, _)
            | (Bytes, _)
            | (String, _)
            | (Symbol, _)
            | (Address, _) => Ok(false),

            (Option(new_opt), Option(old_opt)) => {
                self.judge_subtype(&*new_opt.value_type, &*old_opt.value_type)
            }
            (Option(_), _) => Ok(false),

            (Result(new_res), Result(old_res)) => Ok(self
                .judge_subtype(&*new_res.ok_type, &*old_res.ok_type)?
                && self.judge_subtype(&*new_res.error_type, &*old_res.error_type)?),
            (Result(_), _) => Ok(false),

            (Vec(new_vec), Vec(old_vec)) => {
                self.judge_subtype(&*new_vec.element_type, &*old_vec.element_type)
            }
            (Vec(_), _) => Ok(false),

            (Map(new_map), Map(old_map)) => Ok(self
                .judge_supertype(&*new_map.key_type, &*old_map.key_type)?
                && self.judge_subtype(&*new_map.value_type, &*old_map.value_type)?),
            (Map(_), _) => Ok(false),

            (Set(new_set), Set(old_set)) => {
                self.judge_subtype(&*new_set.element_type, &*old_set.element_type)
            }
            (Set(_), _) => Ok(false),

            (Tuple(new_tup), Tuple(old_tup)) => {
                if new_tup.value_types.len() != old_tup.value_types.len() {
                    return Ok(false);
                }
                for (new_elt, old_elt) in new_tup.value_types.iter().zip(old_tup.value_types.iter())
                {
                    if !self.judge_subtype(new_elt, old_elt)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Tuple(_), _) => Ok(false),

            (BytesN(new_bytes), BytesN(old_bytes)) => Ok(new_bytes.n == old_bytes.n),
            (BytesN(_), _) => Ok(false),

            (Udt(new_udt), Udt(old_udt)) => {
                match (
                    self.new_env.get::<[u8]>(new_udt.name.as_ref()),
                    self.new_env.get::<[u8]>(new_udt.name.as_ref()),
                ) {
                    (Some(new_udt), Some(old_udt)) => self.judge_subtype(*new_udt, *old_udt),
                    _ => Err(xdr::ScHostFnErrorCode::InputArgsInvalid.into()),
                }
            }
            (Udt(_), _) => Ok(false),
        }
    }
}
