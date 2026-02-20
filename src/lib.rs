use wasm_bindgen::prelude::wasm_bindgen;

use crate::inference::InferenceRule;

mod ast;
pub mod inference;
pub mod proof;

#[wasm_bindgen]
pub fn parse_and_validate_proof(proof_file: &str, rules_file: &str) -> Result<(), String> {
    let proof = proof::parse_proof(proof_file.into(), None).map_err(|e| e.to_string())?;
    let rules =
        serde_yaml::from_str::<Vec<InferenceRule>>(rules_file).map_err(|e| e.to_string())?;
    proof.validate(&rules).map_err(|e| e.to_string())
}
