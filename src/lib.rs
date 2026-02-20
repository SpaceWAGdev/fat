use wasm_bindgen::prelude::wasm_bindgen;

use crate::inference::InferenceRule;

mod ast;
pub mod inference;
pub mod proof;

#[wasm_bindgen]
pub fn parse_and_validate_proof(proof_file: &str, rules_file: &str) -> bool {
    let proof = proof::parse_proof(proof_file.into(), None);
    let rules = serde_yaml::from_str::<Vec<InferenceRule>>(rules_file);
    if let (Ok(p), Ok(r)) = (proof, rules) {
        p.validate(&r).is_ok()
    } else {
        false
    }
}
