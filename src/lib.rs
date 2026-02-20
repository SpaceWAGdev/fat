use wasm_bindgen::prelude::wasm_bindgen;

use crate::inference::InferenceRule;

mod ast;
pub mod inference;
pub mod proof;

pub trait AsLaTeX {
    fn as_latex(&self) -> anyhow::Result<String>;
}
pub trait AsTypst {
    fn as_latex(&self) -> anyhow::Result<String>;
}

#[wasm_bindgen]
pub fn parse_and_validate_proof(proof_file: &str, rules_file: &str) -> Result<(), String> {
    let proof = proof::parse_proof(proof_file.into(), None).map_err(|e| e.to_string())?;
    let rules =
        serde_yaml::from_str::<Vec<InferenceRule>>(rules_file).map_err(|e| e.to_string())?;
    proof.validate(&rules).map_err(|e| e.to_string())
}

#[wasm_bindgen]
pub fn render_proof_as_latex(proof_file: &str) -> Result<String, String> {
    let proof = proof::parse_proof(proof_file.into(), None).map_err(|e| e.to_string())?;
    proof.as_latex().map_err(|e| e.to_string())
}
