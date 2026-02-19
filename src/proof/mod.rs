use std::rc::Rc;

use crate::{
    ast::{self, Node},
    inference::{Inference, InferenceRule},
};
use anyhow::{Result, bail, ensure};
use itertools::Itertools;

#[derive(Debug)]
pub struct Proof {
    pub conclusion: ProofStep,
}

impl Proof {
    pub fn validate(&self, rules: &Vec<InferenceRule>) -> Result<()> {
        self.conclusion.validate(rules)
    }

    pub fn get_concluding_expr(&self) -> Node {
        match &self.conclusion {
            ProofStep::Axiom(node) => node.to_owned(),
            ProofStep::Subproof(proof) => proof.get_concluding_expr(),
            ProofStep::Inference {
                antecedents: _,
                expression,
                rule_name: _,
            } => expression.to_owned(),
        }
    }
}

#[derive(Debug)]
pub enum ProofStep {
    Axiom(Node),
    Subproof(Rc<Proof>),
    Inference {
        antecedents: Vec<Rc<ProofStep>>,
        expression: Node,
        rule_name: String,
    },
}

impl ProofStep {
    pub fn validate(&self, rules: &Vec<InferenceRule>) -> Result<()> {
        match self {
            ProofStep::Axiom(_) => Ok(()),
            ProofStep::Subproof(proof) => proof.validate(rules),
            ProofStep::Inference {
                antecedents,
                expression: _,
                rule_name,
            } => {
                let ir = rules
                    .iter()
                    .find(|rule: &&InferenceRule| rule.name == rule_name.as_str());

                let inference = Inference::try_from(self)?;

                match ir {
                    Some(rule) => inference.validate(rule)?,
                    None => bail!(
                        "Rule {rule_name} is not defined. It may be excluded from your proof."
                    ),
                };

                for antecedent in antecedents {
                    antecedent.as_ref().validate(rules)?
                }

                Ok(())
            }
        }
    }
}

struct ProofLine<'a> {
    rulename: &'a str,
    expression: String,
    references: Vec<usize>,
    assumption_removals: Vec<usize>,
    number: usize,
}
/// Return: (inference refs, assumption removals)
fn parse_numeric_references(line: &str) -> (Vec<usize>, Vec<usize>) {
    let mut refs: Vec<usize> = Vec::new();
    let mut assumption_removals: Vec<usize> = Vec::new();

    for segment in line.split(",") {
        if segment.trim().starts_with("(") {
            let cleaned = segment.trim_matches(['(', ')']);
            if let Ok(num) = cleaned.parse() {
                assumption_removals.push(num)
            }
        } else if let Ok(num) = segment.trim().parse() {
            refs.push(num)
        }
    }

    (refs, assumption_removals)
}

fn parse_proofline(line: &str, number: usize) -> Result<ProofLine<'_>> {
    let segments: Vec<&str> = line.split("|").collect();
    ensure!(segments.len() == 3, "Proof line {number} is ill-formed.");

    let (refs, assumption_removals) = parse_numeric_references(segments[2]);
    let expr = format!("({})", segments[1].trim());
    Ok(ProofLine {
        expression: expr,
        references: refs,
        assumption_removals,
        rulename: segments[0].trim(),
        number,
    })
}

fn parse_proof_step(line: ProofLine, proof: &String) -> Result<ProofStep> {
    match line.rulename {
        "Ax" => Ok(ProofStep::Axiom(ast::parser::parse_expression(
            line.expression.as_str(),
        )?)),
        "AnB" => Ok(ProofStep::Subproof(Rc::new(parse_proof(
            proof.to_owned(),
            Some(line.number),
        )?))),
        _ => {
            let mut ante: Vec<Rc<ProofStep>> = Vec::new();

            for line_ref in line.references {
                let proofline =
                    parse_proofline(proof.split("\n").collect_vec()[line_ref], line_ref)?;
                if proofline.number == line.number {
                    bail!("Line {} depends on itself.", line.number)
                }
                let step = parse_proof_step(proofline, proof)?;
                ante.push(Rc::new(step));
            }

            Ok(ProofStep::Inference {
                antecedents: ante,
                expression: ast::parser::parse_expression(line.expression.as_str())?,
                rule_name: line.rulename.to_owned(),
            })
        }
    }
}

pub fn parse_proof(proof: String, starting_point: Option<usize>) -> Result<Proof> {
    let lines: Vec<&str> = proof.lines().collect();

    let conclusion: usize;

    if let Some(position) = starting_point {
        conclusion = position;
    } else {
        conclusion = lines
            .iter()
            .position(|line| line.ends_with("QED"))
            .ok_or_else(|| anyhow::format_err!("Proof has no conclusion."))?;
    }

    return Ok(Proof {
        conclusion: parse_proof_step(
            parse_proofline(lines[conclusion].trim_end_matches("QED"), conclusion)?,
            &proof,
        )?,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_parse() {
        let raw = r#"Ax | A |
Ax | B | 0 QED
"#;
        let proof = parse_proof(raw.to_string(), None).unwrap();
        if let ProofStep::Axiom(_) = proof.conclusion {
        } else {
            panic!("{:?}", proof)
        }
    }

    #[test]
    fn validate() {
        let raw = r#"Ax | A |
        Example | B | 0 QED
        "#;
        let proof = parse_proof(raw.to_string(), None).unwrap();
        let rules = vec![InferenceRule {
            name: "Example".into(),
            rule: Inference {
                antecedent: Rc::new(vec![ast::parser::parse_expression("X").unwrap()]),
                consequent: Rc::new(ast::parser::parse_expression("Y").unwrap()),
            },
        }];
        proof.validate(&rules).unwrap();
    }
}
