use std::rc::Rc;

use crate::{
    ast::{self, Node},
    inference::InferenceRule,
};
use anyhow::{Result, ensure};
use itertools::Itertools;

pub struct Proof {
    conclusion: ProofStep,
}

pub enum ProofStep {
    Axiom(Node),
    Subproof(Rc<Proof>),
    Inference {
        antecedents: Vec<Rc<ProofStep>>,
        expression: Node,
        rule_name: String,
    },
}

struct ProofLine<'a> {
    rulename: &'a str,
    expression: &'a str,
    references: Vec<usize>,
    assumption_removals: Vec<usize>,
    number: usize,
}
/// Return: (inference refs, assumption removals)
fn parse_numeric_references(line: &str) -> Result<(Vec<usize>, Vec<usize>)> {
    let mut refs: Vec<usize> = Vec::new();
    let mut assumption_removals: Vec<usize> = Vec::new();

    for segment in line.split(",") {
        if segment.trim().starts_with("(") {
            let cleaned = segment.trim_matches(['(', ')']);
            assumption_removals.push(segment.parse()?);
        } else {
            refs.push(segment.trim().parse()?);
        }
    }

    return Ok((refs, assumption_removals));
}

fn parse_proofline(line: &str, number: usize) -> Result<ProofLine> {
    let segments: Vec<&str> = line.split("|").collect();
    ensure!(segments.len() == 3, "Inference line is ill-formed.");

    let (refs, assumption_removals) = parse_numeric_references(segments[2])?;

    return Ok(ProofLine {
        expression: segments[1],
        references: refs,
        assumption_removals: assumption_removals,
        rulename: segments[0],
        number: number,
    });
}

fn parse_proof_step(line: ProofLine, proof: &String) -> Result<ProofStep> {
    match line.rulename {
        "Ax" => Ok(ProofStep::Axiom(ast::parser::parse_expression(
            line.expression,
        )?)),
        "AnE" => Ok(ProofStep::Subproof(Rc::new(parse_proof(
            proof.to_owned(),
            Some(line.number),
        )?))),
        _ => {
            let mut ante: Vec<Rc<ProofStep>> = Vec::new();

            for line_ref in line.references {
                let proofline =
                    parse_proofline(proof.split("\n").collect_vec()[line_ref], line_ref)?;
                let step = parse_proof_step(proofline, &proof)?;
                ante.push(Rc::new(step));
            }

            Ok(ProofStep::Inference {
                antecedents: ante,
                expression: ast::parser::parse_expression(line.expression)?,
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
        conclusion: parse_proof_step(parse_proofline(lines[conclusion], conclusion)?, &proof)?,
    });
}

pub fn validate_proof(proof: Proof, rules: Vec<InferenceRule>) -> Result<()> {
    unimplemented!()
}
