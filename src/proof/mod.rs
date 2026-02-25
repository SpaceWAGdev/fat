use crate::{
    ast::{self, Expression},
    inference::{Inference, InferenceRule},
    render::{AsLaTeX, latex},
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

    pub fn conclusion(&self) -> &Expression {
        match &self.conclusion {
            ProofStep::Axiom(node) => node,
            ProofStep::Subproof(proof) => proof.conclusion(),
            ProofStep::Inference {
                antecedents: _,
                expression,
                rule_name: _,
            } => expression,
        }
    }

    pub fn premises(&self) -> Vec<&Expression> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum ProofStep {
    Axiom(Expression),
    Subproof(Box<Proof>),
    Inference {
        antecedents: Vec<Box<ProofStep>>,
        expression: Expression,
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
            if let Ok(num) = cleaned.parse::<usize>() {
                assumption_removals.push(num - 1)
            }
        } else if let Ok(num) = segment.trim().parse::<usize>() {
            refs.push(num - 1)
        }
    }

    (refs, assumption_removals)
}

fn parse_proofline(line: &str, number: usize) -> Result<ProofLine<'_>> {
    let segments: Vec<&str> = line.split("|").collect();
    ensure!(
        segments.len() == 3,
        "Proof line {} is ill-formed.",
        number + 1
    );

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
        // assumption introductions are treated as axioms for now
        "Ax" | "An" | "Pr" => Ok(ProofStep::Axiom(ast::parser::parse_expression(
            line.expression.as_str(),
        )?)),
        "AnB" => Ok(ProofStep::Subproof(Box::new(parse_proof(
            proof.clone(),
            Some(line.number),
        )?))),
        _ => {
            let mut ante: Vec<Box<ProofStep>> = Vec::new();

            for line_ref in line.references {
                let proofline =
                    parse_proofline(proof.split("\n").collect_vec()[line_ref], line_ref)?;
                if proofline.number == line.number {
                    bail!("Line {} depends on itself.", line.number)
                }
                let step = parse_proof_step(proofline, proof)?;
                ante.push(Box::new(step));
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
    let lines: Vec<&str> = proof
        .lines()
        // .filter(|line| !line.trim().starts_with("#")) // comments aren't allowed til i switch to a proper parsing system for proos
        .collect();

    let conclusion: usize;

    if let Some(position) = starting_point {
        conclusion = position;
    } else {
        conclusion = lines
            .iter()
            .position(|line| line.trim().ends_with("QED"))
            .ok_or_else(|| anyhow::format_err!("Proof has no conclusion."))?;
    }

    return Ok(Proof {
        conclusion: parse_proof_step(
            parse_proofline(lines[conclusion].trim_end_matches("QED"), conclusion)?,
            &proof,
        )?,
    });
}

impl AsLaTeX for ProofStep {
    fn as_latex(&self) -> anyhow::Result<String> {
        Ok(match self {
            ProofStep::Axiom(expression) => format!("\\AxiomC{{${}$}}\n", expression.as_latex()?),
            ProofStep::Subproof(proof) => todo!(),
            ProofStep::Inference {
                antecedents,
                expression,
                rule_name,
            } => {
                let mut out: Vec<String> = Vec::new();
                let cmd = match antecedents.len() {
                    1 => "UIC",
                    2 => "BIC",
                    3 => "TIC",
                    4 => "QuaternaryInfC",
                    5 => "QuinaryInfC",
                    _ => {
                        bail!("Can't construct a tree representation for more than 5 antecedents.")
                    }
                };

                for antecedent in antecedents {
                    out.push(antecedent.as_latex()?);
                }

                out.push("\n".into());
                out.push(format!(
                    "\\RL{{${}$}}\n",
                    latex::escape_logic_symbols(rule_name)
                ));
                out.push(format!("\\{cmd}{{${}$}}\n", expression.as_latex()?));
                out.join("\n")
            }
        })
    }
}

impl AsLaTeX for Proof {
    fn as_latex(&self) -> anyhow::Result<String> {
        Ok(format!(
            "\\begin{{prooftree}}\n{}\n\\end{{prooftree}}",
            self.conclusion.as_latex()?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_parse() {
        let raw = r#"Ax | A |
Ax | B | 1 QED
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
        Example | B | 1 QED
        "#;
        let proof = parse_proof(raw.to_string(), None).unwrap();
        let rules = vec![InferenceRule {
            name: "Example".into(),
            rule: Inference {
                antecedent: vec![ast::parser::parse_expression("X").unwrap()],
                consequent: Box::new(ast::parser::parse_expression("Y").unwrap()),
            },
        }];
        proof.validate(&rules).unwrap();
    }
}
