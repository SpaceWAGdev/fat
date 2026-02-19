use crate::{
    ast::Node,
    proof::{self, ProofStep},
};
use anyhow::{Result, bail};
use itertools::Itertools;
use std::{iter::zip, rc::Rc};

#[derive(Debug)]
pub struct Inference {
    pub antecedent: Rc<Vec<Node>>,
    pub consequent: Rc<Node>,
}
#[derive(Debug)]
pub struct InferenceRule {
    pub rule: Inference,
    pub name: String,
}

/// Return type: Vec<Vec<(metavar, objvar)>>
/// Meaning metavar.replace(mapping) gives the new objvar
fn generate_variable_mapping_permutations(
    metavars: &Vec<String>,
    objvars: &Vec<String>,
) -> Result<Vec<Vec<(String, String)>>> {
    anyhow::ensure!(
        metavars.len() == objvars.len(),
        "The number of meta variables and object variables does not match."
    );

    let mut ret: Vec<Vec<(String, String)>> = Vec::new();

    for (_, perm) in metavars.iter().permutations(metavars.len()).enumerate() {
        ret.push(
            perm.into_iter()
                .enumerate()
                .map(|(j, mvar)| (mvar.clone(), objvars[j].clone()))
                .collect(),
        )
    }

    return Ok(ret);
}

impl TryFrom<&ProofStep> for Inference {
    type Error = anyhow::Error;

    fn try_from(value: &ProofStep) -> std::result::Result<Self, Self::Error> {
        match value {
            ProofStep::Axiom(_) => {
                bail!("An axiom, by definition, does not follow from a proof step. ")
            }
            ProofStep::Subproof(_) => bail!("A subproof must be self-contained."),
            ProofStep::Inference {
                antecedents,
                expression,
                rule_name,
            } => Ok(Inference {
                antecedent: Rc::new(
                    antecedents
                        .iter()
                        .map(|step| match step.as_ref() {
                            ProofStep::Axiom(node) => node.to_owned(),
                            ProofStep::Subproof(proof) => proof.get_concluding_expr(),
                            ProofStep::Inference {
                                antecedents,
                                expression,
                                rule_name,
                            } => expression.to_owned(),
                        })
                        .collect_vec(),
                ),
                consequent: Rc::new(expression.to_owned()),
            }),
        }
    }
}

impl Inference {
    fn harvest_variables(&self, rule: &Self) -> Result<Vec<(String, Node)>> {
        let mut ret: Vec<_> = vec![];

        for (oexpr, mexpr) in zip(self.antecedent.as_ref(), rule.antecedent.as_ref()) {
            ret.append(&mut oexpr.harvest_variables(mexpr)?);
        }

        ret.append(
            &mut self
                .consequent
                .harvest_variables(&rule.consequent.as_ref())?,
        );
        // ret.sort();
        // ret.dedup();
        Ok(ret)
    }

    pub fn validate(&self, rule: &InferenceRule) -> Result<()> {
        // TODO: Maybe convert the variables to an easily-comparable type (i.e. not String) for checking inferences?
        // Complexity sort of explodes with all the Vec.sorts()...

        let mappings = self.harvest_variables(&rule.rule)?;

        // self = objectvars; rule = metavars

        if rule.rule.consequent.alpha_replace_all(&mappings) != *self.consequent {
            eprintln!(
                "{} != {}",
                &rule.rule.consequent.alpha_replace_all(&mappings),
                self.consequent
            );
            bail!(
                "{} does not follow from {:?} using {}",
                self.consequent,
                self.antecedent,
                rule.name
            );
        }

        if !self
            .antecedent
            .iter()
            .zip(rule.rule.antecedent.iter())
            .all(|(oa, ma)| *oa == ma.alpha_replace_all(&mappings))
        {
            bail!(
                "{:?} does not follow from {:?} using {}",
                self.consequent,
                self.antecedent,
                rule.name
            );
        }

        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::{self, *};

    #[test]
    fn variable_mappings() {
        let m = vec!["A", "B"].iter().map(|s| s.to_string()).collect();
        let o = vec!["x", "y"].iter().map(|s| s.to_string()).collect();

        assert_eq!(
            generate_variable_mapping_permutations(&m, &o).unwrap(),
            vec![
                vec![
                    ("A".to_string(), "x".to_string()),
                    ("B".to_string(), "y".to_string())
                ],
                vec![
                    ("B".to_string(), "x".to_string()),
                    ("A".to_string(), "y".to_string()),
                ],
            ]
        )
    }

    #[test]
    fn basic_inference() {
        let rule = InferenceRule {
            rule: Inference {
                antecedent: Rc::new(vec![ast::parser::parse_expression("A ^ B").unwrap()]),
                consequent: Rc::new(ast::parser::parse_expression("B ^ A").unwrap()),
            },
            name: { "Example".into() },
        };

        let test_expr = Inference {
            antecedent: Rc::new(vec![ast::parser::parse_expression("X ^ Y").unwrap()]),
            consequent: Rc::new(ast::parser::parse_expression("Y ^ X").unwrap()),
        };

        test_expr.validate(&rule).unwrap()
    }
}
