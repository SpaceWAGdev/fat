use crate::{ast::Expression, proof::ProofStep};
use anyhow::{Result, bail};
use itertools::Itertools;
use std::{collections::HashMap, iter::zip, rc::Rc};

#[derive(Debug)]
pub struct Inference {
    pub antecedent: Vec<Expression>,
    pub consequent: Box<Expression>,
}
#[derive(Debug)]
pub struct InferenceRule {
    pub rule: Inference,
    pub name: String,
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
                rule_name: _,
            } => Ok(Inference {
                antecedent: antecedents
                    .iter()
                    .map(|step| match step.as_ref() {
                        ProofStep::Axiom(node) => node.to_owned(),
                        ProofStep::Subproof(proof) => proof.conclusion().clone(),
                        ProofStep::Inference {
                            antecedents: _,
                            expression,
                            rule_name: _,
                        } => expression.to_owned(),
                    })
                    .collect_vec(),
                consequent: Box::new(expression.to_owned()),
            }),
        }
    }
}

impl Inference {
    fn harvest_variables(&self, rule: &Self) -> Result<HashMap<String, Expression>> {
        let mut ret: HashMap<String, Expression> = HashMap::new();

        for (oexpr, mexpr) in zip(&self.antecedent, &rule.antecedent) {
            ret = ret
                .into_iter()
                .merge(Expression::harvest_variables(&oexpr, &mexpr)?)
                .collect();
        }

        ret = ret
            .into_iter()
            .merge(self.consequent.harvest_variables(&rule.consequent)?)
            .collect();
        // ret.sort();
        // ret.dedup();
        Ok(ret)
    }

    pub fn validate(&self, rule: &InferenceRule) -> Result<()> {
        // TODO: Maybe convert the variables to an easily-comparable type (i.e. not String) for checking inferences?
        // Complexity sort of explodes with all the Vec.sorts()...

        let mappings = self.harvest_variables(&rule.rule)?;

        let alpha_replaced_consquent = rule.rule.consequent.clone().alpha_replace_all(&mappings);

        if alpha_replaced_consquent != *self.consequent {
            eprintln!("{} != {}", alpha_replaced_consquent, self.consequent);
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
            .all(|(oa, ma)| *oa == ma.clone().alpha_replace_all(&mappings))
        {
            bail!(
                "{:?} does not follow from {:?} using {}",
                self.consequent,
                self.antecedent,
                rule.name
            );
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast;

    #[test]
    fn basic_inference() {
        let rule = InferenceRule {
            rule: Inference {
                antecedent: vec![ast::parser::parse_expression("A ^ B").unwrap()],
                consequent: Box::new(ast::parser::parse_expression("B ^ A").unwrap()),
            },
            name: { "Example".into() },
        };

        let test_expr = Inference {
            antecedent: vec![ast::parser::parse_expression("X ^ Y").unwrap()],
            consequent: Box::new(ast::parser::parse_expression("Y ^ X").unwrap()),
        };

        test_expr.validate(&rule).unwrap()
    }
}
