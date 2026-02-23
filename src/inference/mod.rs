use crate::{AsLaTeX, ast::Expression, proof::ProofStep};
use anyhow::{Ok, Result, bail};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, iter::zip};

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct Inference {
    pub antecedent: Vec<Expression>,
    pub consequent: Box<Expression>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
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
                .merge(Expression::harvest_variables(oexpr, mexpr)?)
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
        if *self.consequent == Expression::Literal("⊤".into()) {
            return Ok(());
        }

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
            .all(|(oa, ma)| {
                ma == &Expression::Literal("⊤".into())
                    || *oa == ma.clone().alpha_replace_all(&mappings)
            })
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

impl AsLaTeX for InferenceRule {
    fn as_latex(&self) -> anyhow::Result<String> {
        let rule_latex = self.rule.as_latex()?;
        let mut lines = rule_latex.split("\n").collect_vec();
        let name = format!("\\RL{{$ {} $}}", self.name);
        lines.insert(lines.len() - 2, name.as_str());
        Ok(lines.join("\n"))
    }
}

impl AsLaTeX for Inference {
    fn as_latex(&self) -> anyhow::Result<String> {
        Ok(match self.antecedent.len() {
            0 => format!("\\AXC{{$ {} $}}\n", self.consequent.as_latex()?), // zero-argument inferences are assumed as axioms in tree display, even though they aren't when validating
            1 => format!(
                "{}\n\\UIC{{${}$}}\n",
                self.antecedent[0].as_latex()?,
                self.consequent.as_latex()?
            ),
            2 => format!(
                "{}\n{}\n\\BIC{{${}$}}\n",
                self.antecedent[0].as_latex()?,
                self.antecedent[1].as_latex()?,
                self.consequent.as_latex()?
            ),
            3 => format!(
                "{}\n{}\n{}\n\\TIC{{${}$}}\n",
                self.antecedent[0].as_latex()?,
                self.antecedent[1].as_latex()?,
                self.antecedent[2].as_latex()?,
                self.consequent.as_latex()?
            ),
            4 => format!(
                "{}\n{}\n{}\n{}\n\\QuaternaryInfC{{${}$}}\n",
                self.antecedent[0].as_latex()?,
                self.antecedent[1].as_latex()?,
                self.antecedent[2].as_latex()?,
                self.antecedent[3].as_latex()?,
                self.consequent.as_latex()?
            ),
            5 => format!(
                "{}\n{}\n{}\n{}\n{}\n\\QuinaryInfC{{${}$}}\n",
                self.antecedent[0].as_latex()?,
                self.antecedent[1].as_latex()?,
                self.antecedent[2].as_latex()?,
                self.antecedent[3].as_latex()?,
                self.antecedent[4].as_latex()?,
                self.consequent.as_latex()?
            ),
            _ => bail!("Cannot render an inference with > 5 antecedents as a tree."),
        }
        .to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::{self, parser::parse_expression};

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

    #[test]
    fn serde_rules() {
        let rule = InferenceRule {
            rule: Inference {
                antecedent: vec![ast::parser::parse_expression("A ^ B").unwrap()],
                consequent: Box::new(ast::parser::parse_expression("B ^ A").unwrap()),
            },
            name: { "Example".into() },
        };

        let serialized = serde_yaml::to_string(&rule).unwrap();
        println!("{serialized}");
        let rule_vec = vec![rule.clone(), rule.clone()];
        println!("{}", serde_yaml::to_string(&rule_vec).unwrap());
        assert_eq!(
            serde_yaml::from_str::<InferenceRule>(serialized.as_str()).unwrap(),
            rule
        )
    }

    #[test]
    fn serde_additional_info() {
        let rule = r#"
rule:
    antecedent:
        - (( A ∧ B ))
    consequent: (( B ∧ A ))
name: Example
bla: Blub
"#;
        println!(
            "{:#?}",
            serde_yaml::from_str::<InferenceRule>(rule).unwrap()
        );
    }
    #[test]
    fn latex_render() {
        let rule = r#"
    rule:
        antecedent:
            - (( A ∧ B ))
        consequent: (( B ∧ A ))
    name: Example
    "#;
        println!(
            "{}",
            serde_yaml::from_str::<InferenceRule>(rule)
                .unwrap()
                .as_latex()
                .unwrap()
        );
    }

    #[test]
    fn literal_equality() {
        assert_eq!(
            parse_expression("⊤").unwrap(),
            parse_expression("⊤").unwrap()
        );
        assert_eq!(
            &parse_expression("⊤").unwrap(),
            &parse_expression("⊤").unwrap()
        )
    }
}
