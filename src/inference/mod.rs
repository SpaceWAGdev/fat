use crate::ast::Node;
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
    rule: Inference,
    name: String,
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

    pub fn validate(&self, rule: &Self) -> Result<()> {
        // TODO: Maybe convert the variables to an easily-comparable type (i.e. not String) for checking inferences?
        // Complexity sort of explodes with all the Vec.sorts()...

        let mappings = self.harvest_variables(rule)?;

        // self = objectvars; rule = metavars

        if rule.consequent.alpha_replace_all(&mappings) != *self.consequent {
            eprintln!(
                "{} != {}",
                &rule.consequent.alpha_replace_all(&mappings),
                self.consequent
            );
            bail!(
                "{:?} does not follow from {:?} using {:?} |- {:?}",
                self.consequent,
                self.antecedent,
                rule.antecedent,
                rule.consequent
            );
        }

        if !self
            .antecedent
            .iter()
            .zip(rule.antecedent.iter())
            .all(|(oa, ma)| *oa == ma.alpha_replace_all(&mappings))
        {
            bail!(
                "{:?} does not follow from {:?} using {:?} |- {:?}",
                self.consequent,
                self.antecedent,
                rule.antecedent,
                rule.consequent
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
        let rule = Inference {
            antecedent: Rc::new(vec![ast::parser::parse_expression("A ^ B").unwrap()]),
            consequent: Rc::new(ast::parser::parse_expression("B ^ A").unwrap()),
        };

        let test_expr = Inference {
            antecedent: Rc::new(vec![ast::parser::parse_expression("X ^ Y").unwrap()]),
            consequent: Rc::new(ast::parser::parse_expression("Y ^ X").unwrap()),
        };

        test_expr.validate(&rule).unwrap()
    }
}
