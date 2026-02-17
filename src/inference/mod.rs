use crate::ast::Node;
use anyhow::{Result, bail};
use itertools::Itertools;
use std::rc::Rc;

pub struct Inference {
    antecedent: Rc<Vec<Node>>,
    consequent: Rc<Node>,
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
    fn harvest_variables(&self) -> Vec<String> {
        let mut ret: Vec<_> = vec![];

        for expr in self.antecedent.as_ref() {
            ret.append(&mut expr.harvest_variables());
        }

        ret.append(&mut self.consequent.harvest_variables());
        ret.sort();
        ret.dedup();
        ret
    }

    pub fn validate(&self, rule: &Self) -> Result<()> {
        // TODO: Maybe convert the variables to an easily-comparable type (i.e. not String) for checking inferences?
        // Complexity sort of explodes with all the Vec.sorts()...

        let metavars = rule.harvest_variables();
        let objvars = self.harvest_variables();
        let permutations = generate_variable_mapping_permutations(&metavars, &objvars)?;

        // self = objectvars; rule = metavars
        for mapping in permutations {
            if rule.consequent.alpha_replace_all(&mapping) != *self.consequent {
                eprintln!(
                    "{} != {}",
                    &rule.consequent.alpha_replace_all(&mapping),
                    self.consequent
                );
                continue;
            }

            // TODO: Test all permutations for antecedents, so that the arguments don't necessarily have to follow in order
            // Or: Order everything lexicographically
            if !self
                .antecedent
                .iter()
                .zip(rule.antecedent.iter())
                .all(|(oa, ma)| *oa == ma.alpha_replace_all(&mapping))
            {
                continue;
            }

            return Ok(());
        }

        bail!(
            "{} does not follow from {:?}",
            self.consequent,
            self.antecedent
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

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
    #[should_panic]
    fn variable_mappings_mismatch_metavars() {
        let o = vec!["x", "y"].iter().map(|s| s.to_string()).collect();
        let l = vec!["x", "y", "z"].iter().map(|s| s.to_string()).collect();

        generate_variable_mapping_permutations(&o, &l).unwrap();
    }

    #[test]
    fn basic_inference() {
        let a = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("A".to_string()),
        });
        let b = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("B".to_string()),
        });

        let a_and_b = Node {
            expr_type: ExprType::AND,
            arguments: Arguments::Binary(a.clone(), b.clone()),
        };

        let b_and_a = Node {
            expr_type: ExprType::AND,
            arguments: Arguments::Binary(b.clone(), a.clone()),
        };

        let x = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("x".to_string()),
        });
        let y = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("y".to_string()),
        });

        let x_and_y = Node {
            expr_type: ExprType::AND,
            arguments: Arguments::Binary(x.clone(), y.clone()),
        };

        let y_and_x = Node {
            expr_type: ExprType::AND,
            arguments: Arguments::Binary(y.clone(), x.clone()),
        };

        let rule = Inference {
            antecedent: Rc::new(vec![a_and_b]),
            consequent: Rc::new(b_and_a.clone()),
        };

        let test_expr = Inference {
            antecedent: Rc::new(vec![x_and_y]),
            consequent: Rc::new(y_and_x.clone()),
        };

        test_expr.validate(&rule).unwrap()
    }
}
