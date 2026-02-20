pub mod parser;
use comemo::memoize;
use serde::{Deserialize, Deserializer, Serialize, de::Visitor};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use anyhow::{Result, bail};

use crate::{AsLaTeX, ast::parser::parse_expression};

#[derive(Clone, Hash, PartialOrd)]
pub enum Expression {
    Variable(String),
    Literal(String),
    Negation(Box<Expression>),
    BinaryRelation {
        lhs: Box<Expression>,
        relation: BinaryRelation,
        rhs: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
pub enum BinaryRelation {
    And,
    Or,
    Impl,
    Equiv,
    Xor,
}

impl Display for BinaryRelation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryRelation::And => "∧",
                BinaryRelation::Or => "∨",
                BinaryRelation::Impl => "→",
                BinaryRelation::Equiv => "↔",
                BinaryRelation::Xor => "⊕",
            }
        )
    }
}

impl AsLaTeX for BinaryRelation {
    fn as_latex(&self) -> Result<String> {
        Ok(match self {
            BinaryRelation::And => "\\wedge",
            BinaryRelation::Or => "\\vee",
            BinaryRelation::Impl => "\\to",
            BinaryRelation::Equiv => "\\leftrightarrow",
            BinaryRelation::Xor => "\\nleftrightarrow",
        }
        .into())
    }
}

impl BinaryRelation {
    pub const fn commutative(&self) -> bool {
        use BinaryRelation::*;
        matches!(self, And | Or | Xor | Equiv)
    }
    pub const fn binary(&self) -> bool {
        use BinaryRelation::*;
        matches!(self, And | Or | Xor | Equiv | Impl)
    }
}

impl Expression {
    pub fn harvest_variables(&self, metaexpression: &Self) -> Result<HashMap<String, Expression>> {
        let mut ret: HashMap<String, Expression> = HashMap::new();

        match (metaexpression, self) {
            (Expression::Variable(name), _) => {
                ret.insert(name.to_owned(), self.clone());
            }
            (Expression::Negation(m_argument), Expression::Negation(o_argument)) => {
                ret.extend(o_argument.harvest_variables(m_argument)?);
            }
            (
                Expression::BinaryRelation {
                    lhs: m_lhs,
                    relation: m_relation,
                    rhs: m_rhs,
                },
                Expression::BinaryRelation {
                    lhs: o_lhs,
                    relation: _,
                    rhs: o_rhs,
                },
            ) => {
                if m_relation.commutative() {
                    ret.extend(
                        o_lhs
                            .to_owned()
                            .order_lexicographically()
                            .harvest_variables(m_lhs)?,
                    );
                    ret.extend(
                        o_rhs
                            .to_owned()
                            .order_lexicographically()
                            .harvest_variables(m_rhs)?,
                    );
                }

                ret.extend(o_lhs.harvest_variables(m_lhs)?);
                ret.extend(o_rhs.harvest_variables(m_rhs)?);
            }
            (_, Expression::Literal(_)) => {}
            _ => bail!("{:?} can not be interpreted as {:?}", self, metaexpression),
        };

        Ok(ret)
    }

    pub fn alpha_replace_all(self, replacements: &HashMap<String, Self>) -> Self {
        match self {
            Expression::Variable(ref name) => {
                if let Some(new) = replacements.get(name) {
                    new.to_owned()
                } else {
                    self
                }
            }
            Expression::Literal(_) => self,
            Expression::Negation(argument) => {
                Expression::Negation(Box::new(argument.alpha_replace_all(replacements)))
            }
            Expression::BinaryRelation { lhs, relation, rhs } => Expression::BinaryRelation {
                lhs: Box::new(lhs.alpha_replace_all(replacements)),
                relation,
                rhs: Box::new(rhs.alpha_replace_all(replacements)),
            },
        }
    }

    #[memoize]
    fn order_lexicographically(self) -> Expression {
        match self {
            Expression::Variable(_) => self,
            Expression::Literal(_) => self,
            Expression::Negation(argument) => argument.order_lexicographically(),
            Expression::BinaryRelation {
                lhs: ref orig_lhs,
                relation,
                rhs: ref orig_rhs,
            } => {
                if relation.commutative() && orig_lhs < orig_rhs {
                    return Self::BinaryRelation {
                        lhs: orig_rhs.to_owned(),
                        relation,
                        rhs: orig_lhs.to_owned(),
                    };
                } else {
                    self
                }
            }
        }
    }
}

impl Serialize for Expression {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(format!("({})", self).as_str())
    }
}

struct ExprVisitor;
impl<'de> Visitor<'de> for ExprVisitor {
    type Value = Expression;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "An expression to be parsed by FAT")
    }

    fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        parse_expression(v)
            .map_err(|e| E::custom(format!("invalid logical expression: {}", e.message)))
    }
}

impl<'de> Deserialize<'de> for Expression {
    fn deserialize<D>(deserializer: D) -> Result<Expression, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(ExprVisitor)
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Variable(name1), Self::Variable(name2)) => name1 == name2,
            (Self::Literal(lvalue), Self::Literal(rvalue)) => lvalue == rvalue,
            (Self::Negation(l0), Self::Negation(r0)) => l0 == r0,
            (
                Self::BinaryRelation {
                    lhs: l_lhs,
                    relation: l_relation,
                    rhs: l_rhs,
                },
                Self::BinaryRelation {
                    lhs: r_lhs,
                    relation: r_relation,
                    rhs: r_rhs,
                },
            ) => {
                l_relation == r_relation
                    && ((l_relation.commutative() && (l_lhs == r_rhs && l_rhs == r_lhs))
                        || l_relation == r_relation && l_rhs == r_rhs)
            }
            _ => false,
        }
    }
}

impl Eq for Expression {}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Variable(name) => write!(f, "{name}"),
            Expression::Literal(value) => write!(f, "{value}"),
            Expression::Negation(expression) => write!(f, "¬{expression}"),
            Expression::BinaryRelation { lhs, relation, rhs } => {
                write!(f, "( {lhs} {relation} {rhs} )")
            }
        }
    }
}

impl AsLaTeX for Expression {
    fn as_latex(&self) -> Result<String> {
        Ok(match self {
            Expression::Variable(v) => v.to_string(),
            Expression::Literal(_) => todo!(),
            Expression::Negation(expression) => format!("\\neg {}", expression.as_latex()?),
            Expression::BinaryRelation { lhs, relation, rhs } => {
                format!(
                    "({} {} {})",
                    lhs.as_latex()?,
                    relation.as_latex()?,
                    rhs.as_latex()?
                )
            }
        })
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ast::{Expression, parser::parse_expression};

    #[test]
    fn commutative_equality() {
        assert_eq!(
            parse_expression("AvB").unwrap(),
            parse_expression("BvA").unwrap()
        )
    }

    #[test]
    fn alpha_conversion() {
        let mut replacement = HashMap::<String, Expression>::new();
        replacement.insert("A".into(), parse_expression("X->Y").unwrap());
        assert_eq!(
            parse_expression("AvB")
                .unwrap()
                .alpha_replace_all(&replacement),
            parse_expression("(X->Y)vB").unwrap()
        )
    }

    #[test]
    fn serde() {
        let expr = parse_expression("(AvB) -> C").unwrap();
        let serialized = serde_yaml::to_string(&expr).unwrap();
        assert_eq!(
            serde_yaml::from_str::<Expression>(serialized.as_str()).unwrap(),
            expr
        );
    }
}
