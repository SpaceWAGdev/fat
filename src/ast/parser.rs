use anyhow::{Result, bail};
use comemo::memoize;
use pest::Parser;
use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use std::{fmt::Display, sync::LazyLock};

use crate::ast::{BinaryRelation, Expression};
#[derive(pest_derive::Parser)]
#[grammar = "fatexpr.pest"]
pub struct FatParser;

pub(crate) static PRATT_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    use Rule::*;
    use pest::pratt_parser::{Assoc::*, Op};

    // Precedence is defined lowest to highest
    PrattParser::new()
        .op(Op::infix(and, Left) | Op::infix(or, Left))
        .op(Op::infix(implies, Left) | Op::infix(equiv, Left) | Op::infix(xor, Left))
        .op(Op::prefix(negation))
});

#[derive(Clone, Hash, Debug)]
pub struct ParsingError {
    pub message: String,
}

impl From<anyhow::Error> for ParsingError {
    fn from(value: anyhow::Error) -> Self {
        ParsingError {
            message: value.to_string(),
        }
    }
}

impl std::error::Error for ParsingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error while parsing: {}", self.message)
    }
}

fn parse_proposition(pairs: Pairs<Rule>) -> Result<Expression> {
    PRATT_PARSER
        .map_primary(|primary| {
            Ok(match primary.as_rule() {
                Rule::literal => Expression::Literal(primary.to_string()), // TODO: Check if to_string() and as_str() work the same way here
                Rule::variable => Expression::Variable(primary.to_string()),
                Rule::prop => parse_proposition(primary.into_inner())?,
                rule => bail!("Expr::parse expected atom, found {:?}", rule),
            })
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::and => BinaryRelation::And,
                Rule::or => BinaryRelation::Or,
                Rule::implies => BinaryRelation::Impl,
                Rule::equiv => BinaryRelation::Equiv,
                Rule::xor => BinaryRelation::Xor,
                rule => bail!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Ok(Expression::BinaryRelation {
                lhs: Box::new(lhs?),
                relation: op,
                rhs: Box::new(rhs?),
            })
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negation => Ok(Expression::Negation(Box::new(rhs?))),
            _ => unreachable!(),
        })
        .parse(pairs)
}

fn wrap_parse_expression(expr: &str) -> anyhow::Result<Expression> {
    let mut result = FatParser::parse(Rule::expr, expr)?;
    parse_proposition(result.next().unwrap().into_inner())
}

#[memoize]
pub fn parse_expression(expr: &str) -> Result<Expression, ParsingError> {
    if expr.trim_ascii().is_empty() {
        return Err(ParsingError {
            message: "An empty string is not an expression".to_string(),
        });
    };
    wrap_parse_expression(expr).map_err(|e| ParsingError {
        message: e.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variable_names() {
        assert_eq!(
            parse_expression("AvB").unwrap(),
            Expression::BinaryRelation {
                lhs: Box::new(Expression::Variable("A".to_string())),
                relation: BinaryRelation::Or,
                rhs: Box::new(Expression::Variable("B".to_string()))
            }
        )
    }

    #[test]
    fn reject_double_negation() {
        match parse_expression("!!!(AvB) <-> (A^B) -> (⊤v⊥)") {
            Ok(res) => print!("{}", res),
            Err(e) => println!("{}", e),
        }
    }

    #[test]
    fn all_symbols_nothrow() {
        println!(
            "{}",
            parse_expression("(AvB) ^ (B>-<C) <-> (!A -> B) ").unwrap()
        );
    }

    #[test]
    fn multiple_associativity() {
        assert_eq!(
            parse_expression("A v B v C v D").unwrap(),
            parse_expression("(((A v B) v C) v D)").unwrap()
        )
    }
}
