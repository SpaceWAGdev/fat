use std::vec::IntoIter;

use crate::ast::{self, Node};
use anyhow::{Result, bail};
use itertools::{Itertools, MultiPeek};

#[derive(Debug)]
enum Token {
    LParen,
    RParen,
    Var(String),
    Not,
    And,
    Or,
    Xor,
    Impl,
    Equiv,
    Top,
    Bottom,
}

impl Token {
    const fn is_binary_operation(&self) -> bool {
        use Token::*;
        matches!(self, And | Or | Xor | Impl | Equiv)
    }
}

fn take_until_expr_closed(token_stream: &mut MultiPeek<IntoIter<Token>>) -> Result<Vec<Token>> {
    let mut counter = 0;
    let mut result: Vec<Token> = vec![];
    for token in token_stream.by_ref() {
        match token {
            Token::LParen => {
                counter += 1;
                result.push(token);
            }
            Token::RParen => {
                counter -= 1;
                result.push(token);
            }
            _ => {
                result.push(token);
            }
        }
        if counter == 0 {
            return Ok(result);
        }
    }

    bail!("Unclosed brackets");
}

fn tokenize(expr: &str) -> Result<MultiPeek<IntoIter<Token>>> {
    let mut ret: Vec<Token> = vec![];
    let chars = expr.chars();
    for char in chars {
        match char {
            '(' => ret.push(Token::LParen),
            ')' => ret.push(Token::RParen),
            'v' => ret.push(Token::Or),
            '^' => ret.push(Token::And),
            '+' => ret.push(Token::Xor),
            '>' => ret.push(Token::Impl),
            '!' => ret.push(Token::Not),
            '=' => ret.push(Token::Equiv),
            '0' => ret.push(Token::Bottom),
            '1' => ret.push(Token::Top),
            ' ' => continue,
            'A'..='Z' => ret.push(Token::Var(char.to_string())),
            _ => {
                bail!("Unexpected token {}", char)
            }
        }
    }

    Ok(ret.into_iter().multipeek())
}

/// Expressions need to be bracketed
fn expr(token_stream: &mut MultiPeek<IntoIter<Token>>) -> Result<Node> {
    if let Some(c) = token_stream.next() {
        match c {
            Token::LParen => {
                let temp = take_until_expr_closed(token_stream)?;
                if let Some(tk) = token_stream.next()
                    && tk.is_binary_operation()
                {
                    let lhs = expr(&mut temp.into_iter().multipeek())?;
                    let rhs = expr(token_stream)?;
                    match tk {
                        Token::And => {
                            return Node::new_binary(ast::ExprType::And, lhs, rhs);
                        }
                        Token::Or => {
                            return Node::new_binary(ast::ExprType::Or, lhs, rhs);
                        }
                        Token::Xor => {
                            return Node::new_binary(ast::ExprType::Xor, lhs, rhs);
                        }
                        Token::Impl => {
                            return Node::new_binary(ast::ExprType::Impl, lhs, rhs);
                        }
                        Token::Equiv => {
                            return Node::new_binary(ast::ExprType::Equiv, lhs, rhs);
                        }
                        _ => {
                            bail!("Expected binary operation")
                        }
                    }
                }
                return expr(&mut temp.into_iter().multipeek());
            }

            Token::Not => return Ok(Node::new_not(expr(token_stream)?)),

            Token::Var(name) => return Ok(Node::new_var(name.as_str())),

            _ => {
                bail!("Unexpected token")
            }
        }
    }
    bail!("Expected left parenthesis, unary expression or literal.")
}

pub fn parse_expression(expression: &str) -> Result<Node> {
    expr(&mut tokenize(expression)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_literal_parse() {
        assert_eq!(parse_expression("A").unwrap(), Node::new_var("A"))
    }

    #[test]
    fn expr_parse() {
        assert_eq!(
            parse_expression("(A v B)").unwrap(),
            Node::new_binary(ast::ExprType::Or, Node::new_var("A"), Node::new_var("B")).unwrap()
        );
        assert_eq!(
            parse_expression("((A v B) > C)").unwrap(),
            Node::new_binary(
                ast::ExprType::Impl,
                Node::new_binary(ast::ExprType::Or, Node::new_var("A"), Node::new_var("B"))
                    .unwrap(),
                Node::new_var("C")
            )
            .unwrap()
        );
        assert_eq!(
            parse_expression("((A v B) > C)").unwrap(),
            Node::new_binary(
                ast::ExprType::Impl,
                Node::new_binary(ast::ExprType::Or, Node::new_var("A"), Node::new_var("B"))
                    .unwrap(),
                Node::new_var("C")
            )
            .unwrap()
        );
    }
    #[test]
    fn redundant_parens() {
        assert_eq!(parse_expression("((((A))))").unwrap(), Node::new_var("A"))
    }
    #[test]
    fn all_symbols_nothrow() {
        println!(
            "{}",
            parse_expression("((((AvB)^(C))>(D))=!!(A+B)))").unwrap()
        );
    }
}
