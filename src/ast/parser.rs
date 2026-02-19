use std::vec::IntoIter;

use crate::ast::{self, Node};
use anyhow::{Result, bail};
use itertools::{Itertools, MultiPeek};

#[derive(Debug)]
enum Token {
    LPAREN,
    RPAREN,
    VAR(String),
    NOT,
    AND,
    OR,
    XOR,
    IMPL,
    EQUIV,
    TOP,
    BOTTOM,
}

impl Token {
    const fn is_binary_operation(&self) -> bool {
        use Token::*;
        match self {
            AND | OR | XOR | IMPL | EQUIV => return true,
            _ => return false,
        }
    }
}

fn take_until_expr_closed(token_stream: &mut MultiPeek<IntoIter<Token>>) -> Result<Vec<Token>> {
    let mut counter = 0;
    let mut result: Vec<Token> = vec![];
    while let Some(token) = token_stream.next() {
        match token {
            Token::LPAREN => {
                counter += 1;
                result.push(token);
            }
            Token::RPAREN => {
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
    let mut chars = expr.chars();
    while let Some(char) = chars.next() {
        match char {
            '(' => ret.push(Token::LPAREN),
            ')' => ret.push(Token::RPAREN),
            'v' => ret.push(Token::OR),
            '^' => ret.push(Token::AND),
            '+' => ret.push(Token::XOR),
            '>' => ret.push(Token::IMPL),
            '!' => ret.push(Token::NOT),
            '=' => ret.push(Token::EQUIV),
            '0' => ret.push(Token::BOTTOM),
            '1' => ret.push(Token::TOP),
            ' ' => continue,
            'A'..'Z' => ret.push(Token::VAR(char.to_string())),
            _ => {
                bail!("Unexpected token {}", char)
            }
        }
    }

    return Ok(ret.into_iter().multipeek());
}

/// Expressions need to be bracketed
fn expr(token_stream: &mut MultiPeek<IntoIter<Token>>) -> Result<Node> {
    if let Some(c) = token_stream.next() {
        match c {
            Token::LPAREN => {
                let temp = take_until_expr_closed(token_stream)?;
                if let Some(tk) = token_stream.next() {
                    if tk.is_binary_operation() {
                        let lhs = expr(&mut temp.into_iter().multipeek())?;
                        let rhs = expr(token_stream)?;
                        match tk {
                            Token::AND => {
                                return Node::new_binary(ast::ExprType::AND, lhs, rhs);
                            }
                            Token::OR => {
                                return Node::new_binary(ast::ExprType::OR, lhs, rhs);
                            }
                            Token::XOR => {
                                return Node::new_binary(ast::ExprType::XOR, lhs, rhs);
                            }
                            Token::IMPL => {
                                return Node::new_binary(ast::ExprType::IMPL, lhs, rhs);
                            }
                            Token::EQUIV => {
                                return Node::new_binary(ast::ExprType::EQUIV, lhs, rhs);
                            }
                            _ => {
                                bail!("Expected binary operation")
                            }
                        }
                    }
                }
                return Ok(expr(&mut temp.into_iter().multipeek())?);
            }

            Token::NOT => return Ok(Node::new_not(expr(token_stream)?)),

            Token::VAR(name) => return Ok(Node::new_var(name.as_str())),

            _ => {
                bail!("Unexpected token")
            }
        }
    }
    bail!("Expected left parenthesis, unary expression or literal.")
}

pub fn parse_expression(expression: &str) -> Result<Node> {
    return expr(&mut tokenize(expression)?);
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
            Node::new_binary(ast::ExprType::OR, Node::new_var("A"), Node::new_var("B")).unwrap()
        );
        assert_eq!(
            parse_expression("((A v B) > C)").unwrap(),
            Node::new_binary(
                ast::ExprType::IMPL,
                Node::new_binary(ast::ExprType::OR, Node::new_var("A"), Node::new_var("B"))
                    .unwrap(),
                Node::new_var("C")
            )
            .unwrap()
        );
        assert_eq!(
            parse_expression("((A v B) > C)").unwrap(),
            Node::new_binary(
                ast::ExprType::IMPL,
                Node::new_binary(ast::ExprType::OR, Node::new_var("A"), Node::new_var("B"))
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
}
