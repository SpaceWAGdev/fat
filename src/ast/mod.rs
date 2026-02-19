use anyhow::{Result, ensure};
use std::{fmt::Display, rc::Rc, vec};
pub mod parser;

#[derive(PartialEq, Debug, Clone)]
pub enum ExprType {
    VAR,
    NOT,
    AND,
    OR,
    IMPL,
    XOR,
    EQUIV,
}

impl ExprType {
    pub const fn commutative(&self) -> bool {
        use ExprType::*;
        match self {
            AND | OR | XOR | EQUIV => true,
            _ => false,
        }
    }
    pub const fn binary(&self) -> bool {
        use ExprType::*;
        match self {
            AND | OR | XOR | EQUIV | IMPL => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Arguments {
    Binary(Rc<Node>, Rc<Node>),
    Unary(Rc<Node>),
    Literal(String),
}

#[derive(Debug, Clone)]
pub struct Node {
    pub expr_type: ExprType,
    pub arguments: Arguments,
}

impl Node {
    pub fn new(expr_type: ExprType, arguments: Arguments) -> Node {
        return Self {
            expr_type: expr_type,
            arguments: arguments,
        };
    }

    pub fn new_var(name: &str) -> Self {
        return Self::new(ExprType::VAR, Arguments::Literal(name.to_owned()));
    }

    pub fn new_not(argument: Node) -> Self {
        return Self {
            expr_type: ExprType::NOT,
            arguments: Arguments::Unary(Rc::new(argument)),
        };
    }

    pub fn new_binary(expr_type: ExprType, lhs: Node, rhs: Node) -> Result<Self> {
        ensure!(expr_type.binary());
        return Ok(Self {
            expr_type: expr_type,
            arguments: Arguments::Binary(Rc::new(lhs), Rc::new(rhs)),
        });
    }

    fn get_leftmost_varname(&self) -> &str {
        match &self.arguments {
            Arguments::Literal(c) => return c.as_str(),
            Arguments::Unary(arg) => return arg.get_leftmost_varname(),
            Arguments::Binary(lhs, _) => return lhs.get_leftmost_varname(),
        }
    }

    fn order_lexicographically(&self) -> Self {
        match &self.arguments {
            Arguments::Binary(lhs, rhs) => {
                let ordered_lhs = lhs.order_lexicographically();
                let ordered_rhs = rhs.order_lexicographically();
                if self.expr_type.commutative() {
                    if ordered_rhs.get_leftmost_varname() < ordered_lhs.get_leftmost_varname() {
                        return Node::new_binary(self.expr_type.clone(), ordered_rhs, ordered_lhs)
                            .expect("If A*B was good, B*A should also be good");
                    }
                };
                return Node::new_binary(self.expr_type.clone(), ordered_lhs, ordered_rhs)
                    .expect("If A*B was good, A*B should still be good");
            }
            Arguments::Unary(arg) => return arg.clone().order_lexicographically(),
            Arguments::Literal(_) => return self.clone(),
        }
    }

    /// This only works if self and metavars are lexicographically ordered
    fn _impl_harvest_variables(&self, metaexpression: &Self) -> Result<Vec<(String, Self)>> {
        ensure!(
            self.expr_type == metaexpression.expr_type,
            "Cannot transform {:?} into {:?}",
            metaexpression,
            self
        );

        match (&metaexpression.arguments, &self.arguments) {
            (Arguments::Literal(c), _) => return Ok(vec![(c.clone(), self.clone())]),
            (Arguments::Unary(arg), _) => return arg._impl_harvest_variables(metaexpression),
            (Arguments::Binary(lhs, rhs), _) => {
                let mut ret: Vec<(String, Self)> = vec![];
                ret.append(&mut lhs._impl_harvest_variables(metaexpression)?);
                ret.append(&mut rhs._impl_harvest_variables(metaexpression)?);
                return Ok(ret);
            }
        }
    }

    pub fn harvest_variables(&self, metaexpression: &Self) -> Result<Vec<(String, Self)>> {
        let ordered_oexpression = self.order_lexicographically();
        let ordered_mexpression = metaexpression.order_lexicographically();
        return ordered_oexpression._impl_harvest_variables(&ordered_mexpression);
    }

    pub fn alpha_replace_all(&self, replacements: &Vec<(String, Self)>) -> Self {
        match self.arguments {
            Arguments::Binary(ref node, ref node1) => {
                return Node {
                    expr_type: self.expr_type.clone(),
                    arguments: Arguments::Binary(
                        Rc::new(node.alpha_replace_all(replacements)),
                        Rc::new(node1.alpha_replace_all(replacements)),
                    ),
                };
            }
            Arguments::Unary(ref node) => {
                return Node {
                    arguments: Arguments::Unary(Rc::new(node.alpha_replace_all(replacements))),
                    expr_type: self.expr_type.clone(),
                };
            }
            Arguments::Literal(ref lit) => {
                for (name, replacement) in replacements {
                    if lit == name {
                        return replacement.clone();
                    }
                }
            }
        }
        return self.clone();
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self.expr_type {
            ExprType::VAR => "",
            ExprType::NOT => "¬",
            ExprType::AND => "∧",
            ExprType::OR => "∨",
            ExprType::IMPL => "→",
            ExprType::XOR => "⊕",
            ExprType::EQUIV => "⇔",
        };

        match &self.arguments {
            Arguments::Binary(lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Arguments::Unary(arg) => write!(f, "{op}({arg}"),
            Arguments::Literal(value) => write!(f, "{value}"),
        }
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        if self.expr_type != other.expr_type {
            return false;
        };

        use Arguments::*;
        return match (&self.arguments, &other.arguments) {
            (Literal(lit1), Literal(lit2)) => lit1 == lit2,
            (Unary(arg1), Unary(arg2)) => arg1 == arg2,
            (Binary(lhs1, rhs1), Binary(lhs2, rhs2)) => {
                let matches = (lhs1 == lhs2) && (rhs1 == rhs2);
                if self.expr_type.commutative() {
                    matches | (lhs1 == rhs2 && lhs2 == rhs1)
                } else {
                    matches
                }
            }
            _ => false, // self.arguments was != other.arguments if there is no cartesian product
        };
    }
}

impl Eq for Node {}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_two_equivalent_statements() -> (Node, Node) {
        let a = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("A".to_string()),
        });
        let b = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("B".to_string()),
        });
        let c = Rc::new(Node {
            expr_type: ExprType::VAR,
            arguments: Arguments::Literal("C".to_string()),
        });

        let and = Rc::new(Node {
            expr_type: ExprType::AND,
            arguments: Arguments::Binary(a.clone(), b.clone()),
        });

        let or = Node {
            expr_type: ExprType::OR,
            arguments: Arguments::Binary(and.clone(), c.clone()),
        };

        let or2 = Node {
            expr_type: ExprType::OR,
            arguments: Arguments::Binary(c.clone(), and.clone()),
        };

        return (or, or2);
    }

    #[test]
    fn basic_eq() {
        let (s1, s2) = create_two_equivalent_statements();
        println!("{} = {}", &s1, &s2);
        assert_eq!(s1, s2);
    }

    #[test]
    fn alpha_replacement() {
        let (s1, s2) = create_two_equivalent_statements();
        let s2a = s2.alpha_replace_all(&vec![("A".to_string(), Node::new_var("X"))]);
        assert_ne!(s2, s2a);
        assert_ne!(s1, s2a);
        assert_eq!(s1, s2);

        println!("s1: {}", &s1);
        println!("s2: {}", &s2);
        println!("s2a: {}", &s2a);
    }
}
