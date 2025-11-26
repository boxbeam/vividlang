use std::rc::Rc;

use crate::Operation;

pub type Block = Vec<Stmt>;

pub enum Expr {
    Int(i64),
    Var(Rc<str>),
    BinOp(Operation, Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Block, Block),
    Neg(Box<Expr>),
}

pub enum Cmp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
}

impl Cmp {
    pub fn to_operation(&self) -> Operation {
        match self {
            Cmp::Gt => Operation::Gt,
            Cmp::Lt => Operation::Lt,
            Cmp::Gte => Operation::Gte,
            Cmp::Lte => Operation::Lte,
            Cmp::Eq => Operation::Eq,
        }
    }
}

pub enum Stmt {
    Expr(Expr),
    Declare(Rc<str>, Expr),
    Assign(Rc<str>, Expr),
    Input(Rc<str>),
    Print(Expr),
    If(Expr, Block),
}
