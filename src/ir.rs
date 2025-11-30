use std::rc::Rc;

use crate::type_system::{Constraint, FunctionSignature};

pub type Block = Vec<Stmt>;

#[derive(Clone, Copy, Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
}

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Rc<str>),
    BinOp(Operation, Box<Self>, Box<Self>),
    IfElse(Box<Self>, Block, Block),
    Neg(Box<Self>),
    FunctionCall { func: Box<Expr>, args: Vec<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Declare(Rc<str>, Expr),
    Assign(Rc<str>, Expr),
    Input(Rc<str>),
    Print(Expr),
    If(Expr, Block),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: Rc<str>,
    pub typ: Constraint,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Rc<str>,
    pub signature: FunctionSignature,
    pub body: Block,
}
