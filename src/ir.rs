use std::rc::Rc;

use crate::{
    scope::Location,
    type_system::{Constraint, Type},
};

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub eval: Option<Box<Expr>>,
}

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
    Var(Ident),
    BinOp(Operation, Box<Self>, Box<Self>),
    IfElse(Box<Self>, Block, Block),
    Neg(Box<Self>),
    FunctionCall { func: Box<Expr>, args: Vec<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Declare(Ident, Expr),
    Assign(Ident, Expr),
    Input(Ident),
    Print(Expr),
    If(Expr, Block),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Variable {
    pub name: Ident,
    pub typ: Constraint,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Ident,
    pub signature: FunctionSignature,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub args: Rc<[Variable]>,
    pub ret: Constraint,
}

#[derive(Clone)]
pub struct Ident {
    name: Rc<str>,
    pub(crate) loc: Option<Location>,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Ident {}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<S> From<S> for Ident
where
    S: Into<Rc<str>>,
{
    fn from(value: S) -> Self {
        Ident {
            name: value.into(),
            loc: None,
        }
    }
}

impl Ident {
    pub fn name(&self) -> Rc<str> {
        self.name.clone()
    }

    pub(crate) fn loc(&self) -> Location {
        self.loc.clone().unwrap()
    }
}
