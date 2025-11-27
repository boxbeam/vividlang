use std::rc::Rc;

pub type Block = Vec<Stmt>;

#[derive(Clone, Copy)]
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

pub enum Expr {
    Int(i64),
    Var(Rc<str>),
    BinOp(Operation, Box<Self>, Box<Self>),
    IfElse(Box<Self>, Block, Block),
    Neg(Box<Self>),
}

pub enum Stmt {
    Expr(Expr),
    Declare(Rc<str>, Expr),
    Assign(Rc<str>, Expr),
    Input(Rc<str>),
    Print(Expr),
    If(Expr, Block),
}
