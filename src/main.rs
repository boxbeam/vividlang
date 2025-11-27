use interpreter_backend::{Operation, ir::*, scope::GlobalScope, vm::Vm};
use std::rc::Rc;
use untwine::parser;

parser! {
    sep = #[" \t\r"]+;
    lbsep = #{|c| c.is_ascii_whitespace()}+;

    int: num=<'0'-'9'+> -> Expr { Expr::Int(num.parse().unwrap()) }
    ident: name=<{|c| c.is_ascii_alphabetic()}+> -> Rc<str> { name.into() }

    nested_expr = "(" sep? expr sep? ")" -> Expr;
    negated_term: '-' sep? expr=term -> Expr { Expr::Neg(expr.into()) }

    term = (negated_term | nested_expr | int | #[convert(Expr::Var)] ident) -> Expr;

    cmp_op = match {
        ">=" => Cmp::Gte,
        "<=" => Cmp::Lte,
        "==" => Cmp::Eq,
        "<" => Cmp::Lt,
        ">" => Cmp::Gt,
    } -> Cmp;

    block = "{" lbsep? stmts lbsep? "}" -> Block;

    if_stmt: "if" lbsep expr=expr lbsep? block=block -> Stmt { Stmt::If(expr, block) }

    if_else_expr: "if" lbsep expr=expr lbsep? if_case=block lbsep? "else" lbsep? else_case=block -> Expr {
        Expr::IfElse(expr.into(), if_case, else_case)
    }

    cmp: first=add rest=(sep? cmp_op sep? add)* -> Expr {
        rest.into_iter().fold(first, |l, (op, r)| Expr::BinOp(op.to_operation(), l.into(), r.into()))
    }

    add: first=mul rest=(sep? ["+-"] sep? mul)* -> Expr {
        rest.into_iter().fold(first, |l, (op, r)| {
            let op = if op == '+' {Operation::Add} else {Operation::Sub};
            Expr::BinOp(op, l.into(), r.into())
        })
    }

    mul: first=term rest=(sep? ["*/"] sep? term)* -> Expr {
        rest.into_iter().fold(first, |l, (op, r)| {
            let op = if op == '*' {Operation::Mul} else {Operation::Div};
            Expr::BinOp(op, l.into(), r.into())
        })
    }

    expr = (if_else_expr | #[not] "if" cmp) -> Expr;

    stmt = match {
        "print" sep val=expr => Stmt::Print(val.into()),
        "input" sep name=ident => Stmt::Input(name),
        "let" sep name=ident sep '=' sep val=expr => Stmt::Declare(name, val),
        name=ident sep '=' sep val=expr => Stmt::Assign(name, val),
        expr=expr => Stmt::Expr(expr),
        if_stmt=if_stmt => if_stmt,
    } -> Stmt;

    lb = (sep? #["\n;"] sep?)+;
    pub stmts = lb? stmt$lb* lb? -> Vec<Stmt>;
}

fn main() {
    let src = include_str!("../test.txt");
    let parsed = untwine::parse_pretty(stmts, src, Default::default());
    match parsed {
        Ok(stmts) => {
            let mut global = GlobalScope::default();
            let root_namespace = global.root_namespace().unwrap();
            let main = global
                .compile_function(root_namespace.clone(), stmts)
                .unwrap();

            let mut vm = Vm::default();
            let main_id = vm.register_function(main);
            vm.call_function(main_id);
        }
        Err(err) => println!("{err}"),
    }
}
