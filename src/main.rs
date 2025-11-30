use interpreter_backend::{
    ir::*,
    scope::GlobalScope,
    type_system::{Constraint, Type},
    vm::Vm,
};
use std::rc::Rc;
use untwine::parser;

parser! {
    sep = #[" \t\r"]+;
    lbsep = #{|c| c.is_ascii_whitespace()}+;
    comma = sep? "," sep?;

    int: num=<'0'-'9'+> -> Expr { Expr::Int(num.parse().unwrap()) }
    ident: name=<{|c| c.is_ascii_alphabetic()}+> -> Rc<str> { name.into() }
    var: name=ident typ=(sep? ":" sep? typ)? -> Variable {
        let constraint = typ.map(Constraint::Concrete).unwrap_or_default();
        Variable { name, typ: constraint }
    }
    typ = match {
        "Int" => Type::Int,
        "Bool" => Type::Bool,
    } -> Type;

    nested_expr = "(" sep? expr sep? ")" -> Expr;
    negated_term: '-' sep? expr=term -> Expr { Expr::Neg(expr.into()) }

    term = (negated_term | nested_expr | int | #[convert(Expr::Var)] ident) -> Expr;

    cmp_op = match {
        ">=" => Operation::Gte,
        "<=" => Operation::Lte,
        "==" => Operation::Eq,
        "<" => Operation::Lt,
        ">" => Operation::Gt,
    } -> Operation;

    block = "{" lbsep? stmts lbsep? "}" -> Block;

    if_stmt: "if" lbsep expr=expr lbsep? block=block -> Stmt { Stmt::If(expr, block) }

    if_else_expr: "if" lbsep expr=expr lbsep? if_case=block lbsep? "else" lbsep? else_case=block -> Expr {
        Expr::IfElse(expr.into(), if_case, else_case)
    }

    cmp: first=add rest=(sep? cmp_op sep? add)* -> Expr {
        rest.into_iter().fold(first, |l, (op, r)| Expr::BinOp(op, l.into(), r.into()))
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

    args = "(" lbsep? var$comma* lbsep? ")" -> Vec<Variable>;
    return_type: typ=("->" lbsep? typ)? -> Constraint {
        typ.map(Constraint::Concrete).unwrap_or_default()
    }
    function: "fn" sep name=ident lbsep? args=args lbsep? ret=return_type lbsep? body=block -> FunctionDef {
        let signature = FunctionSignature { return_type: ret, arguments: args.into() };

        FunctionDef {
            name,
            signature,
            body
        }
    }

    lb = (sep? #["\n;"] sep?)+;
    stmts = lb? stmt$lb* lb? -> Vec<Stmt>;
    pub program = lb? function$lb* lb? -> Vec<FunctionDef>;
}

fn main() {
    let src = include_str!("../test.txt");
    let parsed = untwine::parse_pretty(program, src, Default::default());
    match parsed {
        Ok(funcs) => {
            let mut global = GlobalScope::default();
            let root_namespace = global.root_namespace();
            for func in &funcs {
                let key = root_namespace.key(func.name.clone());
                global.declare_function(key, func.signature.clone());
            }

            for func in funcs {
                let key = root_namespace.key(func.name.clone());
                global.implement_function(key, func).unwrap();
            }

            let main_key = root_namespace.key("main");
            let mut vm = global.compile(main_key).unwrap();

            let entry_point = vm.entry_point().unwrap();
            vm.call_function(entry_point);
        }
        Err(err) => println!("{err}"),
    }
}
