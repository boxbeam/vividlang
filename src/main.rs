use interpreter_backend::{
    compile_program,
    ir::*,
    type_system::{Constraint, Type},
};
use untwine::parser;

parser! {
    sep = #[" \t\r"]+;
    lbsep = #{|c| c.is_ascii_whitespace() || *c == ';'}+;
    comma = sep? "," sep?;

    int: num=<'0'-'9'+> -> Expr { Expr::Int(num.parse().unwrap()) }
    ident: name=<{|c| c.is_ascii_alphabetic()}+> -> Ident { name.into() }
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

    term: term=(negated_term | nested_expr | int | #[convert(Expr::Var)] ident) sep? invokes=tuple$(sep?)* -> Expr {
        let mut term = term;
        let mut invokes = invokes;
        while let Some(invoke) = invokes.pop() {
            term = Expr::FunctionCall { func: term.into(), args: invoke }
        }
        term
    }

    cmp_op = match {
        ">=" => Operation::Gte,
        "<=" => Operation::Lte,
        "==" => Operation::Eq,
        "<" => Operation::Lt,
        ">" => Operation::Gt,
    } -> Operation;

    block: "{" lbsep? stmts=stmt$lbsep* trailing=<lbsep?> "}" -> Block {
        let mut stmts = stmts;
        if !trailing.contains(";") {
            let last = stmts.pop();
            let eval = if let Some(Stmt::Expr(last)) = last {
                Some(last.into())
            } else {
                stmts.extend(last);
                None
            };
            Block { stmts, eval }
        } else {
            Block { stmts, eval: None }
        }
    }

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

    tuple = "(" sep? expr$comma* sep? ")" -> Vec<Expr>;

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
        let signature = FunctionSignature { ret, args: args.into() };

        FunctionDef {
            name,
            signature,
            body
        }
    }

    lb = (sep? #["\n;"] sep?)+;
    pub program = lb? function$lb* lb? -> Vec<FunctionDef>;
}

fn main() {
    let src = include_str!("../test.viv");
    let parsed = untwine::parse_pretty(program, src, Default::default());
    match parsed {
        Ok(funcs) => {
            let mut vm = compile_program(funcs).unwrap();
            vm.run();
        }
        Err(err) => println!("{err}"),
    }
}
