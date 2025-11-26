use crate::{Func, ir::*, scope::Scope, vm::Type};

pub(crate) fn compile_expr(expr: Expr, scope: &mut Scope) -> crate::Expr {
    match expr {
        Expr::Int(lit) => crate::Expr::Int(lit),
        Expr::Var(name) => {
            if let Some(local) = scope.lookup(&name) {
                return crate::Expr::Local(local);
            }
            let key = scope.namespace().key(name);
            if let Some(global) = scope.global.lookup_global(&key) {
                let size = global.typ().size();
                let addr = global.addr();
                return crate::Expr::Global { size, addr };
            }
            todo!()
        }
        Expr::BinOp(op, l, r) => {
            let l = compile_expr(*l, scope);
            let r = compile_expr(*r, scope);
            crate::Expr::BinOp(op, l.into(), r.into())
        }
        Expr::Neg(expr) => {
            let expr = compile_expr(*expr, scope);
            crate::Expr::IntNeg(expr.into())
        }
        Expr::IfElse(expr, if_case, else_case) => {
            let expr = compile_expr(*expr, scope);
            let if_case = compile_block(if_case, scope.scope());
            let else_case = compile_block(else_case, scope.scope());
            crate::Expr::IfElse(expr.into(), if_case, else_case)
        }
    }
}

pub(crate) fn compile_block(stmts: Block, mut scope: Scope) -> Vec<crate::Stmt> {
    let mut output = vec![];
    for stmt in stmts {
        let stmt = match stmt {
            Stmt::Expr(expr) => crate::Stmt::Expr(compile_expr(expr, &mut scope)),
            Stmt::Declare(name, expr) => {
                let pos = scope.declare(name, Type::Int);
                let expr = compile_expr(expr, &mut scope);
                crate::Stmt::Assign64(pos, expr)
            }
            Stmt::Assign(name, expr) => {
                let pos = scope.lookup(&name).unwrap();
                let expr = compile_expr(expr, &mut scope);
                crate::Stmt::Assign64(pos, expr)
            }
            Stmt::Print(expr) => crate::Stmt::Print(compile_expr(expr, &mut scope)),
            Stmt::If(expr, stmts) => {
                let expr = compile_expr(expr, &mut scope);
                let block = compile_block(stmts, scope.scope());
                crate::Stmt::If(expr.into(), block)
            }
            Stmt::Input(name) => {
                let pos = scope.lookup(&name).unwrap();
                crate::Stmt::Input(pos)
            }
        };
        output.push(stmt);
    }

    output
}
