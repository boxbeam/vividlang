use std::rc::Rc;

use crate::{
    ir::*,
    scope::{Scope, StackLayout, StackLayoutBuilder},
    type_system::{ResolveError, Type, resolve_id, resolve_stack_layout},
};

fn resolve_var<'a>(
    name: &Rc<str>,
    scope: &Scope,
    layout: &'a StackLayout,
) -> Result<&'a (Type, usize), ResolveError> {
    let id = resolve_id(name, scope)?;
    Ok(&layout.types[id.raw()])
}

fn resolve_addr(
    name: &Rc<str>,
    scope: &Scope,
    layout: &StackLayout,
) -> Result<usize, ResolveError> {
    Ok(resolve_var(name, scope, layout)?.1)
}

pub struct CompiledBlock {
    pub stmts: Vec<crate::Stmt>,
    pub stack_size: usize,
}

pub fn translate_ast(
    block: Block,
    mut builder: StackLayoutBuilder,
) -> Result<CompiledBlock, ResolveError> {
    let scope = builder.scope();
    resolve_stack_layout(&block, scope)?;
    let layout = builder.compute_layout()?;

    let mut scope = builder.scope();
    let stmts = translate_block(block, &mut scope, &layout)?;
    Ok(CompiledBlock {
        stmts,
        stack_size: layout.size,
    })
}

fn translate_block(
    block: Block,
    scope: &mut Scope,
    layout: &StackLayout,
) -> Result<Vec<crate::Stmt>, ResolveError> {
    let mut out = vec![];

    for stmt in block {
        out.push(match stmt {
            Stmt::Expr(expr) => crate::Stmt::Expr(translate_expr(expr, scope, layout)?),
            Stmt::Declare(var, expr) => {
                scope.declare(var.clone());
                crate::Stmt::Assign64(
                    resolve_addr(&var, scope, layout)?,
                    translate_expr(expr, scope, layout)?,
                )
            }
            Stmt::Assign(var, expr) => {
                let (typ, addr) = resolve_var(&var, scope, layout)?;
                let expr = translate_expr(expr, scope, layout)?;
                if typ.size() == 1 {
                    crate::Stmt::Assign64(*addr, expr)
                } else {
                    crate::Stmt::Assign {
                        dst: *addr,
                        size: typ.size(),
                        val: expr,
                    }
                }
            }
            Stmt::Input(var) => crate::Stmt::Input(resolve_addr(&var, scope, layout)?),
            Stmt::Print(expr) => crate::Stmt::Print(translate_expr(expr, scope, layout)?),
            Stmt::If(expr, stmts) => crate::Stmt::If(
                translate_expr(expr, scope, layout)?.into(),
                translate_block(stmts, scope, layout)?,
            ),
        })
    }

    Ok(out)
}

fn translate_expr(
    expr: Expr,
    scope: &mut Scope,
    layout: &StackLayout,
) -> Result<crate::Expr, ResolveError> {
    Ok(match expr {
        Expr::Int(i) => crate::Expr::Int(i),
        Expr::Bool(b) => crate::Expr::Bool(b),
        Expr::Var(var) => crate::Expr::Local(resolve_addr(&var, scope, layout)?),
        Expr::BinOp(op, l, r) => crate::Expr::BinOp(
            op,
            translate_expr(*l, scope, layout)?.into(),
            translate_expr(*r, scope, layout)?.into(),
        ),
        Expr::IfElse(expr, if_case, else_case) => crate::Expr::IfElse(
            translate_expr(*expr, scope, layout)?.into(),
            translate_block(if_case, scope, layout)?,
            translate_block(else_case, scope, layout)?,
        ),
        Expr::Neg(expr) => crate::Expr::IntNeg(translate_expr(*expr, scope, layout)?.into()),
    })
}
