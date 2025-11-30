use std::rc::Rc;

use crate::{
    Dest,
    ir::*,
    scope::{FunctionBuilder, Scope, StackLayout},
    type_system::{FunctionSignature, ResolveError, Type, resolve_id, resolve_stack_layout},
};

fn resolve_var<'a>(
    name: &Rc<str>,
    scope: &Scope,
    layout: &'a StackLayout,
) -> Result<(Type, Dest), ResolveError> {
    match resolve_id(name, scope) {
        Ok(id) => {
            let (typ, addr) = layout.types[id.raw()].clone();
            Ok((typ, Dest::Local(addr)))
        }
        Err(e) => {
            let namespace = scope.namespace();
            let key = namespace.key(name.clone());
            if let Some(global) = scope.global.lookup_global(&key) {
                return Ok((global.typ().clone(), Dest::Global(global.addr())));
            }
            Err(e)
        }
    }
}

fn resolve_addr(name: &Rc<str>, scope: &Scope, layout: &StackLayout) -> Result<Dest, ResolveError> {
    Ok(resolve_var(name, scope, layout)?.1)
}

pub struct CompiledBlock {
    pub stmts: Vec<crate::Stmt>,
    pub signature: FunctionSignature,
}

pub fn translate_function(
    function: FunctionDef,
    mut builder: FunctionBuilder,
) -> Result<CompiledBlock, ResolveError> {
    builder.signature(&function.signature);
    let scope = builder.scope();
    resolve_stack_layout(&function.body, scope)?;
    let layout = builder.compute_layout()?;
    builder.reset();

    builder.signature(&function.signature);
    let mut scope = builder.scope();
    let stmts = translate_block(function.body, &mut scope, &layout)?;
    drop(scope);

    let signature = builder.compute_signature(&function.signature)?;

    Ok(CompiledBlock { stmts, signature })
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
                    crate::Stmt::Assign64(addr, expr)
                } else {
                    crate::Stmt::Assign {
                        dst: addr,
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
        Expr::Var(var) => {
            let (typ, dst) = resolve_var(&var, scope, layout)?;
            crate::Expr::Read(dst, typ.size())
        }
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
        Expr::FunctionCall { func, args } => {}
    })
}
