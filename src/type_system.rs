use std::{collections::HashSet, rc::Rc};

use crate::{
    Operation,
    ir::{Block, Expr, Stmt},
    registry::Id,
    scope::{GlobalKey, GlobalScope, MissingTypes, Scope},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
}

impl Type {
    /// Returns the size, measured in bytes
    pub fn byte_size(&self) -> usize {
        match self {
            Type::Int => std::mem::size_of::<i64>(),
        }
    }

    /// Returns the size, measured in units of 64 bits
    pub fn size(&self) -> usize {
        self.byte_size() / std::mem::size_of::<i64>()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Trait {
    pub(crate) key: GlobalKey,
}

#[derive(Clone)]
pub struct TraitImpl {
    trait_: Trait,
}

impl TraitImpl {
    pub fn new(trait_: Trait) -> Self {
        Self { trait_ }
    }
}

impl TraitImpl {
    pub fn get_trait(&self) -> &Trait {
        &self.trait_
    }
}

#[derive(Clone, Default, Debug)]
pub enum Constraint {
    Implements(Rc<[Trait]>),
    Concrete(Type),
    #[default]
    Unknown,
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch { expected: Type, found: Type },
    DoesNotImplement(Vec<Trait>),
}

#[derive(Debug)]
pub enum ResolveError {
    TypeError(TypeError),
    UndefinedIdentifier(Rc<str>),
    MissingTypes(MissingTypes),
}

impl From<TypeError> for ResolveError {
    fn from(value: TypeError) -> Self {
        ResolveError::TypeError(value)
    }
}

impl From<MissingTypes> for ResolveError {
    fn from(value: MissingTypes) -> Self {
        ResolveError::MissingTypes(value)
    }
}

impl Constraint {
    pub fn check(&self, typ: &Type, globals: &GlobalScope) -> Result<(), TypeError> {
        self.merge(&Constraint::Concrete(typ.clone()), globals)?;
        Ok(())
    }

    pub fn merge(
        &self,
        other: &Constraint,
        globals: &GlobalScope,
    ) -> Result<Constraint, TypeError> {
        use Constraint::*;
        match (self, other) {
            (Concrete(a), Concrete(b)) => {
                if a == b {
                    Ok(self.clone())
                } else {
                    Err(TypeError::Mismatch {
                        expected: a.clone(),
                        found: b.clone(),
                    })
                }
            }
            (Implements(traits), Concrete(c)) | (Concrete(c), Implements(traits)) => {
                let not_implemented: Vec<Trait> = traits
                    .iter()
                    .filter(|t| !globals.is_implemented(c, t))
                    .cloned()
                    .collect();
                if not_implemented.is_empty() {
                    Ok(Concrete(c.clone()))
                } else {
                    Err(TypeError::DoesNotImplement(not_implemented))
                }
            }
            (Implements(a), Implements(b)) => {
                let mut set = HashSet::new();
                set.extend(a.iter());
                set.extend(b.iter());
                Ok(Implements(set.into_iter().cloned().collect()))
            }
            (Unknown, c) | (c, Unknown) => Ok(c.clone()),
        }
    }
}

pub fn expr_constraint(expr: &Expr, scope: &Scope) -> Result<Constraint, ResolveError> {
    Ok(match expr {
        Expr::Int(_) => Constraint::Concrete(Type::Int),
        Expr::Var(name) => scope.lookup(name).cloned().unwrap_or_default(),
        Expr::BinOp(op, l, r) => operation_constraint(op.clone(), l, r, scope)?,
        Expr::Neg(expr) => expr_constraint(expr, scope)?,
        Expr::IfElse(_, if_case, _) => {
            let last = if_case.last();
            let Some(Stmt::Expr(inner)) = last else {
                return Ok(Constraint::Unknown);
            };
            expr_constraint(inner, scope)?
        }
    })
}

pub fn operation_constraint(
    operation: Operation,
    l: &Expr,
    r: &Expr,
    scope: &Scope,
) -> Result<Constraint, ResolveError> {
    use Operation as O;
    match operation {
        O::Add | O::Sub | O::Div | O::Mul | O::Gt | O::Lt | O::Gte | O::Lte | O::Eq => {
            Ok(expr_constraint(l, scope)?.merge(&expr_constraint(r, scope)?, &scope.global)?)
        }
    }
}

pub fn resolve_id(name: &Rc<str>, scope: &Scope) -> Result<Id<Constraint>, ResolveError> {
    scope
        .lookup_id(name)
        .ok_or_else(|| ResolveError::UndefinedIdentifier(name.clone()))
}

pub fn resolve_stack_layout(block: &Block, mut scope: Scope) -> Result<(), ResolveError> {
    for stmt in block {
        match stmt {
            Stmt::Expr(expr) | Stmt::Print(expr) => {
                expr_constraint(expr, &scope)?;
            }
            Stmt::Declare(var, expr) => {
                let id = scope.declare(var.clone());
                let typ = expr_constraint(expr, &scope)?;
                scope.constrain(id, typ)?;
            }
            Stmt::Assign(var, expr) => {
                let id = resolve_id(var, &scope)?;
                let typ = expr_constraint(expr, &scope)?;
                scope.constrain(id, typ)?;
            }
            Stmt::Input(var) => {
                let id = resolve_id(var, &scope)?;
                scope.constrain(id, Constraint::Concrete(Type::Int))?
            }
            Stmt::If(expr, stmts) => {
                expr_constraint(expr, &scope)?;
                resolve_stack_layout(stmts, scope.scope())?;
            }
        }
    }

    Ok(())
}
