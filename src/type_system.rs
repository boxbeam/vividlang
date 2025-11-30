use std::{collections::HashSet, rc::Rc};

use crate::{
    Operation,
    ir::{Block, Expr, Stmt},
    registry::Id,
    scope::{GlobalKey, MissingTypes, Scope},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function(Rc<FunctionSignature>),
}

impl Type {
    /// Returns the size, measured in bytes
    pub fn byte_size(&self) -> usize {
        match self {
            Type::Int => std::mem::size_of::<i64>(),
            Type::Bool => std::mem::size_of::<bool>(),
            Type::Function(_) => std::mem::size_of::<i64>(),
            Type::Void => 0,
        }
    }

    /// Returns the size, measured in units of 64 bits
    pub fn size(&self) -> usize {
        ((self.byte_size() + 3) / 4) * 4
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Trait {
    pub(crate) key: GlobalKey,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub args: Rc<[Type]>,
    pub ret: Type,
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

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
    Implements(Rc<[Trait]>),
    Concrete(Type),
    Ref(Id<Constraint>),
    #[default]
    Unknown,
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch { expected: Type, found: Type },
    DoesNotImplement(Vec<Trait>),
    CannotInvoke(Constraint),
}

#[derive(Debug)]
pub enum ResolveError {
    TypeError(TypeError),
    UndefinedIdentifier(Rc<str>),
    MissingTypes(MissingTypes),
    MissingReturnType,
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
    pub fn check(&self, typ: &Type, scope: &mut Scope) -> Result<(), TypeError> {
        self.merge(&Constraint::Concrete(typ.clone()), scope)?;
        Ok(())
    }

    pub fn merge(&self, other: &Constraint, scope: &mut Scope) -> Result<Constraint, TypeError> {
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
                    .filter(|t| !scope.global.is_implemented(c, t))
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
            (Ref(a_id), Ref(b_id)) => {
                if a_id == b_id {
                    return Ok(scope.get_constraint(*a_id).unwrap().clone());
                }
                let a = scope.get_constraint(*a_id).unwrap();
                let b = scope.get_constraint(*b_id).unwrap();
                let combined = a.merge(&b, scope)?;
                scope.constrain(*a_id, combined.clone());
                scope.constrain(*b_id, combined.clone());
                Ok(combined)
            }
            (Ref(id), other) | (other, Ref(id)) => {
                let stored = scope.get_constraint(*id).unwrap();
                let combined = stored.merge(&other, scope)?;
                scope.constrain(*id, combined.clone());
                Ok(combined)
            }
        }
    }
}

pub fn constrain_expr(expr: &Expr, scope: &mut Scope) -> Result<Constraint, ResolveError> {
    Ok(match expr {
        Expr::Int(_) => Constraint::Concrete(Type::Int),
        Expr::Bool(_) => Constraint::Concrete(Type::Bool),
        Expr::Var(name) => {
            let id = scope
                .lookup_id(name)
                .ok_or_else(|| ResolveError::UndefinedIdentifier(name.clone()))?;
            Constraint::Ref(id)
        }
        Expr::BinOp(op, l, r) => constrain_operation(op.clone(), l, r, scope)?,
        Expr::Neg(expr) => constrain_expr(expr, scope)?,
        Expr::IfElse(_, if_case, _) => {
            let last = if_case.last();
            let Some(Stmt::Expr(inner)) = last else {
                return Ok(Constraint::Unknown);
            };
            constrain_expr(inner, scope)?
        }
        Expr::FunctionCall { func, args } => {
            let constraint = constrain_expr(&*func, scope)?;
            let Constraint::Concrete(Type::Function(signature)) = constraint else {
                return Err(ResolveError::TypeError(TypeError::CannotInvoke(constraint)));
            };
            for (func_arg, arg) in signature.arguments.iter().zip(args) {
                let func_arg_type = &func_arg.typ;
                let arg_type = constrain_expr(arg, scope)?;
                func_arg_type.merge(&arg_type, scope)?;
            }
            signature.return_type.clone()
        }
    })
}

pub fn constrain_operation(
    operation: Operation,
    l: &Expr,
    r: &Expr,
    scope: &mut Scope,
) -> Result<Constraint, ResolveError> {
    use Operation as O;
    match operation {
        O::Add | O::Sub | O::Div | O::Mul => {
            Ok(constrain_expr(l, scope)?.merge(&constrain_expr(r, scope)?, scope)?)
        }
        O::Gt | O::Lt | O::Gte | O::Lte | O::Eq => {
            constrain_expr(l, scope)?.merge(&constrain_expr(r, scope)?, scope)?;
            Ok(Constraint::Concrete(Type::Bool))
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
                constrain_expr(expr, &mut scope)?;
            }
            Stmt::Declare(var, expr) => {
                let id = scope.declare(var.clone());
                let typ = constrain_expr(expr, &mut scope)?;
                scope.constrain(id, typ)?;
            }
            Stmt::Assign(var, expr) => {
                let id = resolve_id(var, &scope)?;
                let typ = constrain_expr(expr, &mut scope)?;
                scope.constrain(id, typ)?;
            }
            Stmt::Input(var) => {
                let id = resolve_id(var, &scope)?;
                scope.constrain(id, Constraint::Concrete(Type::Int))?
            }
            Stmt::If(expr, stmts) => {
                constrain_expr(expr, &mut scope)?;
                resolve_stack_layout(stmts, scope.scope())?;
            }
        }
    }

    Ok(())
}
