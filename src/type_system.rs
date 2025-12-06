use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    bytecode::StackLayout,
    ir::{Block, Expr, FunctionDef, Operation, Stmt},
    registry::{Id, Registry},
    scope::{GlobalKey, GlobalValue, Location},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function(usize),
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
        (self.byte_size() + 7) / 8
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

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub struct FunctionConstraint {
    return_type: Constraint,
    args: Vec<Constraint>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
    Implements(Rc<[Trait]>),
    Concrete(Type),
    Ref(Id<Constraint>),
    Function(Rc<FunctionConstraint>),
    #[default]
    Unknown,
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch { expected: Type, found: Type },
    DoesNotImplement(Vec<Trait>),
    CannotInvoke(Constraint),
    Circular(Id<Constraint>),
    ArgCount { expected: usize, found: usize },
    MissingTypes(Vec<Id<Constraint>>),
}

#[derive(Debug)]
pub enum ResolveError {
    UndefinedIdentifier(Rc<str>),
    MissingReturnType,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TypeKey {
    Scoped(Id<GlobalValue>, Id<()>),
    Return(Id<GlobalValue>),
    Global(Id<GlobalValue>),
}

#[derive(Default)]
pub struct TypeSolver {
    constraints: Registry<TypeKey, Constraint>,
    traits: Registry<GlobalKey, Trait>,
    trait_impls: HashMap<Type, HashMap<Trait, TraitImpl>>,
}

pub struct FunctionLayout {
    ret: Id<Constraint>,
    args: Vec<Id<Constraint>>,
    vars: Vec<Id<Constraint>>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ConcreteSignature {
    ret: Type,
    args: Vec<Type>,
}

fn build_layout(
    types: &mut TypeSolver,
    current_global: Id<GlobalValue>,
    func: &FunctionDef,
) -> FunctionLayout {
    let ret = types.constraint_id(TypeKey::Return(current_global));
    let args: Vec<_> = func
        .signature
        .args
        .iter()
        .map(|arg| types.constraint_id(type_key(current_global, arg.name.loc())))
        .collect();
    let mut vars = vec![];
    populate_layout_block(types, &mut vars, current_global, &func.body);
    FunctionLayout { ret, args, vars }
}

fn populate_layout_expr(
    types: &mut TypeSolver,
    vars: &mut Vec<Id<Constraint>>,
    key: Id<GlobalValue>,
    expr: &Expr,
) {
    match expr {
        Expr::Int(_) => {}
        Expr::Bool(_) => {}
        Expr::Var(_) => {}
        Expr::BinOp(_, l, r) => {
            populate_layout_expr(types, vars, key, l);
            populate_layout_expr(types, vars, key, r);
        }
        Expr::IfElse(expr, if_case, else_case) => {
            populate_layout_expr(types, vars, key, expr);
            populate_layout_block(types, vars, key, if_case);
            populate_layout_block(types, vars, key, else_case);
        }
        Expr::Neg(expr) => populate_layout_expr(types, vars, key, expr),
        Expr::FunctionCall { func, args } => {
            populate_layout_expr(types, vars, key, func);
            for arg in args {
                populate_layout_expr(types, vars, key, arg);
            }
        }
    }
}

fn populate_layout_block(
    types: &mut TypeSolver,
    vars: &mut Vec<Id<Constraint>>,
    key: Id<GlobalValue>,
    block: &Block,
) {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Expr(expr) => populate_layout_expr(types, vars, key, expr),
            Stmt::Declare(ident, expr) => {
                vars.push(types.constraint_id(type_key(key, ident.loc())));
                populate_layout_expr(types, vars, key, expr);
            }
            Stmt::Assign(_, expr) => {
                populate_layout_expr(types, vars, key, expr);
            }
            Stmt::Input(_) => {}
            Stmt::Print(expr) => populate_layout_expr(types, vars, key, expr),
            Stmt::If(expr, block) => {
                populate_layout_expr(types, vars, key, expr);
                populate_layout_block(types, vars, key, block);
            }
        }
    }
    if let Some(expr) = &block.eval {
        populate_layout_expr(types, vars, key, expr);
    }
}

pub fn type_key(current_global: Id<GlobalValue>, loc: Location) -> TypeKey {
    match loc {
        Location::Global(id) => TypeKey::Global(id),
        Location::Local(id) => TypeKey::Scoped(current_global, id),
    }
}

impl TypeSolver {
    pub fn constraint_id(&mut self, key: TypeKey) -> Id<Constraint> {
        self.constraints
            .lookup_id(&key)
            .or_else(|| self.constraints.insert(key, Constraint::Unknown))
            .unwrap()
    }

    pub fn declare_trait(&mut self, key: GlobalKey) -> Option<Trait> {
        let trait_ = Trait { key: key.clone() };
        let clone = trait_.clone();
        self.traits.insert(key, trait_)?;
        Some(clone)
    }

    pub fn compute_abstract_layout(
        &mut self,
        global_id: Id<GlobalValue>,
        func: &FunctionDef,
    ) -> FunctionLayout {
        build_layout(self, global_id, func)
    }

    pub fn compute_stack_layout(
        &mut self,
        abstract_layout: &FunctionLayout,
    ) -> Result<StackLayout, TypeError> {
        let mut size = 0;
        let mut missing = vec![];
        let mut types = vec![];

        for arg in abstract_layout
            .args
            .iter()
            .chain(&abstract_layout.vars)
            .copied()
        {
            let Some(Constraint::Concrete(c)) = self.get_constraint(arg) else {
                missing.push(arg);
                continue;
            };
            types.push((c.clone(), size));
            size += c.size();
        }

        let Some(Constraint::Concrete(ret)) = self.get_constraint(abstract_layout.ret) else {
            missing.push(abstract_layout.ret);
            return Err(TypeError::MissingTypes(missing));
        };

        if !missing.is_empty() {
            return Err(TypeError::MissingTypes(missing));
        }

        Ok(StackLayout {
            locals: types,
            size,
            ret_size: ret.size(),
        })
    }

    pub fn compute_concrete_signature(
        &mut self,
        abstract_layout: &FunctionLayout,
    ) -> Result<ConcreteSignature, TypeError> {
        let mut missing = vec![];
        let mut args = vec![];

        for arg in &abstract_layout.args {
            let Some(Constraint::Concrete(c)) = self.get_constraint(*arg) else {
                missing.push(*arg);
                continue;
            };
            args.push(c.clone());
        }

        let Some(Constraint::Concrete(ret)) = self.get_constraint(abstract_layout.ret) else {
            missing.push(abstract_layout.ret);
            return Err(TypeError::MissingTypes(missing));
        };

        if !missing.is_empty() {
            return Err(TypeError::MissingTypes(missing));
        }

        Ok(ConcreteSignature {
            ret: ret.clone().into(),
            args: args.into(),
        })
    }

    pub fn get_trait_impl(&self, typ: &Type, trait_: &Trait) -> Option<&TraitImpl> {
        self.trait_impls.get(&typ).and_then(|m| m.get(&trait_))
    }

    pub fn get_constraint(&mut self, mut id: Id<Constraint>) -> Option<&mut Constraint> {
        let first_id = id;
        let mut constraint = self.constraints.get(id)?;
        while let Constraint::Ref(ref_id) = constraint {
            if id == first_id {
                return None;
            }
            id = *ref_id;
            constraint = self.constraints.get(id)?;
        }
        self.constraints.get_mut(id)
    }

    pub fn resolve_constraint_ref(&mut self, mut constraint: Constraint) -> Constraint {
        while let Constraint::Ref(ref_id) = constraint {
            constraint = self.constraints.get(ref_id).unwrap().clone();
        }
        constraint.clone()
    }

    pub fn compute_expr_constraint(
        &mut self,
        current_global: Id<GlobalValue>,
        expr: &Expr,
    ) -> Result<Constraint, TypeError> {
        Ok(match expr {
            Expr::Int(_) => Constraint::Concrete(Type::Int),
            Expr::Bool(_) => Constraint::Concrete(Type::Bool),
            Expr::Var(ident) => {
                let id = self.constraint_id(type_key(current_global, ident.loc()));
                Constraint::Ref(id)
            }
            Expr::BinOp(op, l, r) => {
                let l = self.compute_expr_constraint(current_global, l)?;
                let r = self.compute_expr_constraint(current_global, r)?;
                self.op_constraint(op, &l, &r)?
            }
            Expr::IfElse(expr, if_case, else_case) => {
                let expr = self.compute_expr_constraint(current_global, expr)?;
                self.merge(&expr, &Constraint::Concrete(Type::Bool))?;
                self.compute_block_constraint(current_global, if_case)?;
                self.compute_block_constraint(current_global, else_case)?;
                Constraint::Concrete(Type::Void)
            }
            Expr::Neg(expr) => self.compute_expr_constraint(current_global, expr)?,
            Expr::FunctionCall { func, args } => {
                let mut arg_constraints = vec![];
                for arg in args {
                    arg_constraints.push(self.compute_expr_constraint(current_global, arg)?);
                }
                let computed_fn = Constraint::Function(
                    FunctionConstraint {
                        return_type: Constraint::Unknown,
                        args: arg_constraints,
                    }
                    .into(),
                );

                let expr = self.compute_expr_constraint(current_global, func)?;

                let merged = self.merge(&expr, &computed_fn)?;
                let Constraint::Function(resolved_constraint) = merged else {
                    return Err(TypeError::CannotInvoke(merged));
                };
                resolved_constraint.return_type.clone()
            }
        })
    }

    pub fn compute_block_constraint(
        &mut self,
        current_global: Id<GlobalValue>,
        block: &Block,
    ) -> Result<Constraint, TypeError> {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Expr(expr) => {
                    self.compute_expr_constraint(current_global, expr)?;
                }
                Stmt::Declare(ident, expr) => {
                    let id = self.constraint_id(type_key(current_global, ident.loc()));
                    let expr = self.compute_expr_constraint(current_global, expr)?;
                    self.merge(&Constraint::Ref(id), &expr)?;
                }
                Stmt::Assign(ident, expr) => {
                    let id = self.constraint_id(type_key(current_global, ident.loc()));
                    let expr = self.compute_expr_constraint(current_global, expr)?;
                    self.merge(&Constraint::Ref(id), &expr)?;
                }
                Stmt::Input(ident) => {
                    let id = self.constraint_id(type_key(current_global, ident.loc()));
                    self.merge(&Constraint::Ref(id), &Constraint::Concrete(Type::Int))?;
                }
                Stmt::Print(expr) => {
                    let expr = self.compute_expr_constraint(current_global, expr)?;
                    self.merge(&expr, &Constraint::Concrete(Type::Int))?;
                }
                Stmt::If(expr, stmts) => {
                    let expr = self.compute_expr_constraint(current_global, expr)?;
                    self.merge(&expr, &Constraint::Concrete(Type::Bool))?;
                    self.compute_block_constraint(current_global, stmts)?;
                }
            }
        }
        if let Some(expr) = &block.eval {
            self.compute_expr_constraint(current_global, expr)
        } else {
            Ok(Constraint::Concrete(Type::Void))
        }
    }

    pub fn resolve_function(
        &mut self,
        current_global: Id<GlobalValue>,
        func: &FunctionDef,
    ) -> Result<(), TypeError> {
        let constraint = self.compute_block_constraint(current_global, &func.body)?;
        let key = TypeKey::Return(current_global);
        let id = self.constraint_id(key);
        let ret = self.merge(&constraint, &func.signature.ret)?;
        self.merge(&Constraint::Ref(id), &ret)?;

        for arg in func.signature.args.iter() {
            let loc = arg.name.loc();
            let key = type_key(current_global, loc);
            let id = self.constraint_id(key);
            self.merge(&Constraint::Ref(id), &arg.typ)?;
        }
        Ok(())
    }

    pub fn op_constraint(
        &mut self,
        op: &Operation,
        l: &Constraint,
        r: &Constraint,
    ) -> Result<Constraint, TypeError> {
        Ok(match (op, l, r) {
            (Operation::Add | Operation::Sub | Operation::Mul | Operation::Div, l, r) => {
                self.merge(l, r)?;
                self.merge(r, l)?
            }
            (
                Operation::Gt | Operation::Lt | Operation::Gte | Operation::Lte | Operation::Eq,
                l,
                r,
            ) => {
                self.merge(l, r)?;
                self.merge(r, l)?;
                Constraint::Concrete(Type::Bool)
            }
        })
    }

    pub fn merge(&mut self, a: &Constraint, b: &Constraint) -> Result<Constraint, TypeError> {
        match (a, b) {
            (Constraint::Function(_), Constraint::Concrete(Type::Function(addr)))
            | (Constraint::Concrete(Type::Function(addr)), Constraint::Function(_)) => {
                Ok(Constraint::Concrete(Type::Function(*addr)))
            }
            (Constraint::Unknown, c) | (c, Constraint::Unknown) => Ok(c.clone()),
            (Constraint::Concrete(a), Constraint::Concrete(b)) => {
                if a == b {
                    Ok(Constraint::Concrete(a.clone()))
                } else {
                    Err(TypeError::Mismatch {
                        expected: a.clone(),
                        found: b.clone(),
                    })
                }
            }
            (Constraint::Implements(traits), Constraint::Concrete(c))
            | (Constraint::Concrete(c), Constraint::Implements(traits)) => {
                let not_implemented: Vec<_> = traits
                    .iter()
                    .filter(|t| self.get_trait_impl(c, t).is_none())
                    .cloned()
                    .collect();
                if not_implemented.is_empty() {
                    Ok(Constraint::Concrete(c.clone()))
                } else {
                    Err(TypeError::DoesNotImplement(not_implemented))
                }
            }
            (Constraint::Ref(a), Constraint::Ref(b)) => {
                if a == b {
                    return Ok(Constraint::Ref(*a));
                }
                let a_constraint = self.get_constraint(*a).unwrap().clone();
                let b_constraint = self.get_constraint(*b).unwrap().clone();
                let merged = self.merge(&a_constraint, &b_constraint)?;
                *self.get_constraint(*a).unwrap() = merged;
                *self.get_constraint(*b).unwrap() = Constraint::Ref(*a);
                Ok(Constraint::Ref(*a))
            }
            (Constraint::Ref(ref_id), c) => {
                let referenced = self.get_constraint(*ref_id).unwrap().clone();
                let merged = self.merge(&referenced, c)?;
                let referenced = self.get_constraint(*ref_id).unwrap();
                *referenced = merged.clone();
                Ok(merged)
            }
            (c, Constraint::Ref(ref_id)) => {
                let referenced = self.get_constraint(*ref_id).unwrap().clone();
                let merged = self.merge(&referenced, c)?;
                Ok(merged)
            }
            (Constraint::Implements(a), Constraint::Implements(b)) => {
                let all: HashSet<_> = a.iter().chain(b.iter()).cloned().collect();
                Ok(Constraint::Implements(all.into_iter().collect()))
            }
            (Constraint::Function(a), Constraint::Function(b)) => {
                let return_type = self.merge(&a.return_type, &b.return_type)?;
                if a.args.len() != b.args.len() {
                    return Err(TypeError::ArgCount {
                        expected: b.args.len(),
                        found: a.args.len(),
                    });
                }
                let mut args = vec![];

                for (arg_a, arg_b) in a.args.iter().zip(b.args.iter()) {
                    args.push(self.merge(arg_a, arg_b)?);
                }

                Ok(Constraint::Function(
                    FunctionConstraint { return_type, args }.into(),
                ))
            }
            (Constraint::Function(_), c) | (c, Constraint::Function(_)) => {
                Err(TypeError::CannotInvoke(c.clone()))
            }
        }
    }
}
