use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

use crate::ir::{Block, Expr, FunctionDef, Stmt};
use crate::registry::Id;
use crate::type_system::ResolveError;
use crate::{
    registry::{Registry, Shadowing},
    type_system::{Trait, TraitImpl, Type},
};

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Namespace {
    name: Rc<str>,
}

impl Namespace {
    pub fn is_root(&self) -> bool {
        self.name.is_empty()
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct GlobalKey(Namespace, Rc<str>);

impl GlobalKey {
    pub fn namespace(&self) -> Namespace {
        self.0.clone()
    }

    pub fn name(&self) -> Rc<str> {
        self.1.clone()
    }
}

impl Debug for GlobalKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.0.name, self.1)
    }
}

impl Namespace {
    pub fn key(&self, name: impl Into<Rc<str>>) -> GlobalKey {
        GlobalKey(self.clone(), name.into())
    }
}

#[derive(Clone)]
pub struct GlobalValue {
    key: GlobalKey,
}

#[derive(Clone, Debug)]
pub enum Location {
    Global(Id<GlobalValue>),
    Local(Id<()>),
}

impl GlobalValue {
    pub fn key(&self) -> &GlobalKey {
        &self.key
    }
}

#[derive(Default)]
pub struct GlobalScope {
    namespaces: Registry<Rc<str>, Namespace>,
    globals: Registry<GlobalKey, GlobalValue>,
    traits: Registry<GlobalKey, Trait>,
    impls: HashMap<Type, HashMap<Trait, TraitImpl>>,
}

impl GlobalScope {
    /// Create a namespace, if it hasn't been created yet.
    pub fn create_namespace(&mut self, name: Rc<str>) -> Option<Namespace> {
        let namespace = Namespace { name: name.clone() };
        self.namespaces.insert(name, namespace.clone())?;
        Some(namespace)
    }

    pub fn namespace(&mut self, name: Rc<str>) -> Namespace {
        if let Some(namespace) = self.create_namespace(name.clone()) {
            return namespace;
        }
        self.namespaces
            .lookup(&name)
            .expect("Namespace exists")
            .clone()
    }

    pub fn root_namespace(&mut self) -> Namespace {
        self.namespace("".into())
    }

    pub fn register_global(&mut self, key: GlobalKey) -> Id<GlobalValue> {
        let global = GlobalValue { key: key.clone() };
        self.globals.insert(key, global).unwrap()
    }

    /// Attempts to register a trait.
    /// Returns nothing if the trait is already registered.
    pub fn register_trait(&mut self, key: GlobalKey) -> Option<Trait> {
        let trait_ = Trait { key: key.clone() };
        self.traits.insert(key, trait_.clone())?;
        Some(trait_)
    }

    pub fn implement_trait(&mut self, typ: Type, trait_impl: TraitImpl) -> bool {
        let impls = self.impls.entry(typ).or_default();
        let trait_ = trait_impl.get_trait().clone();
        if impls.contains_key(&trait_) {
            return false;
        }
        impls.insert(trait_, trait_impl);
        true
    }

    pub fn is_implemented(&self, typ: &Type, trait_: &Trait) -> bool {
        self.impls
            .get(typ)
            .is_some_and(|impls| impls.contains_key(trait_))
    }

    pub fn lookup_global(&self, key: &GlobalKey) -> Option<&GlobalValue> {
        self.globals.lookup(key)
    }
}

fn resolve_name(name: Rc<str>, scope: &Scope) -> Result<Location, ResolveError> {
    if let Some(local) = scope.variables.lookup_id(&name) {
        Ok(Location::Local(local))
    } else if let Some(global) = scope
        .global
        .globals
        .lookup_id(&scope.namespace().key(name.clone()))
    {
        Ok(Location::Global(global.clone()))
    } else {
        Err(ResolveError::UndefinedIdentifier(name))
    }
}

fn resolve_block(block: &mut Block, mut scope: Scope) -> Result<(), ResolveError> {
    for stmt in &mut block.stmts {
        match stmt {
            Stmt::Expr(expr) => resolve_expr(expr, &mut scope)?,
            Stmt::Declare(name, expr) => {
                let id = scope.declare(name.name());
                name.loc.replace(Location::Local(id));
                resolve_expr(expr, &mut scope)?;
            }
            Stmt::Assign(name, expr) => {
                let loc = resolve_name(name.name(), &scope)?;
                name.loc.replace(loc);
                resolve_expr(expr, &mut scope)?;
            }
            Stmt::Input(name) => {
                let loc = resolve_name(name.name(), &scope)?;
                name.loc.replace(loc);
            }
            Stmt::Print(expr) => resolve_expr(expr, &mut scope)?,

            Stmt::If(expr, block) => {
                resolve_expr(expr, &mut scope)?;
                let scope = scope.scope();
                resolve_block(block, scope)?;
            }
        }
    }
    if let Some(expr) = &mut block.eval {
        resolve_expr(expr, &mut scope)?;
    }
    Ok(())
}

fn resolve_expr(expr: &mut Expr, scope: &mut Scope) -> Result<(), ResolveError> {
    match expr {
        Expr::Int(_) => {}
        Expr::Bool(_) => {}
        Expr::Var(ident) => {
            let loc = resolve_name(ident.name(), scope)?;
            ident.loc.replace(loc);
        }
        Expr::BinOp(_, l, r) => {
            resolve_expr(l, scope)?;
            resolve_expr(r, scope)?;
        }
        Expr::IfElse(expr, if_case, else_case) => {
            resolve_expr(expr, scope)?;
            let if_scope = scope.scope();
            resolve_block(if_case, if_scope)?;
            let else_scope = scope.scope();
            resolve_block(else_case, else_scope)?;
        }
        Expr::Neg(expr) => resolve_expr(expr, scope)?,
        Expr::FunctionCall { func, args } => {
            resolve_expr(func, scope)?;
            for arg in args.iter_mut() {
                resolve_expr(arg, scope)?;
            }
        }
    }
    Ok(())
}

pub(crate) struct NameResolver<'a> {
    namespace: Namespace,
    pub(crate) global: &'a mut GlobalScope,
    variables: Registry<Rc<str>, (), Shadowing>,
}

impl<'a> NameResolver<'a> {
    pub fn new(global: &'a mut GlobalScope, namespace: Namespace) -> Self {
        Self {
            namespace,
            global,
            variables: Default::default(),
        }
    }

    pub fn scope(&'_ mut self) -> Scope<'_> {
        Scope {
            namespace: self.namespace.clone(),
            global: &mut self.global,
            variables: &mut self.variables,
            locals: vec![],
        }
    }

    pub fn resolve_function(&mut self, function: &mut FunctionDef) -> Result<(), ResolveError> {
        let mut scope = self.scope();
        for arg in function.signature.args.iter_mut() {
            let id = scope.declare(arg.name.name());
            arg.name.loc.replace(Location::Local(id));
        }
        resolve_block(&mut function.body, scope)
    }
}

pub struct Scope<'a> {
    namespace: Namespace,
    pub global: &'a mut GlobalScope,
    variables: &'a mut Registry<Rc<str>, (), Shadowing>,
    locals: Vec<Rc<str>>,
}

impl<'a> Scope<'a> {
    /// Creates a child scope.
    pub fn scope(&'_ mut self) -> Scope<'_> {
        Scope {
            namespace: self.namespace.clone(),
            variables: &mut self.variables,
            global: &mut self.global,
            locals: vec![],
        }
    }

    /// Declares a local variable, and returns its address within the scope.
    pub fn declare(&mut self, name: impl Into<Rc<str>>) -> Id<()> {
        let name = name.into();

        self.locals.push(name.clone());
        let var_id = self.variables.insert(name, ()).expect("Can always shadow");

        var_id
    }

    pub fn namespace(&self) -> &Namespace {
        &self.namespace
    }
}

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        for name in &self.locals {
            self.variables.unbind(name);
        }
    }
}
