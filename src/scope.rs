use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

use crate::compile_bytecode::translate_ast;
use crate::registry::Id;
use crate::type_system::{Constraint, ResolveError};
use crate::{
    ir::Stmt,
    registry::{Registry, Shadowing},
    type_system::{Trait, TraitImpl, Type},
};

use super::vm::*;

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Namespace {
    name: Rc<str>,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct GlobalKey(Namespace, Rc<str>);

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
pub struct GlobalVariable {
    key: GlobalKey,
    typ: Type,
    addr: usize,
}

impl GlobalVariable {
    pub fn key(&self) -> &GlobalKey {
        &self.key
    }

    pub fn typ(&self) -> &Type {
        &self.typ
    }

    pub fn addr(&self) -> usize {
        self.addr
    }
}

#[derive(Default)]
pub struct GlobalScope {
    namespaces: Registry<Rc<str>, Namespace>,
    globals: Registry<GlobalKey, GlobalVariable>,
    traits: Registry<GlobalKey, Trait>,
    impls: HashMap<Type, HashMap<Trait, TraitImpl>>,
    globals_size: usize,
}

impl GlobalScope {
    /// Create a namespace, if it hasn't been created yet.
    pub fn create_namespace(&mut self, name: Rc<str>) -> Option<Namespace> {
        let namespace = Namespace { name: name.clone() };
        self.namespaces.insert(name, namespace.clone())?;
        Some(namespace)
    }

    /// Claim the root namespace, where the main file's globals will go.
    /// The root namespace can be claimed only once, nothing will be returned
    /// after the first invocation.
    pub fn root_namespace(&mut self) -> Option<Namespace> {
        self.create_namespace("".into())
    }

    /// Attempts to register a global variable, returning its address.
    /// Returns nothing if the global is already registered.
    pub fn register_global(&mut self, key: GlobalKey, typ: Type) -> Option<usize> {
        let addr = self.globals_size;
        let size = typ.size();

        let global = GlobalVariable {
            key: key.clone(),
            typ,
            addr,
        };

        self.globals.insert(key, global)?;
        self.globals_size += size;
        Some(addr)
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

    pub fn lookup_global(&self, key: &GlobalKey) -> Option<&GlobalVariable> {
        self.globals.lookup(&key)
    }

    pub fn compile_function(
        &mut self,
        namespace: Namespace,
        stmts: Vec<Stmt>,
    ) -> Result<Function, ResolveError> {
        let layout = self.layout(namespace);
        let bytecode = translate_ast(stmts, layout)?;
        let func = crate::compile_stmts(bytecode.stmts);
        Ok(Function {
            func,
            stack_size: bytecode.stack_size,
        })
    }

    pub fn layout(&mut self, namespace: Namespace) -> StackLayoutBuilder {
        StackLayoutBuilder {
            namespace,
            global: self,
            variables: Default::default(),
        }
    }
}

pub struct StackLayoutBuilder<'a> {
    namespace: Namespace,
    pub global: &'a mut GlobalScope,
    variables: Registry<Rc<str>, Constraint, Shadowing>,
}

pub struct StackLayout {
    pub types: Vec<(Type, usize)>,
    pub size: usize,
}

#[derive(Debug)]
pub struct MissingTypes(pub Vec<Rc<str>>);

impl StackLayoutBuilder<'_> {
    pub fn scope(&mut self) -> Scope {
        Scope {
            namespace: self.namespace.clone(),
            global: &mut self.global,
            variables: &mut self.variables,
            locals: vec![],
        }
    }

    pub fn namespace(&self) -> &Namespace {
        &self.namespace
    }

    pub fn compute_layout(&mut self) -> Result<StackLayout, ResolveError> {
        let mut missing_types: Vec<Rc<str>> = vec![];

        let mut size = 0;
        let mut types: Vec<(Type, usize)> = vec![];
        for i in 0..self.variables.len() {
            let name = self.variables.key(i);
            let constraint = self.variables.get(i).unwrap();
            let Constraint::Concrete(t) = constraint else {
                missing_types.push(name.clone());
                continue;
            };

            types.push((t.clone(), size));
            size += t.size();
        }
        self.variables = Default::default();

        Ok(StackLayout { types, size })
    }
}

pub struct Scope<'a> {
    namespace: Namespace,
    pub global: &'a mut GlobalScope,
    variables: &'a mut Registry<Rc<str>, Constraint, Shadowing>,
    locals: Vec<Rc<str>>,
}

impl<'a> Scope<'a> {
    /// Creates a child scope.
    pub fn scope(&mut self) -> Scope {
        Scope {
            namespace: self.namespace.clone(),
            variables: &mut self.variables,
            global: &mut self.global,
            locals: vec![],
        }
    }

    /// Declares a local variable, and returns its address within the scope.
    pub fn declare(&mut self, name: impl Into<Rc<str>>) -> Id<Constraint> {
        let name = name.into();

        self.locals.push(name.clone());
        let var_id = self
            .variables
            .insert(name, Constraint::Unknown)
            .expect("Can always shadow");

        var_id
    }

    pub fn constrain(
        &mut self,
        id: Id<Constraint>,
        constraint: Constraint,
    ) -> Result<(), ResolveError> {
        let current = self
            .variables
            .get_mut(id.raw())
            .expect("ID is always valid");

        *current = current.merge(&constraint, &self.global)?;
        Ok(())
    }

    pub fn lookup_id(&self, name: &str) -> Option<Id<Constraint>> {
        self.variables.lookup_id(name)
    }

    /// Looks up the constraint of a variable.
    pub fn lookup(&self, name: &str) -> Option<&Constraint> {
        self.variables.lookup(name)
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
