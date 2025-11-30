use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

use crate::compile_bytecode::translate_function;
use crate::ir::FunctionDef;
use crate::registry::Id;
use crate::type_system::{Constraint, FunctionSignature, ResolveError};
use crate::{
    registry::{Registry, Shadowing},
    type_system::{Trait, TraitImpl, Type},
};

use super::vm::*;

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
    functions: Registry<GlobalKey, FunctionSignature>,
    function_impls: HashMap<Id<FunctionSignature>, Function>,
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

    pub fn get_function(&self, id: Id<FunctionSignature>) -> Option<FunctionSignature> {
        self.functions.get(id.raw()).cloned()
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

    pub fn declare_function(
        &mut self,
        key: GlobalKey,
        func: FunctionSignature,
    ) -> Option<Id<FunctionSignature>> {
        if let Some(_) = self.globals.lookup(&key) {
            return None;
        }
        let typ = Type::Function(func.clone().into());
        let function_id = self.functions.insert(key.clone(), func)?;
        self.register_global(key, typ)?;
        Some(function_id)
    }

    pub fn implement_function(
        &mut self,
        key: GlobalKey,
        func: FunctionDef,
    ) -> Result<(), ResolveError> {
        let Some(id) = self.functions.lookup_id(&key) else {
            return Err(ResolveError::UndefinedIdentifier(key.name()));
        };
        let func = self.compile_function(key.namespace(), func)?;
        self.function_impls.entry(id).or_insert(func);
        Ok(())
    }

    pub fn compile(mut self, entry_point: GlobalKey) -> Result<Vm, ResolveError> {
        let mut vm = Vm::default();

        let main_key = self.root_namespace().key("main");
        let Some(main_id) = self.functions.lookup_id(&main_key) else {
            return Err(ResolveError::UndefinedIdentifier(entry_point.name()));
        };
        vm.entry_point = Some(main_id.raw());

        let mut funcs: Vec<_> = self.function_impls.into_iter().collect();
        funcs.sort_by_key(|(id, _)| id.raw());

        for (_, func) in funcs {
            vm.register_function(func);
        }

        Ok(vm)
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
        function: FunctionDef,
    ) -> Result<Function, ResolveError> {
        let layout = FunctionBuilder::new(self, namespace);

        let signature = function.signature.clone();
        let bytecode = translate_function(function, layout)?;
        let func = crate::compile_stmts(bytecode.stmts);

        Ok(Function { func, signature })
    }
}

pub struct FunctionBuilder<'a> {
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

impl<'a> FunctionBuilder<'a> {
    pub fn new(global: &'a mut GlobalScope, namespace: Namespace) -> Self {
        Self {
            namespace,
            global,
            variables: Default::default(),
        }
    }

    pub fn signature(&mut self, signature: &FunctionSignature) {
        for arg in signature.args.iter() {
            self.variables.insert(arg.name.clone(), arg.typ.clone());
        }
        self.variables
            .insert("".into(), signature.return_type.clone());
    }

    pub fn compute_signature(
        &self,
        signature: &FunctionSignature,
    ) -> Result<FunctionSignature, ResolveError> {
        let mut args = vec![];
        let mut missing_types = vec![];
        let mut stack_size = 0;
        for arg in signature.arguments.iter() {
            let typ = self
                .variables
                .lookup(&arg.name)
                .ok_or_else(|| ResolveError::UndefinedIdentifier(arg.name.clone()))?;
            let Constraint::Concrete(t) = typ else {
                missing_types.push(arg.name.clone());
                continue;
            };
            args.push(t.clone());
            stack_size += t.size();
        }

        if !missing_types.is_empty() {
            return Err(ResolveError::MissingTypes(MissingTypes(missing_types)));
        }

        let ret = self.variables.lookup("");
        let Some(Constraint::Concrete(ret)) = ret else {
            return Err(ResolveError::MissingReturnType);
        };

        Ok(FunctionSignature {
            stack_size,
            args: args.into(),
            ret: ret.clone(),
        })
    }

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

    pub(crate) fn reset(&mut self) {
        self.variables = Default::default();
    }

    pub fn compute_layout(&self) -> Result<StackLayout, ResolveError> {
        let mut missing_types: Vec<Rc<str>> = vec![];

        let mut size = 0;
        let mut types: Vec<(Type, usize)> = vec![];
        for i in 0..self.variables.len() {
            let name = self.variables.key(i);
            if name.is_empty() {
                continue;
            }
            let constraint = self.variables.get(i).unwrap();
            let Constraint::Concrete(t) = constraint else {
                missing_types.push(name.clone());
                continue;
            };

            types.push((t.clone(), size));
            size += t.size();
        }

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

        *current = constraint;
        Ok(())
    }

    pub fn get_constraint(&self, id: Id<Constraint>) -> Option<Constraint> {
        let orig_id = id;
        let mut constraint = self.variables.get(id.raw())?;
        while let Constraint::Ref(id) = constraint {
            if *id == orig_id {
                return Some(Constraint::Unknown);
            }
            constraint = self.variables.get(id.raw())?;
        }
        Some(constraint.clone())
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
