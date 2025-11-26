use std::rc::Rc;

use crate::{
    Func, compile_bytecode,
    ir::Stmt,
    registry::{NoRedefine, Registry, Shadowing},
};

use super::vm::*;

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Namespace(Rc<str>);
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct GlobalKey(Namespace, Rc<str>);

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
    namespaces: Registry<Rc<str>, Namespace, NoRedefine>,
    globals: Registry<GlobalKey, GlobalVariable, NoRedefine>,
    globals_size: usize,
}

impl GlobalScope {
    /// Create a namespace, if it hasn't been created yet.
    pub fn create_namespace(&mut self, name: Rc<str>) -> Option<Namespace> {
        let namespace = Namespace(name.clone());
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

    pub fn lookup_global(&self, key: &GlobalKey) -> Option<&GlobalVariable> {
        self.globals.lookup(&key)
    }

    pub fn compile_function(&mut self, namespace: Namespace, stmts: Vec<Stmt>) -> Function {
        let mut layout = self.layout(namespace);
        let scope = layout.scope();
        let func = compile_bytecode::compile_block(stmts, scope);
        let func = crate::compile_stmts(func);
        Function {
            func,
            stack_size: layout.size(),
        }
    }

    pub fn layout(&mut self, namespace: Namespace) -> StackLayout {
        StackLayout {
            namespace,
            global: self,
            variables: Default::default(),
            size: 0,
        }
    }
}

pub struct StackLayout<'a> {
    namespace: Namespace,
    pub global: &'a mut GlobalScope,
    variables: Registry<Rc<str>, Type, Shadowing>,
    size: usize,
}

impl StackLayout<'_> {
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn scope(&mut self) -> Scope {
        Scope {
            namespace: self.namespace.clone(),
            global: &mut self.global,
            variables: &mut self.variables,
            size: &mut self.size,
            locals: vec![],
        }
    }

    pub fn namespace(&self) -> &Namespace {
        &self.namespace
    }
}

pub struct Scope<'a> {
    namespace: Namespace,
    pub global: &'a mut GlobalScope,
    variables: &'a mut Registry<Rc<str>, Type, Shadowing>,
    locals: Vec<Rc<str>>,
    size: &'a mut usize,
}

impl<'a> Scope<'a> {
    /// Creates a child scope.
    pub fn scope(&mut self) -> Scope {
        Scope {
            namespace: self.namespace.clone(),
            variables: &mut self.variables,
            global: &mut self.global,
            size: &mut self.size,
            locals: vec![],
        }
    }

    /// Declares a local variable, and returns its address within the scope.
    pub fn declare(&mut self, name: impl Into<Rc<str>>, typ: Type) -> usize {
        let typ_size = typ.size();
        let name = name.into();

        self.locals.push(name.clone());
        self.variables.insert(name, typ);

        let address = *self.size;
        *self.size += typ_size;
        address
    }

    /// Looks up the relative address of a variable.
    pub fn lookup(&mut self, name: &str) -> Option<usize> {
        let index = self.variables.lookup_id(name)?.raw();
        let addr = self.variables.entries()[..index]
            .iter()
            .map(|t| t.size())
            .sum::<usize>();
        Some(addr)
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
