use std::collections::BTreeMap;

mod bytecode;
mod compile;
pub mod ir;
mod registry;
pub mod scope;
pub mod type_system;
pub mod vm;

pub use type_system::ResolveError;

use crate::{
    bytecode::BytecodeEmitter,
    ir::FunctionDef,
    scope::{GlobalScope, NameResolver},
    type_system::{TypeError, TypeSolver},
    vm::Vm,
};

#[derive(Debug)]
pub enum CompileError {
    Type(TypeError),
    Resolve(ResolveError),
}

impl From<TypeError> for CompileError {
    fn from(value: TypeError) -> Self {
        CompileError::Type(value)
    }
}

impl From<ResolveError> for CompileError {
    fn from(value: ResolveError) -> Self {
        CompileError::Resolve(value)
    }
}

pub fn compile_program(functions: Vec<FunctionDef>) -> Result<Vm, CompileError> {
    let mut global = GlobalScope::default();
    let namespace = global.root_namespace();

    let mut function_map = BTreeMap::new();

    for func in functions {
        let key = namespace.key(func.name.name());
        let id = global.register_global(key);
        function_map.insert(id, func);
    }

    for (_, func) in &mut function_map {
        let mut name_resolver = NameResolver::new(&mut global, namespace.clone());
        name_resolver.resolve_function(func)?;
    }

    let mut type_solver = TypeSolver::default();
    let mut layouts = BTreeMap::new();
    for (id, func) in &function_map {
        type_solver.resolve_function(*id, func)?;
        let layout = type_solver.compute_abstract_layout(*id, func);
        layouts.insert(*id, layout);
    }

    let mut bytecode = BytecodeEmitter::new(&mut type_solver);
    for (id, layout) in &layouts {
        bytecode.declare_function(*id, layout)?;
    }

    let mut vm = Vm::default();

    for (id, func) in function_map {
        let name = func.name.name();
        let func = bytecode.translate_function(id, func)?;
        let layout = func.layout;

        let func = compile::compile_stmts(func.stmts, layout.size);
        let id = vm.register_function(vm::CompiledFunction { layout, func });
        if &*name == "main" {
            vm.entry_point = Some(id);
        }
    }

    Ok(vm)
}
