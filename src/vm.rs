use std::cell::UnsafeCell;

use crate::{bytecode::StackLayout, compile::Func};

pub struct Vm {
    pub stack: UnsafeCell<Box<[i64]>>,
    return_register: UnsafeCell<[i64; 256]>,
    functions: UnsafeCell<Vec<CompiledFunction>>,
    pub(crate) entry_point: Option<usize>,
}

impl Default for Vm {
    fn default() -> Self {
        Vm {
            stack: UnsafeCell::new(vec![0; 64 * 1024].into()),
            return_register: [0; 256].into(),
            functions: Default::default(),
            entry_point: None,
        }
    }
}

pub struct CompiledFunction {
    pub layout: StackLayout,
    pub(crate) func: Func<'static>,
}

impl Vm {
    pub fn register_function(&mut self, func: CompiledFunction) -> usize {
        let functions = self.functions.get_mut();
        functions.push(func);
        functions.len() - 1
    }

    pub(crate) fn get_function(&self, id: usize) -> *const CompiledFunction {
        let functions = self.functions.get() as *const Vec<CompiledFunction>;
        unsafe {
            let functions = &*functions;
            let func = &functions[id];
            func as _
        }
    }

    pub fn base_frame(&'_ mut self) -> StackGuard<'_> {
        StackGuard {
            stack: self.stack.get_mut().as_mut_ptr(),
            vm: self,
        }
    }

    pub fn run(&mut self) {
        if let Some(entry_point) = self.entry_point {
            let func = self.get_function(entry_point);
            unsafe {
                let func = &*func;
                let stack = self.base_frame();
                func.func.invoke(stack, 0);
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct StackGuard<'a> {
    pub stack: *mut i64,
    pub vm: &'a Vm,
}

impl<'a> StackGuard<'a> {
    pub fn stack_frame(&'_ mut self, current_stack_size: usize) -> StackGuard<'_> {
        StackGuard {
            vm: self.vm,
            stack: unsafe { self.stack.offset(current_stack_size as isize) },
        }
    }

    pub fn get<T>(&self, addr: usize) -> &mut T {
        let stack = self.get_raw(addr);
        unsafe { &mut *stack }
    }

    pub fn get_raw<T>(&self, addr: usize) -> *mut T {
        unsafe { self.stack.offset(addr as isize) as _ }
    }

    pub fn get_global_raw<T>(&self, addr: usize) -> *mut T {
        unsafe { (*self.vm.stack.get()).as_mut_ptr().offset(addr as isize) as *mut T }
    }

    pub fn get_global<T>(&self, addr: usize) -> &mut T {
        let stack = self.get_global_raw(addr);
        unsafe { &mut *stack }
    }

    pub fn val_register_raw<T>(&self) -> *mut T {
        self.vm.return_register.get() as *mut T
    }

    pub fn get_val_register<T>(&self) -> T {
        unsafe { std::ptr::read(self.val_register_raw()) }
    }

    pub fn set_val_register<T>(&mut self, val: T) {
        unsafe { *self.val_register_raw() = val }
    }
}

pub const fn destructor<T>() -> fn(*mut ()) {
    |data| {
        let ptr = data as *mut T;
        unsafe { drop(std::ptr::read(ptr)) }
    }
}

pub const fn stack_size<T>() -> usize {
    std::mem::size_of::<T>() / std::mem::size_of::<i64>()
}
