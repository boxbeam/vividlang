use std::cell::UnsafeCell;

use crate::{Func, type_system::FunctionSignature};

pub struct Vm {
    pub stack: Vec<i64>,
    return_register: UnsafeCell<[i64; 256]>,
    stack_frames: Vec<usize>,
    stack_base: usize,
    functions: UnsafeCell<Vec<Function>>,
    pub(crate) entry_point: Option<usize>,
}

impl Default for Vm {
    fn default() -> Self {
        Vm {
            stack: vec![],
            return_register: [0; 256].into(),
            stack_frames: vec![],
            stack_base: 0,
            functions: Default::default(),
            entry_point: None,
        }
    }
}

pub struct Function {
    pub signature: FunctionSignature,
    pub(crate) func: Func<'static>,
}

impl Vm {
    pub fn register_function(&mut self, func: Function) -> usize {
        let functions = self.functions.get_mut();
        functions.push(func);
        functions.len() - 1
    }

    pub(crate) fn get_function_raw(&mut self, id: usize) -> *const Func<'static> {
        let functions = self.functions.get() as *const Vec<Function>;
        let func = unsafe { &((*functions)[id]) };
        &func.func as *const _
    }

    pub fn call_function(&mut self, id: usize) {
        let functions = self.functions.get() as *const Vec<Function>;
        let func = unsafe { &((*functions)[id]) };
        let mut stack_frame = self.stack_frame(func.signature.stack_size);
        func.func.invoke(&mut stack_frame);
    }

    pub fn entry_point(&self) -> Option<usize> {
        self.entry_point.clone()
    }
}

pub struct StackGuard<'a> {
    pub vm: &'a mut Vm,
}

impl<'a> Drop for StackGuard<'a> {
    fn drop(&mut self) {
        self.vm.stack.truncate(self.vm.stack_base);
        self.vm.stack_base = self.vm.stack_frames.pop().expect("Stack frame is present");
    }
}

impl<'a> StackGuard<'a> {
    pub fn stack_frame(&mut self, size: usize) -> StackGuard {
        self.vm.stack_frame(size)
    }

    pub fn get<T>(&mut self, addr: usize) -> &mut T {
        let stack = self.get_raw(addr);
        unsafe { &mut *stack }
    }

    pub fn get_raw<T>(&mut self, addr: usize) -> *mut T {
        let absolute_addr = self.vm.stack_base + addr;
        let stack = self.vm.stack[absolute_addr..].as_mut_ptr() as *mut T;
        stack
    }

    pub fn get_global_raw<T>(&mut self, addr: usize) -> *mut T {
        self.vm.stack[addr..].as_mut_ptr() as *mut T
    }

    pub fn get_global<T>(&mut self, addr: usize) -> &mut T {
        let stack = self.get_global_raw(addr);
        unsafe { &mut *stack }
    }

    pub fn val_raw<T>(&mut self) -> *mut T {
        self.vm.return_register.get() as *mut T
    }

    pub fn get_val<T>(&mut self) -> T {
        unsafe { std::ptr::read(self.val_raw()) }
    }

    pub fn set_val<T>(&mut self, val: T) {
        unsafe { *self.val_raw() = val }
    }
}

impl Vm {
    pub fn stack_frame(&mut self, size: usize) -> StackGuard {
        self.stack_frames.push(self.stack_base);
        self.stack_base = self.stack.len();

        self.stack.reserve(size);
        self.stack.extend(std::iter::repeat_n(0, size));

        StackGuard { vm: self }
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
