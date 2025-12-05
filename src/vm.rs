use std::cell::UnsafeCell;

use crate::{bytecode::StackLayout, compile::Func};

pub struct Vm {
    pub stack: Box<[i64]>,
    return_register: UnsafeCell<[i64; 256]>,
    stack_frames: Vec<usize>,
    stack_base: usize,
    stack_len: usize,
    functions: UnsafeCell<Vec<CompiledFunction>>,
    pub(crate) entry_point: Option<usize>,
}

impl Default for Vm {
    fn default() -> Self {
        Vm {
            stack: vec![0; 64 * 1024].into(),
            return_register: [0; 256].into(),
            stack_frames: vec![],
            stack_base: 0,
            stack_len: 0,
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

    pub fn run(&mut self) {
        if let Some(entry_point) = self.entry_point {
            let func = self.get_function(entry_point);
            unsafe {
                let func = &*func;
                let mut stack = self.stack_frame(func.layout.size, 0);
                func.func.invoke(&mut stack);
            }
        }
    }
}

pub struct StackGuard<'a> {
    pub vm: &'a mut Vm,
}

impl<'a> Drop for StackGuard<'a> {
    fn drop(&mut self) {
        self.vm.stack_len = self.vm.stack_base;
        self.vm.stack_base = self.vm.stack_frames.pop().expect("Stack frame is present");
    }
}

impl<'a> StackGuard<'a> {
    pub fn stack_frame(&mut self, stack_size: usize, arg_size: usize) -> StackGuard {
        self.vm.stack_frame(stack_size, arg_size)
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

    pub fn push_arg(&mut self, size: usize) {
        let dst = self.vm.stack[self.vm.stack_len..].as_mut_ptr();
        let src = self.val_raw::<i64>();
        unsafe { std::ptr::copy(src, dst, size) };
        self.vm.stack_len += size;
    }
}

impl Vm {
    pub fn reserve(&mut self, stack_size: usize) {
        if self.stack_len + stack_size > self.stack.len() {
            panic!("Stack overflow");
        }
    }

    pub fn stack_frame(&mut self, stack_size: usize, arg_size: usize) -> StackGuard {
        self.stack_frames.push(self.stack_base);
        self.stack_base = self.stack_len - arg_size;
        self.stack_len = self.stack_base + stack_size;

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
