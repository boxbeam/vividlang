use std::ops::{Deref, DerefMut};

pub struct Stack {
    pub(crate) inner: Box<[i64]>,
    pub(crate) len: usize,
}

impl Deref for Stack {
    type Target = [i64];

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}

impl DerefMut for Stack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.inner
    }
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            inner: vec![0; 32000].into_boxed_slice(),
            len: 0,
        }
    }

    pub fn reserve(&mut self, size: usize) {
        if self.len + size > self.inner.len() {
            let mut new = vec![0i64; self.inner.len() * 2].into_boxed_slice();
            let dst = (*new).as_mut_ptr();
            let src = (*self.inner).as_mut_ptr();
            unsafe { std::ptr::copy(src, dst, self.inner.len()) }
            self.inner = new;
        }
        self.len += size;
    }

    pub fn release(&mut self, size: usize) {
        self.len -= size;
    }
}
