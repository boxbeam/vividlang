use std::marker::PhantomData;

use crate::{ir::Operation, vm::StackGuard};

fn make_i64_op(
    l: Func<'static>,
    r: Func<'static>,
    f: impl Fn(i64, i64) -> i64 + 'static,
) -> Func<'static> {
    make_func(move |stack| {
        let l = l.eval(stack);
        let r = r.eval(stack);
        stack.set_val(f(l, r));
    })
}

fn make_i64_r_const_op(
    l: Func<'static>,
    r: i64,
    f: impl Fn(i64, i64) -> i64 + 'static,
) -> Func<'static> {
    make_func(move |stack| {
        let l = l.eval(stack);
        stack.set_val(f(l, r));
    })
}

impl Operation {
    fn to_func(self, l: Box<Expr>, r: Box<Expr>) -> Func<'static> {
        let l = l.compile();

        if let &Expr::Int(val) = &*r {
            return match self {
                Operation::Add => make_i64_r_const_op(l, val, |a, b| a + b),
                Operation::Sub => make_i64_r_const_op(l, val, |a, b| a - b),
                Operation::Mul => make_i64_r_const_op(l, val, |a, b| a * b),
                Operation::Div => make_i64_r_const_op(l, val, |a, b| a / b),
                Operation::Gt => make_i64_r_const_op(l, val, |a, b| (a > b) as i64),
                Operation::Lt => make_i64_r_const_op(l, val, |a, b| (a < b) as i64),
                Operation::Gte => make_i64_r_const_op(l, val, |a, b| (a >= b) as i64),
                Operation::Lte => make_i64_r_const_op(l, val, |a, b| (a <= b) as i64),
                Operation::Eq => make_i64_r_const_op(l, val, |a, b| (a == b) as i64),
            };
        }

        let r = r.compile();
        match self {
            Operation::Add => make_i64_op(l, r, |a, b| a + b),
            Operation::Sub => make_i64_op(l, r, |a, b| a - b),
            Operation::Mul => make_i64_op(l, r, |a, b| a * b),
            Operation::Div => make_i64_op(l, r, |a, b| a / b),
            Operation::Gt => make_i64_op(l, r, |a, b| (a > b) as i64),
            Operation::Lt => make_i64_op(l, r, |a, b| (a < b) as i64),
            Operation::Gte => make_i64_op(l, r, |a, b| (a >= b) as i64),
            Operation::Lte => make_i64_op(l, r, |a, b| (a <= b) as i64),
            Operation::Eq => make_i64_op(l, r, |a, b| (a == b) as i64),
        }
    }
}

pub(crate) enum Expr {
    Int(i64),
    Bool(bool),
    Read(Dest, usize),
    BinOp(Operation, Box<Expr>, Box<Expr>),
    IntNeg(Box<Expr>),
    Native(Func<'static>),
    FunctionCall {
        id: usize,
        stack_size: usize,
        args: Vec<(Expr, usize)>,
    },
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
}

pub(crate) enum Dest {
    Global(usize),
    Local(usize),
}

pub(crate) enum Stmt {
    Expr(Expr),
    Assign64(Dest, Expr),
    Assign { dst: Dest, size: usize, val: Expr },
    Print(Expr),
    Input(Dest),
    If(Expr, Vec<Stmt>),
}

pub type State<'a> = StackGuard<'a>;

type RawFunc = for<'a> fn(*const (), &'a mut State);

pub(crate) struct Func<'a> {
    f: RawFunc,
    data: *const (),
    drop: fn(*const ()),
    phantom: PhantomData<&'a ()>,
}

fn make_func<'a, F>(f: F) -> Func<'a>
where
    F: Fn(&mut State) + 'a,
{
    let data = Box::into_raw(Box::new(f)) as *const ();
    let f: RawFunc = |data: *const (), stack| {
        let f = data as *const F;
        unsafe { (*f)(stack) }
    };
    let drop = |data: *const ()| {
        let f = data as *mut F;
        let f = unsafe { Box::from_raw(f) };
        drop(f);
    };
    Func {
        f,
        data,
        drop,
        phantom: PhantomData,
    }
}

fn make_lit<T>(val: T) -> Func<'static>
where
    T: Clone + 'static,
{
    make_func(move |stack| {
        stack.set_val(val.clone());
    })
}

fn make_func_cont<'a, F, C>(f: F, cont: C) -> Func<'a>
where
    C: MaybeCont + 'a,
    F: Fn(&mut State) + 'a,
{
    // TODO: Remove extra pointer dereference
    make_func(move |stack| {
        f(stack);
        cont.invoke(stack);
    })
}

impl<'a> Drop for Func<'a> {
    fn drop(&mut self) {
        let data = self.data;
        (self.drop)(data)
    }
}

impl<'a> Func<'a> {
    pub fn invoke(&self, stack: &mut State) {
        (self.f)(self.data as _, stack)
    }

    pub fn eval<T>(&self, stack: &mut State) -> T {
        self.invoke(stack);
        stack.get_val()
    }
}

impl Expr {
    fn compile(self) -> Func<'static> {
        let func = match self {
            Expr::Int(i) => make_lit(i),
            Expr::Bool(b) => make_lit(b),
            Expr::Read(dst, size) => match dst {
                Dest::Global(g) => make_func(move |stack| {
                    let ptr = stack.vm.stack[g..].as_mut_ptr();
                    let val = stack.val_raw::<i64>();
                    unsafe {
                        std::ptr::copy(ptr, val, size);
                    }
                }),
                Dest::Local(l) => make_func(move |stack| {
                    let ptr = stack.get_raw::<i64>(l);
                    let val = stack.val_raw::<i64>();
                    unsafe {
                        std::ptr::copy(ptr, val, size);
                    }
                }),
            },
            Expr::BinOp(op, l, r) => op.to_func(l, r),
            Expr::IntNeg(expr) => {
                let expr = expr.compile();
                make_func(move |stack| {
                    let val = -expr.eval::<i64>(stack);
                    stack.set_val(val);
                })
            }
            Expr::Native(func) => func,
            Expr::IfElse(expr, if_true, if_false) => {
                let expr = expr.compile();
                let if_true = compile_stmts(if_true);
                let if_false = compile_stmts(if_false);
                make_func(move |stack| {
                    if expr.eval::<bool>(stack) {
                        if_true.invoke(stack)
                    } else {
                        if_false.invoke(stack)
                    }
                })
            }
            Expr::FunctionCall {
                id,
                stack_size,
                args,
            } => {
                let push_args = push_args(args);
                make_func(move |stack| {
                    let mut inner = stack.stack_frame(stack_size);
                    push_args.invoke(&mut inner);
                    inner.vm.call_function(id);
                })
            }
        };

        func
    }

    /// TODO: Const inlining
    #[allow(unused)]
    fn is_const(&self) -> bool {
        match self {
            Expr::Int(_) | Expr::Bool(_) => true,
            Expr::Read(_, _) => false,
            Expr::BinOp(_, l, r) => l.is_const() && r.is_const(),
            Expr::IntNeg(expr) => expr.is_const(),
            Expr::Native(_) => false,
            Expr::IfElse(expr, _, _) => expr.is_const(),
            Expr::FunctionCall { .. } => false,
        }
    }
}

fn push_args(mut args: Vec<(Expr, usize)>) -> Func<'static> {
    let Some((expr, size)) = args.pop() else {
        return make_func(|_| {});
    };

    let mut last = make_func(push_arg(expr, 0, size));
    let mut addr = size;

    while let Some((expr, size)) = args.pop() {
        last = make_func_cont(push_arg(expr, addr, size), last);
        addr += size;
    }
    last
}

fn push_arg(arg: Expr, addr: usize, size: usize) -> impl Fn(&mut State) {
    let arg = arg.compile();
    move |stack| {
        arg.invoke(stack);
        let src: *mut i64 = stack.val_raw();
        let dst: *mut i64 = stack.get_raw(addr);
        unsafe { std::ptr::copy(src, dst, size) };
    }
}

trait MaybeCont {
    fn invoke(&self, state: &mut State);
}

impl<'a> MaybeCont for Func<'a> {
    fn invoke(&self, state: &mut State) {
        self.invoke(state);
    }
}

impl MaybeCont for () {
    fn invoke(&self, _state: &mut State) {}
}

fn compile_stmt_continuation<C: MaybeCont + 'static>(stmt: Stmt, next: C) -> Func<'static> {
    match stmt {
        Stmt::Expr(expr) => {
            let expr = expr.compile();
            make_func_cont(
                move |stack| {
                    expr.invoke(stack);
                },
                next,
            )
        }
        Stmt::Assign64(pos, expr) => {
            let expr = expr.compile();
            match pos {
                Dest::Global(g) => make_func_cont(
                    move |stack| {
                        let val = expr.eval::<i64>(stack);
                        *stack.get_global(g) = val;
                    },
                    next,
                ),
                Dest::Local(l) => make_func_cont(
                    move |stack| {
                        let val = expr.eval::<i64>(stack);
                        *stack.get(l) = val;
                    },
                    next,
                ),
            }
        }
        Stmt::Print(expr) => {
            let expr = expr.compile();
            make_func_cont(
                move |stack| {
                    let val = expr.eval::<i64>(stack);
                    println!("{val}");
                },
                next,
            )
        }
        Stmt::If(expr, func) => {
            let expr = expr.compile();
            let func = compile_stmts(func);
            make_func_cont(
                move |stack| {
                    if expr.eval::<bool>(stack) {
                        func.invoke(stack);
                    }
                },
                next,
            )
        }
        Stmt::Input(pos) => match pos {
            Dest::Global(g) => make_func_cont(
                move |stack| {
                    let line = std::io::stdin().lines().next().unwrap().unwrap();
                    let num: i64 = line.parse().unwrap();
                    *stack.get_global(g) = num;
                },
                next,
            ),
            Dest::Local(l) => make_func_cont(
                move |stack| {
                    let line = std::io::stdin().lines().next().unwrap().unwrap();
                    let num: i64 = line.parse().unwrap();
                    *stack.get(l) = num;
                },
                next,
            ),
        },
        Stmt::Assign {
            dst,
            size,
            val: expr,
        } => {
            let expr = expr.compile();
            match dst {
                Dest::Global(g) => make_func_cont(
                    move |stack| {
                        let dst: *mut i64 = stack.get_global_raw(g);
                        let src: *mut i64 = stack.val_raw();
                        expr.invoke(stack);
                        unsafe { std::ptr::copy(src, dst, size) };
                    },
                    next,
                ),
                Dest::Local(l) => make_func_cont(
                    move |stack| {
                        let dst: *mut i64 = stack.get_raw(l);
                        let src: *mut i64 = stack.val_raw();
                        expr.invoke(stack);
                        unsafe { std::ptr::copy(src, dst, size) };
                    },
                    next,
                ),
            }
        }
    }
}

pub fn compile_stmts(mut stmts: Vec<Stmt>) -> Func<'static> {
    let Some(last) = stmts.pop() else {
        return make_func(|_| ());
    };
    let mut last = compile_stmt_continuation(last, ());

    while let Some(prev) = stmts.pop() {
        last = compile_stmt_continuation(prev, last);
    }
    last
}
