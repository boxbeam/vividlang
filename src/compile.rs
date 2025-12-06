use std::marker::PhantomData;

use crate::{ir::Operation, vm::StackGuard};

fn transmute<T, V>(val: T) -> V
where
    V: Copy,
{
    unsafe { *(&val as *const _ as *const V) }
}

fn make_op<T>(l: Box<Expr>, r: Box<Expr>, f: impl Fn(T, T) -> T + 'static) -> Func<'static>
where
    T: 'static + Copy,
{
    if let &Expr::Int(val) = &*r {
        let val = unsafe { *(&val as *const _ as *const T) };
        return make_i64_op_r_const(l, val, f);
    }
    let l = l.compile();
    let r = r.compile();
    make_func(move |stack| {
        let l = transmute(l.invoke(stack));
        let r = transmute(r.invoke(stack));
        transmute(f(l, r))
    })
}

fn make_i64_op_r_const<T>(l: Box<Expr>, r: T, f: impl Fn(T, T) -> T + 'static) -> Func<'static>
where
    T: 'static + Copy,
{
    if let Expr::Read(dest, _) = &*l {
        return make_i64_op_var_const(dest.clone(), r, f);
    }
    let l = l.compile();
    make_func(move |stack| {
        let l = transmute(l.invoke(stack));
        transmute(f(l, r))
    })
}

fn make_i64_op_var_const<T>(l: Dest, r: T, f: impl Fn(T, T) -> T + 'static) -> Func<'static>
where
    T: 'static + Copy,
{
    match l {
        Dest::Global(g) => make_func(move |stack| {
            let l = *stack.get_global(g);
            transmute(f(l, r))
        }),
        Dest::Local(l) => make_func(move |stack| {
            let l = *stack.get(l);
            transmute(f(l, r))
        }),
    }
}

impl Operation {
    fn to_func(self, l: Box<Expr>, r: Box<Expr>) -> Func<'static> {
        match self {
            Operation::Add => make_op::<i64>(l, r, |a, b| a + b),
            Operation::Sub => make_op::<i64>(l, r, |a, b| a - b),
            Operation::Mul => make_op::<i64>(l, r, |a, b| a * b),
            Operation::Div => make_op::<i64>(l, r, |a, b| a / b),
            Operation::Gt => make_op::<i64>(l, r, |a, b| (a > b) as i64),
            Operation::Lt => make_op::<i64>(l, r, |a, b| (a < b) as i64),
            Operation::Gte => make_op::<i64>(l, r, |a, b| (a >= b) as i64),
            Operation::Lte => make_op::<i64>(l, r, |a, b| (a <= b) as i64),
            Operation::Eq => make_op::<i64>(l, r, |a, b| (a == b) as i64),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Expr {
    Int(i64),
    Bool(bool),
    Read(Dest, usize),
    BinOp(Operation, Box<Expr>, Box<Expr>),
    IntNeg(Box<Expr>),
    FunctionCall {
        id: usize,
        stack_size: usize,
        args_size: usize,
        args: Vec<(Expr, usize)>,
        ret_size: usize,
    },
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub(crate) enum Dest {
    Global(usize),
    Local(usize),
}

#[derive(Debug)]
pub(crate) enum Stmt {
    Expr(Expr),
    Assign { dst: Dest, size: usize, val: Expr },
    Print(Expr),
    Input(Dest),
    If(Expr, Vec<Stmt>),
}

pub type State<'a> = StackGuard<'a>;

type RawFunc = for<'a> fn(*const (), &'a mut State) -> i64;

pub(crate) struct Func<'a> {
    f: RawFunc,
    data: *const (),
    drop: fn(*const ()),
    phantom: PhantomData<&'a ()>,
}

impl std::fmt::Debug for Func<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Func")
    }
}

fn make_func<'a, F>(f: F) -> Func<'a>
where
    F: Fn(&mut State) -> i64 + 'a,
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

fn make_primitive_lit<T>(val: T) -> Func<'static>
where
    T: Clone + 'static,
{
    make_func(move |_| unsafe { *(&val as *const _ as *const i64) })
}

fn make_func_cont<'a, F, C>(f: F, cont: C) -> Func<'a>
where
    C: MaybeCont + 'a,
    F: Fn(&mut State) -> i64 + 'a,
{
    // TODO: Remove extra pointer dereference
    make_func(move |stack| {
        let val = f(stack);
        cont.invoke(stack, val)
    })
}

impl<'a> Drop for Func<'a> {
    fn drop(&mut self) {
        let data = self.data;
        (self.drop)(data)
    }
}

impl<'a> Func<'a> {
    pub fn invoke(&self, stack: &mut State) -> i64 {
        (self.f)(self.data as _, stack)
    }
}

impl Expr {
    fn compile(self) -> Func<'static> {
        let func = match self {
            Expr::Int(i) => make_primitive_lit(i),
            Expr::Bool(b) => make_primitive_lit(b),
            Expr::Read(dst, size) => {
                if size <= 1 {
                    match dst {
                        Dest::Global(g) => make_func(move |stack| stack.vm.stack[g]),
                        Dest::Local(l) => make_func(move |stack| *stack.get(l)),
                    }
                } else {
                    todo!()
                }
            }
            Expr::BinOp(op, l, r) => op.to_func(l, r),
            Expr::IntNeg(expr) => {
                let expr = expr.compile();
                make_func(move |stack| -expr.invoke(stack))
            }
            Expr::IfElse(expr, if_true, if_false) => {
                let expr = expr.compile();
                let if_true = compile_stmts(if_true);
                let if_false = compile_stmts(if_false);
                make_func(move |stack| {
                    if expr.invoke(stack) != 0 {
                        if_true.invoke(stack)
                    } else {
                        if_false.invoke(stack)
                    }
                })
            }
            Expr::FunctionCall {
                id,
                stack_size,
                args_size,
                args,
                ret_size,
            } => {
                let push_args = push_args(args);
                if ret_size <= 1 {
                    make_func(move |stack| {
                        push_args.invoke(stack);
                        let mut inner = stack.stack_frame(stack_size, args_size);
                        let func = inner.vm.get_function(id);
                        unsafe { (*func).func.invoke(&mut inner) }
                    })
                } else {
                    todo!()
                }
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
            Expr::IfElse(expr, _, _) => expr.is_const(),
            Expr::FunctionCall { .. } => false,
        }
    }
}

fn push_args(mut args: Vec<(Expr, usize)>) -> Func<'static> {
    let Some((expr, size)) = args.pop() else {
        return make_func(|_| 0);
    };

    let mut last = if size <= 1 {
        make_func(push_arg_primitive(expr))
    } else {
        make_func(push_arg(expr, size))
    };

    while let Some((expr, size)) = args.pop() {
        last = if size <= 1 {
            make_func_cont(push_arg_primitive(expr), last)
        } else {
            make_func_cont(push_arg(expr, size), last)
        };
    }
    last
}

fn push_arg_primitive(arg: Expr) -> impl Fn(&mut State) -> i64 {
    let arg = arg.compile();
    move |stack| {
        let val = arg.invoke(stack);
        stack.push_arg_raw(val);
        0
    }
}

fn push_arg(arg: Expr, size: usize) -> impl Fn(&mut State) -> i64 {
    let arg = arg.compile();
    move |stack| {
        arg.invoke(stack);
        stack.push_arg(size);
        0
    }
}

trait MaybeCont {
    fn invoke(&self, state: &mut State, last: i64) -> i64;
}

impl<'a> MaybeCont for Func<'a> {
    fn invoke(&self, state: &mut State, _last: i64) -> i64 {
        self.invoke(state)
    }
}

impl MaybeCont for () {
    fn invoke(&self, _state: &mut State, last: i64) -> i64 {
        last
    }
}

fn compile_stmt_continuation<C: MaybeCont + 'static>(stmt: Stmt, next: C) -> Func<'static> {
    match stmt {
        Stmt::Expr(expr) => {
            let expr = expr.compile();
            make_func_cont(move |stack| expr.invoke(stack), next)
        }
        Stmt::Print(expr) => {
            let expr = expr.compile();
            make_func_cont(
                move |stack| {
                    let val = expr.invoke(stack);
                    println!("{val}");
                    0
                },
                next,
            )
        }
        Stmt::If(expr, func) => {
            let expr = expr.compile();
            let func = compile_stmts(func);
            make_func_cont(
                move |stack| {
                    if expr.invoke(stack) != 0 {
                        func.invoke(stack);
                    }
                    0
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
                    0
                },
                next,
            ),
            Dest::Local(l) => make_func_cont(
                move |stack| {
                    let line = std::io::stdin().lines().next().unwrap().unwrap();
                    let num: i64 = line.parse().unwrap();
                    *stack.get(l) = num;
                    0
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
            if size <= 1 {
                match dst {
                    Dest::Global(g) => make_func_cont(
                        move |stack| {
                            *stack.get_global(g) = expr.invoke(stack);
                            0
                        },
                        next,
                    ),
                    Dest::Local(l) => make_func_cont(
                        move |stack| {
                            *stack.get(l) = expr.invoke(stack);
                            0
                        },
                        next,
                    ),
                }
            } else {
                match dst {
                    Dest::Global(g) => make_func_cont(
                        move |stack| {
                            let dst: *mut i64 = stack.get_global_raw(g);
                            let src: *mut i64 = stack.val_raw();
                            expr.invoke(stack);
                            unsafe { std::ptr::copy(src, dst, size) };
                            0
                        },
                        next,
                    ),
                    Dest::Local(l) => make_func_cont(
                        move |stack| {
                            let dst: *mut i64 = stack.get_raw(l);
                            let src: *mut i64 = stack.val_raw();
                            expr.invoke(stack);
                            unsafe { std::ptr::copy(src, dst, size) };
                            0
                        },
                        next,
                    ),
                }
            }
        }
    }
}

pub fn compile_stmts(mut stmts: Vec<Stmt>) -> Func<'static> {
    let Some(last) = stmts.pop() else {
        return make_func(|_| 0);
    };
    let mut last = compile_stmt_continuation(last, ());

    while let Some(prev) = stmts.pop() {
        last = compile_stmt_continuation(prev, last);
    }
    last
}
