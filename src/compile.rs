use std::{marker::PhantomData, rc::Rc};

use crate::{ir::Operation, vm::StackGuard};

fn transmute<T, V>(val: T) -> V
where
    V: Copy,
{
    unsafe { *(&val as *const _ as *const V) }
}

fn make_op<T>(
    l: Box<Expr>,
    r: Box<Expr>,
    f: impl Fn(T, T) -> T + 'static,
    cont: impl MaybeCont + 'static,
    stack_size: usize,
) -> Func<'static>
where
    T: 'static + Copy,
{
    if let &Expr::Int(val) = &*r {
        let val = unsafe { *(&val as *const _ as *const T) };
        return make_op_r_const(l, val, f, cont, stack_size);
    }
    let r = r.compile((), stack_size);
    l.compile(
        make_func(move |stack, prev| {
            let r = r.invoke(stack, 0);
            cont.cont(stack, transmute(f(transmute(prev), transmute(r))))
        }),
        stack_size,
    )
}

fn make_op_r_const<T>(
    l: Box<Expr>,
    r: T,
    f: impl Fn(T, T) -> T + 'static,
    cont: impl MaybeCont + 'static,
    stack_size: usize,
) -> Func<'static>
where
    T: 'static + Copy,
{
    if let Expr::Read(dest, _) = &*l {
        return make_i64_op_var_const(dest.clone(), r, f, cont);
    }

    l.compile(
        make_func(move |stack, prev| cont.cont(stack, transmute(f(transmute(prev), r)))),
        stack_size,
    )
}

fn make_i64_op_var_const<T>(
    l: Dest,
    r: T,
    f: impl Fn(T, T) -> T + 'static,
    cont: impl MaybeCont + 'static,
) -> Func<'static>
where
    T: 'static + Copy,
{
    match l {
        Dest::Global(g) => make_func(move |stack, _| {
            let l = *stack.get_global(g);
            cont.cont(stack, transmute(f(l, r)))
        }),
        Dest::Local(l) => make_func(move |stack, _| {
            let l = *stack.get(l);
            cont.cont(stack, transmute(f(l, r)))
        }),
    }
}

impl Operation {
    fn to_func(
        self,
        l: Box<Expr>,
        r: Box<Expr>,
        cont: impl MaybeCont + 'static,
        stack_size: usize,
    ) -> Func<'static> {
        match self {
            Operation::Add => make_op::<i64>(l, r, |a, b| a + b, cont, stack_size),
            Operation::Sub => make_op::<i64>(l, r, |a, b| a - b, cont, stack_size),
            Operation::Mul => make_op::<i64>(l, r, |a, b| a * b, cont, stack_size),
            Operation::Div => make_op::<i64>(l, r, |a, b| a / b, cont, stack_size),
            Operation::Gt => make_op::<i64>(l, r, |a, b| (a > b) as i64, cont, stack_size),
            Operation::Lt => make_op::<i64>(l, r, |a, b| (a < b) as i64, cont, stack_size),
            Operation::Gte => make_op::<i64>(l, r, |a, b| (a >= b) as i64, cont, stack_size),
            Operation::Lte => make_op::<i64>(l, r, |a, b| (a <= b) as i64, cont, stack_size),
            Operation::Eq => make_op::<i64>(l, r, |a, b| (a == b) as i64, cont, stack_size),
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
    FunctionCall { id: usize, args: Vec<(Expr, usize)> },
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

type RawFunc = for<'a> fn(*const (), State, prev: i64) -> i64;

/// Box<dyn Fn...> introduces an unnecessary layer of indirection via a vtable pointing to a function pointer.
/// This is a custom wide pointer inspired by https://github.com/zesterer/vm-perf/blob/main/src/closure_continuations.rs#L46-L49
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

/// Calling convention: All expressions which evaluate to <= 64 bits
/// are passed in the return value of the closure. All other values
/// are passed in the return register of the VM.
fn make_func<'a, F>(f: F) -> Func<'a>
where
    F: Fn(State, i64) -> i64 + 'a,
{
    let data = Rc::into_raw(Rc::new(f)) as *const ();
    let f: RawFunc = |data: *const (), stack, prev| {
        let f = data as *const F;
        unsafe { (*f)(stack, prev) }
    };
    let drop = |data: *const ()| {
        let f = data as *mut F;
        let f = unsafe { Rc::from_raw(f) };
        drop(f);
    };
    Func {
        f,
        data,
        drop,
        phantom: PhantomData,
    }
}

fn make_func_cont<'a, F>(f: F, cont: impl MaybeCont + 'a) -> Func<'a>
where
    F: Fn(State, i64) -> i64 + 'a,
{
    make_func(move |stack, prev| {
        let val = f(stack, prev);
        cont.cont(stack, val)
    })
}

fn make_primitive_lit<T>(val: T, cont: impl MaybeCont + 'static) -> Func<'static>
where
    T: Clone + 'static,
{
    make_func(move |stack, _| unsafe { cont.cont(stack, *(&val as *const _ as *const i64)) })
}

impl<'a> Drop for Func<'a> {
    fn drop(&mut self) {
        let data = self.data;
        (self.drop)(data)
    }
}

trait MaybeCont {
    fn cont(&self, stack: State, prev: i64) -> i64;
}

impl<'a> MaybeCont for Func<'a> {
    fn cont(&self, stack: State, prev: i64) -> i64 {
        self.invoke(stack, prev)
    }
}

impl MaybeCont for () {
    fn cont(&self, _stack: State, prev: i64) -> i64 {
        prev
    }
}

impl<'a> Func<'a> {
    pub fn invoke(&self, stack: State, prev: i64) -> i64 {
        (self.f)(self.data, stack, prev)
    }
}

impl Expr {
    fn compile(self, cont: impl MaybeCont + 'static, stack_size: usize) -> Func<'static> {
        let func = match self {
            Expr::Int(i) => make_primitive_lit(i, cont),
            Expr::Bool(b) => make_primitive_lit(b, cont),
            Expr::Read(dst, size) => {
                if size <= 1 {
                    match dst {
                        Dest::Global(g) => make_func(move |stack, _| {
                            cont.cont(stack, unsafe { stack.get_global_raw::<i64>(g).read() })
                        }),
                        Dest::Local(l) => make_func(move |stack, _| {
                            let prev = *stack.get(l);
                            cont.cont(stack, prev)
                        }),
                    }
                } else {
                    todo!()
                }
            }
            Expr::BinOp(op, l, r) => op.to_func(l, r, cont, stack_size),
            Expr::IntNeg(expr) => expr.compile(
                make_func(move |stack, prev| cont.cont(stack, -prev)),
                stack_size,
            ),
            Expr::IfElse(expr, if_true, if_false) => {
                let if_true = compile_stmts(if_true, stack_size);
                let if_false = compile_stmts(if_false, stack_size);
                expr.compile(
                    make_func(move |stack, prev| {
                        let val = if prev == 1 {
                            if_true.invoke(stack, 0)
                        } else {
                            if_false.invoke(stack, 0)
                        };
                        cont.cont(stack, val)
                    }),
                    stack_size,
                )
            }
            Expr::FunctionCall { id, args } => {
                let call = make_func(move |mut stack, _| {
                    let inner = stack.stack_frame(stack_size);
                    let func = inner.vm.get_function(id);
                    let val = unsafe { (*func).func.invoke(inner, 0) };
                    cont.cont(stack, val)
                });
                push_args(args, call, stack_size)
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

fn push_args(
    mut args: Vec<(Expr, usize)>,
    cont: Func<'static>,
    stack_size: usize,
) -> Func<'static> {
    let Some((expr, size)) = args.pop() else {
        return cont;
    };

    let mut last = if size <= 1 {
        push_arg_primitive(expr, cont, stack_size)
    } else {
        push_arg(expr, size, cont, stack_size)
    };

    let mut addr = stack_size + size;

    while let Some((expr, size)) = args.pop() {
        last = if size <= 1 {
            push_arg_primitive(expr, last, addr)
        } else {
            push_arg(expr, size, last, addr)
        };
        addr += size;
    }
    last
}

fn push_arg_primitive(arg: Expr, cont: impl MaybeCont + 'static, pos: usize) -> Func<'static> {
    arg.compile(
        make_func(move |stack, prev| {
            unsafe { stack.stack.offset(pos as isize).write(prev) }
            cont.cont(stack, 0)
        }),
        pos,
    )
}

fn push_arg(arg: Expr, size: usize, cont: impl MaybeCont + 'static, pos: usize) -> Func<'static> {
    arg.compile(
        make_func(move |stack, _prev| {
            unsafe {
                let dst = stack.stack.offset(pos as isize);
                let src = stack.val_register_raw::<i64>();
                std::ptr::copy(src, dst, size);
            }
            cont.cont(stack, 0)
        }),
        pos,
    )
}

fn compile_stmt_continuation<C: MaybeCont + 'static>(
    stmt: Stmt,
    cont: C,
    stack_size: usize,
) -> Func<'static> {
    match stmt {
        Stmt::Expr(expr) => expr.compile(cont, stack_size),
        Stmt::Print(expr) => expr.compile(
            make_func_cont(
                move |_stack, prev| {
                    println!("{prev}");
                    0
                },
                cont,
            ),
            stack_size,
        ),
        Stmt::If(expr, block) => {
            let block = compile_stmts(block, stack_size);
            expr.compile(
                make_func_cont(
                    move |stack, prev| {
                        if prev == 1 {
                            block.invoke(stack, 0);
                        }
                        0
                    },
                    cont,
                ),
                stack_size,
            )
        }
        Stmt::Input(pos) => match pos {
            Dest::Global(g) => make_func_cont(
                move |stack, _| {
                    let line = std::io::stdin().lines().next().unwrap().unwrap();
                    let num: i64 = line.parse().unwrap();
                    *stack.get_global(g) = num;
                    0
                },
                cont,
            ),
            Dest::Local(l) => make_func_cont(
                move |stack, _| {
                    let line = std::io::stdin().lines().next().unwrap().unwrap();
                    let num: i64 = line.parse().unwrap();
                    *stack.get(l) = num;
                    0
                },
                cont,
            ),
        },
        Stmt::Assign {
            dst,
            size,
            val: expr,
        } => {
            let assign = if size <= 1 {
                match dst {
                    Dest::Global(g) => make_func_cont(
                        move |stack, prev| {
                            *stack.get_global(g) = prev;
                            0
                        },
                        cont,
                    ),
                    Dest::Local(l) => make_func_cont(
                        move |stack, prev| {
                            *stack.get(l) = prev;
                            0
                        },
                        cont,
                    ),
                }
            } else {
                match dst {
                    Dest::Global(g) => make_func_cont(
                        move |stack, _| {
                            let dst: *mut i64 = stack.get_global_raw(g);
                            let src: *mut i64 = stack.val_register_raw();
                            unsafe { std::ptr::copy(src, dst, size) };
                            0
                        },
                        cont,
                    ),
                    Dest::Local(l) => make_func_cont(
                        move |stack, _| {
                            let dst: *mut i64 = stack.get_raw(l);
                            let src: *mut i64 = stack.val_register_raw();
                            unsafe { std::ptr::copy(src, dst, size) };
                            0
                        },
                        cont,
                    ),
                }
            };
            expr.compile(assign, stack_size)
        }
    }
}

pub fn compile_stmts(mut stmts: Vec<Stmt>, stack_size: usize) -> Func<'static> {
    let Some(last) = stmts.pop() else {
        return make_func(|_, _| 0);
    };
    let mut last = compile_stmt_continuation(last, (), stack_size);

    while let Some(prev) = stmts.pop() {
        last = compile_stmt_continuation(prev, last, stack_size);
    }
    last
}
