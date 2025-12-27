use crate::{
    compile::{self, Dest},
    ir::{Block, Expr, FunctionDef, Stmt},
    registry::{Id, Registry, to_id},
    scope::{GlobalValue, Location},
    type_system::{Constraint, FunctionLayout, Type, TypeError, TypeKey, TypeSolver, type_key},
};

#[derive(Debug)]
pub struct StackLayout {
    pub locals: Vec<(Type, usize)>,
    pub size: usize,
    pub ret_size: usize,
}

pub struct BytecodeFunction {
    pub stmts: Vec<compile::Stmt>,
    pub layout: StackLayout,
}

#[derive(Clone, Debug)]
struct GlobalAddress {
    addr: usize,
}

pub struct BytecodeEmitter<'a> {
    pub(crate) types: &'a mut TypeSolver,
    globals: Registry<Id<GlobalValue>, GlobalAddress>,
    functions: Registry<Id<GlobalValue>, StackLayout>,
    size: usize,
}

impl<'a> BytecodeEmitter<'a> {
    pub fn new(types: &'a mut TypeSolver) -> Self {
        Self {
            types,
            globals: Default::default(),
            functions: Default::default(),
            size: 0,
        }
    }

    pub fn translate_function(
        &mut self,
        id: Id<GlobalValue>,
        func: FunctionDef,
    ) -> Result<BytecodeFunction, TypeError> {
        let func_layout = self.types.compute_abstract_layout(id, &func);
        let layout = self.types.compute_stack_layout(&func_layout)?;

        let stmts = self.translate_block(&layout, id, func.body)?;
        Ok(BytecodeFunction { stmts, layout })
    }

    fn translate_block(
        &mut self,
        layout: &StackLayout,
        id: Id<GlobalValue>,
        block: Block,
    ) -> Result<Vec<compile::Stmt>, TypeError> {
        let mut stmts = block.stmts;
        stmts.extend(block.eval.map(|expr| Stmt::Expr(*expr)));
        let mut out = vec![];

        for stmt in stmts {
            out.push(match stmt {
                Stmt::Expr(expr) => compile::Stmt::Expr(self.translate_expr(layout, id, expr)?),
                Stmt::Declare(ident, expr) | Stmt::Assign(ident, expr) => {
                    let (dst, size) = self.resolve_ident(layout, id, ident.loc())?;
                    let val = self.translate_expr(layout, id, expr)?;
                    compile::Stmt::Assign { dst, size, val }
                }
                Stmt::Input(ident) => {
                    let (dst, _) = self.resolve_ident(layout, id, ident.loc())?;
                    compile::Stmt::Input(dst)
                }
                Stmt::Print(expr) => {
                    let expr = self.translate_expr(layout, id, expr)?;
                    compile::Stmt::Print(expr)
                }
                Stmt::If(expr, block) => {
                    let expr = self.translate_expr(layout, id, expr)?;
                    let block = self.translate_block(layout, id, block)?;
                    compile::Stmt::If(expr, block)
                }
            })
        }

        Ok(out)
    }

    pub fn declare_function(
        &mut self,
        id: Id<GlobalValue>,
        layout: &FunctionLayout,
    ) -> Result<(), TypeError> {
        let stack_layout = self.types.compute_stack_layout(layout)?;
        let addr = self
            .functions
            .insert(id, stack_layout)
            .expect("Function is not declared twice");
        let key = TypeKey::Global(id);
        let id = self.types.constraint_id(key);
        *self.types.get_constraint(id).unwrap() = Constraint::Concrete(Type::Function(addr.raw()));
        Ok(())
    }

    fn translate_expr(
        &mut self,
        layout: &StackLayout,
        id: Id<GlobalValue>,
        expr: Expr,
    ) -> Result<compile::Expr, TypeError> {
        Ok(match expr {
            Expr::Int(i) => compile::Expr::Int(i),
            Expr::Bool(b) => compile::Expr::Bool(b),
            Expr::Var(ident) => {
                let (addr, size) = self.resolve_ident(layout, id, ident.loc())?;
                compile::Expr::Read(addr, size)
            }
            Expr::BinOp(op, l, r) => compile::Expr::BinOp(
                op,
                self.translate_expr(layout, id, *l)?.into(),
                self.translate_expr(layout, id, *r)?.into(),
            ),
            Expr::IfElse(expr, if_case, else_case) => {
                let expr = self.translate_expr(layout, id, *expr)?;
                let if_case = self.translate_block(layout, id, if_case)?;
                let else_case = self.translate_block(layout, id, else_case)?;
                compile::Expr::IfElse(expr.into(), if_case, else_case)
            }
            Expr::Neg(expr) => {
                let expr = self.translate_expr(layout, id, *expr)?;
                compile::Expr::IntNeg(expr.into())
            }
            Expr::FunctionCall { func, args } => {
                let typ = self.types.compute_expr_constraint(id, &func)?;
                let typ = self.types.resolve_constraint_ref(typ);

                let Constraint::Concrete(Type::Function(func_addr)) = typ else {
                    return Err(TypeError::CannotInvoke(typ));
                };

                let func_id = to_id(func_addr);
                let stack_layout = self
                    .functions
                    .get(func_id)
                    .expect("Stack layout is defined");
                let arg_sizes: Vec<_> = stack_layout
                    .locals
                    .iter()
                    .take(args.len())
                    .map(|(typ, _)| typ.size())
                    .collect();

                let mut new_args = vec![];
                for (arg, size) in args.into_iter().zip(arg_sizes) {
                    let new_arg = self.translate_expr(layout, id, arg)?;
                    new_args.push((new_arg, size));
                }

                compile::Expr::FunctionCall {
                    id: func_addr,
                    args: new_args,
                }
            }
        })
    }

    fn new_global(&mut self, id: Id<GlobalValue>, size: usize) -> GlobalAddress {
        let addr = GlobalAddress { addr: self.size };
        self.size += size;
        self.globals.insert(id, addr.clone());
        addr
    }

    fn resolve_ident(
        &mut self,
        layout: &StackLayout,
        id: Id<GlobalValue>,
        loc: Location,
    ) -> Result<(Dest, usize), TypeError> {
        let key = type_key(id, loc.clone());
        let id = self.types.constraint_id(key);
        let typ = self.types.get_constraint(id).cloned().unwrap();
        Ok(match loc {
            Location::Global(gid) => {
                let Constraint::Concrete(c) = typ else {
                    return Err(TypeError::MissingTypes(vec![id]));
                };
                let global = match self.globals.lookup(&gid) {
                    Some(addr) => addr.addr,
                    None => self.new_global(gid, c.size()).addr,
                };
                (Dest::Global(global), c.size())
            }
            Location::Local(id) => {
                let (typ, addr) = &layout.locals[id.raw()];
                (Dest::Local(*addr), typ.size())
            }
        })
    }
}
