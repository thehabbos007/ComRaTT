use crate::source::{Binop, Const, Type};
use crate::types::{Sym, TypedExpr};
use std::collections::HashMap;

pub use comratt_vm::bytecode::{Function, Op};

use super::{FunRef, FunctionPrototype};

struct Compiler {
    functions: Vec<Option<Function>>,
    fn_map: HashMap<String, FunRef>,
    ops: Vec<Op>,
    locals: HashMap<String, u16>,
    next_local: u16,
}

impl Compiler {
    fn alloc_local(&mut self, name: &str) -> u16 {
        let idx = self.next_local;
        self.locals.insert(name.to_string(), idx);
        self.next_local += 1;
        idx
    }

    fn emit(&mut self, op: Op) {
        self.ops.push(op);
    }
    fn pos(&self) -> u32 {
        self.ops.len() as u32
    }

    fn compile_fn(&mut self, name: &str, params: &[(Sym, Type)], body: &TypedExpr) -> Function {
        self.ops.clear();
        self.locals.clear();
        self.next_local = 0;
        for (p, _) in params {
            self.alloc_local(p);
        }
        self.compile_expr(body);
        self.emit(Op::Return);
        Function {
            name: name.to_string(),
            param_count: params.len() as u16,
            local_count: self.next_local,
            ops: self.ops.clone(),
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) {
        match expr {
            TypedExpr::TConst(Const::CInt(n), _) => self.emit(Op::ConstI32(*n)),
            TypedExpr::TConst(Const::CBool(b), _) => self.emit(Op::ConstBool(*b)),
            TypedExpr::TConst(Const::CUnit, _) => self.emit(Op::ConstUnit),

            TypedExpr::TName(name, _) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.emit(Op::Load(idx));
                } else if let Some(&fn_ref) = self.fn_map.get(name) {
                    // 0-arg function call, as it's just a TName without TApp
                    match fn_ref {
                        FunRef::Wasm(idx) => self.emit(Op::CallWasm(idx, 0)),
                        FunRef::Bytecode(idx) => self.emit(Op::CallBytecode(idx, 0)),
                    }
                } else {
                    panic!("unbound variable in bytecode: {name}");
                }
            }

            TypedExpr::TPrim(op, lhs, rhs, _) => {
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                self.emit(match op {
                    Binop::Add => Op::Add,
                    Binop::Sub => Op::Sub,
                    Binop::Mul => Op::Mul,
                    Binop::Div => Op::Div,
                    Binop::Eq => Op::Eq,
                    Binop::Neq => Op::Neq,
                    Binop::Lt => Op::Lt,
                    Binop::Lte => Op::Lte,
                    Binop::Gt => Op::Gt,
                    Binop::Gte => Op::Gte,
                });
            }

            TypedExpr::TLet(name, _ty, rhs, body) => {
                self.compile_expr(rhs);
                let idx = self.alloc_local(name);
                self.emit(Op::Store(idx));
                self.compile_expr(body);
            }

            TypedExpr::TIfThenElse(cond, then_br, else_br, _) => {
                self.compile_expr(cond);
                let jif = self.pos();
                self.emit(Op::JumpIfFalse(0));
                self.compile_expr(then_br);
                let jmp = self.pos();
                self.emit(Op::Jump(0));
                let else_start = self.pos();
                self.compile_expr(else_br);
                let end = self.pos();
                self.ops[jif as usize] = Op::JumpIfFalse(else_start);
                self.ops[jmp as usize] = Op::Jump(end);
            }

            TypedExpr::TApp(f, args, _) => {
                if let TypedExpr::TName(name, _) = f.as_ref() {
                    for a in args {
                        self.compile_expr(a);
                    }
                    match self.fn_map.get(name) {
                        Some(FunRef::Wasm(idx)) => self.emit(Op::CallWasm(*idx, args.len() as u8)),
                        Some(FunRef::Bytecode(idx)) => {
                            self.emit(Op::CallBytecode(*idx, args.len() as u8))
                        }
                        None => panic!("unknown function: {name}"),
                    }
                } else {
                    panic!("unsupported application form in bytecode");
                }
            }

            TypedExpr::TTuple(elems, _) => {
                for e in elems {
                    self.compile_expr(e);
                }
                self.emit(Op::MakeTuple(elems.len() as u8));
            }

            TypedExpr::TAccess(expr, idx, _) => {
                self.compile_expr(expr);
                self.emit(Op::AccessTuple(*idx as u8));
            }

            TypedExpr::TLam(..) => {
                panic!("unsupported in hybrid bytecode: TLam (use top-level functions)")
            }
            other => panic!("unsupported in hybrid bytecode: {other:?}"),
        }
    }
}

pub fn compile_reactive(
    fns: &[FunctionPrototype],
    fn_map: &mut HashMap<String, FunRef>,
) -> Vec<Function> {
    let mut compiler = Compiler {
        functions: vec![],
        fn_map: fn_map.clone(),
        ops: vec![],
        locals: HashMap::new(),
        next_local: 0,
    };

    // Reserve slots for named reactive functions so they can reference each other
    for (name, _, _) in fns {
        let idx = compiler.functions.len() as u32;
        fn_map.insert(name.clone(), FunRef::Bytecode(idx));
        compiler.fn_map.insert(name.clone(), FunRef::Bytecode(idx));
        compiler.functions.push(None);
    }

    for (i, (name, params, body)) in fns.iter().enumerate() {
        let func = compiler.compile_fn(name, params, body);
        compiler.functions[i] = Some(func);
    }

    compiler.functions.into_iter().map(|f| f.unwrap()).collect()
}
