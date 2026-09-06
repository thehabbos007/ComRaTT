use crate::source::{Binop, ClockExpr, Const, Type};
use crate::types::{Sym, TypedExpr, find_free_var_names};
use std::collections::{BTreeSet, HashMap, HashSet};

pub use comratt_vm::bytecode::{Function, Op};

use super::{FunRef, FunctionPrototype};

struct Compiler {
    functions: Vec<Option<Function>>,
    fn_map: HashMap<String, FunRef>,
    ops: Vec<Op>,
    locals: HashMap<String, u16>,
    next_local: u16,
    channel_indices: HashMap<String, u16>,
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
                    panic!("non-name function in bytecode: {f:?}");
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

            TypedExpr::TAdvance(name, _) => {
                self.emit(Op::Load(self.locals[name]));
                self.emit(Op::Force);
            }

            TypedExpr::TDelay(body, _, _) if matches!(body.as_ref(), TypedExpr::TWait(..)) => {
                let TypedExpr::TWait(channel_name, _) = body.as_ref() else {
                    unreachable!()
                };
                let idx = *self
                    .channel_indices
                    .get(channel_name)
                    .unwrap_or_else(|| panic!("unknown channel: {channel_name}"));
                self.emit(Op::Wait(idx));
            }

            TypedExpr::TDelay(body, clock, _) => {
                let fv = find_free_var_names(body, &HashSet::new());
                let mut captures: Vec<String> = fv
                    .into_iter()
                    .filter(|n| self.locals.contains_key(n))
                    .collect();
                captures.sort();
                for c in &captures {
                    self.emit(Op::Load(self.locals[c]));
                }

                if self.channel_indices.is_empty() {
                    self.emit(Op::ConstClock(0x0));
                } else {
                    self.compile_clock_expr(clock);
                }

                let thunk_idx = self.compile_thunk(body, &captures);
                self.emit(Op::Thunk(thunk_idx, captures.len() as u8));
            }

            TypedExpr::TLam(..) => {
                panic!("unsupported in hybrid bytecode: TLam (use top-level functions)")
            }

            TypedExpr::TSelect(v1, v2, branches, _) => {
                // Start by getting the indices of the args
                let Some(&v1_idx) = self.locals.get(v1) else {
                    unreachable!("Unbound v1 variable in select: {}", v1);
                };

                let Some(&v2_idx) = self.locals.get(v2) else {
                    unreachable!("Unbound v2 variable in select: {}", v2);
                };

                // Push the current ticking clock
                self.emit(Op::PushTick);

                // Compare v1 clock with current tick using bitwise AND
                self.emit(Op::Load(v1_idx));
                self.emit(Op::GetClock);
                self.emit(Op::BitAnd);

                // If we jump here we know that the active channel is
                // not in the clock of v1 and so it must be in the clock
                // of v2.
                // Save the position and emit a JIF placeholder.
                let jif_v2_only = self.pos();
                self.emit(Op::JumpIfFalse(0));

                // Here we did not jump so the active channel
                // is in the clock of v1.
                // Now check v2 also.
                self.emit(Op::PushTick);
                self.emit(Op::Load(v2_idx));
                self.emit(Op::GetClock);
                self.emit(Op::BitAnd);

                // If we jump here we know that the active channel
                // is not in the clock of v2, only v1.
                // Save the position and emit a JIF placeholder.
                let jif_v1_only = self.pos();
                self.emit(Op::JumpIfFalse(0));

                // At this point we did not jump and know that
                // the both case is relevant
                self.compile_expr(&branches[2].2);

                // Jump end target for the both case
                let jmp_end_both = self.pos();
                self.emit(Op::Jump(0));

                // Actual JIF target for v2 only
                let v2_only = self.pos();
                self.compile_expr(&branches[1].2);

                // Jump end target for v2 only
                let jmp_end_v2_only = self.pos();
                self.emit(Op::Jump(0));

                // Actual JIF target for v1 only
                let v1_only = self.pos();
                self.compile_expr(&branches[0].2);

                // Jump end target for all branches
                let end = self.pos();

                // Fix up placeholder jumping points
                self.ops[jif_v2_only as usize] = Op::JumpIfFalse(v2_only);
                self.ops[jif_v1_only as usize] = Op::JumpIfFalse(v1_only);

                self.ops[jmp_end_both as usize] = Op::Jump(end);
                self.ops[jmp_end_v2_only as usize] = Op::Jump(end);
            }

            other => panic!("unsupported in hybrid bytecode: {other:?}"),
        }
    }

    fn compile_thunk(&mut self, body: &TypedExpr, captures: &[String]) -> u32 {
        let saved = (
            std::mem::take(&mut self.ops),
            std::mem::take(&mut self.locals),
            self.next_local,
        );
        self.next_local = 0;
        for c in captures {
            self.alloc_local(c);
        }
        self.compile_expr(body);
        self.emit(Op::Return);

        let idx = self.functions.len() as u32;
        self.functions.push(Some(Function {
            name: format!("thunk_{idx}"),
            param_count: captures.len() as u16,
            local_count: self.next_local,
            ops: std::mem::take(&mut self.ops),
        }));

        self.ops = saved.0;
        self.locals = saved.1;
        self.next_local = saved.2;
        idx
    }

    fn compile_clock_expr(&mut self, clock: &BTreeSet<ClockExpr>) {
        if clock.is_empty() {
            self.emit(Op::ConstClock(0x0));
            return;
        }

        let mut first = true;
        for ce in clock {
            match ce {
                ClockExpr::Wait(name) => {
                    let idx = *self
                        .channel_indices
                        .get(name)
                        .unwrap_or_else(|| panic!("unknown channel in clock expr: {name}"));
                    self.emit(Op::ConstClock(1u32 << idx));
                }
                ClockExpr::Cl(var) => {
                    if let Some(&local_idx) = self.locals.get(var) {
                        self.emit(Op::Load(local_idx));
                        self.emit(Op::GetClock);
                    } else {
                        self.emit(Op::ConstClock(0x0));
                    }
                }
                ClockExpr::Universal => {
                    panic!("Tried to generate clock from symbolic clockexpr")
                }
                ClockExpr::Var(v) => {
                    panic!("Received clock expression, should have been resolved {v:?}")
                }
            }
            if !first {
                self.emit(Op::BitOr);
            }
            first = false;
        }
    }
}

pub fn compile_reactive(
    fns: &[FunctionPrototype],
    fn_map: &mut HashMap<String, FunRef>,
    channel_indices: HashMap<String, u16>,
) -> Vec<Function> {
    let mut compiler = Compiler {
        functions: vec![],
        fn_map: fn_map.clone(),
        ops: vec![],
        locals: HashMap::new(),
        next_local: 0,
        channel_indices,
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
