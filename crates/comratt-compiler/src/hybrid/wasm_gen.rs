use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, GlobalSection,
    GlobalType, Instruction, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::source::{Binop, Const};
use crate::types::{collect_local_names, max_tuple_depth, TypedExpr};

use super::FunctionPrototype;

const HEAP_PTR_GLOBAL: u32 = 0;

pub fn compile_pure(fns: &[FunctionPrototype]) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut code = CodeSection::new();
    let mut memory = MemorySection::new();
    let mut globals = GlobalSection::new();

    memory.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });

    // Global ehap pointer for tuple allocs
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &wasm_encoder::ConstExpr::i32_const(0),
    );

    let fn_indices: HashMap<&str, u32> = fns
        .iter()
        .enumerate()
        .map(|(i, (name, _, _))| (name.as_str(), i as u32))
        .collect();

    for (i, (name, params, body)) in fns.iter().enumerate() {
        let param_types: Vec<ValType> = params.iter().map(|_| ValType::I32).collect();
        types.ty().function(param_types, vec![ValType::I32]);
        functions.function(i as u32);
        exports.export(name, ExportKind::Func, i as u32);

        let mut locals_map: HashMap<&str, u32> = HashMap::new();
        for (j, (pname, _)) in params.iter().enumerate() {
            locals_map.insert(pname.as_str(), j as u32);
        }
        let param_count = params.len() as u32;
        let let_locals = collect_local_names(body);
        for (j, local_name) in let_locals.iter().enumerate() {
            locals_map.insert(local_name.as_str(), param_count + j as u32);
        }

        let tuple_depth = max_tuple_depth(body);
        let uses_tuples = tuple_depth > 0;

        // let-bound vars + heap ptr + return_val + tuples
        let saved_hp_local = param_count + let_locals.len() as u32;
        let return_val_local = saved_hp_local + 1;
        let tuple_base_start = return_val_local + 1;
        let extra_local_count =
            let_locals.len() as u32 + if uses_tuples { 2 + tuple_depth } else { 0 };

        let extra_locals: Vec<(u32, ValType)> = if extra_local_count == 0 {
            vec![]
        } else {
            vec![(extra_local_count, ValType::I32)]
        };
        let mut func = Function::new(extra_locals);

        let mut ctx = EmitCtx {
            locals_map: &locals_map,
            fn_indices: &fn_indices,
            tuple_base_start,
            tuple_depth: 0,
        };

        if uses_tuples {
            // save global heap_ptr on entry
            func.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
            func.instruction(&Instruction::LocalSet(saved_hp_local));
        }

        ctx.emit_expr(&mut func, body);

        if uses_tuples {
            // heap stuff to work with tuples, we restore original return value
            func.instruction(&Instruction::LocalSet(return_val_local));
            func.instruction(&Instruction::LocalGet(saved_hp_local));
            func.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));
            func.instruction(&Instruction::LocalGet(return_val_local));
        }

        func.instruction(&Instruction::End);
        code.function(&func);
    }

    module.section(&types);
    module.section(&functions);
    module.section(&memory);
    module.section(&globals);
    module.section(&exports);
    module.section(&code);
    module.finish()
}

struct EmitCtx<'a> {
    locals_map: &'a HashMap<&'a str, u32>,
    fn_indices: &'a HashMap<&'a str, u32>,
    tuple_base_start: u32,
    tuple_depth: u32,
}

impl EmitCtx<'_> {
    fn emit_expr(&mut self, func: &mut Function, expr: &TypedExpr) {
        match expr {
            TypedExpr::TConst(Const::CInt(n), _) => {
                func.instruction(&Instruction::I32Const(*n));
            }
            TypedExpr::TConst(Const::CBool(b), _) => {
                func.instruction(&Instruction::I32Const(*b as i32));
            }
            TypedExpr::TConst(Const::CUnit, _) => {
                func.instruction(&Instruction::I32Const(0));
            }

            TypedExpr::TName(name, _) => {
                if let Some(&idx) = self.locals_map.get(name.as_str()) {
                    func.instruction(&Instruction::LocalGet(idx));
                } else {
                    panic!("unbound variable in WASM: {name}");
                }
            }

            TypedExpr::TPrim(op, lhs, rhs, _) => {
                self.emit_expr(func, lhs);
                self.emit_expr(func, rhs);
                func.instruction(&match op {
                    Binop::Add => Instruction::I32Add,
                    Binop::Sub => Instruction::I32Sub,
                    Binop::Mul => Instruction::I32Mul,
                    Binop::Div => Instruction::I32DivS,
                    Binop::Eq => Instruction::I32Eq,
                    Binop::Neq => Instruction::I32Ne,
                    Binop::Lt => Instruction::I32LtS,
                    Binop::Lte => Instruction::I32LeS,
                    Binop::Gt => Instruction::I32GtS,
                    Binop::Gte => Instruction::I32GeS,
                });
            }

            TypedExpr::TLet(name, _, rhs, body) => {
                self.emit_expr(func, rhs);
                let idx = self.locals_map[name.as_str()];
                func.instruction(&Instruction::LocalSet(idx));
                self.emit_expr(func, body);
            }

            TypedExpr::TIfThenElse(cond, then_br, else_br, _) => {
                self.emit_expr(func, cond);
                func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
                self.emit_expr(func, then_br);
                func.instruction(&Instruction::Else);
                self.emit_expr(func, else_br);
                func.instruction(&Instruction::End);
            }

            TypedExpr::TApp(f, args, _) => {
                if let TypedExpr::TName(name, _) = f.as_ref() {
                    for a in args {
                        self.emit_expr(func, a);
                    }
                    let idx = self
                        .fn_indices
                        .get(name.as_str())
                        .unwrap_or_else(|| panic!("unknown function in WASM: {name}"));
                    func.instruction(&Instruction::Call(*idx));
                } else {
                    panic!("unsupported application in WASM: {f:?}");
                }
            }

            TypedExpr::TTuple(elems, _) => {
                let base_local = self.tuple_base_start + self.tuple_depth;
                self.tuple_depth += 1;

                // save global heap_ptr, then reserve space for tuple
                func.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
                func.instruction(&Instruction::LocalSet(base_local));
                func.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
                func.instruction(&Instruction::I32Const((elems.len() * 4) as i32));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));

                // store the values!!
                for (i, e) in elems.iter().enumerate() {
                    func.instruction(&Instruction::LocalGet(base_local));
                    self.emit_expr(func, e);
                    func.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                        offset: (i * 4) as u64,
                        align: 2,
                        memory_index: 0,
                    }));
                }

                func.instruction(&Instruction::LocalGet(base_local));
                self.tuple_depth -= 1;
            }

            TypedExpr::TAccess(expr, idx, _) => {
                self.emit_expr(func, expr);
                func.instruction(&Instruction::I32Load(wasm_encoder::MemArg {
                    offset: (*idx as u64) * 4,
                    align: 2,
                    memory_index: 0,
                }));
            }

            other => panic!("unsupported in WASM codegen: {other:?}"),
        }
    }
}
