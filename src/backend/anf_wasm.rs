use wasm_encoder::{
    BlockType, Elements, ExportKind, Function, Instruction, MemArg, NameMap, ValType,
};

use itertools::Itertools;
use std::collections::BTreeSet;
use std::ops::{Deref, DerefMut};

use crate::anf::{AExpr, AnfExpr, AnfProg, AnfToplevel, CExpr};
use crate::source::{Binop, Const, Type};
use crate::types::count_tfun_args;

use super::wasm_emitter::WasmEmitter;

const WASM_WORD_SIZE: i32 = 4;
const WASM_ALIGNMENT_SIZE: u32 = 4;
const LOCAL_DUP_I32_NAME: &str = "dupi32";

pub struct AnfWasmEmitter<'a> {
    wasm_emitter: WasmEmitter<'a>,

    prog: &'a AnfProg,
}

impl<'a> Deref for AnfWasmEmitter<'a> {
    type Target = WasmEmitter<'a>;

    fn deref(&self) -> &Self::Target {
        &self.wasm_emitter
    }
}
impl DerefMut for AnfWasmEmitter<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.wasm_emitter
    }
}

impl<'a> AnfWasmEmitter<'a> {
    pub fn new(prog: &'a AnfProg) -> Self {
        Self {
            wasm_emitter: WasmEmitter::new(),
            prog,
        }
    }

    pub fn emit(mut self) -> Vec<u8> {
        self.prepare_emit();

        for def in &self.prog.0 {
            self.forward_declare_functions(def);
        }

        for def in &self.prog.0 {
            self.process_function(def);
        }

        self.wasm_emitter
            .name_section
            .functions(&self.wasm_emitter.function_name_map);
        self.wasm_emitter
            .name_section
            .locals(&self.wasm_emitter.locals_name_map);
        self.wasm_emitter
            .name_section
            .types(&self.wasm_emitter.type_name_map);

        self.wasm_emitter.finalize_emit()
    }

    fn forward_declare_functions(&mut self, def: &'a AnfToplevel) {
        let AnfToplevel::FunDef(name, args, _body, ret_type) = def else {
            return;
        };

        //Elements::Functions(())
        // self.element_section.active(table_index, offset, elements)
        let func_idx = self.function_section.len();
        let type_idx = self.register_function_type(name, args, ret_type);

        self.function_section.function(type_idx);
        self.func_map.insert(name.as_str(), func_idx);

        self.export_section.export(name, ExportKind::Func, func_idx);

        // Debug info
        self.function_name_map.append(func_idx, name);
        self.type_name_map.append(type_idx, name);
    }

    fn process_function(&mut self, def: &'a AnfToplevel) {
        match def {
            AnfToplevel::FunDef(name, args, body, _ret_type) => {
                self.locals_map.clear();

                for (i, (arg_name, _)) in args.iter().enumerate() {
                    self.locals_map.insert(arg_name, i as u32);
                }

                let mut local_types = BTreeSet::new();
                body.traverse_locals(&mut local_types);

                let args_len = args.len() as u32;
                for (i, (arg_name, _)) in local_types.iter().enumerate() {
                    let idx = args_len + i as u32;
                    self.locals_map.insert(arg_name, idx);
                }
                let next_local = args_len + local_types.len() as u32;

                // the local $dup variable is used to store the pointer
                // to a closure while it is being allocated.
                self.locals_map.insert(LOCAL_DUP_I32_NAME, next_local);
                local_types.insert((LOCAL_DUP_I32_NAME, Type::TInt));

                let local_types = local_types
                    .into_iter()
                    .map(|(_, ty)| self.wasm_type(&ty))
                    .collect_vec();

                let mut func = Function::new_with_locals_types(local_types);
                self.compile_anf_expr(&mut func, body);
                func.instruction(&Instruction::End);

                self.code_section.function(&func);

                let mut locals_name_map = NameMap::new();
                self.locals_map
                    .iter()
                    .sorted_by_key(|l| l.1)
                    .for_each(|(name, idx)| {
                        locals_name_map.append(*idx, name);
                    });
                let func_idx = self.func_map[name.as_str()];
                self.locals_name_map.append(func_idx, &locals_name_map)
            }
            AnfToplevel::Channel(_) => {}
            AnfToplevel::Output(_, _) => {}
        }
    }

    fn compile_anf_expr(&mut self, func: &mut Function, expr: &'a AnfExpr) {
        match expr {
            AnfExpr::AExpr(aexpr) => self.compile_atomic(func, aexpr),
            AnfExpr::CExp(cexpr) => self.compile_computation(func, cexpr),
            AnfExpr::Let(name, _ty, rhs, body) => {
                self.compile_anf_expr(func, rhs);

                let local_idx = self.locals_map[name.as_str()];

                func.instruction(&Instruction::LocalSet(local_idx));
                self.compile_anf_expr(func, body);
            }
        }
    }

    fn compile_atomic(&mut self, func: &mut Function, aexpr: &'a AExpr) {
        match aexpr {
            AExpr::Const(Const::CInt(n), _) => {
                func.instruction(&Instruction::I64Const(*n as i64));
            }
            AExpr::Const(Const::CBool(b), _) => {
                func.instruction(&Instruction::I32Const(*b as i32));
            }
            AExpr::Const(Const::CUnit, _) => {
                func.instruction(&Instruction::I32Const(-1));
            }
            AExpr::Var(name, _) => {
                if let Some(&local_idx) = self.locals_map.get(name.as_str()) {
                    func.instruction(&Instruction::LocalGet(local_idx));
                } else if let Some(&func_idx) = self.func_map.get(name.as_str()) {
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            AExpr::Lam(
                lam_args,
                box AnfExpr::CExp(CExpr::App(AExpr::Var(app_name, var_typ), app_args, app_typ)),
                lam_typ,
            ) => {
                // Invariants:
                // - App fun should be a toplevel (given by func_map.get below)
                // - Lambda has a App immediately inside body (given by match)
                // - Args are _fully_ applied in the inner App (below assertion)
                // - App args - lambda args = the arguments to populate the closure with (below assertion ensures non-negative)
                //
                // Random note: free variables allowed as arguments, should be populated in-place.
                //
                // What do we need for allocating the closure?
                // - Function index aka pointer
                // - Arity
                // - Length?
                // - Already applied args / free vars
                //
                // Simplifications
                // - arguments are i32 values stored directly in memory
                // i.e. they are not pointers to data stored elsewhere

                // Invariant assertions
                assert_eq!(
                    count_tfun_args(&var_typ),
                    app_args.len(),
                    "Lambda application should be fully applied"
                );
                assert!(
                    app_args.len().checked_sub(lam_args.len()).is_some(),
                    "Closure population candidates should be non-negative"
                );

                let fun_idx = self
                    .func_map
                    .get(app_name.as_str())
                    .expect("Closure function should be a toplevel function.");

                let arity = app_args.len() as i32;

                let num_populate_args = arity - lam_args.len() as i32;

                let closure_size_bytes = (1 + // Function pointer
                    1 + // arity argument
                    arity) // Number of arguments in the top-level function
                    * WASM_WORD_SIZE;

                // malloc(closure_size) -> closure_ptr
                // [i32]
                self.wasm_emitter.malloc(func, closure_size_bytes);
                // The first store will consume this value from the stack.
                // We cannot write it to a local because we would need to forward declare it
                // before reaching this.
                // However, WASM has no dup so we need to add a single local to all top level functions
                // to make sure that we have it available.

                let dup_local_idx = self.locals_map[LOCAL_DUP_I32_NAME];
                func.instruction(&Instruction::LocalTee(dup_local_idx));

                // populate function pointer and arity -> closure_ptr
                // [i32]
                let fp_offset = WASM_WORD_SIZE * 0;
                let fp_arg = MemArg {
                    offset: fp_offset as u64,
                    align: WASM_ALIGNMENT_SIZE,
                    memory_index: 0,
                };
                func.instruction(&Instruction::I32Const(*fun_idx as i32));
                func.instruction(&Instruction::I32Store(fp_arg));

                let arity_offset = WASM_WORD_SIZE * 1;
                let arity_arg = MemArg {
                    offset: arity_offset as u64,
                    align: WASM_ALIGNMENT_SIZE,
                    memory_index: 0,
                };
                func.instruction(&Instruction::LocalGet(dup_local_idx));
                func.instruction(&Instruction::I32Const(arity));
                func.instruction(&Instruction::I32Store(arity_arg));

                // populate all known arguments
                // return ptr from malloc
            }
            _ => panic!("Attempted to compile invalid atomic ANF expression {aexpr}"),
        }
    }

    fn compile_computation(&mut self, func: &mut Function, cexpr: &'a CExpr) {
        match cexpr {
            CExpr::Prim(op, left, right, typ) => {
                self.compile_atomic(func, left);
                self.compile_atomic(func, right);

                let instr = match (op, typ) {
                    (Binop::Add, Type::TInt) => Instruction::I64Add,
                    (Binop::Sub, Type::TInt) => Instruction::I64Sub,
                    (Binop::Mul, Type::TInt) => Instruction::I64Mul,
                    (Binop::Div, Type::TInt) => Instruction::I64DivS,
                    (Binop::Eq, Type::TBool) => Instruction::I64Eq,
                    (Binop::Lt, Type::TBool) => Instruction::I64LtS,
                    (Binop::Lte, Type::TBool) => Instruction::I64LeS,
                    (Binop::Gt, Type::TBool) => Instruction::I64GtS,
                    (Binop::Gte, Type::TBool) => Instruction::I64GeS,
                    (Binop::Neq, Type::TBool) => Instruction::I64Ne,
                    _ => panic!("Unsupported primitive operation"),
                };

                func.instruction(&instr);
            }

            CExpr::App(f, args, _) => {
                for arg in args {
                    self.compile_atomic(func, arg);
                }
                match f {
                    AExpr::Var(name, _) => {
                        let idx = self.func_map[name.as_str()];
                        func.instruction(&Instruction::Call(idx));
                    }
                    _ => panic!("Function call target must be a variable"),
                }
            }

            CExpr::IfThenElse(cond, then_br, else_br, typ) => {
                self.compile_atomic(func, cond);

                let result_type = BlockType::Result(self.wasm_type(typ));

                // func.instruction(&Instruction::I32WrapI64);
                func.instruction(&Instruction::If(result_type));
                self.compile_anf_expr(func, then_br);
                func.instruction(&Instruction::Else);
                self.compile_anf_expr(func, else_br);
                func.instruction(&Instruction::End);
            }

            CExpr::Tuple(_, _) => panic!("Tuple operations not supported in WASM backend"),
            CExpr::Access(_, _, _) => panic!("Tuple access not supported in WASM backend"),
        }
    }

    fn register_function_type(
        &mut self,
        name: &'a str,
        args: &[(String, Type)],
        ret_type: &Type,
    ) -> u32 {
        let params = args
            .iter()
            .map(|(_, ty)| self.wasm_type(ty))
            .collect::<Vec<_>>();

        let results = [self.wasm_type(ret_type)];

        let idx = self.type_section.len();

        self.type_section
            .ty()
            .function(params.iter().copied(), results.iter().copied());

        self.type_map.insert(name, idx);
        idx
    }

    fn wasm_type(&self, ty: &Type) -> ValType {
        match ty {
            Type::TInt => ValType::I64,
            Type::TBool => ValType::I32,
            Type::TUnit => ValType::I32,
            Type::TFun(_, _) => ValType::I32,
            Type::TProduct(_) => panic!("Product types not supported as value types"),
            Type::TVar(_) => panic!("Type variables not supported as value types"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use crate::anf::{AExpr, AnfExpr, AnfProg, AnfToplevel, CExpr};
    use crate::passes::anf::ANFConversion;
    use crate::passes::Pass as _;
    use crate::source::{Binop, Const, Type};
    use crate::types::{TypedExpr, TypedProg, TypedToplevel};
    use wasmparser::Parser;

    fn validate_wasm(bytes: &[u8]) {
        wasmparser::validate(bytes).unwrap();
        let parser = Parser::new(0);
        for payload in parser.parse_all(bytes) {
            payload.unwrap();
        }
    }

    #[test]
    fn test_atomic_const() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::AExpr(AExpr::Const(Const::CInt(42), Type::TInt)),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_atomic_variable() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![("x".to_string(), Type::TInt)],
            AnfExpr::AExpr(AExpr::Var("x".to_string(), Type::TInt)),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_simple_arithmetic() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::CExp(CExpr::Prim(
                Binop::Add,
                AExpr::Const(Const::CInt(40), Type::TInt),
                AExpr::Const(Const::CInt(2), Type::TInt),
                Type::TInt,
            )),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_let_binding() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::Let(
                "x".to_string(),
                Type::TInt,
                Box::new(AnfExpr::CExp(CExpr::Prim(
                    Binop::Add,
                    AExpr::Const(Const::CInt(1), Type::TInt),
                    AExpr::Const(Const::CInt(2), Type::TInt),
                    Type::TInt,
                ))),
                Box::new(AnfExpr::AExpr(AExpr::Var("x".to_string(), Type::TInt))),
            ),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_nested_let_bindings() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::Let(
                "x".to_string(),
                Type::TInt,
                Box::new(AnfExpr::CExp(CExpr::Prim(
                    Binop::Add,
                    AExpr::Const(Const::CInt(1), Type::TInt),
                    AExpr::Const(Const::CInt(2), Type::TInt),
                    Type::TInt,
                ))),
                Box::new(AnfExpr::Let(
                    "y".to_string(),
                    Type::TInt,
                    Box::new(AnfExpr::CExp(CExpr::Prim(
                        Binop::Add,
                        AExpr::Var("x".to_string(), Type::TInt),
                        AExpr::Const(Const::CInt(3), Type::TInt),
                        Type::TInt,
                    ))),
                    Box::new(AnfExpr::AExpr(AExpr::Var("y".to_string(), Type::TInt))),
                )),
            ),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_function_call() {
        let prog = AnfProg(vec![
            AnfToplevel::FunDef(
                "id".to_string(),
                vec![("x".to_string(), Type::TInt)],
                AnfExpr::AExpr(AExpr::Var("x".to_string(), Type::TInt)),
                Type::TInt,
            ),
            AnfToplevel::FunDef(
                "main".to_string(),
                vec![],
                AnfExpr::CExp(CExpr::App(
                    AExpr::Var(
                        "id".to_string(),
                        Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
                    ),
                    vec![AExpr::Const(Const::CInt(42), Type::TInt)],
                    Type::TInt,
                )),
                Type::TInt,
            ),
        ]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_conditional() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::CExp(CExpr::IfThenElse(
                AExpr::Const(Const::CBool(true), Type::TBool),
                Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(1), Type::TInt))),
                Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(0), Type::TInt))),
                Type::TInt,
            )),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }

    #[test]
    fn test_complex_computation() {
        // Computes: let x = 1 + 2 in
        //          let y = x + 3 in
        //          if y > 5 then y else 0
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::Let(
                "x".to_string(),
                Type::TInt,
                Box::new(AnfExpr::CExp(CExpr::Prim(
                    Binop::Add,
                    AExpr::Const(Const::CInt(1), Type::TInt),
                    AExpr::Const(Const::CInt(2), Type::TInt),
                    Type::TInt,
                ))),
                Box::new(AnfExpr::Let(
                    "y".to_string(),
                    Type::TInt,
                    Box::new(AnfExpr::CExp(CExpr::Prim(
                        Binop::Add,
                        AExpr::Var("x".to_string(), Type::TInt),
                        AExpr::Const(Const::CInt(3), Type::TInt),
                        Type::TInt,
                    ))),
                    Box::new(AnfExpr::Let(
                        "cond".to_string(),
                        Type::TInt,
                        Box::new(AnfExpr::CExp(CExpr::Prim(
                            Binop::Gt,
                            AExpr::Var("y".to_string(), Type::TInt),
                            AExpr::Const(Const::CInt(5), Type::TInt),
                            Type::TBool,
                        ))),
                        Box::new(AnfExpr::CExp(CExpr::IfThenElse(
                            AExpr::Var("cond".to_string(), Type::TBool),
                            Box::new(AnfExpr::AExpr(AExpr::Var("y".to_string(), Type::TInt))),
                            Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(0), Type::TInt))),
                            Type::TInt,
                        ))),
                    )),
                )),
            ),
            Type::TInt,
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        let mut file = std::fs::File::create("test.wasm").unwrap();
        file.write_all(&wasm).unwrap();
    }

    #[test]
    #[should_panic(expected = "Function types not supported as value types")]
    fn test_unsupported_function_type() {
        let prog = AnfProg(vec![AnfToplevel::FunDef(
            "main".to_string(),
            vec![],
            AnfExpr::AExpr(AExpr::Const(
                Const::CUnit,
                Type::TFun(Box::new(Type::TUnit), Box::new(Type::TUnit)),
            )),
            Type::TFun(Box::new(Type::TUnit), Box::new(Type::TUnit)),
        )]);

        let emitter = AnfWasmEmitter::new(&prog);
        emitter.emit();
    }

    #[test]
    fn test_complex_if_then_else() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "test".into(),
            vec![("n".into(), Type::TInt)],
            TypedExpr::TIfThenElse(
                TypedExpr::TPrim(
                    Binop::Gt,
                    TypedExpr::TPrim(
                        Binop::Mul,
                        TypedExpr::TName("n".into(), Type::TInt).b(),
                        TypedExpr::TName("n".into(), Type::TInt).b(),
                        Type::TInt,
                    )
                    .b(),
                    TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                    Type::TBool,
                )
                .b(),
                TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
                TypedExpr::TConst(Const::CBool(false), Type::TBool).b(),
                Type::TBool,
            )
            .b(),
            Type::TBool,
        )]);

        let prog = ANFConversion::new().run(prog);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_multiple_functions() {
        let prog = AnfProg(vec![
            AnfToplevel::FunDef(
                "add".to_string(),
                vec![("x".to_string(), Type::TInt), ("y".to_string(), Type::TInt)],
                AnfExpr::CExp(CExpr::Prim(
                    Binop::Add,
                    AExpr::Var("x".to_string(), Type::TInt),
                    AExpr::Var("y".to_string(), Type::TInt),
                    Type::TInt,
                )),
                Type::TInt,
            ),
            AnfToplevel::FunDef(
                "main".to_string(),
                vec![],
                AnfExpr::CExp(CExpr::App(
                    AExpr::Var(
                        "add".to_string(),
                        Type::TFun(
                            Box::new(Type::TInt),
                            Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                        ),
                    ),
                    vec![
                        AExpr::Const(Const::CInt(40), Type::TInt),
                        AExpr::Const(Const::CInt(2), Type::TInt),
                    ],
                    Type::TInt,
                )),
                Type::TInt,
            ),
        ]);

        let emitter = AnfWasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);
    }
}
