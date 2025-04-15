use wasm_encoder::{
    BlockType, ConstExpr, Elements, ExportKind, Function, Instruction, MemArg, NameMap, ValType,
};

use itertools::Itertools;
use std::borrow::Cow;
use std::collections::BTreeSet;
use std::iter;
use std::ops::{Deref, DerefMut};

use crate::anf::{AExpr, AnfExpr, AnfProg, AnfToplevel, CExpr};
use crate::backend::wasm_emitter::{CLOSURE_HEAP_INDEX, LOCATION_HEAP_INDEX};
use crate::source::{Binop, Const, Type};
use crate::types::count_tfun_args;

use super::wasm_emitter::WasmEmitter;

const WASM_WORD_SIZE: i32 = 4;
// const WASM_DWORD_SIZE: i32 = 8;
/// Allignment is a power of 2. So we align with 2^2 = 4 bytes
const WASM_ALIGNMENT_SIZE: u32 = 2;
const LOCAL_DUP_I32_NAME: &str = "dupi32";
const FUNCTION_INDEX_OFFSET: u64 = 0;
const ARITY_OFFSET: u64 = WASM_WORD_SIZE as u64;
const POPULATE_OFFSET: u64 = WASM_WORD_SIZE as u64 * 2;

pub struct AnfWasmEmitter<'a> {
    wasm_emitter: WasmEmitter<'a>,

    dispatch_offset: u32,
    location_dispatch_offset: u32,
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
            dispatch_offset: 0,
            location_dispatch_offset: 0,
            prog,
        }
    }

    pub fn emit(mut self) -> Vec<u8> {
        self.prepare_emit();

        for def in &self.prog.0 {
            self.forward_declare_functions(def);
        }

        self.dispatch_offset = self.function_section.len();
        self.location_dispatch_offset = self.function_section.len() + 1;

        for def in &self.prog.0 {
            self.process_function(def);
        }

        self.gen_dispatch();
        self.gen_location_dispatch();

        self.declare_table_entries();

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
        self.func_args.insert(
            name.as_str(),
            args.iter()
                .map(|(str, typ)| (str.as_str(), typ.clone()))
                .collect_vec(),
        );

        self.export_section.export(name, ExportKind::Func, func_idx);

        // Debug info
        self.function_name_map.append(func_idx, name);
        self.type_name_map.append(type_idx, name);
    }

    fn gen_dispatch(&mut self) {
        // generate a wasm function
        // takes a closure pointer
        // returns anything (could be numeric value, could be pointer, could be bool, the sky is the limit)
        // (func (param i32) (result i32))
        let name = "dispatch";
        // TODO: we assume an invariant where we have a partially applied function with one remaining argument
        //   so the dispatch function will take a closure and that last argument.
        //   In an ideal world, we would populate the closure fully (failing if we are "over applying")
        //   before passing it on to dispatch.
        let params = [ValType::I32, ValType::I32];
        self.locals_map.clear();
        // self.locals_map.insert("closure_ptr", 0);

        let mut func = Function::new_with_locals_types(params.iter().cloned());

        // break table with target labels and default case
        // generate with a loop
        // default case is at index NUM_FUNCTIONS?

        let func_map_len = self.func_map.len() as u32;

        // Open as many blocks as there are functions + 1 for default
        for _ in 0..(func_map_len + 1) {
            func.instruction(&Instruction::Block(BlockType::Empty));
        }

        let target_labels = (0..func_map_len).collect_vec();

        // Load and push function index
        let fun_index_load_arg = MemArg {
            offset: FUNCTION_INDEX_OFFSET,
            align: WASM_ALIGNMENT_SIZE,
            memory_index: 0,
        };
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::I32Load(fun_index_load_arg));

        // In the innermost block
        func.instruction(&Instruction::BrTable(
            Cow::from(target_labels),
            func_map_len,
        ));
        func.instruction(&Instruction::End);

        // Compile each break case. Order is important.
        for (name, idx) in self.func_map.iter().sorted_by(|a, b| a.1.cmp(b.1)) {
            // find args and put on stack
            let func_args = &self.func_args[name];
            let mut arg = MemArg {
                // beware we're subtracting by one here because the offset is not the end of the malloc block
                // so if we end up getting jumbled values, look at this again.
                offset: FUNCTION_INDEX_OFFSET
                    + ((2 + func_args.len() as u64 - 1) * WASM_WORD_SIZE as u64),
                align: WASM_ALIGNMENT_SIZE,
                memory_index: CLOSURE_HEAP_INDEX,
            };
            for _ in (0..func_args.len()).rev() {
                // Closure pointer on stack
                func.instruction(&Instruction::LocalGet(0));
                func.instruction(&Instruction::I32Load(arg));
                arg.offset -= WASM_WORD_SIZE as u64;
            }

            // Push function index
            let type_index = self.type_map[name];
            func.instruction(&Instruction::I32Const(*idx as i32));
            func.instruction(&Instruction::ReturnCallIndirect {
                type_index,
                table_index: 0,
            });
            // Close the block
            func.instruction(&Instruction::End);
        }

        // Default case, unreachable
        func.instruction(&Instruction::Unreachable);

        // End function block
        func.instruction(&Instruction::End);

        let func_idx = self.function_section.len();
        let type_idx = self.register_function_type(
            name,
            &[
                (
                    "closure_ptr".to_owned(),
                    // This type is irrelevant. I just want to produce a "i32"
                    Type::TFun(Type::TInt.b(), Type::TInt.b()),
                ),
                (
                    "last_argument".to_owned(),
                    // This type is potentially incorrect. Just be aware that chaning away from the "god i32" type
                    // that we may run into problems.
                    Type::TFun(Type::TInt.b(), Type::TInt.b()),
                ),
            ],
            &Type::TInt,
        );

        self.function_section.function(type_idx);
        self.code_section.function(&func);
        self.func_map.insert(name, func_idx);

        self.export_section.export(name, ExportKind::Func, func_idx);

        self.function_name_map.append(func_idx, name);
        self.type_name_map.append(type_idx, name);

        let mut locals_name_map = NameMap::new();
        self.locals_map
            .iter()
            .sorted_by_key(|l| l.1)
            .for_each(|(name, idx)| {
                locals_name_map.append(*idx, name);
            });
        self.locals_name_map.append(func_idx, &locals_name_map);
    }

    fn gen_location_dispatch(&mut self) {
        let name = "location_dispatch";

        let params = [];
        self.locals_map.clear();

        let mut func = Function::new_with_locals_types(params.iter().cloned());

        // Load closure heap ptr from location heap ptr
        let loc_heap_load_arg = MemArg {
            offset: 0, // TODO NAME THIS CONST
            align: WASM_ALIGNMENT_SIZE,
            memory_index: LOCATION_HEAP_INDEX,
        };
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::I32Load(loc_heap_load_arg));

        // Call dispatch with the closure pointer and unit argument
        let dispatch_type_index = self.type_map["dispatch"];
        func.instruction(&Instruction::I32Const(-1));
        func.instruction(&Instruction::I32Const(self.dispatch_offset as i32));
        func.instruction(&Instruction::ReturnCallIndirect {
            type_index: dispatch_type_index,
            table_index: 0,
        });

        func.instruction(&Instruction::End);

        let func_idx = self.function_section.len();
        let type_idx = self.register_function_type(
            name,
            &[(
                "location_ptr".to_owned(),
                // This type is irrelevant. I just want to produce a "i32"
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )],
            &Type::TInt,
        );

        self.function_section.function(type_idx);
        self.code_section.function(&func);
        self.func_map.insert(name, func_idx);

        self.export_section.export(name, ExportKind::Func, func_idx);

        self.function_name_map.append(func_idx, name);
        self.type_name_map.append(type_idx, name);

        let mut locals_name_map = NameMap::new();
        self.locals_map
            .iter()
            .sorted_by_key(|l| l.1)
            .for_each(|(name, idx)| {
                locals_name_map.append(*idx, name);
            });
        self.locals_name_map.append(func_idx, &locals_name_map);
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

                let local_types = local_types
                    .into_iter()
                    .map(|(_, ty)| self.wasm_type(&ty))
                    // This is the type for the local $dupi32 variable
                    .chain(iter::once(ValType::I32))
                    .collect_vec();

                // the local $dup variable is used to store the pointer
                // to a closure while it is being allocated.
                self.locals_map.insert(LOCAL_DUP_I32_NAME, next_local);

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
            AnfToplevel::Channel(_, _) => {}
            AnfToplevel::Output(_, _) => {}
        }
    }

    fn compile_anf_expr(&mut self, func: &mut Function, expr: &'a AnfExpr) {
        match expr {
            AnfExpr::AExpr(aexpr) => {
                self.compile_atomic(func, aexpr);
            }
            AnfExpr::CExp(cexpr) => self.compile_computation(func, cexpr),
            AnfExpr::Let(name, _ty, rhs, body) => {
                self.compile_anf_expr(func, rhs);

                let local_idx = self.locals_map[name.as_str()];

                func.instruction(&Instruction::LocalSet(local_idx));
                self.compile_anf_expr(func, body);
            }
        }
    }

    fn compile_atomic(&mut self, func: &mut Function, aexpr: &'a AExpr) -> Type {
        match aexpr {
            AExpr::Const(Const::CInt(n), ty) => {
                func.instruction(&Instruction::I32Const(*n));
                ty.clone()
            }
            AExpr::Const(Const::CBool(b), ty) => {
                func.instruction(&Instruction::I32Const(*b as i32));
                ty.clone()
            }
            AExpr::Const(Const::CUnit, ty) => {
                func.instruction(&Instruction::I32Const(-1));
                ty.clone()
            }
            AExpr::Const(Const::CLaterUnit, ty) => {
                func.instruction(&Instruction::I32Const(-1));
                ty.clone()
            }
            AExpr::Var(name, ty) => {
                if let Some(&local_idx) = self.locals_map.get(name.as_str()) {
                    // we need to identify if this is a function pointer and not just a local
                    func.instruction(&Instruction::LocalGet(local_idx));
                } else if let Some(&func_idx) = self.func_map.get(name.as_str()) {
                    func.instruction(&Instruction::Call(func_idx));
                } else {
                    panic!("Undefined variable: {}", name);
                }

                ty.clone()
            }
            AExpr::Lam(
                lam_args,
                box AnfExpr::CExp(CExpr::App(AExpr::Var(app_name, var_typ), app_args, _)),
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
                    count_tfun_args(var_typ),
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

                let closure_size_bytes = (1 + // Function pointer
                    1) * WASM_WORD_SIZE + // arity argument
                    (arity // Number of arguments in the top-level function
                    * WASM_WORD_SIZE);

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
                let fp_arg = MemArg {
                    offset: FUNCTION_INDEX_OFFSET,
                    align: WASM_ALIGNMENT_SIZE,
                    memory_index: CLOSURE_HEAP_INDEX,
                };
                func.instruction(&Instruction::I32Const(*fun_idx as i32));
                func.instruction(&Instruction::I32Store(fp_arg));

                let arity_arg = MemArg {
                    offset: ARITY_OFFSET,
                    align: WASM_ALIGNMENT_SIZE,
                    memory_index: CLOSURE_HEAP_INDEX,
                };
                func.instruction(&Instruction::LocalGet(dup_local_idx));
                // All bound variables in the lambda are the variables yet to be populated.
                func.instruction(&Instruction::I32Const(lam_args.len() as i32));
                func.instruction(&Instruction::I32Store(arity_arg));

                // populate all known arguments
                // We traverse the app args in reverse, skipping over bound variables
                for (idx, arg) in app_args.iter().rev().enumerate().skip(lam_args.len()) {
                    let arg_offset = WASM_WORD_SIZE * (2 + idx as i32);
                    let arg_arg = MemArg {
                        offset: arg_offset as u64,
                        align: WASM_ALIGNMENT_SIZE,
                        memory_index: CLOSURE_HEAP_INDEX,
                    };

                    func.instruction(&Instruction::LocalGet(dup_local_idx));
                    let _comp_ty = self.compile_atomic(func, arg);
                    /*
                    if !matches!(comp_ty, Type::TInt) {
                        func.instruction(&Instruction::I64ExtendI32S);
                    }
                    */
                    func.instruction(&Instruction::I32Store(arg_arg));
                }
                // return ptr from malloc
                func.instruction(&Instruction::LocalGet(dup_local_idx));

                // If this is an async closure, call location_malloc instead.
                // Populate that and return the pointer to location heap.
                // Assuming that the lambda will always have either a () or {}
                // argument.
                assert_eq!(lam_args.len(), 1, "Length of closure args should be 1");
                if let Some((_, arg_ty)) = lam_args.first()
                    && let Type::TLaterUnit = arg_ty
                {
                    // At this point the stack top is a pointer to the closure heap.
                    // The clock is a hardcoded bogus value atm. but will have to be
                    // based on either a concrete "wait" expression or a clock-of expression later.
                    // This will probably just be a recursive call to self.compile_atomic
                    // that ends up in those two cases.

                    // DROP CLOSURE HEAP POINTER
                    func.instruction(&Instruction::Drop);

                    // ALLOCATE SPACE IN LOCATION HEAP AND GET PTR
                    self.location_malloc(func);

                    // POPULATE CLOSURE PART OF LOCATION AT OFFSET 0
                    let closure_arg = MemArg {
                        offset: 0,
                        align: WASM_ALIGNMENT_SIZE,
                        memory_index: LOCATION_HEAP_INDEX,
                    };

                    // GET THE PTR TO HEAP
                    func.instruction(&Instruction::LocalGet(dup_local_idx));

                    // STORE IT
                    func.instruction(&Instruction::I32Store(closure_arg));

                    // NOW THE STACK IS EMPTY AND WE NEED NEED TO STORE THE CLOCK.
                    // LOCATION HEAP ALLOCATION ARE FIXED SIZE, SO WE CAN ARITHMETIC OURSELVES OUT OF
                    // NOT HAVING THE PTR.

                    // GET THE START OF NEXT ALLOCATION ($next_location)
                    func.instruction(&Instruction::GlobalGet(1));

                    // SUBTRACT 4 TO GET CLOCK OFFSET
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Sub);

                    // STORE BOGUS CLOCK FOR NOW
                    func.instruction(&Instruction::I32Const(42));

                    let clock_arg = MemArg {
                        offset: 0,
                        align: WASM_ALIGNMENT_SIZE,
                        memory_index: LOCATION_HEAP_INDEX,
                    };
                    func.instruction(&Instruction::I32Store(clock_arg));

                    // RETURN THE BASE POINTER FOR LOCATION
                    func.instruction(&Instruction::GlobalGet(1));
                    func.instruction(&Instruction::I32Const(8));
                    func.instruction(&Instruction::I32Sub);
                }

                lam_typ.clone()
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
                    (Binop::Add, Type::TInt) => Instruction::I32Add,
                    (Binop::Sub, Type::TInt) => Instruction::I32Sub,
                    (Binop::Mul, Type::TInt) => Instruction::I32Mul,
                    (Binop::Div, Type::TInt) => Instruction::I32DivS,
                    (Binop::Eq, Type::TBool) => Instruction::I32Eq,
                    (Binop::Lt, Type::TBool) => Instruction::I32LtS,
                    (Binop::Lte, Type::TBool) => Instruction::I32LeS,
                    (Binop::Gt, Type::TBool) => Instruction::I32GtS,
                    (Binop::Gte, Type::TBool) => Instruction::I32GeS,
                    (Binop::Neq, Type::TBool) => Instruction::I32Ne,
                    _ => panic!("Unsupported primitive operation"),
                };

                func.instruction(&instr);
            }

            CExpr::App(f, args, _) => {
                match f {
                    AExpr::Var(_, Type::TFun(..)) => (),
                    _ => {
                        // We don't want this behaviour
                        // when dispatching a closure.
                        for arg in args {
                            self.compile_atomic(func, arg);
                        }
                    }
                }
                match f {
                    AExpr::Var(name, Type::TFun(_, box Type::TFun(..))) => {
                        // Get the function index from the closure
                        let Some(&local_idx) = self.locals_map.get(name.as_str()) else {
                            panic!("Got non-local variable [{}] in closure call", name);
                        };

                        // Read the arity from the closure pointer
                        let arity_arg = MemArg {
                            offset: ARITY_OFFSET,
                            align: WASM_ALIGNMENT_SIZE,
                            memory_index: CLOSURE_HEAP_INDEX,
                        };

                        let populate_arg = MemArg {
                            offset: POPULATE_OFFSET,
                            align: WASM_ALIGNMENT_SIZE,
                            memory_index: CLOSURE_HEAP_INDEX,
                        };

                        for arg in args {
                            // Push the closure pointer onto the stack
                            func.instruction(&Instruction::LocalGet(local_idx));
                            // Subtract 1 from the arity
                            func.instruction(&Instruction::LocalGet(local_idx));
                            func.instruction(&Instruction::I32Load(arity_arg));
                            func.instruction(&Instruction::I32Const(1));
                            func.instruction(&Instruction::I32Sub);
                            // Store updated arity back in the closure
                            func.instruction(&Instruction::I32Store(arity_arg));

                            // remaining arity * word size + closure ptr gives us the index to write at (offset by 8 i.e. the first cell for arg)
                            func.instruction(&Instruction::LocalGet(local_idx));
                            func.instruction(&Instruction::I32Load(arity_arg));
                            func.instruction(&Instruction::I32Const(4));
                            func.instruction(&Instruction::I32Mul);
                            func.instruction(&Instruction::LocalGet(local_idx));
                            func.instruction(&Instruction::I32Add);
                            // Compile the argument to be stored
                            let _ = self.compile_atomic(func, arg);
                            // Store wants: first where then what
                            func.instruction(&Instruction::I32Store(populate_arg));
                        }
                    }
                    AExpr::Var(name, Type::TFun(..)) => {
                        let Some(&local_idx) = self.locals_map.get(name.as_str()) else {
                            panic!("Got non-local variable [{}] in closure call", name);
                        };

                        func.instruction(&Instruction::LocalGet(local_idx));
                        // In the cases where we call dispatch with a closure pointer,
                        // we want the stack to look like the following before return_call_indirect
                        // - Pointer to closure
                        // - Last argument for closure application
                        // - Table index for dispatch function
                        // Hence the different handling of arguments in this match case.
                        // Invariant: when calling a closure, only 1 argument is missing and
                        // thus only 1 argument exists.
                        // Is this too strict?
                        assert!(args.len() == 1);
                        let arg = args
                            .first()
                            .expect("Attempted to dispatch a closure with multiple arguments");
                        self.compile_atomic(func, arg);
                        func.instruction(&Instruction::I32Const(self.dispatch_offset as i32));
                        func.instruction(&Instruction::ReturnCallIndirect {
                            type_index: self.dispatch_offset,
                            table_index: 0,
                        });
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

            CExpr::Tuple(exprs, _) => {
                // Tuples are allocated into the heap.
                // Nested tuples will be allocated depth first

                let tuple_len = exprs.len() as i32;
                let tuple_size_bytes = tuple_len * WASM_WORD_SIZE;

                // malloc(closure_size) -> closure_ptr
                // [i32]
                self.wasm_emitter.malloc(func, tuple_size_bytes);

                let dup_local_idx = self.locals_map[LOCAL_DUP_I32_NAME];
                func.instruction(&Instruction::LocalSet(dup_local_idx));

                // populate all tuple exprs
                for (idx, expr) in exprs.iter().enumerate() {
                    let expr_offset = WASM_WORD_SIZE * (idx as i32);
                    let expr_arg = MemArg {
                        offset: expr_offset as u64,
                        align: WASM_ALIGNMENT_SIZE,
                        memory_index: 0,
                    };

                    func.instruction(&Instruction::LocalGet(dup_local_idx));
                    let _comp_ty = self.compile_atomic(func, expr);
                    func.instruction(&Instruction::I32Store(expr_arg));
                }
                // return ptr from malloc
                func.instruction(&Instruction::LocalGet(dup_local_idx));
            }
            CExpr::Access(expr, idx, _) => {
                self.compile_atomic(func, expr);

                let access_arg = MemArg {
                    offset: (idx * WASM_WORD_SIZE) as u64,
                    align: WASM_ALIGNMENT_SIZE,
                    memory_index: 0,
                };

                func.instruction(&Instruction::I32Load(access_arg));
            }
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
            Type::TInt => ValType::I32,
            Type::TBool => ValType::I32,
            Type::TUnit => ValType::I32,
            Type::TLaterUnit => ValType::I32,
            Type::TFun(_, _) => ValType::I32,
            Type::TProduct(_) => ValType::I32,
            Type::TVar(_) => panic!("Type variables not supported as value types"),
        }
    }

    fn declare_table_entries(&mut self) {
        let elements = self
            .func_map
            .values()
            .sorted_by(|a, b| a.cmp(b))
            .copied()
            .collect_vec();
        self.element_section.active(
            Some(0),
            &ConstExpr::i32_const(0),
            Elements::Functions(elements.into()),
        );
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
