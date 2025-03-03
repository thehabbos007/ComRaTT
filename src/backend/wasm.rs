use itertools::Itertools;
use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ElementSection, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, Instruction, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::source::{Binop, Const, Type};
use crate::types::{traverse_locals, TypedExpr, TypedProg, TypedToplevel};

pub struct WasmEmitter<'a> {
    type_section: TypeSection,
    function_section: FunctionSection,
    export_section: ExportSection,
    memory_section: MemorySection,
    code_section: CodeSection,
    global_section: GlobalSection,
    element_section: ElementSection,

    type_map: HashMap<&'a str, u32>,
    func_map: HashMap<&'a str, u32>,

    locals_map: HashMap<&'a str, u32>,
    next_local: u32,

    prog: &'a TypedProg,
}

impl<'a> WasmEmitter<'a> {
    pub fn new(prog: &'a TypedProg) -> Self {
        Self {
            type_section: TypeSection::new(),
            function_section: FunctionSection::new(),
            export_section: ExportSection::new(),
            memory_section: MemorySection::new(),
            code_section: CodeSection::new(),
            global_section: GlobalSection::new(),
            element_section: ElementSection::new(),

            type_map: HashMap::new(),
            func_map: HashMap::new(),

            locals_map: HashMap::new(),
            next_local: 0,

            prog,
        }
    }

    pub fn emit(&mut self) -> Vec<u8> {
        // Now heap
        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        // Later heap
        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        for def in &self.prog.0 {
            self.forward_declare_functions(def);
        }

        for def in &self.prog.0 {
            self.process_function(def);
        }

        let mut module = Module::new();
        if !self.type_section.is_empty() {
            module.section(&self.type_section);
        }
        // <Import section would be here>
        if !self.function_section.is_empty() {
            module.section(&self.function_section);
        }
        // <Table section would be here>
        if !self.memory_section.is_empty() {
            module.section(&self.memory_section);
        }
        if !self.global_section.is_empty() {
            module.section(&self.global_section);
        }
        if !self.export_section.is_empty() {
            module.section(&self.export_section);
        }
        // <Start section would be here>
        if !self.element_section.is_empty() {
            module.section(&self.element_section);
        }
        // <Data count section here>
        if !self.code_section.is_empty() {
            module.section(&self.code_section);
        }
        // <Data section would be here>

        module.finish()
    }

    fn forward_declare_functions(&mut self, def: &'a TypedToplevel) {
        let TypedToplevel::TFunDef(name, args, _body, ret_type) = def else {
            return;
        };

        let type_idx = self.register_function_type(name, args, ret_type);
        self.function_section.function(type_idx);

        let func_idx = self.func_map.len() as u32;
        self.func_map.insert(name.as_str(), func_idx);
    }

    fn process_function(&mut self, def: &'a TypedToplevel) {
        match def {
            TypedToplevel::TFunDef(name, args, body, _ret_type) => {
                // Reset locals at every def
                self.locals_map.clear();
                self.next_local = 0;

                for (i, (arg_name, _)) in args.iter().enumerate() {
                    self.locals_map.insert(&arg_name, i as u32);
                }
                self.next_local = args.len() as u32;

                let mut local_types = Vec::new();
                traverse_locals(&body, &mut local_types);
                local_types.dedup();

                for (i, (arg_name, _)) in local_types.iter().enumerate() {
                    self.locals_map.insert(arg_name, self.next_local + i as u32);
                }
                self.next_local += local_types.len() as u32;

                let local_types = local_types
                    .into_iter()
                    .map(|(_, ty)| self.wasm_type(&ty))
                    .collect_vec();

                let mut func = Function::new_with_locals_types(local_types);
                self.compile_expr(&mut func, body);
                func.instruction(&Instruction::End);

                self.code_section.function(&func);

                if name == "main" {
                    let func_idx = self.func_map[name.as_str()];

                    self.export_section
                        .export("main", ExportKind::Func, func_idx);
                }
            }

            TypedToplevel::Channel(_) => {}
            TypedToplevel::Output(_, _) => {}
        }
    }

    fn register_function_type(
        &mut self,
        name: &'a str,
        args: &[(String, Type)],
        ret_type: &Type,
    ) -> u32 {
        if let Some(&idx) = self.type_map.get(&name) {
            return idx;
        }

        let params = args
            .iter()
            .map(|(_, ty)| self.wasm_type(ty))
            .collect::<Vec<_>>();

        let results = vec![self.wasm_type(ret_type)];

        let idx = self.type_map.len() as u32;

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
            Type::TFun(_, _) => panic!("Function types not supported as value types"),
            Type::TProduct(_) => panic!("Product types not supported as value types"),
            Type::TVar(_) => panic!("Type variables not supported as value types"),
        }
    }

    fn compile_expr(&mut self, func: &mut Function, expr: &TypedExpr) {
        match expr {
            TypedExpr::TConst(c, _) => match c {
                Const::CInt(n) => {
                    func.instruction(&Instruction::I64Const(*n as i64));
                }
                Const::CBool(b) => {
                    func.instruction(&Instruction::I32Const(*b as i32));
                }
                Const::CUnit => {
                    func.instruction(&Instruction::I32Const(-1));
                }
            },

            TypedExpr::TPrim(op, left, right, typ) => {
                self.compile_expr(func, left);
                self.compile_expr(func, right);

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

            TypedExpr::TLet(name, _, rhs, body) => {
                self.compile_expr(func, rhs);

                let Some(local_idx) = self.locals_map.get(name.as_str()) else {
                    panic!("Variable not declared: {}", &name);
                };

                func.instruction(&Instruction::LocalSet(*local_idx));

                self.compile_expr(func, body);
            }

            TypedExpr::TName(name, _) => {
                if let Some(&idx) = self.func_map.get(name.as_str()) {
                    func.instruction(&Instruction::Call(idx));
                } else if let Some(&local_idx) = self.locals_map.get(name.as_str()) {
                    func.instruction(&Instruction::LocalGet(local_idx));
                } else {
                    panic!("Undeclared variable: {}", name);
                }
            }

            TypedExpr::TApp(fn_expr, args, _) => {
                for arg in args {
                    self.compile_expr(func, arg);
                }

                match &**fn_expr {
                    TypedExpr::TName(name, _) => {
                        let idx = self.func_map[name.as_str()];
                        func.instruction(&Instruction::Call(idx));
                    }
                    _ => panic!("Function call target must be a name"),
                }
            }

            TypedExpr::TIfThenElse(cond, then_branch, else_branch, typ) => {
                self.compile_expr(func, cond);

                let result_type = BlockType::Result(self.wasm_type(typ));

                func.instruction(&Instruction::If(result_type));
                self.compile_expr(func, then_branch);
                func.instruction(&Instruction::Else);
                self.compile_expr(func, else_branch);
                func.instruction(&Instruction::End);
            }

            _ => panic!("Unsupported expression type"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use crate::source::{Const, Type};
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
    fn test_const_int() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TConst(Const::CInt(42), Type::TInt)),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_arithmetic() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TConst(Const::CInt(40), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                Type::TInt,
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_if_else() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TIfThenElse(
                Box::new(TypedExpr::TConst(Const::CBool(true), Type::TBool)),
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(0), Type::TInt)),
                Type::TInt,
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_function_call() {
        let prog = TypedProg(vec![
            // id(x) = x
            TypedToplevel::TFunDef(
                "id".to_string(),
                vec![("x".to_string(), Type::TInt)],
                Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                Type::TInt,
            ),
            // main() = id(42)
            TypedToplevel::TFunDef(
                "main".to_string(),
                vec![],
                Box::new(TypedExpr::TApp(
                    Box::new(TypedExpr::TName(
                        "id".to_string(),
                        Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
                    )),
                    vec![TypedExpr::TConst(Const::CInt(42), Type::TInt)],
                    Type::TInt,
                )),
                Type::TInt,
            ),
        ]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_let_binding() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TLet(
                "x".to_string(),
                Type::TInt,
                Box::new(TypedExpr::TConst(Const::CInt(42), Type::TInt)),
                Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_comparison() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TPrim(
                Binop::Lt,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                Type::TBool,
            )),
            Type::TBool,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    #[should_panic(expected = "Product types not supported as value types")]
    fn test_unsupported_product_type() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TConst(Const::CUnit, Type::TProduct(vec![]))),
            Type::TProduct(vec![]),
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        emitter.emit();
    }

    #[test]
    #[should_panic(expected = "Function types not supported as value types")]
    fn test_unsupported_function_type() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TConst(
                Const::CUnit,
                Type::TFun(Box::new(Type::TUnit), Box::new(Type::TUnit)),
            )),
            Type::TFun(Box::new(Type::TUnit), Box::new(Type::TUnit)),
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        emitter.emit();
    }

    #[test]
    fn test_multiple_functions() {
        let prog = TypedProg(vec![
            // add(x, y) = x + y
            TypedToplevel::TFunDef(
                "add".to_string(),
                vec![("x".to_string(), Type::TInt), ("y".to_string(), Type::TInt)],
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                    Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
                    Type::TInt,
                )),
                Type::TInt,
            ),
            // main() = add(40, 2)
            TypedToplevel::TFunDef(
                "main".to_string(),
                vec![],
                Box::new(TypedExpr::TApp(
                    Box::new(TypedExpr::TName(
                        "add".to_string(),
                        Type::TFun(
                            Box::new(Type::TInt),
                            Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                        ),
                    )),
                    vec![
                        TypedExpr::TConst(Const::CInt(40), Type::TInt),
                        TypedExpr::TConst(Const::CInt(2), Type::TInt),
                    ],
                    Type::TInt,
                )),
                Type::TInt,
            ),
        ]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_local_variable_access() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![("x".to_string(), Type::TInt)],
            Box::new(TypedExpr::TLet(
                "y".to_string(),
                Type::TInt,
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                    Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                    Type::TInt,
                )),
                Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn test_nested_let_bindings() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TLet(
                "x".to_string(),
                Type::TInt,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TLet(
                    "y".to_string(),
                    Type::TInt,
                    Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                    Box::new(TypedExpr::TPrim(
                        Binop::Add,
                        Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                        Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
                        Type::TInt,
                    )),
                )),
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    #[should_panic(expected = "Undeclared variable: undefined")]
    fn test_undefined_variable() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TName("undefined".to_string(), Type::TInt)),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();

        validate_wasm(&wasm);
    }

    #[test]
    fn pretty_wasm() {
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_string(),
            vec![],
            Box::new(TypedExpr::TLet(
                "x".to_string(),
                Type::TInt,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TLet(
                    "y".to_string(),
                    Type::TInt,
                    Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                    Box::new(TypedExpr::TPrim(
                        Binop::Add,
                        Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                        Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
                        Type::TInt,
                    )),
                )),
            )),
            Type::TInt,
        )]);

        let mut emitter = WasmEmitter::new(&prog);
        let wasm = emitter.emit();
        validate_wasm(&wasm);

        let mut file = std::fs::File::create("test.wasm").unwrap();
        file.write_all(&wasm).unwrap();
    }
}
