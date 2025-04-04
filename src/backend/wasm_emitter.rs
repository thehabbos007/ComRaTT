use anyhow::{Context, Result};
use wasm_encoder::{
    reencode::{Reencode, RoundtripReencoder},
    CodeSection, ElementSection, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, ImportSection, IndirectNameMap, Instruction, MemorySection, MemoryType, Module,
    NameMap, NameSection, RefType, TableSection, TableType, TypeSection, ValType,
};

use std::collections::HashMap;
use wasmparser::{IndirectNaming, Name, Naming, Payload};

use crate::source::Type;

pub struct WasmEmitter<'a> {
    pub module: Module,
    pub type_section: TypeSection,
    pub function_section: FunctionSection,
    pub export_section: ExportSection,
    pub memory_section: MemorySection,
    pub code_section: CodeSection,
    pub global_section: GlobalSection,
    pub element_section: ElementSection,
    pub name_section: NameSection,

    // Debug info
    pub function_name_map: NameMap,
    pub type_name_map: NameMap,
    pub locals_name_map: IndirectNameMap,

    pub type_map: HashMap<&'a str, u32>,
    pub func_map: HashMap<&'a str, u32>,
    pub func_args: HashMap<&'a str, Vec<(&'a str, Type)>>,

    pub locals_map: HashMap<&'a str, u32>,
}

// const MALLOC_ARGS: [(&str, Type); 1] = [("size", Type::TInt)];
const MALLOC_FUN_IDX: u32 = 0;
const MALLOC_BODY: &str = r#"
(module
    (global $next_ptr (mut i32) (i32.const 0))
    (type $malloc (func (param i32) (result i32)))
    (func $malloc (export "malloc") (param $size i32) (result i32)
        ;; Define a local to hold the current value i.e. the beginning
        ;; of this new allocation
        (local $old i32)
        ;; Put the current ptr on the stack, write it to $old and put the value
        ;; on the stack again
        (global.get $next_ptr)
        (local.tee $old)
        ;; Put size on the stack
        (local.get $size)
        ;; Add $old and $size
        (i32.add)
        ;; Write new offset to the global
        (global.set $next_ptr)
        ;; Return $old i.e. the beginning of this new allocation
        (local.get $old)
    )
)
"#;

// Apply arguments to a closure
// After doing this, make sure to check if arity is 0 and call in that case
const POPULATE_CLOSURE: &str = r#"
"#;

impl Default for WasmEmitter<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl WasmEmitter<'_> {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            type_section: TypeSection::new(),
            function_section: FunctionSection::new(),
            export_section: ExportSection::new(),
            memory_section: MemorySection::new(),
            code_section: CodeSection::new(),
            global_section: GlobalSection::new(),
            element_section: ElementSection::new(),
            name_section: NameSection::new(),

            function_name_map: NameMap::new(),
            type_name_map: NameMap::new(),
            locals_name_map: IndirectNameMap::new(),

            type_map: HashMap::new(),
            func_map: HashMap::new(),
            func_args: HashMap::new(),

            locals_map: HashMap::new(),
        }
    }

    pub fn prepare_emit(&mut self) {
        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        self.export_section.export("heap", ExportKind::Memory, 0);

        self.gen_malloc().unwrap();
    }

    pub fn finalize_emit(mut self) -> Vec<u8> {
        self.module.section(&self.type_section);
        self.module.section(&ImportSection::new());
        self.module.section(&self.function_section);

        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            minimum: 128,
            maximum: None,
            table64: false,
            shared: false,
        });
        self.module.section(&tables);

        self.module.section(&self.memory_section);
        self.module.section(&self.global_section);
        self.module.section(&self.export_section);
        // <Start section would be here>
        self.module.section(&self.element_section);
        // <Data count section here>
        self.module.section(&self.code_section);
        // <Data section would be here>
        self.module.section(&self.name_section);

        self.module.finish()
    }

    fn gen_malloc(&mut self) -> Result<()> {
        let bytes = wat::parse_str(MALLOC_BODY)?;
        self.parse_wasm_module(&bytes).context("add malloc")?;

        self.func_map.insert("malloc", MALLOC_FUN_IDX);
        self.func_args.insert("malloc", vec![("size", Type::TInt)]);
        self.type_map.insert("malloc", 0);

        Ok(())
    }

    fn parse_wasm_module(&mut self, data: &[u8]) -> Result<()> {
        let parser = wasmparser::Parser::new(0);

        for payload in parser.parse_all(data) {
            match payload? {
                Payload::TypeSection(types) => {
                    RoundtripReencoder.parse_type_section(&mut self.type_section, types)?
                }
                Payload::GlobalSection(globals) => {
                    RoundtripReencoder.parse_global_section(&mut self.global_section, globals)?
                }
                Payload::MemorySection(memories) => {
                    RoundtripReencoder.parse_memory_section(&mut self.memory_section, memories)?
                }
                Payload::ExportSection(exports) => {
                    RoundtripReencoder.parse_export_section(&mut self.export_section, exports)?
                }
                Payload::ElementSection(elemnts) => {
                    RoundtripReencoder.parse_element_section(&mut self.element_section, elemnts)?
                }
                Payload::CodeSectionEntry(func_body) => {
                    RoundtripReencoder.parse_function_body(&mut self.code_section, func_body)?
                }
                Payload::FunctionSection(func) => {
                    RoundtripReencoder.parse_function_section(&mut self.function_section, func)?
                }
                Payload::CustomSection(custom) => {
                    if let wasmparser::KnownCustom::Name(subsections) = custom.as_known() {
                        for name in subsections.into_iter() {
                            match name? {
                                Name::Function(funcs) => {
                                    funcs.into_iter().map(|x| x.unwrap()).for_each(
                                        |Naming { index, name }| {
                                            self.function_name_map.append(index, name);
                                        },
                                    );
                                }
                                Name::Local(locals) => {
                                    locals.into_iter().map(|x| x.unwrap()).for_each(
                                        |IndirectNaming { index, names }| {
                                            let mut name_map = NameMap::new();
                                            names.into_iter().map(|x| x.unwrap()).for_each(
                                                |Naming { index, name }| {
                                                    name_map.append(index, name);
                                                },
                                            );
                                            self.locals_name_map.append(index, &name_map);
                                        },
                                    );
                                }
                                Name::Type(types) => {
                                    types.into_iter().map(|x| x.unwrap()).for_each(
                                        |Naming { index, name }| {
                                            self.type_name_map.append(index, name);
                                        },
                                    );
                                }
                                _ => (),
                            }
                        }
                    }
                }
                _ => {}
            };
        }

        Ok(())
    }

    pub fn malloc(&self, func: &mut Function, size_bytes: i32) {
        // malloc is in the wasm blob as index `MALLOC_FUN_IDX` function
        // inject code that invokes this function with closure_size

        func.instruction(&Instruction::I32Const(size_bytes));
        func.instruction(&Instruction::Call(MALLOC_FUN_IDX));
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use wasmparser::Validator;

    use wasmtime_wast::WastContext;

    #[test]
    fn parse_malloc() {
        wat::parse_str(MALLOC_BODY).unwrap();
    }

    #[test]
    fn test_malloc_generation() -> Result<()> {
        // Create a new emitter
        let mut emitter = WasmEmitter::new();

        emitter.prepare_emit();

        // Finalize the module
        let wasm_bytes = emitter.finalize_emit();

        // let rasm = wasmprinter::print_bytes(&wasm_bytes)?;
        // panic!("{rasm}");

        // Validate the generated WASM
        let mut validator = Validator::new();
        validator.validate_all(&wasm_bytes)?;

        Ok(())
    }

    #[test]
    fn malloc_test() {
        let test = r#"
        (assert_return (invoke "malloc" (i32.const 8)) (i32.const 0))
        (assert_return (invoke "malloc" (i32.const 16)) (i32.const 8))
        (assert_return (invoke "malloc" (i32.const 1)) (i32.const 24))
        "#;

        let malloc_test = format!("{MALLOC_BODY}{test}");
        test_wast(malloc_test);
    }

    fn test_wast(wast: String) {
        let engine = wasmtime::Engine::default();
        let store = wasmtime::Store::new(&engine, ());
        let mut ctx = WastContext::new(store);
        ctx.run_buffer("inline_test", wast.as_bytes()).unwrap();
    }
}
