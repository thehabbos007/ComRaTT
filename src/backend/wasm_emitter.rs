use anyhow::{Context, Result};
use wasm_encoder::{
    reencode::{Reencode, RoundtripReencoder},
    CodeSection, ElementSection, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, ImportSection, IndirectNameMap, Instruction, MemorySection, MemoryType, Module,
    NameMap, NameSection, RefType, TableSection, TableType, TypeSection,
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
    pub import_section: ImportSection,
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
    parsed_modules: u32,
}

pub const CLOSURE_HEAP_INDEX: u32 = 0;
pub const LOCATION_HEAP_INDEX: u32 = 1;

// const MALLOC_ARGS: [(&str, Type); 1] = [("size", Type::TInt)];
pub const WAIT_FUN_IDX: u32 = 0;
pub const SET_OUTPUT_TO_LOCATION_IDX: u32 = 1;
const MALLOC_FUN_IDX: u32 = 2;
const LOCATION_MALLOC_FUN_IDX: u32 = 3;
pub const CLOCK_OF_FUN_IDX: u32 = 4;

const PREGEN_MODULE: &str = r#"
(module
    (type $wait (func (param i32) (result i32)))
    (import "ffi" "wait" (func $wait (type $wait)))

    (type $set_output_to_location (func (param i32 i32) (result i32)))
    (import "ffi" "set_output_to_location" (func $set_output_to_location (type $set_output_to_location)))

    (global $next_ptr (export "next_ptr") (mut i32) (i32.const 0))
    (global $next_location (export "next_location") (mut i32) (i32.const 0))

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

    (type $location_malloc (func (result i32)))
    (func $location_malloc (export "location_malloc") (result i32)
        ;; Define a local to hold the current value i.e. the beginning
        ;; of this new allocation
        (local $old i32)
        ;; Put the current ptr on the stack, write it to $old and put the value
        ;; on the stack again
        (global.get $next_location)
        (local.tee $old)
        ;; Put 8 on the stack: 4 bytes for clock and 4 bytes for ptr to closure heap
        (i32.const 8)
        ;; Add $old and 8
        (i32.add)
        ;; Write new offset to the global
        (global.set $next_location)
        ;; Return $old i.e. the beginning of this new allocation
        (local.get $old)
    )

    (type $clock_of (func (param i32) (result i32)))
    (func $clock_of (export "clock_of") (param $location_ptr i32) (result i32)
        ;; Push the location ptr
        (local.get $location_ptr)

        ;; Load the clock part (2nd half) of the allocation from the location heap
        (i32.load 1 offset=4)
    )
)
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
            import_section: ImportSection::new(),
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

            parsed_modules: 0,
        }
    }

    pub fn next_fun_index(&self) -> u32 {
        self.func_map.len() as u32
    }

    pub fn prepare_emit(&mut self) {
        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        self.memory_section.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        // Table export is really only needed for debugging
        self.export_section
            .export("heap", ExportKind::Memory, CLOSURE_HEAP_INDEX);
        self.export_section
            .export("location", ExportKind::Memory, LOCATION_HEAP_INDEX);
        self.export_section.export("table", ExportKind::Table, 0);

        self.gen_module().unwrap();
    }

    pub fn finalize_emit(mut self) -> Vec<u8> {
        self.module.section(&self.type_section);
        self.module.section(&self.import_section);
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

    fn gen_module(&mut self) -> Result<()> {
        let bytes = wat::parse_str(PREGEN_MODULE)?;
        self.parse_wasm_module(&bytes).context("add module")?;

        self.func_map.insert("wait", WAIT_FUN_IDX);
        self.func_args.insert("wait", vec![("channel", Type::TInt)]);
        self.type_map.insert("wait", WAIT_FUN_IDX);

        self.func_map
            .insert("set_output_to_location", SET_OUTPUT_TO_LOCATION_IDX);
        self.func_args.insert(
            "set_output_to_location",
            vec![
                ("output_channel_index", Type::TInt),
                ("location_ptr", Type::TInt),
            ],
        );
        self.type_map
            .insert("set_output_to_location", SET_OUTPUT_TO_LOCATION_IDX);

        self.func_map.insert("malloc", MALLOC_FUN_IDX);
        self.func_args.insert("malloc", vec![("size", Type::TInt)]);
        self.type_map.insert("malloc", MALLOC_FUN_IDX);

        self.func_map
            .insert("location_malloc", LOCATION_MALLOC_FUN_IDX);
        self.func_args.insert("location_malloc", vec![]);
        self.type_map
            .insert("location_malloc", LOCATION_MALLOC_FUN_IDX);

        self.func_map.insert("clock_of", CLOCK_OF_FUN_IDX);
        self.func_args
            .insert("clock_of", vec![("location_ptr", Type::TInt)]);
        self.type_map.insert("clock_of", CLOCK_OF_FUN_IDX);

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
                Payload::ImportSection(import_section) => RoundtripReencoder
                    .parse_import_section(&mut self.import_section, import_section)?,
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
                                            let index = index + self.parsed_modules;
                                            self.function_name_map.append(index, name);
                                        },
                                    );
                                }
                                Name::Local(locals) => {
                                    locals.into_iter().map(|x| x.unwrap()).for_each(
                                        |IndirectNaming { index, names }| {
                                            let mut name_map = NameMap::new();
                                            let index = index + self.parsed_modules;
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
                                            let index = index + self.parsed_modules;
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

        self.parsed_modules += 1;

        Ok(())
    }

    pub fn malloc(&self, func: &mut Function, size_bytes: i32) {
        // malloc is in the wasm blob as index `MALLOC_FUN_IDX` function
        // inject code that invokes this function with closure_size

        func.instruction(&Instruction::I32Const(size_bytes));
        func.instruction(&Instruction::Call(MALLOC_FUN_IDX));
    }

    pub fn location_malloc(&self, func: &mut Function) {
        // location_malloc is in the wasm blob as index `LOCATION_MALLOC_FUN_IDX` function
        // inject code that invokes this function

        func.instruction(&Instruction::Call(LOCATION_MALLOC_FUN_IDX));
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use wasmparser::Validator;

    #[test]
    fn parse_pregen_module() {
        wat::parse_str(PREGEN_MODULE).unwrap();
    }

    #[test]
    fn test_pregeneration() -> Result<()> {
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
}
