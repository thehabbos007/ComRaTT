pub mod anf_wasm;
pub mod wasm;
pub mod wasm_emitter;

pub fn compile_to_wasm(prog: &crate::types::TypedProg) -> Vec<u8> {
    let emitter = wasm::BareWasmEmitter::new(prog);
    emitter.emit()
}

pub fn compile_anf_to_wasm(prog: &crate::anf::AnfProg) -> Vec<u8> {
    let emitter = anf_wasm::AnfWasmEmitter::new(prog);
    emitter.emit()
}
