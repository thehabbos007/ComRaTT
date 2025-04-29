pub mod anf_wasm;
pub mod wasm_emitter;

pub fn compile_anf_to_wasm(prog: &crate::anf::AnfProg) -> (Vec<u8>, Vec<String>) {
    let emitter = anf_wasm::AnfWasmEmitter::new(prog);
    emitter.emit()
}
