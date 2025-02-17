pub mod wasm;

pub fn compile_to_wasm(prog: &crate::types::TypedProg) -> Vec<u8> {
    let mut emitter = wasm::WasmEmitter::new(prog);
    emitter.emit()
}
