pub mod bytecode;
pub mod vm;

pub use bytecode::{Function, Op};
pub use vm::{Value, VM};

// LSP in Zed goes out the window
// with this cfg. Too lazy to find a fix atm.
//#[cfg(target_arch = "wasm32")]
pub mod wasm_exports {
    use crate::{
        vm::{WasmBackend, VM},
        Function, Value,
    };
    use postcard;
    use wasm_bindgen::prelude::*;

    struct JsWasmBackend {
        callback: js_sys::Function,
    }

    impl WasmBackend for JsWasmBackend {
        fn call(&mut self, fn_idx: u32, args: &[i32]) -> i32 {
            let _ = self.callback.apply(&JsValue::NULL, &js_sys::Array::new());
            42
        }
    }

    #[wasm_bindgen]
    pub fn vm_entrypoint(
        serialized_bytecode: &[u8],
        callback: js_sys::Function,
    ) -> Result<JsValue, JsError> {
        let deserialized_bytecode_functions: Vec<Function> =
            postcard::from_bytes(&serialized_bytecode)
                .expect("Failed to deserialize bytecode functions");
        // Testing out the callback
        let mut js_backend = JsWasmBackend { callback };
        js_backend.call(2, &[5]);
        let mut _vm = VM::new(deserialized_bytecode_functions, js_backend);

        Ok(JsValue::NULL)
    }
}
