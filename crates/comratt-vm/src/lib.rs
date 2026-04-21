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
            // According to MDN, NULL context is ok for apply
            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/apply
            let js_args = js_sys::Array::new();
            js_args.push(&JsValue::from(fn_idx));
            for arg in args {
                js_args.push(&JsValue::from(*arg));
            }

            // How do we want to handle the error case?
            let result = self
                .callback
                .apply(&JsValue::NULL, &js_args)
                .expect("WasmBackend callback failed");

            // How do we want to handle the error case?
            result.as_f64().unwrap_or(42.0) as i32
        }
    }

    impl From<Value> for JsValue {
        fn from(value: Value) -> Self {
            match value {
                Value::I32(n) => JsValue::from(n),
                Value::Bool(b) => JsValue::from(b),
                Value::Unit => todo!(),
                Value::Thunk {
                    fun_idx,
                    captures,
                    clock,
                } => todo!(),
                Value::Wait { channel_idx, clock } => todo!(),
                Value::Tuple(values) => todo!(),
            }
        }
    }

    #[wasm_bindgen]
    pub struct WasmVM(VM<JsWasmBackend>);

    #[wasm_bindgen]
    impl WasmVM {
        #[wasm_bindgen(constructor)]
        pub fn new(
            serialized_bytecode: &[u8],
            callback: js_sys::Function,
        ) -> Result<WasmVM, JsError> {
            let deserialized_bytecode_functions: Vec<Function> =
                postcard::from_bytes(&serialized_bytecode)
                    .expect("Failed to deserialize bytecode functions");
            let js_backend = JsWasmBackend { callback };
            let vm = VM::new(deserialized_bytecode_functions, js_backend);

            Ok(WasmVM(vm))
        }

        #[wasm_bindgen]
        pub fn execute(&mut self, fn_idx: u32, args: &[i32]) -> JsValue {
            self.0
                .execute(fn_idx, args.into_iter().map(|v| Value::I32(*v)).collect())
                .into()
        }

        #[wasm_bindgen]
        pub fn call_wasm(&mut self, idx: u32, args: &[i32]) -> JsValue {
            self.0.call_wasm(idx, args).into()
        }
    }
}
