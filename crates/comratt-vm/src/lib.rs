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
                Value::Unit => JsValue::NULL,
                rest => JsValue::from_str(&format!("Opaque value: {rest:?}")),
            }
        }
    }

    #[wasm_bindgen]
    pub struct WasmVM {
        vm: VM<JsWasmBackend>,
        output_thunks: Vec<Value>,
    }

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

            Ok(WasmVM {
                vm,
                output_thunks: vec![],
            })
        }

        #[wasm_bindgen]
        pub fn execute(&mut self, fn_idx: u32, args: &[i32]) -> JsValue {
            self.vm
                .execute(fn_idx, args.into_iter().map(|v| Value::I32(*v)).collect())
                .into()
        }

        #[wasm_bindgen]
        pub fn call_wasm(&mut self, idx: u32, args: &[i32]) -> JsValue {
            self.vm.call_wasm(idx, args).into()
        }

        #[wasm_bindgen]
        pub fn init_channels(&mut self, count: usize) {
            self.vm.init_channels(count);
        }

        #[wasm_bindgen]
        pub fn init_output(&mut self, init_fn_idx: u32) {
            self.output_thunks
                .push(self.vm.execute(init_fn_idx, vec![]));
        }

        /// JS output `[output_idx, value]` row per output that ticked.
        #[wasm_bindgen]
        pub fn step(&mut self, channel_idx: usize, val: i32) -> js_sys::Array {
            self.vm.channels[channel_idx] = val;
            let mask: u32 = 1u32 << channel_idx;
            let updates = js_sys::Array::new();
            let WasmVM { vm, output_thunks } = self;
            for (i, thunk) in output_thunks.iter_mut().enumerate() {
                if thunk.clock() & mask == 0 {
                    continue;
                }
                let stepped = match std::mem::replace(thunk, Value::Unit) {
                    Value::Thunk {
                        fun_idx, captures, ..
                    } => vm.execute(fun_idx, captures.into_vec()),
                    other => panic!("expected thunk at output {i}, got {other:?}"),
                };
                let row = js_sys::Array::new();
                row.push(&JsValue::from(i as u32));
                match stepped {
                    Value::Tuple(elems) => {
                        let mut elems = elems.into_vec();
                        let next = elems.remove(1);
                        row.push(&elems.remove(0).into());
                        *thunk = next;
                    }
                    other => {
                        row.push(&other.into());
                    }
                }
                updates.push(&row);
            }
            updates
        }

        #[wasm_bindgen]
        pub fn run_bytecode_main(&mut self, idx: u32, args: &[i32]) -> JsValue {
            let args = args.iter().map(|&v| Value::I32(v)).collect();
            let v = self.vm.execute(idx, args);
            self.vm.force_all(v).into()
        }
    }
}
