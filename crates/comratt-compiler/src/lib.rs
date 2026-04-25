#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(assert_matches)]

pub mod format;
pub mod hybrid;
pub mod infer;
pub mod parse;
pub mod passes;
pub mod source;
pub mod types;

// LSP in Zed goes out the window
// with this cfg. Too lazy to find a fix atm.
//#[cfg(target_arch = "wasm32")]
pub mod wasm_exports {
    use wasm_bindgen::prelude::*;

    use crate::{
        hybrid::{self, FunRef, HybridProgram},
        infer::infer_all,
        source::Prog,
    };

    #[wasm_bindgen]
    pub struct BrowserProgram(HybridProgram);

    #[wasm_bindgen]
    impl BrowserProgram {
        pub fn wasm_bytes(&self) -> Vec<u8> {
            self.0.wasm_bytes.clone()
        }

        #[wasm_bindgen]
        pub fn serialized_bytecode(&self) -> Vec<u8> {
            postcard::to_allocvec(&self.0.bytecode_fns).expect("serialization error")
        }

        pub fn output_labels(&self) -> Vec<JsValue> {
            self.0
                .outputs
                .iter()
                .map(|(n, _)| JsValue::from_str(n))
                .collect()
        }

        pub fn channel_names(&self) -> Vec<JsValue> {
            self.0
                .channels
                .iter()
                .map(|(n, _)| JsValue::from_str(n))
                .collect()
        }

        pub fn pure_fn_names(&self) -> Vec<JsValue> {
            self.0
                .pure_fn_names
                .iter()
                .map(|n| JsValue::from_str(n))
                .collect()
        }

        pub fn output_init_indices(&self) -> Vec<u32> {
            (0..self.0.outputs.len())
                .map(|i| format!("#output_init_{i}"))
                .map(|output_name| match self.0.fn_map.get(&output_name) {
                    Some(FunRef::Bytecode(idx)) => *idx,
                    other => panic!("output init: {output_name:?} is a: {other:?}"),
                })
                .collect()
        }

        /// None if no main function is present/main is wasm-compiled
        pub fn main_bytecode_idx(&self) -> Option<u32> {
            match self.0.fn_map.get("main")? {
                FunRef::Bytecode(idx) => Some(*idx),
                FunRef::Wasm(_) => None,
            }
        }
    }

    #[wasm_bindgen]
    pub fn compile(source: &str) -> Result<BrowserProgram, JsError> {
        let prog = Prog::parse(&source).expect("parse error");
        let typed = infer_all(prog);
        let compiled = hybrid::compile(&typed);

        Ok(BrowserProgram(compiled))
    }
}
