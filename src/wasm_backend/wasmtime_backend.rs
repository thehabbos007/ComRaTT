use anyhow::Result;
use comratt_vm::vm::WasmBackend;

pub struct WasmtimeBackend {
    store: wasmtime::Store<()>,
    fns: Vec<wasmtime::Func>,
}

impl WasmtimeBackend {
    pub fn new(wasm_bytes: &[u8], fn_names: &[String]) -> Result<Self> {
        let engine = wasmtime::Engine::default();
        let mut store = wasmtime::Store::new(&engine, ());

        let fns = if fn_names.is_empty() {
            vec![]
        } else {
            let module = wasmtime::Module::new(&engine, wasm_bytes)?;
            let instance = wasmtime::Instance::new(&mut store, &module, &[])?;
            fn_names
                .iter()
                .map(|name| {
                    instance
                        .get_func(&mut store, name)
                        .unwrap_or_else(|| panic!("WASM export '{name}' not found"))
                })
                .collect()
        };

        Ok(WasmtimeBackend { store, fns })
    }
}

impl WasmBackend for WasmtimeBackend {
    fn call(&mut self, fn_idx: u32, args: &[i32]) -> i32 {
        let wasm_args: Vec<wasmtime::Val> = args.iter().map(|&v| wasmtime::Val::I32(v)).collect();
        let mut results = [wasmtime::Val::I32(0)];
        self.fns[fn_idx as usize]
            .call(&mut self.store, &wasm_args, &mut results)
            .expect("WASM call failed");
        match results[0] {
            wasmtime::Val::I32(n) => n,
            ref other => panic!("WASM function {fn_idx} returned non-i32: {other:?}"),
        }
    }
}
