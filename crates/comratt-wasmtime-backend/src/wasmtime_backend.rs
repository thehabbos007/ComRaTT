use anyhow::Result;
use comratt_vm::vm::WasmBackend;
use wasmtime::{Caller, Linker, Val};

#[derive(Clone)]
struct Closure {
    fn_idx: u32,
    arity: u32,
    args: Vec<i32>,
}

struct StoreData {
    closures: Vec<Closure>,
    fn_names: Vec<String>,
}

pub struct WasmtimeBackend {
    store: wasmtime::Store<StoreData>,
    fns: Vec<wasmtime::Func>,
}

impl WasmtimeBackend {
    pub fn new(wasm_bytes: &[u8], fn_names: &[String]) -> Result<Self> {
        let engine = wasmtime::Engine::default();
        let data = StoreData {
            closures: vec![],
            fn_names: fn_names.to_vec(),
        };
        let mut store = wasmtime::Store::new(&engine, data);

        let fns = if fn_names.is_empty() {
            vec![]
        } else {
            let mut linker = Linker::new(&engine);
            linker.func_wrap(
                "env",
                "closure_new",
                |mut caller: Caller<'_, StoreData>, fn_idx: i32, arity: i32| -> i32 {
                    let closures = &mut caller.data_mut().closures;
                    closures.push(Closure {
                        fn_idx: fn_idx as u32,
                        arity: arity as u32,
                        args: vec![],
                    });
                    (closures.len() - 1) as i32
                },
            )?;
            linker.func_wrap(
                "env",
                "closure_apply",
                |mut caller: Caller<'_, StoreData>, handle: i32, arg: i32| -> i32 {
                    let mut closure = caller.data().closures[handle as usize].clone();
                    closure.args.push(arg);

                    if closure.args.len() as u32 == closure.arity {
                        let name = caller.data().fn_names[closure.fn_idx as usize].clone();
                        let func = caller
                            .get_export(&name)
                            .and_then(|e| e.into_func())
                            .unwrap_or_else(|| panic!("closure target '{name}' not found"));
                        let args: Vec<Val> = closure.args.iter().map(|&v| Val::I32(v)).collect();
                        let mut results = [Val::I32(0)];
                        func.call(&mut caller, &args, &mut results)
                            .expect("closure call failed");
                        match results[0] {
                            Val::I32(n) => n,
                            ref other => panic!("closure returned non-i32: {other:?}"),
                        }
                    } else {
                        let closures = &mut caller.data_mut().closures;
                        closures.push(closure);
                        (closures.len() - 1) as i32
                    }
                },
            )?;

            let module = wasmtime::Module::new(&engine, wasm_bytes)?;
            let instance = linker.instantiate(&mut store, &module)?;
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
