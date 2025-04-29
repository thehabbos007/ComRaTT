use wasmtime::*;

fn main() -> wasmtime::Result<()> {
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_multi_memory(true);
    config.wasm_function_references(true);
    config.wasm_tail_call(true);

    let engine = Engine::new(&config)?;

    let wasm = include_bytes!("../freevar.wasm");
    let module = Module::new(&engine, wasm)?;

    let mut linker = Linker::new(&engine);

    linker.func_wrap(
        "host",
        "host_func",
        |caller: Caller<'_, u32>, param: i32| {
            println!("Got {} from WebAssembly", param);
            println!("my host state is: {}", caller.data());
        },
    )?;

    let mut store: Store<u32> = Store::new(&engine, 4);

    let instance = linker.instantiate(&mut store, &module)?;
    let main = instance.get_typed_func::<i32, i32>(&mut store, "main")?;
    let dispatch = instance.get_typed_func::<i32, i32>(&mut store, "dispatch")?;

    let res = main.call(&mut store, 42)?;
    println!("{res:?}");

    // call dispatch with the closure pointer returned from main
    // and supply the unit argument
    let result = dispatch.call(&mut store, res)?;
    println!("{result:?}");

    Ok(())
}
