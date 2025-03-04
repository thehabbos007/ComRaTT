use wasmtime::*;

fn main() -> wasmtime::Result<()> {
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_multi_memory(true);
    config.wasm_function_references(true);

    let engine = Engine::new(&config)?;

    // Modules can be compiled through either the text or binary format
    let wat = include_str!("../mm-gc.wat");
    let module = Module::new(&engine, wat)?;

    // Host functionality can be arbitrary Rust functions and is provided
    // to guests through a `Linker`.
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
    let hello = instance.get_typed_func::<(), (i32, i32, i32)>(&mut store, "main")?;

    // And finally we can call the wasm!
    let res = hello.call(&mut store, ())?;
    println!("{res:?}");

    let now = instance
        .get_memory(&mut store, "now_heap")
        .unwrap()
        .data(&mut store);
    dbg!(&now[..5]);

    let later = instance
        .get_memory(&mut store, "later_heap")
        .unwrap()
        .data(&mut store);
    dbg!(&later[..5]);

    let dump = store.into_data();
    dbg!(&dump);

    Ok(())
}
