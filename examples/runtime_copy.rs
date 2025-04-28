use wasmtime::*;

// naive impl.
// Make a struct etc.
// Get the bytes instead of reading from a file.
//
// What do we need to do?
// - We should execute functions that result in pointers to heap
// for now we can just read a file and execute main.
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
    let dispatch = instance.get_typed_func::<(i32, i32), i32>(&mut store, "dispatch")?;

    let res = main.call(&mut store, 42)?;
    println!("{res:?}");

    // call dispatch with the closure pointer returned from main
    // and supply the unit argument
    let result = dispatch.call(&mut store, (res, -1))?;
    println!("{result:?}");
    //let lambda_1 = instance.get_typed_func::<(i32, i32), i32>(&mut store, "#lambda_1")?;
    //let result = lambda_1.call(&mut store, (69, -1));
    //println!("lambda_1 gave {result:?}");
    //let lambda_2 = instance.get_typed_func::<(i32, i32), i32>(&mut store, "#lambda_2")?;
    //let result = lambda_2.call(&mut store, (0, -1));
    //println!("lambda_2 gave {result:?}");

    //let result = dispatch.call(&mut store, (0, -1))?;
    //println!("dispatch 0 gave {result:?}");

    /*
    let memory = instance
        .get_memory(&mut store, "heap")
        .expect("No memory found");

    let sliced = memory.data(&mut store);
    //println!("{}", sliced[res as usize]);
    println!("{:?}", &sliced[..32]);
    */
    //let table = instance.get_table(&mut store, "table").unwrap();
    //let first = table.get(&mut store, 4).unwrap();
    //let first_func = first.as_func().unwrap().unwrap();
    //println!("{}", first_func.ty(&mut store));

    Ok(())
}
