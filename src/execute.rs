use wasmtime::*;

fn dummy_linker<T>(store: &mut Store<T>, module: &Module) -> anyhow::Result<Linker<T>> {
    let mut linker = Linker::new(store.engine());
    linker.allow_shadowing(true);
    for import in module.imports() {
        let extern_ = import.ty().default_value(&mut *store)?;
        linker
            .define(&store, import.module(), import.name(), extern_)
            .unwrap();
    }
    Ok(linker)
}

pub fn execute_wasm_main(program: &[u8], args: Vec<i32>) -> Result<()> {
    let engine = Engine::default();
    let module = Module::from_binary(&engine, program)?;

    let mut store = Store::new(&engine, ());

    let linker = dummy_linker(&mut store, &module).unwrap();

    let instance = linker.instantiate(&mut store, &module).unwrap();

    let run = instance.get_func(&mut store, "main").unwrap();

    let res_count = run.ty(&store).results().len();
    let mut res = vec![Val::I32(0); res_count];
    let args = args.into_iter().map(|v| v.into()).collect::<Vec<Val>>();

    run.call(&mut store, args.as_slice(), &mut res)?;

    eprintln!("\n\n=======================\n Execution output:\n    {res:?}");
    Ok(())
}
