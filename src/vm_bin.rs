use comratt::{
    hybrid::{self, FunRef},
    infer::infer_all,
    source::Prog,
    wasm_backend::WasmtimeBackend,
};
use comratt_vm::{Value, VM};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let source_path = &args[1];
    let source = std::fs::read_to_string(source_path).expect("failed to read file");

    let prog = Prog::parse(&source).expect("parse error");
    let typed = infer_all(prog);
    let compiled = hybrid::compile(&typed);

    eprintln!("WASM functions: {:?}", compiled.pure_fn_names);
    eprintln!("Bytecode functions:");
    for (i, f) in compiled.bytecode_fns.iter().enumerate() {
        eprintln!(
            "[{i}] {} (params={}, locals={})",
            f.name, f.param_count, f.local_count
        );
        for (j, op) in f.ops.iter().enumerate() {
            eprintln!("   |{j:3}: {op:?}");
        }
    }

    let runtime_args: Vec<i32> = args[2..]
        .iter()
        .map(|s| s.parse().expect("args must be integers"))
        .collect();

    let fn_ref = *compiled.fn_map.get("main").expect("no 'main' function");

    let backend = WasmtimeBackend::new(&compiled.wasm_bytes, &compiled.pure_fn_names)
        .expect("WASM backend init failed");
    let mut vm = VM::new(compiled.bytecode_fns, backend);

    let final_val = match fn_ref {
        FunRef::Wasm(idx) => Value::I32(vm.call_wasm(idx, &runtime_args)),
        FunRef::Bytecode(idx) => {
            let args: Vec<Value> = runtime_args.iter().map(|&v| Value::I32(v)).collect();
            vm.execute(idx, args)
        }
    };

    let forced = vm.force_all(final_val);

    println!("Result: {forced:?}");
}
