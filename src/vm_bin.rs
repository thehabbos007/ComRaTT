use comratt::{hybrid, infer::infer_all, source::Prog};
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

    let rest = &args[2..];
    let runtime_args: Vec<i32> = rest
        .iter()
        .map(|s| s.parse().expect("args must be integers"))
        .collect();
    let fn_ref = *compiled.fn_map.get("main").expect("no 'main' function");

    let mut vm = VM::new(compiled.bytecode_fns);

    let args: Vec<Value> = runtime_args.iter().map(|&v| Value::I32(v)).collect();
    let final_val = vm.execute(fn_ref, args);

    eprintln!("\n=== Result ===");
    eprintln!("{final_val:?}");
    println!("{}", final_val.as_i32());
}
