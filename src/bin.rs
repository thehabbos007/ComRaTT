use std::collections::HashMap;
use std::io::BufRead;

use comratt::{
    hybrid::{self, FunRef},
    infer::infer_all,
    source::Prog,
};
use comratt_backend::wasmtime_backend::WasmtimeBackend;
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

    let output_labels: Vec<String> = compiled.outputs.iter().map(|(n, _)| n.clone()).collect();
    let channel_names: Vec<String> = compiled.channels.iter().map(|(n, _)| n.clone()).collect();
    let fn_map = compiled.fn_map;

    let backend = WasmtimeBackend::new(&compiled.wasm_bytes, &compiled.pure_fn_names)
        .expect("WASM backend init failed");
    let mut vm = VM::new(compiled.bytecode_fns, backend);

    if !output_labels.is_empty() {
        reactive_loop(&mut vm, &fn_map, &output_labels, &channel_names);
    } else {
        pure_run(&mut vm, &fn_map, &args[2..]);
    }
}

fn reactive_loop(
    vm: &mut VM<WasmtimeBackend>,
    fn_map: &HashMap<String, FunRef>,
    output_labels: &[String],
    channel_names: &[String],
) {
    vm.init_channels(channel_names.len());

    // execute output-init functions
    let mut thunks: Vec<Value> = (0..output_labels.len())
        .map(|i| {
            let init_name = format!("#output_init_{i}");
            let fn_ref = *fn_map
                .get(&init_name)
                .unwrap_or_else(|| panic!("missing {init_name}"));
            match fn_ref {
                FunRef::Bytecode(idx) => vm.execute(idx, vec![]),
                FunRef::Wasm(_) => panic!("{init_name} compiled to WASM incorrectly"),
            }
        })
        .collect();

    eprintln!("Channels: {channel_names:?}");
    eprintln!("Int per line:");

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.expect("stdin read error");
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let val: i32 = match line.parse() {
            Ok(v) => v,
            Err(_) => {
                eprintln!("expected integer, got {line:?}");
                continue;
            }
        };

        // we just support one channel, 0, for now...
        vm.channels[0] = val;
        let channel_mask: u32 = 1;

        for (i, thunk) in thunks.iter_mut().enumerate() {
            if thunk.clock() & channel_mask == 0 {
                continue;
            }
            let stepped = match std::mem::replace(thunk, Value::Unit) {
                Value::Thunk {
                    fun_idx, captures, ..
                } => vm.execute(fun_idx, captures.into_vec()),
                other => panic!("expected thunk at output {i}, got {other:?}"),
            };
            match stepped {
                Value::Tuple(elems) => {
                    let mut elems = elems.into_vec();
                    assert_eq!(elems.len(), 2, "signals should produce 2-tuple");
                    let next = elems.remove(1);
                    let out_val = elems.remove(0);
                    println!("[{}] {}: {}", i, output_labels[i], out_val.as_i32());
                    *thunk = next;
                }
                other => {
                    println!("[{}] {}: {other:?}", i, output_labels[i]);
                }
            }
        }
    }
}

fn pure_run(vm: &mut VM<WasmtimeBackend>, fn_map: &HashMap<String, FunRef>, raw_args: &[String]) {
    let runtime_args: Vec<i32> = raw_args
        .iter()
        .map(|s| s.parse().expect("args must be integers"))
        .collect();

    let fn_ref = *fn_map.get("main").expect("no 'main' function");

    let final_val = match fn_ref {
        FunRef::Wasm(idx) => Value::I32(vm.call_wasm(idx, &runtime_args)),
        FunRef::Bytecode(idx) => {
            let args: Vec<Value> = runtime_args.iter().map(|&v| Value::I32(v)).collect();
            let num_args = args.len();
            let param_count = vm.bytecode_fns[idx as usize].param_count as usize;
            assert!(
                num_args == param_count,
                "Cannot call bytecode main function expecting {} args with {}",
                param_count,
                num_args
            );
            vm.execute(idx, args)
        }
    };

    let forced = vm.force_all(final_val);
    println!("Result: {forced:?}");
}
