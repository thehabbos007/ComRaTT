use std::collections::HashMap;
use std::io::BufRead;

use comratt_compiler::{
    hybrid::{self, FunRef},
    infer::infer_all,
    source::Prog,
};
use comratt_vm::{Value, VM};
use comratt_wasmtime_backend::wasmtime_backend::WasmtimeBackend;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let source_path = &args[1];
    let source = std::fs::read_to_string(source_path).expect("failed to read file");

    let prog = Prog::parse(&source).unwrap_or_else(|e| {
        eprintln!("parse error!\n {e}");
        std::process::exit(1);
    });

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

    for (i, name) in channel_names.iter().enumerate() {
        eprintln!("  [{i}] {name}");
    }
    eprintln!("Input channel data: <channel_idx> <int_value>");

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.expect("stdin read error");
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let mut parts = line.split_ascii_whitespace();
        let (Some(idx_tok), Some(val_tok), None) = (parts.next(), parts.next(), parts.next())
        else {
            eprintln!("expected '<channel_idx> <value>', got {line:?}");
            continue;
        };
        let (Ok(channel_idx), Ok(val)) = (idx_tok.parse::<usize>(), val_tok.parse::<i32>()) else {
            eprintln!("could not parse '{idx_tok} {val_tok}'");
            continue;
        };
        if channel_idx >= vm.channels.len() {
            eprintln!("channel {channel_idx}/{} out of range", vm.channels.len());
            continue;
        }

        vm.channels[channel_idx] = val;
        let channel_mask: u32 = 1u32 << channel_idx;
        vm.current_tick = Some(channel_mask);

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
        vm.current_tick = None;
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
