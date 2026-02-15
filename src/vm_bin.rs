use comratt_vm::{Function, Op, Value, VM};

fn factorial_program() -> Vec<Function> {
    vec![
        Function {
            name: "factorial".into(),
            param_count: 1,
            local_count: 1,
            ops: vec![
                // 0: if n <= 1 jump to base case
                Op::Load(0),        // 0: push n
                Op::ConstI32(1),    // 1: push 1
                Op::Lte,            // 2: n <= 1
                Op::JumpIfFalse(6), // 3: if false, jump to recursive case
                // base case: return 1
                Op::ConstI32(1), // 4: push 1
                Op::Return,      // 5: return 1
                // recursive case: n * factorial(n - 1)
                Op::Load(0),            // 6: push n
                Op::Load(0),            // 7: push n
                Op::ConstI32(1),        // 8: push 1
                Op::Sub,                // 9: n - 1
                Op::CallBytecode(0, 1), // 10: factorial(n-1)
                Op::Mul,                // 11: n * factorial(n-1)
                Op::Return,             // 12: return
            ],
        },
        Function {
            name: "main".into(),
            param_count: 1,
            local_count: 1,
            ops: vec![
                Op::Load(0),            // 0: push n
                Op::CallBytecode(0, 1), // 1: factorial(n)
                Op::Return,             // 2: return
            ],
        },
    ]
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let input: i32 = args
        .get(1)
        .map(|s| s.parse().expect("argument must be an integer"))
        .unwrap_or(10);

    let fns = factorial_program();
    let main_idx = fns.len() as u32 - 1;

    eprintln!("Bytecode functions:");
    for (i, f) in fns.iter().enumerate() {
        eprintln!(
            "[{i}] {} (params={}, locals={})",
            f.name, f.param_count, f.local_count
        );
        for (j, op) in f.ops.iter().enumerate() {
            eprintln!("   |{j:3}: {op:?}");
        }
    }

    let mut vm = VM::new(fns);
    let result = vm.execute(main_idx, vec![Value::I32(input)]);

    eprintln!("\nResult value:");
    println!("{}", result.as_i32());
}
