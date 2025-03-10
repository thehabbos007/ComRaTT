use comratt::backend::{compile_anf_to_wasm, compile_to_wasm};
use comratt::infer::infer_all;
use comratt::passes::{run_program_passes, run_program_passes_anf};
use comratt::runtime::ReactiveMachine;
use comratt::source::Prog;
use comratt::types::{TypedExpr, TypedProg, TypedToplevel};
use lexopt::Arg;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read as _, Write};

struct Args {
    input: Option<OsString>,
    anf: bool,
    exec: bool,
}

impl Args {
    fn read(&self) -> String {
        match self.input {
            Some(ref path) => {
                let mut file = File::open(path).unwrap();
                let mut contents = String::new();
                file.read_to_string(&mut contents).unwrap();
                contents
            }
            None => {
                let mut contents = String::new();
                io::stdin().read_to_string(&mut contents).unwrap();
                contents
            }
        }
    }
}

fn main() -> Result<(), lexopt::Error> {
    let mut args = Args {
        input: None,
        anf: false,
        exec: false,
    };

    let mut parser = lexopt::Parser::from_env();

    while let Some(arg) = parser.next()? {
        match arg {
            Arg::Value(path) => args.input = Some(path),
            Arg::Long("anf") => args.anf = true,
            Arg::Long("exec") => args.exec = true,
            _ => return Ok(()),
        }
    }

    let input = args.read();

    let prog = match Prog::parse(&input) {
        Ok(prog) => prog,
        Err(err) => {
            eprintln!("{}", err);
            return Ok(());
        }
    };
    let prog = infer_all(prog);
    let (output_to_expr, channels) = extract_runtime_information(&prog);

    let wasm_bytes;

    if args.anf {
        if let Ok(bytes) = compile_and_write_prog(prog) {
            wasm_bytes = bytes;
        } else {
            panic!();
        }
    } else if let Ok(bytes) = compile_and_write_prog_anf(prog) {
        wasm_bytes = bytes;
    } else {
        panic!();
    }

    if args.exec {
        let machine = ReactiveMachine::init(output_to_expr, wasm_bytes, channels);
    }

    Ok(())
}

fn extract_runtime_information(
    prog: &TypedProg,
) -> (HashMap<String, TypedExpr>, Vec<TypedToplevel>) {
    // træk information ud om outputs
    // her kan vi sikre at de channels der bruges rent faktisk findes
    // her kan vi IKKE associere channels med delayed computations
    // delays skal senere hen også have en clock, men delays findes ikke i det typede AST.
    // så hvad gør vi der?
    // Vi skal se på "init" i den reaktive maskine.
    let mut output_to_expr = HashMap::new();
    let mut channels = vec![];
    for toplevel in prog.0.iter() {
        match toplevel {
            TypedToplevel::Output(name, texp) => {
                output_to_expr.insert(name.clone(), *texp.clone());
            }
            chan @ TypedToplevel::Channel(_) => channels.push(chan.clone()),
            _ => (),
        }
    }
    (output_to_expr, channels)
}

fn compile_and_write_prog(prog: TypedProg) -> Result<Vec<u8>, lexopt::Error> {
    let prog = run_program_passes(prog);
    let res = compile_to_wasm(&prog);
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();

    Ok(res)
}

fn compile_and_write_prog_anf(prog: TypedProg) -> Result<Vec<u8>, lexopt::Error> {
    let prog = run_program_passes_anf(prog);
    let res = compile_anf_to_wasm(&prog);
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();

    Ok(res)
}
