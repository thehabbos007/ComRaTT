use comratt::backend::compile_anf_to_wasm;
use comratt::infer::infer_all;
use comratt::passes::run_program_passes_anf;
use comratt::runtime::Runtime;
use comratt::source::Prog;
use comratt::types::TypedProg;
use lexopt::Arg;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read as _, Write};

struct Args {
    input: Option<OsString>,
    run: bool,
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
        run: false,
    };

    let mut parser = lexopt::Parser::from_env();

    while let Some(arg) = parser.next()? {
        match arg {
            Arg::Value(path) => args.input = Some(path),
            Arg::Long("run") => args.run = true,
            Arg::Long(what) => {
                eprintln!("The argument '--{}' isn't used/required anymore.", what);
                return Ok(());
            }
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

    eprintln!("{}", prog);

    let prog = infer_all(prog);
    let wasm_bytes;

    // You thought the anf did something? Never has.
    if let Ok(bytes) = compile_and_write_prog(prog) {
        wasm_bytes = bytes;
    } else {
        panic!("Failed to compile and write oopsies");
    }

    if args.run {
        let machine = Runtime::init(&wasm_bytes);
        machine.run();
    }

    Ok(())
}

fn compile_and_write_prog(prog: TypedProg) -> Result<Vec<u8>, lexopt::Error> {
    let prog = run_program_passes_anf(prog);
    let res = compile_anf_to_wasm(&prog);
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();
    eprintln!();

    Ok(res)
}
