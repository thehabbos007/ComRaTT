use lexopt::Arg;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read as _, Write};
use ComRaTT::backend::{compile_anf_to_wasm, compile_to_wasm};
use ComRaTT::infer::infer_all;
use ComRaTT::passes::{run_program_passes, run_program_passes_anf};
use ComRaTT::source::Prog;
use ComRaTT::types::TypedProg;

struct Args {
    input: Option<OsString>,
    anf: bool,
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
    };

    let mut parser = lexopt::Parser::from_env();

    while let Some(arg) = parser.next()? {
        match arg {
            Arg::Value(path) => args.input = Some(path),
            Arg::Long("anf") => args.anf = true,
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

    if args.anf {
        compile_prog_anf(prog)?;
    } else {
        compile_prog(prog)?;
    }

    Ok(())
}

fn compile_prog(prog: TypedProg) -> Result<(), lexopt::Error> {
    let prog = run_program_passes(prog);
    let res = compile_to_wasm(&prog);
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();

    println!("{}", &prog.untyped().to_string());

    Ok(())
}

fn compile_prog_anf(prog: TypedProg) -> Result<(), lexopt::Error> {
    let prog = run_program_passes_anf(prog);
    let res = compile_anf_to_wasm(&prog);
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();

    Ok(())
}
