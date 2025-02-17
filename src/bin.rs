use lexopt::Arg;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read as _, Write};
use ComRaTT::backend::compile_to_wasm;
use ComRaTT::infer::infer_all;
use ComRaTT::passes::run_program_passes;
use ComRaTT::source::Prog;

struct Args {
    input: Option<OsString>,
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
    let mut args = Args { input: None };

    let mut parser = lexopt::Parser::from_env();

    while let Some(arg) = parser.next()? {
        match arg {
            Arg::Value(path) => args.input = Some(path),
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
    let prog = run_program_passes(prog);
    // println!("{}", &prog.untyped().to_string());
    let res = compile_to_wasm(&prog);

    let mut stdout = std::io::stdout().lock();
    stdout.write_all(&res).unwrap();

    Ok(())
}
