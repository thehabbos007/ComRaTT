use lexopt::Arg;
use std::fs::File;
use std::io::{self, Read as _};
use std::path::PathBuf;
use ComRaTT::source::Prog;

struct Args {
    input: Option<PathBuf>,
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
    use lexopt::prelude::*;

    let mut args = Args { input: None };

    let mut parser = lexopt::Parser::from_env();
    /*
    while let Some(arg) = parser.next()? {
        match arg {
            Arg::Positional("input") => args.input = parser.expect_value()?,
            _ => return Ok(()),
        }
    }
    */

    let input = args.read();

    // let prog = match Prog::parse(&input) {
    //     Ok(prog) => prog,
    //     Err(err) => {
    //         eprintln!("{}", err);
    //         return Ok(());
    //     }
    // };

    Ok(())
}
