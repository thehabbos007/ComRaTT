use comratt::runtime::Reactive_machine;
use std::collections::HashMap;

fn main() {
    let mut runtime = Reactive_machine::init(HashMap::new(), HashMap::new());
    runtime.exec("../runtime".to_owned());
}
