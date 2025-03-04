use std::{
    collections::HashMap,
    fs,
    io::{self, stdin},
};

use wasmtime::*;

pub struct Reactive_machine {
    // N: output names to wasm function names?
    // Are indices better? ideally we associate output to location
    // but we do not know locations at this point...
    // but! we dont need to know locations in the initialization
    // only after!
    N: HashMap<String, String>,
    // channel to names. i32 is placeholder for proper channel type
    delayed: HashMap<i32, Vec<String>>,
    engine: Engine,
    // associate buffered input channels with their latest values
    // types are placeholder
    input_buffer: HashMap<i32, i32>,
}

impl Reactive_machine {
    pub fn init(N: HashMap<String, String>, delayed: HashMap<i32, Vec<String>>) -> Self {
        let mut config = Config::new();
        config.wasm_gc(true);
        config.wasm_multi_memory(true);
        config.wasm_function_references(true);

        let engine = Engine::new(&config).expect("Failed to init engine in Runtime");
        Reactive_machine {
            N,
            delayed,
            engine,
            input_buffer: HashMap::new(),
        }
    }

    pub fn exec(&mut self, module_name: String) {
        // what the f is 4 here?
        let mut store: Store<u32> = Store::new(&self.engine, 4);

        // Fetch the generated module
        let comratt_file = format!("{}.wat", module_name);
        let comratt_prog = fs::read_to_string(comratt_file).expect("ComRaTT file did not exist");
        let module =
            Module::new(&self.engine, comratt_prog).expect("Failed to init ComRaTT module");

        // Create a single memory region to pass to the wasm module
        // needs to be imported in the wasm/wat file
        // We could also just do them directly in wasm/wat
        // and yank them out for use here.
        let mem_type = MemoryType::new(1, Some(1024));
        let mem = Memory::new(&mut store, mem_type).expect("Failed to init memory");

        // let mut linker = Linker::new(&self.engine);
        // linker.instantiate svarer til at "starte"
        // let instance = linker.instantiate(&mut store, &module).unwrap();
        let instance =
            Instance::new(&mut store, &module, &[mem.into()]).expect("Failed to init instance");

        // if data arrived lookup the names of the functions to run i.e. associated with
        // the input channel.. we know this based on the use of delay..
        // THEN run all computations
        // THEN find all output channels associated with
        // computations and produce output
        // We need to model the reactive machine.
        // We also need buffer(s) for the buffered channels...

        // Just testing interop out
        let _ = mem.write(&mut store, 0, &[42]);
        let load = instance
            .get_typed_func::<i32, i32>(&mut store, "load")
            .unwrap();
        let result = load.call(&mut store, 0).unwrap();
        println!("{}", result);

        // listen for "keyboard events" on a budget
        for _ in io::stdin().lines() {
            println!("Hans");
        }

        // idea
        // each input type is a "worker" thread
        // that puts items in an MSPC channel here?
    }
}
