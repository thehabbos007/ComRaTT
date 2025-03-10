use std::{
    collections::HashMap,
    io::{self},
};

use itertools::Itertools;
use wasmtime::*;

use crate::types::TypedExpr;
use crate::types::TypedToplevel;

const CHANNELS: &[&str] = &["KEYBOARD"];

pub struct ReactiveMachine {
    // N: output names to wasm function names?
    // Are indices better? ideally we associate output to location
    // but we do not know locations at this point...
    // but! we dont need to know locations in the initialization
    // only after!
    output_to_texpr: HashMap<String, TypedExpr>,
    // channel names to closure/delayed comp names.
    delayed: HashMap<String, Vec<String>>,
    engine: Engine,
    // associate buffered input channels with their latest values
    // i32 type is placeholder
    input_buffer: HashMap<String, i32>,
    program: Vec<u8>,
}

impl ReactiveMachine {
    fn invalid_channels_are_used(channels: Vec<TypedToplevel>) -> bool {
        let mut channels_invalid = false;
        for channel in channels {
            if let TypedToplevel::Channel(name) = channel {
                if !CHANNELS.contains(&name.as_str()) {
                    channels_invalid = true;
                }
            }
        }
        channels_invalid
    }

    pub fn init(
        output_to_texpr: HashMap<String, TypedExpr>,
        program: Vec<u8>,
        channels: Vec<TypedToplevel>,
    ) -> Self {
        if ReactiveMachine::invalid_channels_are_used(channels) {
            panic!("Reactive programm attempted to use invalid channels");
        }

        let mut config = Config::new();
        config.wasm_gc(true);
        config.wasm_multi_memory(true);
        config.wasm_function_references(true);

        let engine = Engine::new(&config).expect("Failed to init engine in Runtime");

        let module = Module::new(&engine, &program).expect("Failed to init ComRaTT module");
        let mut store: Store<u32> = Store::new(&engine, 4);
        // maybe we need to add memory regions here
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to init instance");

        // into_extern also exists if we need everything at some point
        let funcs = instance
            .exports(&mut store)
            .filter_map(|export| export.into_func())
            .collect_vec();

        let delayed = HashMap::new();
        ReactiveMachine {
            output_to_texpr,
            delayed,
            engine,
            input_buffer: HashMap::new(),
            program,
        }
    }

    fn main_reactive_loop(&mut self) {
        // loop while waiting for input on a channel
        // when new data arrives
        // do input_transition
        // followed by output_transition
    }

    fn input_transition(&mut self) {
        // we received data on kappa
        // modify value in input buffer if needed
        // transition heap
        // - all locations (i.e. closures) waiting for kappa are "now"
        // - all locations (i.e. closures) NOT waiting for kappa are "later"
    }
    fn output_transition(&mut self) {
        // fire off delayed computations related to the updated kappa
        // compute new output values where relevant by
        // - going through x -> l in N (self.output_to_texpr) and checking whether
        // l depends on kappa
        // Perform garbage collection by deleting all now-closures and marking
        // later closures as now
    }

    pub fn exec(&mut self) {
        // what the f is 4 here?
        let mut store: Store<u32> = Store::new(&self.engine, 4);

        // Fetch the generated module
        // let comratt_file = format!("{}.wat", module_name);
        // let comratt_prog = fs::read_to_string(comratt_file).expect("ComRaTT file did not exist");
        // let module = Module::new(&self.engine, comratt_prog).expect("Failed to init ComRaTT module");
        let module =
            Module::new(&self.engine, &self.program).expect("Failed to init ComRaTT module");

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
