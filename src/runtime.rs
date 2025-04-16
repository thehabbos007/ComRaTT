use wasmtime::*;

pub struct Runtime {
    engine: Engine,
    program: Vec<u8>, // boxed slice pls
}

// An initial, naive version that does not consider non-main functions, input channels, transitions
// or anything advanced.
impl Runtime {
    pub fn init(program: &[u8]) -> Self {
        let mut config = Config::new();
        config.wasm_gc(true);
        config.wasm_multi_memory(true);
        config.wasm_function_references(true);
        config.wasm_tail_call(true);

        let engine = Engine::new(&config).expect("Failed to initialise Engine in runtime");

        let program = program.to_vec();

        Runtime { engine, program }
    }

    pub fn run(&self) {
        let module =
            Module::new(&self.engine, &self.program).expect("Failed to init ComRaTT module");
        let mut store: Store<u32> = Store::new(&self.engine, 4);
        let instance = Instance::new(&mut store, &module, &[]).expect("Failed to init instance");
        let location_dispatch = instance
            .get_typed_func::<i32, i32>(&mut store, "location_dispatch")
            .expect("Failed to retrieve dispatch function");

        // We need to receive some information about what functions to
        // call initially. This should be part of the init transition
        // of the reactive machine.
        // For now we just assume a main that needs a single integer argument.

        let main = instance
            .get_typed_func::<i32, i32>(&mut store, "main")
            .expect("Failed to retrieve main function");

        let res = main
            .call(&mut store, 42)
            .expect("Failed to call main function");
        println!("Reactive machine received ptr {res:?} from calling main");

        let clos = instance.get_memory(&mut store, "heap").unwrap();
        let loc = instance.get_memory(&mut store, "location").unwrap();
        println!("closure {:?}", &clos.data(&mut store)[..48]);
        println!("location {:?}", &loc.data(&mut store)[..48]);

        // Clock of testing
        let clock_of = instance
            .get_typed_func::<i32, i32>(&mut store, "clock_of")
            .expect("Failed to retrieve clock of function");

        let clock = clock_of
            .call(&mut store, res)
            .expect("failed to call clock of");
        println!("clock for {res} is {clock}");

        // call dispatch with the closure pointer returned from main
        // and supply the unit argument
        let result = location_dispatch
            .call(&mut store, res)
            .expect("Failed to call dispatch");
        println!("Dispatching ptr resulted in {result:?}");
    }
}
