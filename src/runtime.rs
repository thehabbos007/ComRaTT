use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use wasmtime::*;

use crate::source::Type;

#[derive(Debug)]
struct RuntimeState {
    output_to_location: HashMap<i32, i32>,
}

impl RuntimeState {
    fn new() -> Self {
        Self {
            output_to_location: HashMap::new(),
        }
    }

    fn ffi_set_output_to_location(&mut self, output_channel_index: i32, location_ptr: i32) -> i32 {
        self.output_to_location
            .insert(output_channel_index, location_ptr);
        // return the ptr again to conform to the return type of dispatch
        // and avoiding to work around that. a bit hacky.
        location_ptr
    }
}

pub struct Runtime {
    engine: Engine,
    program: Vec<u8>, // boxed slice pls
    channels: Vec<(String, Type)>,
    output_channels: Vec<String>,
}

// An initial, naive version that does not consider non-main functions, input channels, transitions
// or anything advanced.
impl Runtime {
    pub fn init(
        program: &[u8],
        channels: Vec<(String, Type)>,
        output_channels: Vec<String>,
    ) -> Self {
        let mut config = Config::new();
        config.wasm_gc(true);
        config.wasm_multi_memory(true);
        config.wasm_function_references(true);
        config.wasm_tail_call(true);

        let engine = Engine::new(&config).expect("Failed to initialise Engine in runtime");

        let program = program.to_vec();

        Runtime {
            engine,
            program,
            channels,
            output_channels,
        }
    }

    pub fn run(&self) {
        #[allow(unused)]
        #[derive(Debug)]
        enum ChanTyp {
            I32(i32),
            TupI32(i32, i32),
        }
        let init_channel_map = self
            .channels
            .iter()
            .enumerate()
            .map(|(chan_idx, (_, typ))| {
                (
                    2i32.pow(chan_idx as u32),
                    match typ {
                        Type::TInt => ChanTyp::I32(333),
                        Type::TProduct(typs) if typs == &[Type::TInt, Type::TInt] => {
                            ChanTyp::TupI32(0, 0)
                        }
                        typ => panic!("Unsupported type for channel mapping {}", typ),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        let channels: Arc<Mutex<HashMap<i32, ChanTyp>>> = Arc::new(Mutex::new(init_channel_map));

        let chan = channels.clone();

        let module =
            Module::new(&self.engine, &self.program).expect("Failed to init ComRaTT module");

        eprintln!(
            "Runtime started with output channels: {:?}",
            &self.output_channels
        );

        let state = RuntimeState::new();
        let mut store: Store<RuntimeState> = Store::new(&self.engine, state);

        let wait_ffi = move |channel: i32| -> i32 {
            let channels = chan.lock().unwrap();
            println!(
                "Runtime requested channel {} value: {:?}",
                channel, channels[&channel]
            );
            match channels[&channel] {
                ChanTyp::I32(val) => val,
                ChanTyp::TupI32(_, _) => todo!("We need to malloc tuples"),
            }
        };
        let wait = Func::wrap(&mut store, wait_ffi);

        let set_output_to_location = Func::wrap(
            &mut store,
            |mut caller: Caller<'_, RuntimeState>, output_channel_index: i32, location_ptr: i32| {
                let state = caller.data_mut();
                state.ffi_set_output_to_location(output_channel_index, location_ptr);
                location_ptr
            },
        );

        let ffi_functions = [wait.into(), set_output_to_location.into()];

        let instance =
            Instance::new(&mut store, &module, &ffi_functions).expect("Failed to init instance");

        let init = instance
            .get_typed_func::<(), ()>(&mut store, "init")
            .expect("Failed to retrieve init function");

        init.call(&mut store, ()).expect("Failed to call init");

        let clos = instance.get_memory(&mut store, "heap").unwrap();
        let loc = instance.get_memory(&mut store, "location").unwrap();
        //println!("closure {:?}", &clos.data(&mut store)[..48]);
        // println!("location {:?}", &loc.data(&mut store)[..48]);

        let clos_data = clos.data(&mut store).to_vec();
        let loc_data = loc.data(&mut store).to_vec();

        println!("State is {:?} after init", store.data().output_to_location);
        for (k, v) in &store.data().output_to_location {
            println!("Output channel with index {k} points to location at {v}");
            let fp = loc_data[*v as usize];
            println!("Function pointer at location is {fp}");

            println!(
                "Location at {v} points to function {:?}",
                clos_data[fp as usize]
            );
        }
        println!("closure {:?}", &clos_data[..48]);
        println!("location {:?}", &loc_data[..48]);

        let location_dispatch = instance
            .get_typed_func::<i32, i32>(&mut store, "location_dispatch")
            .expect("Cannot location dispatch");

        for (output_idx, location_ptr) in &store.data().output_to_location.clone() {
            let sig = location_dispatch
                .call(&mut store, *location_ptr)
                .expect("Failed to location dispatch");

            let clos_data = clos.data(&mut store).to_vec();
            let loc_data = loc.data(&mut store).to_vec();
            println!("sig is {:?}", sig);
            println!("closure {:?}", &clos_data[..48]);
            println!("location {:?}", &loc_data[..48]);
            if self.output_channels[*output_idx as usize] == "print" {
                let bytes = &clos_data.as_slice()[sig as usize..(sig as usize + 4)];
                let value = i32::from_le_bytes(bytes.try_into().unwrap());
                println!("Value is {}", value);
            }
        }
    }
}
