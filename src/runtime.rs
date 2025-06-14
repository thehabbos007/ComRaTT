use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use tokio::{sync::mpsc, task::JoinSet};
use wasmtime::*;

use crate::{constants::OutputKind, source::Type};

#[derive(Debug)]
struct RuntimeState {
    output_to_location: HashMap<i32, Vec<i32>>,
}

#[derive(Debug)]
enum Event {
    Keyboard(u8),
}

impl RuntimeState {
    fn new() -> Self {
        Self {
            output_to_location: HashMap::new(),
        }
    }

    fn ffi_set_output_to_location(&mut self, output_channel_index: i32, location_ptr: i32) -> i32 {
        // When a new output is registered, slot the location pointer into the output_to_location map
        self.output_to_location
            .entry(output_channel_index)
            .or_default()
            .push(location_ptr);
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
    pub async fn init(
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

    pub async fn run(&self) {
        let mut join_set = JoinSet::new();
        let (tx, mut rx) = mpsc::channel::<Event>(1024);

        let keyboard_tx = tx.clone();
        join_set.spawn(async move {
            let getch = getch::Getch::new();
            let kb_tx = keyboard_tx;
            loop {
                if let Ok(key) = getch.getch() {
                    kb_tx.send(Event::Keyboard(key)).await.unwrap();
                }
            }
        });

        #[allow(unused)]
        #[derive(Debug)]
        enum ChanTyp {
            I32(i32),
        }
        let init_channel_map = self
            .channels
            .iter()
            .enumerate()
            .map(|(chan_idx, (_, typ))| {
                (
                    2i32.pow(chan_idx as u32),
                    match typ {
                        Type::TInt => ChanTyp::I32(0),
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

            match channels[&channel] {
                ChanTyp::I32(val) => val,
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

        let shared = instance.get_memory(&mut store, "heap").unwrap();
        let loc = instance.get_memory(&mut store, "location").unwrap();
        let shared_heap_end = instance
            .get_global(&mut store, "next_ptr")
            .expect("Failed to get shared heap end")
            .get(&mut store)
            .i32()
            .unwrap() as usize;

        let location_heap_end = instance
            .get_global(&mut store, "next_location")
            .expect("Failed to get location heap end")
            .get(&mut store)
            .i32()
            .unwrap() as usize;
        //println!("Shared heap  {:?}", &clos.data(&mut store)[..32]);
        //println!("Location heap {:?}", &loc.data(&mut store)[..32]);

        let shared_contents = &shared.data(&mut store)[..shared_heap_end];
        println!(" ");
        println!("Shared heap contents after init");
        let length = shared_contents.len() / 4;
        let mut indices = String::new();
        let mut values = String::new();
        for (idx, chunk) in shared_contents.chunks(4).enumerate() {
            let byte = i32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            if idx == length - 1 {
                values.push_str(&format!(" {:02} ", byte));
            } else {
                values.push_str(&format!(" {:02} |", byte));
            }
            let idx = idx * 4;
            let idx = format!(" {:02}  ", idx);
            indices.push_str(&idx);
        }
        println!("    Index:          {}", indices);
        println!("    Heap contents: [{}]", values);

        let location_contents = &loc.data(&mut store)[..location_heap_end];
        println!(" ");
        println!("Location heap contents after init");
        let mut indices = String::new();
        let mut values = String::new();
        for (idx, chunk) in location_contents.chunks(8).enumerate() {
            let ptr = i32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            let clock = i32::from_le_bytes([chunk[4], chunk[5], chunk[6], chunk[7]]);

            let adjusted = idx * 8;
            if idx == 0 {
                let idx = format!("      {:02}", adjusted);
                indices.push_str(&idx);
            } else {
                let idx = format!("           {:02}", adjusted);
                indices.push_str(&idx);
            }

            values.push_str(&format!(" [ {:02} | {:02} ] ", ptr, clock));
        }
        println!("    Index:         {}", indices);
        println!("    Heap contents: {}", values);

        let location_dispatch = instance
            .get_typed_func::<i32, i32>(&mut store, "location_dispatch")
            .expect("Cannot location dispatch");

        let keyboard_channel = 2i32.pow(
            self.channels
                .iter()
                .position(|(s, _)| s == "keyboard")
                .expect("Keyboard channel not found") as u32,
        );
        while let Some(event) = rx.recv().await {
            let channel = match event {
                Event::Keyboard(key) => {
                    channels
                        .lock()
                        .unwrap()
                        .insert(keyboard_channel, ChanTyp::I32(key as i32));
                    keyboard_channel
                }
            };

            let runtime_state = store.data();
            let output_to_location: HashMap<_, _> = runtime_state
                .output_to_location
                .clone()
                .iter()
                .map(|(&output_index, locations)| {
                    let filtered_locations = locations
                        .iter()
                        .map(|&location_ptr| {
                            if let Ok(clock_value) =
                                self.extract_clock(&mut store, &instance, location_ptr)
                                && clock_value & channel != 0
                            {
                                // eprintln!(
                                //     "Requesting dispatch for {location_ptr} clock {clock_value}"
                                // );
                                let sig = location_dispatch
                                    .call(&mut store, location_ptr)
                                    .expect("Failed to location dispatch");

                                let shared_heap_end = instance
                                    .get_global(&mut store, "next_ptr")
                                    .expect("Failed to get shared heap end")
                                    .get(&mut store)
                                    .i32()
                                    .unwrap()
                                    as usize;

                                let location_heap_end = instance
                                    .get_global(&mut store, "next_location")
                                    .expect("Failed to get location heap end")
                                    .get(&mut store)
                                    .i32()
                                    .unwrap()
                                    as usize;
                                //println!("Shared heap end: {}", shared_heap_end);
                                //println!("Location heap end: {}", location_heap_end);

                                let clos_data = instance
                                    .get_memory(&mut store, "heap")
                                    .unwrap()
                                    .data(&mut store)
                                    .to_vec();
                                let bytes = &clos_data[sig as usize..(sig as usize + 4)];
                                let value = i32::from_le_bytes(bytes.try_into().unwrap());

                                let bytes = &clos_data[(sig as usize + 4)..(sig as usize + 8)];
                                let new_ptr = i32::from_le_bytes(bytes.try_into().unwrap());

                                // eprintln!(
                                //     "Loc {} produced Sig {} :: PTR {}",
                                //     location_ptr, value, new_ptr
                                // );
                                let kind: OutputKind = output_index.into();
                                match kind {
                                    OutputKind::Print => {
                                        eprintln!("{value}");
                                    }
                                    OutputKind::PrintAscii => {
                                        eprintln!("{}", value as u8 as char);
                                    }
                                    OutputKind::Unknown(this) => panic!("what is {this}"),
                                }

                                let shared = instance.get_memory(&mut store, "heap").unwrap();
                                let shared_contents = &shared.data(&mut store)[..shared_heap_end];
                                println!(" ");
                                println!("Shared heap contents after tick");
                                let length = shared_contents.len() / 4;
                                let mut indices = String::new();
                                let mut values = String::new();
                                for (idx, chunk) in shared_contents.chunks(4).enumerate() {
                                    let byte = i32::from_le_bytes([
                                        chunk[0], chunk[1], chunk[2], chunk[3],
                                    ]);
                                    if idx == length - 1 {
                                        values.push_str(&format!(" {:02} ", byte));
                                    } else {
                                        values.push_str(&format!(" {:02} |", byte));
                                    }
                                    let idx = idx * 4;
                                    let idx = format!(" {:02}  ", idx);
                                    indices.push_str(&idx);
                                }
                                println!("    Index:          {}", indices);
                                println!("    Heap contents: [{}]", values);

                                let loc = instance.get_memory(&mut store, "location").unwrap();
                                let location_contents = &loc.data(&mut store)[..location_heap_end];
                                println!(" ");
                                println!("Location heap contents after tick");
                                let mut indices = String::new();
                                let mut values = String::new();
                                for (idx, chunk) in location_contents.chunks(8).enumerate() {
                                    let ptr = i32::from_le_bytes([
                                        chunk[0], chunk[1], chunk[2], chunk[3],
                                    ]);
                                    let clock = i32::from_le_bytes([
                                        chunk[4], chunk[5], chunk[6], chunk[7],
                                    ]);

                                    let adjusted = idx * 8;
                                    if idx == 0 {
                                        let idx = format!("      {:02}", adjusted);
                                        indices.push_str(&idx);
                                    } else {
                                        let idx = format!("           {:02}", adjusted);
                                        indices.push_str(&idx);
                                    }
                                    values.push_str(&format!(" [ {:02} | {:02} ] ", ptr, clock));
                                }
                                println!("    Index:         {}", indices);
                                println!("    Heap contents: {}", values);
                                // println!("closure  {:0>2?}", &clos.data(&mut store)[..128]);
                                // println!("---------------");
                                // println!("location {:0>2?}", &loc.data(&mut store)[..128]);

                                new_ptr
                            } else {
                                location_ptr
                            }
                        })
                        .collect::<Vec<i32>>();
                    (output_index, filtered_locations)
                })
                .collect();

            *store.data_mut() = RuntimeState { output_to_location };
        }
        join_set.join_all().await;
    }

    fn extract_clock(
        &self,
        store: &mut Store<RuntimeState>,
        instance: &Instance,
        location: i32,
    ) -> Result<i32, anyhow::Error> {
        let memory = instance
            .get_memory(&mut *store, "location")
            .ok_or_else(|| anyhow::anyhow!("Failed to get heap memory from WASM instance"))?;

        let memory_data = memory.data(&mut *store);

        let bytes = &memory_data[(location as usize + 4)..(location as usize + 8)];
        let value = i32::from_le_bytes(bytes.try_into().unwrap());

        Ok(value)
    }
}
