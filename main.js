import init_comp, { compile } from './bindgen-out/compiler/comratt_compiler.js';
import init_vm, { WasmVM } from './bindgen-out/vm/comratt_vm.js';
await init_comp();
await init_vm();

const test_prog = `
fact : int -> int
def fact n =
  if n <= 1
  then 1
  else n * (fact (n - 1));

main : int -> int
def main x = fact x;
`;

window.handleSubmit = async function(event) {
    event.preventDefault();

    var source = document.getElementById("source").value;

    try {
        if(source == "") {
            source = test_prog;
        }

        const program = compile(source);
        const pure_fn_names = program.pure_fn_names();
        const channel_names = program.channel_names();
        const wasm_bytes = program.wasm_bytes();
        const serialized_bytecode = program.serialized_bytecode();
        const output_labels = program.output_labels();
        console.log("WASM functions:", pure_fn_names);

        if(output_labels.length != 0) {
            console.log("Output labels exist, reactive loop")
        } else {
            console.log("Output labels dont exist, pure run")
        }


        // Instantiate the wasm bytes from compiling source
        const { instance, module } = await WebAssembly.instantiate(wasm_bytes);

        // Define callback here to capture the functions exported from wasm
        const callback = (fun_index, ...args) => {
            // instance.exports is an object, so we cannot directly use fun_index in a stable way,
            // hence the "lookup" via pure_fn_names
            const fun_name = pure_fn_names[fun_index];
            const fun_to_call = instance.exports[fun_name];
            if (args.length != fun_to_call.length)
            {
                console.log("Argument count mismatch when calling exported function '%s', got: %d, expected: %d", fun_name, args.length, fun_to_call.length);
                throw new Error("Argument count mismatch");
            }
            return fun_to_call(args);
        };

        const vm = new WasmVM(serialized_bytecode, callback);
        // Test out the callback
        console.log(vm.call_wasm(1, new Int32Array([5])));

    } catch (e) {
        console.error("compile error:", e.message);
    }
}
