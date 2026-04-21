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

function wasmCallback(fun_index, ...args) {
    console.log("callback", fun_index, args);
}

window.handleSubmit = function(event) {
    event.preventDefault();

    const source = document.getElementById("source").value;

    try {
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

        const vm = new WasmVM(serialized_bytecode, wasmCallback);
        // Test out the callback
        vm.call_wasm(89, new Int32Array([5, 2]));

    } catch (e) {
        console.error("compile error:", e.message);
    }
}
