import init_comp, { compile } from './bindgen-out/comratt_compiler.js';
import init_vm, { vm_entrypoint } from './bindgen-out/comratt_vm.js';
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

        vm_entrypoint(serialized_bytecode, () => { console.log("Hi from js callback")});
    } catch (e) {
        console.error("compile error:", e.message);
    }
}
