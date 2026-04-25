import init_comp, { compile } from './bindgen-out/compiler/comratt_compiler.js';
import init_vm, { WasmVM } from './bindgen-out/vm/comratt_vm.js';
await init_comp();
await init_vm();

const log = document.getElementById('log');
const inputs = document.getElementById('inputs');
const append = (log, line) => { log.textContent += line + '\n'; };

document.getElementById('run').addEventListener('submit', async (event) => {
    event.preventDefault();
    log.textContent = '';
    inputs.replaceChildren();

    const source = document.getElementById('source').value;

    let program;
    try {
        program = compile(source);
    } catch (e) {
        append(log, 'compile error: ' + e.message);
        return;
    }
    const pure_fn_names = program.pure_fn_names();
    const channel_names = program.channel_names();
    const output_labels = program.output_labels();
    const init_indices = program.output_init_indices();
    const main_bc = program.main_bytecode_idx();

    // Instantiate the wasm bytes from compiling source
    const { instance } = await WebAssembly.instantiate(program.wasm_bytes());
    // Define callback here to capture the functions exported from wasm
    const callback = (fun_index, ...args) =>
        // instance.exports is an object, so we cannot directly use fun_index in a stable way,
        // hence the "lookup" via pure_fn_names
        instance.exports[pure_fn_names[fun_index]](...args);
    const vm = new WasmVM(program.serialized_bytecode(), callback);

    if (output_labels.length === 0) {
        const raw = document.getElementById('main_args').value.trim();
        const args = raw === '' ? [] : raw.split(/\s+/).map(s => parseInt(s, 10));
        if (args.some(Number.isNaN)) {
            append(log, 'main args must be integers');
            return;
        }
        const result = main_bc !== undefined
            ? vm.run_bytecode_main(main_bc, new Int32Array(args))
            : instance.exports.main(...args);
        append(log, 'Result: ' + result);
        return;
    }

    vm.init_channels(channel_names.length);
    for (const idx of init_indices) vm.init_output(idx);

    inputs.onclick = (event) => {
        const button = event.target.closest('button[data-chan-idx]');
        if (!button) return;
        const idx = parseInt(button.dataset.chanIdx, 10);
        const v = parseInt(button.previousElementSibling.value, 10);
        if (Number.isNaN(v)) return;
        for (const r of vm.step(idx, v)) {
            append(log, `[${r[0]}] ${output_labels[r[0]]}: ${r[1]}`);
        }
    };

    channel_names.forEach((name, idx) => {
        const row = document.createElement('div');
        const input = document.createElement('input');
        input.type = 'number';
        input.placeholder = `Chan: ${name} [${idx}]`;
        const button = document.createElement('button');
        button.type = 'button';
        button.textContent = 'submit';
        button.dataset.chanIdx = idx;
        row.append(input, button);
        inputs.append(row);
    });
});
