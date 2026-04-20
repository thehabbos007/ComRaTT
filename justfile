wasm:
    just build-wasm "comratt-vm"
    just build-wasm "comratt-compiler"
    just bindgen-vm
    just bindgen-comp

vm:
    just build-vm
    just bindgen-vm

comp:
    just build-comp
    just bindgen-comp

build-wasm CRATE:
 cargo build -p {{CRATE}} --target wasm32-unknown-unknown --release

build-vm:
 just build-wasm "comratt-vm"

build-comp:
 just build-wasm "comratt-compiler"

bindgen-vm:
 wasm-bindgen target/wasm32-unknown-unknown/release/comratt_vm.wasm --out-dir bindgen-out --target web --no-typescript

bindgen-comp:
 wasm-bindgen target/wasm32-unknown-unknown/release/comratt_compiler.wasm --out-dir bindgen-out --target web --no-typescript

serve:
 miniserve . --port 8080
