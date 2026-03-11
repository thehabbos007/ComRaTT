use std::collections::{HashMap, HashSet};

use comratt_vm::bytecode;

use crate::{
    source::Type,
    types::{Sym, TypedExpr, TypedProg, TypedToplevel},
};

pub mod bytecode_gen;
pub mod wasm_gen;

#[derive(Clone, Copy, Debug)]
pub enum FunRef {
    Wasm(u32),
    Bytecode(u32),
}

pub type FunctionPrototype = (String, Vec<(Sym, Type)>, TypedExpr);

pub struct HybridProgram {
    pub wasm_bytes: Vec<u8>,
    pub bytecode_fns: Vec<bytecode::Function>,
    pub fn_map: HashMap<String, FunRef>,
    pub pure_fn_names: Vec<String>,
    pub channels: Vec<(String, Type)>,
    pub channel_indices: HashMap<String, u16>,
    pub outputs: Vec<(String, TypedExpr)>,
}

pub fn compile(prog: &TypedProg) -> HybridProgram {
    let mut pure_fns: Vec<FunctionPrototype> = vec![];
    let mut reactive_fns: Vec<FunctionPrototype> = vec![];
    let mut fn_map: HashMap<String, FunRef> = HashMap::new();

    let mut channels: Vec<(String, Type)> = vec![];
    let mut channel_indices: HashMap<String, u16> = HashMap::new();
    let mut outputs: Vec<(String, TypedExpr)> = vec![];

    for def in &prog.defs {
        match def {
            TypedToplevel::Channel(name, ty) => {
                let idx = channels.len() as u16;
                channel_indices.insert(name.clone(), idx);
                channels.push((name.clone(), ty.clone()));
            }
            TypedToplevel::Output(name, expr) => {
                outputs.push((name.clone(), (**expr).clone()));
            }
            _ => {}
        }
    }

    let mut reactive_names: HashSet<String> = HashSet::new();
    let all_fns: Vec<_> = prog
        .defs
        .iter()
        .filter_map(|def| {
            if let TypedToplevel::TFunDef(name, args, body, _ty) = def {
                Some((name.clone(), args.clone(), (**body).clone()))
            } else {
                None
            }
        })
        .collect();

    for (name, _, body) in &all_fns {
        if is_reactive(body) {
            reactive_names.insert(name.clone());
        }
    }

    for (name, args, body) in all_fns {
        if reactive_names.contains(&name) {
            reactive_fns.push((name, args, body));
        } else {
            let fun_ref = FunRef::Wasm(pure_fns.len() as u32);
            fn_map.insert(name.clone(), fun_ref);
            pure_fns.push((name, args, body));
        }
    }

    let pure_fn_names: Vec<String> = pure_fns.iter().map(|(n, ..)| n.clone()).collect();

    let wasm_bytes = if pure_fns.is_empty() {
        wat::parse_str("(module)").unwrap()
    } else {
        wasm_gen::compile_pure(&pure_fns)
    };

    // Initial output evaluation
    for (i, (_name, expr)) in outputs.iter().enumerate() {
        let init_name = format!("#output_init_{i}");
        reactive_fns.push((init_name, vec![], expr.clone()));
    }

    let bytecode_fns = bytecode_gen::compile_reactive(&reactive_fns, &mut fn_map);

    HybridProgram {
        wasm_bytes,
        bytecode_fns,
        fn_map,
        pure_fn_names,
        channels,
        channel_indices,
        outputs,
    }
}

fn is_reactive(expr: &TypedExpr) -> bool {
    match expr {
        TypedExpr::TWait(..) => true,
        TypedExpr::TLam(params, _, ty, Some(_))
            if params.is_empty() && matches!(ty, Type::TLater(..)) =>
        {
            true
        }
        TypedExpr::TLam(_, body, _, _) => is_reactive(body),
        TypedExpr::TApp(f, args, _) => {
            if args.is_empty() {
                if let TypedExpr::TName(_, ty) = f.as_ref() {
                    if matches!(ty, Type::TLater(..)) {
                        return true;
                    }
                }
            }
            is_reactive(f) || args.iter().any(is_reactive)
        }
        TypedExpr::TPrim(_, l, r, _) => is_reactive(l) || is_reactive(r),
        TypedExpr::TLet(_, _, rhs, body) => is_reactive(rhs) || is_reactive(body),
        TypedExpr::TIfThenElse(c, t, e, _) => is_reactive(c) || is_reactive(t) || is_reactive(e),
        TypedExpr::TTuple(es, _) => es.iter().any(is_reactive),
        TypedExpr::TAccess(e, _, _) => is_reactive(e),
        _ => false,
    }
}
