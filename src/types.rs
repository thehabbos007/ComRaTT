use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use crate::source::{Binop, ClockExprs, Const, Type};

pub type Sym = String;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypedExpr {
    /// A constant value with (the constant, its type)
    TConst(Const, Type),
    /// A variable name with (the name, its type)
    TName(Sym, Type),
    /// A lambda with (args and their types, body expression, the lambda's type, an optional clock (when it's a delayed computation))
    TLam(Vec<(Sym, Type)>, Box<TypedExpr>, Type, Option<ClockExprs>),
    /// A function application with (function expression, argument expressions, result type)
    TApp(Box<TypedExpr>, Vec<TypedExpr>, Type),
    /// A primitive operation with (the operator, left operand, right operand, result type)
    TPrim(Binop, Box<TypedExpr>, Box<TypedExpr>, Type),
    /// A let binding with (bound name, type, bound expression, body expression)
    TLet(Sym, Type, Box<TypedExpr>, Box<TypedExpr>),
    /// A conditional with (condition, then branch, else branch, result type)
    TIfThenElse(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>, Type),
    /// A tuple with (element expressions, tuple type)
    TTuple(Vec<TypedExpr>, Type),
    /// A tuple access with (tuple expression, index, result type)
    TAccess(Box<TypedExpr>, i32, Type),
    /// Wait on a channel
    TWait(String, Type),
    /// A delayed computation with (body, clock, result type => TLater(inner, clock))
    TDelay(Box<TypedExpr>, ClockExprs, Type),
    /// Advance a delayed value with (name, result type => inner type after unwrapping TLater)
    TAdvance(Sym, Type),
}

impl TypedExpr {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn ty(&self) -> Type {
        match self {
            TypedExpr::TConst(_, ty) => ty.clone(),
            TypedExpr::TName(_, ty) => ty.clone(),
            TypedExpr::TLam(_, _, ty, _) => ty.clone(),
            TypedExpr::TApp(_, _, ty) => ty.clone(),
            TypedExpr::TPrim(_, _, _, ty) => ty.clone(),
            TypedExpr::TLet(_, ty, _, _) => ty.clone(),
            TypedExpr::TIfThenElse(_, _, _, ty) => ty.clone(),
            TypedExpr::TTuple(_, ty) => ty.clone(),
            TypedExpr::TAccess(_, _, ty) => ty.clone(),
            TypedExpr::TWait(_, ty) => ty.clone(),
            TypedExpr::TDelay(_, _, ty) => ty.clone(),
            TypedExpr::TAdvance(_, ty) => ty.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypedToplevel {
    /// A function definition with (name, args and their types, body expression, return type)
    TFunDef(Sym, Vec<(Sym, Type)>, Box<TypedExpr>, Type),
    Channel(String, Type),
    Output(String, Box<TypedExpr>),
}

impl TypedToplevel {
    pub fn get_type(&self) -> Type {
        match self {
            TypedToplevel::TFunDef(_, _, _, typ) => typ.clone(),
            TypedToplevel::Channel(_, typ) => typ.clone(),
            TypedToplevel::Output(_, typed_expr) => typed_expr.ty(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypedProg {
    pub defs: Vec<TypedToplevel>,
    pub sorted_inputs: Vec<(Sym, Type)>,
}

impl Deref for TypedProg {
    type Target = Vec<TypedToplevel>;

    fn deref(&self) -> &Self::Target {
        &self.defs
    }
}

impl From<Vec<TypedToplevel>> for TypedProg {
    fn from(v: Vec<TypedToplevel>) -> Self {
        TypedProg {
            defs: v,
            sorted_inputs: vec![],
        }
    }
}

#[macro_export]
macro_rules! assert_typed_expr {
    ($expr:expr, $pattern:pat => $assertions:block) => {
        match $expr {
            $pattern => $assertions,
            _ => panic!("Expression did not match expected pattern"),
        }
    };
}

pub fn final_type(ty: &Type) -> Type {
    match ty {
        Type::TInt | Type::TBool | Type::TUnit | Type::TVar(_) | Type::TLater(..) => ty.clone(),
        Type::TFun(_, t2) => final_type(t2),
        Type::TProduct(ts) => final_type_tproduct(ts),
        Type::TSig(_) => ty.clone(),
        Type::TBox(_) => ty.clone(),
    }
}

pub fn final_type_tproduct(ts: &[Type]) -> Type {
    match ts {
        [] => panic!("final_type_tproduct: Attempted to take final type of empty tproduct"),
        [t] => t.clone(),
        [_, rest @ ..] => final_type_tproduct(rest),
    }
}

pub fn count_tfun_args(ty: &Type) -> usize {
    match ty {
        Type::TFun(_, t2) => 1 + count_tfun_args(t2),
        _ => 0,
    }
}

fn tfun_len_n_rec(ty: Type, n: usize, acc: &mut Vec<Type>) -> (Type, Vec<Type>) {
    match ty {
        Type::TFun(ty, next_ty) if n > 0 => {
            acc.push(*ty);
            tfun_len_n_rec(*next_ty, n - 1, acc)
        }
        t if n == 0 => (t, acc.to_owned()),
        Type::TBox(ty) | Type::TLater(ty, _) => (*ty, acc.to_owned()),
        Type::TInt | Type::TBool | Type::TUnit => {
            panic!("Attempted to traverse a non-TFun type at n = {}", n)
        }
        ty => panic!("Too many arguments for function, n: {n} got: {ty}"),
    }
}

/// Unfold a type for use in fuctions. This will pop [`n`] elements of a [`Type::TFun`] type and return
/// the remaining type and the popped types.
pub fn tfun_len_n(ty: Type, n: usize) -> (Type, Vec<Type>) {
    let (typ, popped) = tfun_len_n_rec(ty, n, &mut Vec::new());
    debug_assert_eq!(popped.len(), n);
    (typ, popped)
}

/// Build a TFun from a slice of argument types and a return type.
/// types: [TInt, TInt] and ret_ty: TBool will result in
/// TFun(TInt, TFun(TInt, TBool))
pub fn build_function_type(types: &[Type], ret_ty: Type) -> Type {
    match types {
        [] => ret_ty,
        [ty, types @ ..] => Type::TFun(ty.clone().b(), build_function_type(types, ret_ty).b()),
    }
}

pub fn traverse_locals<'a>(expr: &'a TypedExpr, locals: &mut Vec<(&'a str, Type)>) {
    match expr {
        TypedExpr::TLet(name, typ, rhs, body) => {
            locals.push((name.as_str(), typ.clone()));
            traverse_locals(rhs, locals);
            traverse_locals(body, locals);
        }
        TypedExpr::TIfThenElse(condition, then_branch, else_branch, _) => {
            traverse_locals(condition, locals);
            traverse_locals(then_branch, locals);
            traverse_locals(else_branch, locals);
        }
        TypedExpr::TPrim(_, left, right, _) => {
            traverse_locals(left, locals);
            traverse_locals(right, locals);
        }
        TypedExpr::TLam(..) => {
            panic!("get_names_for_forward_declaration: Lambda should have been lifted");
        }
        TypedExpr::TTuple(exprs, _) => {
            for expr in exprs {
                traverse_locals(expr, locals);
            }
        }
        TypedExpr::TAccess(expr, _, _) => {
            traverse_locals(expr, locals);
        }
        TypedExpr::TDelay(body, _, _) => {
            traverse_locals(body, locals);
        }
        TypedExpr::TConst(_, _)
        | TypedExpr::TName(_, _)
        | TypedExpr::TApp(_, _, _)
        | TypedExpr::TWait(_, _)
        | TypedExpr::TAdvance(_, _) => {}
    }
}

pub fn find_free_var_names(expr: &TypedExpr, bound: &HashSet<String>) -> HashSet<String> {
    match expr {
        TypedExpr::TConst(..) => HashSet::new(),
        TypedExpr::TWait(..) => HashSet::new(),
        TypedExpr::TAdvance(name, _) | TypedExpr::TName(name, _) if bound.contains(name) => {
            HashSet::new()
        }
        TypedExpr::TAdvance(name, _) | TypedExpr::TName(name, _) => [name.clone()].into(),
        TypedExpr::TPrim(_, l, r, _) => {
            let mut s = find_free_var_names(l, bound);
            s.extend(find_free_var_names(r, bound));
            s
        }
        TypedExpr::TLet(name, _, rhs, body) => {
            let mut s = find_free_var_names(rhs, bound);
            let mut b2 = bound.clone();
            b2.insert(name.clone());
            s.extend(find_free_var_names(body, &b2));
            s
        }
        TypedExpr::TLam(params, body, _, _) => {
            let mut b2 = bound.clone();
            for (p, _) in params {
                b2.insert(p.clone());
            }
            find_free_var_names(body, &b2)
        }
        TypedExpr::TApp(f, args, _) => {
            let mut s = find_free_var_names(f, bound);
            for a in args {
                s.extend(find_free_var_names(a, bound));
            }
            s
        }
        TypedExpr::TIfThenElse(c, t, e, _) => {
            let mut s = find_free_var_names(c, bound);
            s.extend(find_free_var_names(t, bound));
            s.extend(find_free_var_names(e, bound));
            s
        }
        TypedExpr::TTuple(es, _) => es
            .iter()
            .flat_map(|e| find_free_var_names(e, bound))
            .collect(),
        TypedExpr::TAccess(e, _, _) => find_free_var_names(e, bound),
        TypedExpr::TDelay(body, _, _) => find_free_var_names(body, bound),
    }
}

pub fn find_free_vars(
    expr: &TypedExpr,
    bound: &HashSet<(String, Type)>,
) -> HashSet<(String, Type)> {
    match expr {
        TypedExpr::TConst(_, _) => HashSet::new(),
        TypedExpr::TWait(_, _) => HashSet::new(),
        TypedExpr::TName(name, ty) => {
            let name = name.clone();
            let ty = ty.clone();
            if bound.contains(&(name.clone(), ty.clone())) {
                HashSet::new()
            } else {
                let mut set = HashSet::new();
                set.insert((name, ty));
                set
            }
        }
        TypedExpr::TPrim(_, left, right, _) => {
            let mut left_free = find_free_vars(left, bound);
            let right_free = find_free_vars(right, bound);
            left_free.extend(right_free);
            left_free
        }
        TypedExpr::TLam(args, body, ..) => {
            let mut new_bound = bound.clone();
            for (name, ty) in args {
                let name = name.clone();
                let ty = ty.clone();
                new_bound.insert((name, ty));
            }
            find_free_vars(body, &new_bound)
        }
        TypedExpr::TApp(fn_expr, args, _) => {
            let mut free = find_free_vars(fn_expr, bound);
            for arg in args {
                free.extend(find_free_vars(arg, bound));
            }
            free
        }
        TypedExpr::TLet(name, ty, rhs, body) => {
            let mut free = find_free_vars(rhs, bound);
            let mut new_bound = bound.clone();
            let name = name.clone();
            let ty = ty.clone();
            new_bound.insert((name, ty));
            free.extend(find_free_vars(body, &new_bound));
            free
        }
        TypedExpr::TIfThenElse(condition, then_branch, else_branch, _) => {
            let mut free = find_free_vars(condition, bound);
            free.extend(find_free_vars(then_branch, bound));
            free.extend(find_free_vars(else_branch, bound));
            free
        }
        TypedExpr::TTuple(exprs, _) => {
            let mut free = HashSet::new();
            for expr in exprs {
                free.extend(find_free_vars(expr, bound));
            }
            free
        }
        TypedExpr::TAccess(expr, _, _) => find_free_vars(expr, bound),
        TypedExpr::TDelay(body, _, _) => find_free_vars(body, bound),
        TypedExpr::TAdvance(name, ty) => {
            let name = name.clone();
            let ty = ty.clone();
            if bound.contains(&(name.clone(), ty.clone())) {
                HashSet::new()
            } else {
                let mut set = HashSet::new();
                set.insert((name, ty));
                set
            }
        }
    }
}

pub fn substitute_binding(bind_old: &str, bind_new: &str, expr: TypedExpr) -> TypedExpr {
    match expr {
        TypedExpr::TName(binding, ty) if binding == bind_old => {
            TypedExpr::TName(bind_new.to_string(), ty)
        }
        TypedExpr::TPrim(op, left, right, ty) => TypedExpr::TPrim(
            op,
            Box::new(substitute_binding(bind_old, bind_new, *left)),
            Box::new(substitute_binding(bind_old, bind_new, *right)),
            ty,
        ),
        TypedExpr::TLam(args, body, ty, clock) => TypedExpr::TLam(
            args,
            Box::new(substitute_binding(bind_old, bind_new, *body)),
            ty,
            clock,
        ),
        TypedExpr::TApp(func, args, ty) => TypedExpr::TApp(
            Box::new(substitute_binding(bind_old, bind_new, *func)),
            args.into_iter()
                .map(|arg| substitute_binding(bind_old, bind_new, arg))
                .collect(),
            ty,
        ),
        TypedExpr::TLet(name, ty, rhs, body) if name != bind_old => TypedExpr::TLet(
            name,
            ty,
            Box::new(substitute_binding(bind_old, bind_new, *rhs)),
            Box::new(substitute_binding(bind_old, bind_new, *body)),
        ),
        TypedExpr::TLet(name, ty, rhs, body) => TypedExpr::TLet(
            name,
            ty,
            Box::new(substitute_binding(bind_old, bind_new, *rhs)),
            body,
        ),
        TypedExpr::TIfThenElse(condition, then_branch, else_branch, ty) => TypedExpr::TIfThenElse(
            Box::new(substitute_binding(bind_old, bind_new, *condition)),
            Box::new(substitute_binding(bind_old, bind_new, *then_branch)),
            Box::new(substitute_binding(bind_old, bind_new, *else_branch)),
            ty,
        ),
        TypedExpr::TTuple(texps, ty) => TypedExpr::TTuple(
            texps
                .into_iter()
                .map(|texp| substitute_binding(bind_old, bind_new, texp))
                .collect(),
            ty,
        ),
        TypedExpr::TAccess(texp, idx, ty) => TypedExpr::TAccess(
            Box::new(substitute_binding(bind_old, bind_new, *texp)),
            idx,
            ty,
        ),
        TypedExpr::TDelay(body, clock, ty) => TypedExpr::TDelay(
            Box::new(substitute_binding(bind_old, bind_new, *body)),
            clock,
            ty,
        ),
        TypedExpr::TAdvance(name, ty) if name == bind_old => {
            TypedExpr::TAdvance(bind_new.to_string(), ty)
        }
        TypedExpr::TName(_, _) => expr,
        TypedExpr::TConst(_, _) => expr,
        TypedExpr::TWait(_, _) => expr,
        TypedExpr::TAdvance(_, _) => expr,
    }
}

pub fn unpack_type(ty: &Type) -> Vec<Type> {
    match ty {
        Type::TLater(typ, ..) | Type::TBox(typ) => unpack_type(typ),
        Type::TFun(t1, t2) => {
            let mut types = vec![*t1.clone()];
            types.extend(unpack_type(t2));
            types
        }
        Type::TProduct(ts) => ts.clone(),
        Type::TVar(_) | Type::TSig(_) | Type::TInt | Type::TBool | Type::TUnit => vec![],
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingKind {
    Local,
    Toplevel,
}

pub type BindingContext = HashMap<(String, Type), BindingKind>;

pub fn collect_local_names(expr: &TypedExpr) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    collect_local_names_inner(expr, &mut seen, &mut out);
    out
}

fn collect_local_names_inner(
    expr: &TypedExpr,
    seen: &mut std::collections::HashSet<String>,
    out: &mut Vec<String>,
) {
    match expr {
        TypedExpr::TLet(name, _, rhs, body) => {
            if seen.insert(name.clone()) {
                out.push(name.clone());
            }
            collect_local_names_inner(rhs, seen, out);
            collect_local_names_inner(body, seen, out);
        }
        TypedExpr::TIfThenElse(c, t, e, _) => {
            collect_local_names_inner(c, seen, out);
            collect_local_names_inner(t, seen, out);
            collect_local_names_inner(e, seen, out);
        }
        TypedExpr::TPrim(_, l, r, _) => {
            collect_local_names_inner(l, seen, out);
            collect_local_names_inner(r, seen, out);
        }
        TypedExpr::TApp(f, args, _) => {
            collect_local_names_inner(f, seen, out);
            for a in args {
                collect_local_names_inner(a, seen, out);
            }
        }
        TypedExpr::TTuple(elems, _) => {
            for e in elems {
                collect_local_names_inner(e, seen, out);
            }
        }
        TypedExpr::TAccess(e, _, _) => {
            collect_local_names_inner(e, seen, out);
        }
        _ => {}
    }
}

pub fn max_tuple_depth(expr: &TypedExpr) -> u32 {
    match expr {
        TypedExpr::TTuple(elems, _) => 1 + elems.iter().map(max_tuple_depth).max().unwrap_or(0),
        TypedExpr::TLet(_, _, rhs, body) => max_tuple_depth(rhs).max(max_tuple_depth(body)),
        TypedExpr::TPrim(_, l, r, _) => max_tuple_depth(l).max(max_tuple_depth(r)),
        TypedExpr::TIfThenElse(c, t, e, _) => max_tuple_depth(c)
            .max(max_tuple_depth(t))
            .max(max_tuple_depth(e)),
        TypedExpr::TApp(f, args, _) => args
            .iter()
            .fold(max_tuple_depth(f), |acc, a| acc.max(max_tuple_depth(a))),
        TypedExpr::TAccess(e, _, _) => max_tuple_depth(e),
        _ => 0,
    }
}
