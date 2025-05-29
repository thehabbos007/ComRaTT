#![allow(unused)]

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    ops::Deref,
};

use ena::unify::InPlaceUnificationTable;

use crate::{
    source::{Binop, ClockExpr, ClockExprs, Const, Expr, Prog, Toplevel, Type, TypeVar},
    types::*,
};

#[derive(Debug, Clone)]
enum Constraint {
    TypeEqual(Type, Type),
}

#[derive(Debug, Clone)]
struct TypeOutput {
    constraints: Vec<Constraint>,
    texp: TypedExpr,
}
impl TypeOutput {
    fn new(constraints: Vec<Constraint>, texp: TypedExpr) -> Self {
        Self { constraints, texp }
    }
}

type Binding = (Type, Option<ClockExprs>);

#[derive(Debug, Clone, Default)]
struct Context {
    bindings_context: BindingContext,
    pub channels: Vec<(Sym, Type)>,
}

impl Context {
    fn insert_channel(&mut self, binding_name: Sym, value: Type) -> usize {
        let idx = self.channels.len();
        self.channels.push((binding_name, value));
        idx
    }

    fn get_channel(&mut self, binding_name: Sym) -> Option<&Type> {
        self.channels.iter().find_map(|(name, ty)| {
            if name == &binding_name {
                Some(ty)
            } else {
                None
            }
        })
    }

    fn insert_binding(&mut self, binding_name: Sym, value: Type) -> Option<Binding> {
        self.bindings_context.insert(binding_name, value)
    }

    fn get_binding(&self, binding_name: &String) -> Option<&Binding> {
        self.bindings_context.get(binding_name)
    }

    fn promote_tick(self, clock: &ClockExprs) -> Self {
        Self {
            bindings_context: self.bindings_context.promote_tick(clock),
            channels: self.channels.clone(),
        }
    }

    fn attempt_advance(&self, key: &Sym) -> Type {
        self.bindings_context.attempt_advance(key)
    }

    fn insert_clock(
        &mut self,
        binding_name: Sym,
        value: Type,
        clock: ClockExprs,
    ) -> Option<Binding> {
        self.bindings_context
            .insert_clock(binding_name, value, clock)
    }
}

impl From<BindingContext> for Context {
    fn from(value: BindingContext) -> Self {
        Context {
            bindings_context: value,
            channels: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum BindingContext {
    //// Bindings
    Bindings(HashMap<Sym, Binding>),
    // Bindings, the clock for the tick and the bindings after the tick
    Tick(HashMap<Sym, Binding>, ClockExprs, HashMap<Sym, Binding>),
}

impl Default for BindingContext {
    fn default() -> Self {
        BindingContext::Bindings(HashMap::new())
    }
}

impl BindingContext {
    fn get(&self, key: &Sym) -> Option<&Binding> {
        match self {
            BindingContext::Bindings(bindings) => bindings.get(key),
            BindingContext::Tick(bindings, _, tick_bindings) => {
                tick_bindings.get(key).or_else(|| bindings.get(key))
            }
        }
    }

    fn attempt_advance(&self, key: &Sym) -> Type {
        match self {
            BindingContext::Bindings(_) => panic!(
                "Tried to advance name {} without a tick in the context",
                key
            ),
            BindingContext::Tick(bindings, tick_clock, _) => {
                let Some((ty, _)) = bindings.get(key) else {
                    panic!("Tried to advance nonexisting name {}", key);
                };
                ty.clone()
            }
        }
    }

    fn insert(&mut self, binding_name: Sym, value: Type) -> Option<Binding> {
        match self {
            BindingContext::Bindings(bindings) => bindings.insert(binding_name, (value, None)),
            BindingContext::Tick(_, _, bindings) => bindings.insert(binding_name, (value, None)),
        }
    }

    fn insert_clock(
        &mut self,
        binding_name: Sym,
        value: Type,
        clock: ClockExprs,
    ) -> Option<Binding> {
        match self {
            BindingContext::Bindings(bindings) => {
                bindings.insert(binding_name, (value, Some(clock)))
            }
            BindingContext::Tick(_, _, bindings) => {
                bindings.insert(binding_name, (value, Some(clock)))
            }
        }
    }

    fn binding_clock(&self, binding_name: Sym) -> Option<ClockExprs> {
        match self {
            BindingContext::Bindings(bindings) => bindings
                .get(&binding_name)
                .and_then(|(_, clock)| clock.clone()),
            BindingContext::Tick(bindings, clock, _) => bindings
                .get(&binding_name)
                .and_then(|(_, clock)| clock.clone()),
        }
    }

    fn is_under_tick(&self, clock: &HashSet<Sym>) -> Result<(), TypeError> {
        match self {
            BindingContext::Tick(_, tick, _) => Ok(()),
            _ => Err(TypeError::NotUnderTick),
        }
    }

    fn promote_tick(self, clock: &ClockExprs) -> Self {
        match self {
            BindingContext::Bindings(bindings) => {
                BindingContext::Tick(bindings, clock.clone(), HashMap::new())
            }
            BindingContext::Tick(_, tick, _) => {
                panic!("Already under a tick with clock [{:?}]", tick)
            }
        }
    }
}

impl From<HashMap<Sym, Binding>> for BindingContext {
    fn from(value: HashMap<Sym, Binding>) -> Self {
        BindingContext::Bindings(value)
    }
}

struct Inference {
    unification_table: InPlaceUnificationTable<TypeVar>,
}

impl Inference {
    fn fresh_ty_var(&mut self) -> TypeVar {
        self.unification_table.new_key(None)
    }

    fn infer(&mut self, mut context: Context, expr: Expr) -> (Type, TypeOutput) {
        match expr {
            Expr::Const(c) => match c {
                Const::CInt(_) => (
                    Type::TInt,
                    TypeOutput::new(vec![], TypedExpr::TConst(c, Type::TInt)),
                ),
                Const::CBool(_) => (
                    Type::TBool,
                    TypeOutput::new(vec![], TypedExpr::TConst(c, Type::TBool)),
                ),
                Const::CUnit => (
                    Type::TUnit,
                    TypeOutput::new(vec![], TypedExpr::TConst(c, Type::TUnit)),
                ),
                Const::CLaterUnit => {
                    panic!("CLaterUnit literals are not suppported")
                    /*
                    Type::TLaterUnit),
                    TypeOutput::new(vec![], TypedExpr::TConst(c, Type::TLaterUnit)),
                    */
                }
                Const::Never => {
                    let tvar = Type::TVar(self.fresh_ty_var());
                    (
                        Type::TLater(Type::TSig(tvar.clone().b()).b(), BTreeSet::new()),
                        TypeOutput::new(
                            vec![],
                            TypedExpr::TConst(
                                c,
                                Type::TLater(Type::TSig(tvar.b()).b(), BTreeSet::new()),
                            ),
                        ),
                    )
                }
            },
            Expr::Var(name) => {
                if let Some((ty, _)) = context.get_binding(&name) {
                    (
                        ty.clone(),
                        TypeOutput::new(vec![], TypedExpr::TName(name, ty.clone())),
                    )
                } else {
                    panic!("Type checking unbound variable {}", name)
                }
            }
            Expr::Access(expr, idx) => match self.infer(context, *expr) {
                (Type::TProduct(types), type_output) if (idx as usize) < types.len() => {
                    let typ = types[idx as usize].clone();
                    // Propagate the constraints and create a typed access expression
                    (
                        typ.clone(),
                        TypeOutput::new(
                            type_output.constraints,
                            TypedExpr::TAccess(type_output.texp.b(), idx, typ),
                        ),
                    )
                }
                // A sig is a tuple of 2 elements, so the idx must be 0 or 1
                (Type::TSig(t), type_output) if (idx as usize) < 2 => {
                    let typ = if idx == 0 {
                        *t
                    } else {
                        Type::TLater(Type::TSig(t).b(), ClockExpr::Symbolic.into())
                    };
                    (
                        typ.clone(),
                        TypeOutput::new(
                            type_output.constraints,
                            TypedExpr::TAccess(type_output.texp.b(), idx, typ),
                        ),
                    )
                }
                // Is panic the right thing to do? The alternative is to create an insatisfiable constraint maybe?
                (ty, _) => {
                    panic!("Type checking access with out-of-bounds index or invalid expr: {ty}")
                }
            },
            Expr::Tuple(ts) => {
                if ts.len() < 2 {
                    // Is panic the right thing to do? The alternative is to create an insatisfiable constraint maybe?
                    panic!("Type checking tuple with less than 2 elements")
                } else {
                    let inferred = ts.into_iter().map(|t| self.infer(context.clone(), t));
                    let (types, type_outputs): (Vec<Type>, Vec<TypeOutput>) = inferred.unzip();
                    let (constraints, tyexps): (Vec<Vec<Constraint>>, Vec<TypedExpr>) =
                        type_outputs
                            .into_iter()
                            .map(|type_output| (type_output.constraints, type_output.texp))
                            .unzip();

                    let tproduct = Type::TProduct(types);
                    (
                        tproduct.clone(),
                        TypeOutput::new(
                            constraints.into_iter().flatten().collect(),
                            TypedExpr::TTuple(tyexps, tproduct),
                        ),
                    )
                }
            }
            Expr::Sig(val, later) => {
                let (val_ty, mut val_output) = self.infer(context.clone(), *val);
                let (later_ty, mut later_output) = self.infer(context.clone(), *later);
                let mut constraints = Vec::new();
                constraints.append(&mut val_output.constraints);
                constraints.append(&mut later_output.constraints);
                constraints.push(Constraint::TypeEqual(
                    later_ty.clone(),
                    Type::TLater(
                        Type::TSig(val_ty.clone().b()).b(),
                        ClockExpr::Symbolic.into(),
                    ),
                ));

                (
                    Type::TSig(val_ty.clone().b()),
                    TypeOutput::new(
                        constraints,
                        TypedExpr::TTuple(
                            vec![val_output.texp, later_output.texp],
                            Type::TSig(val_ty.b()),
                        ),
                    ),
                )
            }

            Expr::IfThenElse(condition, then, elseb) => match (
                self.check(context.clone(), condition, Type::TBool),
                self.infer(context.clone(), *then),
                self.infer(context, *elseb),
            ) {
                (mut cond_output, (then_ty, mut then_output), (else_ty, mut else_output)) => {
                    let mut constraints = Vec::new();
                    constraints.append(&mut cond_output.constraints);
                    constraints.append(&mut then_output.constraints);
                    constraints.append(&mut else_output.constraints);
                    constraints.append(&mut vec![Constraint::TypeEqual(then_ty.clone(), else_ty)]);
                    (
                        then_ty.clone(),
                        TypeOutput::new(
                            constraints,
                            TypedExpr::TIfThenElse(
                                cond_output.texp.b(),
                                then_output.texp.b(),
                                else_output.texp.b(),
                                then_ty,
                            ),
                        ),
                    )
                }
                _ => panic!("Failed to infer type of IfThenElse"),
            },
            Expr::Delay(e, clock) => {
                // Introduce a tick given the clock
                let context = context.promote_tick(&clock);
                // Call recursively, propagate constraints and generate a new '{} -> ty'
                let (ty, type_output) = self.infer(context, *e);
                (
                    Type::TLater(ty.clone().b(), clock.clone()),
                    TypeOutput::new(
                        type_output.constraints,
                        TypedExpr::TLam(
                            Vec::new(),
                            type_output.texp.b(),
                            Type::TLater(ty.clone().b(), clock.clone()),
                            Some(clock),
                        ),
                    ),
                )
            }
            Expr::Wait(name) => {
                let Some(ty) = context.get_channel(name.clone()) else {
                    panic!("Type checking wait on unbound channel {}", name)
                };
                let clock: ClockExprs = ClockExpr::Wait(name.clone()).into();
                (
                    Type::TLater(ty.clone().b(), clock.clone()),
                    TypeOutput::new(
                        Vec::new(),
                        TypedExpr::TLam(
                            Vec::new(),
                            TypedExpr::TWait(name.clone(), ty.clone()).b(),
                            Type::TLater(ty.clone().b(), clock.clone()),
                            Some(clock),
                        ),
                    ),
                )
            }
            // advance must look only in the left bindings i.e. before the tick (going back in time)
            // We check that the name being advanced is a "later thunk" i.e. TLaterUnit -> T.
            // When we introduce a "cl()" (clock of) construct, we will need to check statically
            // that delay is done with regard to the clock of whatever is being advanced.
            // The input channels of this clock can change at runtime, but at comptime we need
            // to do some checking.
            Expr::Advance(name) => match context.attempt_advance(&name) {
                Type::TLater(box ty, clock) => {
                    let later_type = Type::TLater(ty.clone().b(), clock.clone());
                    (
                        ty.clone(),
                        TypeOutput::new(
                            Vec::new(),
                            TypedExpr::TApp(
                                TypedExpr::TName(name, later_type).b(),
                                Vec::new(),
                                ty.clone(),
                            ),
                        ),
                    )
                }
                expr => panic!("Cannot advance arbitrary expr {expr}"),
            },
            // TODO we must not allow functions under a tick
            // also, if there is a tick we need to insert to the right of the tick
            // also, if the binding rhs is a delayed computation, we need to also
            // insert the clock in the context to be able to advance it.
            Expr::Let(name, box Expr::Delay(delayed, clock), body) => {
                let rhs = Expr::Delay(delayed, clock.clone());
                let (rhs_type, mut rhs_output) = self.infer(context.clone(), rhs);
                let _ = context.insert_clock(name.clone(), rhs_type, clock);
                let (body_type, mut body_output) = self.infer(context, *body);
                let mut constraints = Vec::new();
                constraints.append(&mut rhs_output.constraints);
                constraints.append(&mut body_output.constraints);
                (
                    body_type.clone(),
                    TypeOutput::new(
                        constraints,
                        TypedExpr::TLet(name, body_type, rhs_output.texp.b(), body_output.texp.b()),
                    ),
                )
            }
            Expr::Let(name, box rhs, body) => {
                let (rhs_type, mut rhs_output) = self.infer(context.clone(), rhs);
                let _ = context.insert_binding(name.clone(), rhs_type);
                let (body_type, mut body_output) = self.infer(context, *body);
                let mut constraints = Vec::new();
                constraints.append(&mut rhs_output.constraints);
                constraints.append(&mut body_output.constraints);
                (
                    body_type.clone(),
                    TypeOutput::new(
                        constraints,
                        TypedExpr::TLet(name, body_type, rhs_output.texp.b(), body_output.texp.b()),
                    ),
                )
            }
            Expr::App(fun, args) => match self.infer(context.clone(), *fun) {
                (fun_ty, mut fun_output) => {
                    let mut current_fun_ty = fun_ty.clone();
                    let mut constraints = fun_output.constraints;
                    let mut typed_args = Vec::new();

                    for arg in args {
                        let (arg_ty, arg_output) = self.infer(context.clone(), arg);
                        typed_args.push(arg_output.texp);

                        constraints.extend(arg_output.constraints);

                        let result_ty = Type::TVar(self.fresh_ty_var());

                        constraints.push(Constraint::TypeEqual(
                            current_fun_ty,
                            Type::TFun(arg_ty.b(), result_ty.clone().b()),
                        ));

                        current_fun_ty = result_ty;
                    }

                    // Final result type after applying all arguments
                    let ret_ty = current_fun_ty;

                    (
                        ret_ty.clone(),
                        TypeOutput::new(
                            constraints,
                            TypedExpr::TApp(fun_output.texp.b(), typed_args, ret_ty),
                        ),
                    )
                }
                (ty, _) => panic!(
                    "infer app: Type of function being applied was not TFun but {:?}",
                    ty
                ),
            },
            Expr::Prim(op, left, right) => match op {
                Binop::Add | Binop::Mul | Binop::Div | Binop::Sub => {
                    match (self.infer(context.clone(), *left), self.infer(context, *right)) {
                        ((left_ty, mut left_output), (right_ty, mut right_output)) => {
                            let Ok(_) = self.unify_ty_ty(&left_ty, &Type::TInt) else {
                                panic!("Failed to unify operand of primitive arithmetic operation with int type")

                            };
                            let Ok(_) = self.unify_ty_ty(&left_ty, &right_ty) else {
                                panic!("Failed to unify operands of primitive operations")

                            };
                            let mut constraints = Vec::new();
                            constraints.append(&mut left_output.constraints);
                            constraints.append(&mut right_output.constraints);
                            (
                                Type::TInt,
                                TypeOutput::new(
                                    constraints,
                                    TypedExpr::TPrim(
                                        op,
                                        left_output.texp.b(),
                                        right_output.texp.b(),
                                        Type::TInt,
                                    ),
                                ),
                            )
                        },
                        _ => panic!(
                            "Failed to infer type of primitive expression. Use of operator {:?} is only allowed on either two int or two bool operands",
                            op
                        ),
                    }
                }
                Binop::Lt | Binop::Lte | Binop::Gt | Binop::Gte => {
                    match (self.infer(context.clone(), *left), self.infer(context, *right)) {
                        ((left_ty, mut left_output), (right_ty, mut right_output)) => {
                            let Ok(_) = self.unify_ty_ty(&left_ty, &Type::TInt) else {
                                panic!("Failed to unify operand of primitive comparison operation with bool type")

                            };
                            let Ok(_) = self.unify_ty_ty(&left_ty, &right_ty) else {
                                panic!("Failed to unify operands of primitive operations")

                            };
                            let mut constraints = Vec::new();
                            constraints.append(&mut left_output.constraints);
                            constraints.append(&mut right_output.constraints);
                            (
                                Type::TBool,
                                TypeOutput::new(
                                    constraints,
                                    TypedExpr::TPrim(
                                        op,
                                        left_output.texp.b(),
                                        right_output.texp.b(),
                                        Type::TBool,
                                    ),
                                ),
                            )
                        }
                        _ => panic!(
                            "Failed to infer type of primitive expression. Use of operator {:?} is only allowed on either two int or two bool operands",
                            op
                        ),
                    }
                }

                Binop::Eq | Binop::Neq => {
                    match (self.infer(context.clone(), *left), self.infer(context, *right)) {
                        ((left_ty, mut left_output), (right_ty, mut right_output)) => {
                            let Ok(_) = self.unify_ty_ty(&left_ty, &right_ty) else {
                                panic!("Failed to unify operands of primitive operations")
                            };

                            let mut constraints = Vec::new();
                            constraints.append(&mut left_output.constraints);
                            constraints.append(&mut right_output.constraints);
                            (
                                Type::TBool,
                                TypeOutput::new(
                                    constraints,
                                    TypedExpr::TPrim(
                                        op,
                                        left_output.texp.b(),
                                        right_output.texp.b(),
                                        Type::TBool,
                                    ),
                                ),
                            )
                        },
                        _ => panic!(
                            "Failed to infer type of primitive expression. Use of operator {:?} is only allowed on either two int or two bool operands",
                            op
                        ),
                    }
                }
            },
            Expr::Lam(args, body) => {
                // Generate type variables for each argument
                let mut args_ty_vars = Vec::new();
                for _ in args.clone() {
                    args_ty_vars.push(Type::TVar(self.fresh_ty_var()));
                }
                let args_with_types = args.into_iter().zip(args_ty_vars.clone());
                // Add them to the context
                for (arg, ty_var) in args_with_types.clone() {
                    context.insert_binding(arg, ty_var);
                }
                let (body_type, body_output) = self.infer(context, *body);
                let lambda_type = build_function_type(args_ty_vars.as_slice(), body_type);
                let lambda = TypedExpr::TLam(
                    args_with_types.collect(),
                    body_output.texp.b(),
                    lambda_type.clone(),
                    None,
                );
                (
                    lambda_type,
                    TypeOutput::new(body_output.constraints, lambda),
                )
            }
            Expr::Box(e) => {
                // box e = () -> e
                // allocate as a regular closure
                // Do we need any constraints on what can be boxed? Probably not.
                let (ty, type_output) = self.infer(context, *e);
                let box_ty = Type::TBox(ty.b());
                (
                    box_ty.clone(),
                    TypeOutput::new(
                        type_output.constraints,
                        TypedExpr::TLam(Vec::new(), type_output.texp.b(), box_ty, None),
                    ),
                )
            }
            Expr::Unbox(e) => {
                // unbox e = e ()
                // e should type check to a unit lambda
                // Do we need further or other constraints on what can be unboxed?
                let (ty, type_output) = self.infer(context, *e);
                match ty {
                    Type::TBox(box ty) => (
                        ty.clone(),
                        TypeOutput::new(
                            type_output.constraints,
                            TypedExpr::TApp(type_output.texp.b(), Vec::new(), ty),
                        ),
                    ),
                    _ => panic!("Tried to unbox something not boxed {ty}"),
                }
            }
        }
    }

    fn check(&mut self, mut context: Context, expr: Box<Expr>, ty: Type) -> TypeOutput {
        match (expr.clone(), ty.clone()) {
            (box Expr::Lam(mut args, body), Type::TFun(box from, box to)) => {
                // In practice lambdas only have 1 argument, so just take that single one
                let arg = args.remove(0);
                context.insert_binding(arg.to_owned(), from.clone());
                let body_output = self.check(context, body, to.clone());
                TypeOutput::new(
                    body_output.constraints,
                    TypedExpr::TLam(vec![(arg, from)], body_output.texp.b(), to, None),
                )
            }

            (expr, expected_ty) => {
                let (actual_ty, mut output) = self.infer(context, *expr);
                output
                    .constraints
                    .push(Constraint::TypeEqual(expected_ty, actual_ty));
                output
            }
        }
    }

    fn normalize_ty(&mut self, ty: &Type) -> Type {
        match ty {
            Type::TInt => Type::TInt,
            Type::TBool => Type::TBool,
            Type::TUnit => Type::TUnit,
            Type::TLater(ty, clock) => {
                let ty = self.normalize_ty(ty);
                Type::TLater(ty.b(), clock.clone())
            }
            Type::TSig(ty) => {
                let ty = self.normalize_ty(ty);
                Type::TSig(ty.b())
            }
            Type::TFun(from, to) => {
                let from = self.normalize_ty(from);
                let to = self.normalize_ty(to);
                Type::TFun(from.b(), to.b())
            }
            Type::TProduct(ts) => {
                let normalized = ts.iter().map(|t| self.normalize_ty(t));
                Type::TProduct(normalized.collect())
            }
            Type::TVar(v) => match self.unification_table.probe_value(*v) {
                Some(ty) => self.normalize_ty(&ty),
                None => Type::TVar(self.unification_table.find(*v)),
            },
            Type::TBox(t) => {
                let t = self.normalize_ty(t);
                Type::TBox(t.b())
            }
        }
    }

    fn unify_ty_ty(&mut self, unnorm_left: &Type, unnorm_right: &Type) -> Result<(), TypeError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);
        match (left, right) {
            (Type::TInt, Type::TInt) => Ok(()),
            (Type::TBool, Type::TBool) => Ok(()),
            (Type::TUnit, Type::TUnit) => Ok(()),
            // We most certainly don't want to compare clocks here
            // as e.g. two Sig Int should be equal at the type level
            // even though they're not delayed on the same clock.
            (Type::TLater(t1, _), Type::TLater(t2, _)) => self.unify_ty_ty(&t1, &t2),
            (Type::TSig(t1), Type::TSig(t2)) => self.unify_ty_ty(&t1, &t2),
            (Type::TFun(fst_from, fst_to), Type::TFun(snd_from, snd_to)) => {
                self.unify_ty_ty(fst_from.as_ref(), snd_from.as_ref())?;
                self.unify_ty_ty(fst_to.as_ref(), snd_to.as_ref())
            }
            (Type::TProduct(fst_ts), Type::TProduct(snd_ts)) => {
                let zipped = fst_ts.into_iter().zip(snd_ts);
                zipped.for_each(|(fst, snd)| {
                    self.unify_ty_ty(&fst, &snd);
                });
                Ok(())
            }
            (Type::TVar(a), Type::TVar(b)) => self
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(l, r)| TypeError::TypeNotEqual(l, r)),
            (Type::TVar(v), ty) | (ty, Type::TVar(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| TypeError::InfiniteType(v, ty))?;
                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeError::TypeNotEqual(l, r))
            }
            (Type::TBox(t1), Type::TBox(t2)) => self.unify_ty_ty(&t1, &t2),
            (left, right) => Err(TypeError::TypeNotEqual(left, right)),
        }
    }

    fn unification(&mut self, constraints: &[Constraint]) -> Result<(), TypeError> {
        for constraint in constraints {
            match constraint {
                Constraint::TypeEqual(left, right) => self.unify_ty_ty(left, right)?,
            }
        }
        Ok(())
    }

    fn substitute(&mut self, ty: Type) -> (BTreeSet<TypeVar>, Type) {
        match ty {
            Type::TInt => (BTreeSet::new(), Type::TInt),
            Type::TBool => (BTreeSet::new(), Type::TBool),
            Type::TUnit => (BTreeSet::new(), Type::TUnit),
            Type::TLater(t, clock) => {
                let (mut t_unbound, t) = self.substitute(*t);
                (t_unbound, Type::TLater(t.b(), clock))
            }
            Type::TBox(t) => {
                let (mut t_unbound, t) = self.substitute(*t);
                (t_unbound, Type::TBox(t.b()))
            }
            Type::TSig(t) => {
                let (mut t_unbound, t) = self.substitute(*t);
                (t_unbound, Type::TSig(t.b()))
            }
            Type::TFun(from, to) => {
                let (mut from_unbound, from) = self.substitute(*from);
                let (mut to_unbound, to) = self.substitute(*to);
                from_unbound.extend(to_unbound);
                (from_unbound, Type::TFun(from.b(), to.b()))
            }
            Type::TProduct(ts) => {
                let mut unbounds = BTreeSet::new();
                for t in ts.clone() {
                    let (unbound, t) = self.substitute(t);
                    unbounds.extend(unbound);
                }
                (unbounds, Type::TProduct(ts))
            }
            Type::TVar(v) => {
                let root = self.unification_table.find(v);
                match self.unification_table.probe_value(root) {
                    Some(ty) => self.substitute(ty),
                    None => {
                        let mut unbound = BTreeSet::new();
                        unbound.insert(root);
                        (unbound, Type::TVar(root))
                    }
                }
            }
        }
    }

    fn substitute_texp(&mut self, texp: TypedExpr) -> (BTreeSet<TypeVar>, TypedExpr) {
        match texp {
            TypedExpr::TConst(c, ty) => (BTreeSet::new(), TypedExpr::TConst(c, ty)),
            TypedExpr::TWait(name, ty) => {
                let (unbound, ty) = self.substitute(ty);
                (unbound, TypedExpr::TWait(name, ty))
            }
            TypedExpr::TName(name, ty) => {
                let (unbound, ty) = self.substitute(ty);
                (unbound, TypedExpr::TName(name, ty))
            }
            TypedExpr::TLam(args, body, ty, clock) => {
                let mut unbounds = BTreeSet::new();
                let mut new_args = Vec::new();
                for arg in args.clone() {
                    let (unbound, ty) = self.substitute(arg.1);
                    new_args.push((arg.0, ty));
                    unbounds.extend(unbound);
                }
                let (unbound, body) = self.substitute_texp(*body.clone());
                unbounds.extend(unbound);
                let (unbound, ty) = self.substitute(ty);
                unbounds.extend(unbound);
                (unbounds, TypedExpr::TLam(new_args, body.b(), ty, clock))
            }
            TypedExpr::TApp(fun, args, ty) => {
                let mut unbounds = BTreeSet::new();
                let (unbound_fun, fun) = self.substitute_texp(*fun);
                unbounds.extend(unbound_fun);
                let mut new_args = Vec::new();
                for arg in args.clone() {
                    let (unbound, arg_texp) = self.substitute_texp(arg);
                    new_args.push(arg_texp);
                    unbounds.extend(unbound);
                }

                let (unbound, ty) = self.substitute(ty);
                unbounds.extend(unbound);
                (unbounds, TypedExpr::TApp(fun.b(), new_args, ty))
            }
            TypedExpr::TPrim(op, left, right, ty) => {
                let (mut unbound_left, left) = self.substitute_texp(*left);
                let (unbound_right, right) = self.substitute_texp(*right);
                unbound_left.extend(unbound_right);
                let (unbounds, ty) = self.substitute(ty);
                unbound_left.extend(unbounds);
                (unbound_left, TypedExpr::TPrim(op, left.b(), right.b(), ty))
            }
            TypedExpr::TLet(name, ty, rhs, body) => {
                let (mut unbound_rhs, rhs) = self.substitute_texp(*rhs);
                let (unbound_body, body) = self.substitute_texp(*body);
                unbound_rhs.extend(unbound_body);
                let (unbounds, ty) = self.substitute(ty);
                unbound_rhs.extend(unbounds);
                (unbound_rhs, TypedExpr::TLet(name, ty, rhs.b(), body.b()))
            }
            TypedExpr::TIfThenElse(cond, then, else_branch, ty) => {
                let (mut unbound_cond, cond) = self.substitute_texp(*cond);
                let (unbound_then, then) = self.substitute_texp(*then);
                let (unbound_else, else_branch) = self.substitute_texp(*else_branch);
                unbound_cond.extend(unbound_then);
                unbound_cond.extend(unbound_else);
                let (unbounds, ty) = self.substitute(ty);
                unbound_cond.extend(unbounds);
                (
                    unbound_cond,
                    TypedExpr::TIfThenElse(cond.b(), then.b(), else_branch.b(), ty),
                )
            }
            TypedExpr::TTuple(ts, ty) => {
                let mut unbound = BTreeSet::new();
                let mut new_ts = Vec::new();
                for t in ts {
                    let (unbounds, t) = self.substitute_texp(t);
                    unbound.extend(unbounds);
                    new_ts.push(t);
                }
                let (unbounds, ty) = self.substitute(ty);
                unbound.extend(unbounds);
                (unbound, TypedExpr::TTuple(new_ts, ty))
            }
            TypedExpr::TAccess(texp, idx, ty) => {
                let (mut unbound, texp) = self.substitute_texp(*texp);
                let (unbounds, ty) = self.substitute(ty);
                unbound.extend(unbounds);
                (unbound, TypedExpr::TAccess(texp.b(), idx, ty))
            }
        }
    }

    fn infer_all_toplevels(
        &mut self,
        toplevels: Vec<Toplevel>,
        mut context: Context,
    ) -> (Vec<TypedToplevel>, Vec<(Sym, Type)>) {
        let mut typed_toplevels = Vec::new();
        for toplevel in &toplevels {
            match toplevel {
                Toplevel::FunDef(name, fun_ty, args, body) => {
                    context.insert_binding(name.to_owned(), fun_ty.clone());
                }
                Toplevel::Channel(name, ty) => {
                    context.insert_channel(name.to_owned(), ty.clone());
                    typed_toplevels.push(TypedToplevel::Channel(name.to_owned(), ty.clone()))
                }
                Toplevel::Output(name, expr) => {}
            }
        }

        // TODO we need to error out somewhere if there are duplicate channel names
        context.channels.sort_by(|a, b| a.0.cmp(&b.0));

        for toplevel in toplevels {
            match toplevel {
                Toplevel::FunDef(name, fun_ty, args, body) => {
                    let (ret_ty, types) = tfun_len_n(fun_ty.clone(), args.len());
                    let args_with_types = args.into_iter().zip(types.into_iter());

                    let mut context = context.clone();
                    context.insert_binding(name.to_owned(), fun_ty.clone());
                    for (arg, typ) in args_with_types.clone() {
                        context.insert_binding(arg.to_owned(), typ);
                    }

                    let (body_type, mut body_output) = self.infer(context, body);
                    body_output
                        .constraints
                        .push(Constraint::TypeEqual(ret_ty.clone(), body_type.clone()));

                    // Constraint solving went well
                    if self.unification(&body_output.constraints).is_ok() {
                        // Substitute types
                        let (mut unbound, ty) = self.substitute(body_type);
                        let (unbound_body, body) = self.substitute_texp(body_output.texp);
                        unbound.extend(unbound_body);

                        let typed_toplevel = TypedToplevel::TFunDef(
                            name.to_owned(),
                            args_with_types.collect(),
                            body.b(),
                            ret_ty,
                        );

                        typed_toplevels.push(typed_toplevel);
                    } else {
                        panic!(
                            "Error: Unsolved constraints: {:#?}",
                            body_output.constraints
                        );
                    }
                }
                Toplevel::Channel(name, _) => {
                    // Channels are handled in the pre-processing loop above.
                }
                Toplevel::Output(name, expr) => {
                    // type expr
                    let mut context = context.clone();
                    let (expr_type, mut output) = self.infer(context, expr);
                    let output_ty = Type::TVar(self.fresh_ty_var());
                    // push Later int constraint, because we want a delayed closure
                    let expected_type = Type::TLater(output_ty.b(), ClockExpr::Symbolic.into());
                    output
                        .constraints
                        .push(Constraint::TypeEqual(expr_type.clone(), expected_type));

                    if self.unification(&output.constraints).is_ok() {
                        let (mut unbound, ty) = self.substitute(expr_type);
                        let (_, typed_expr) = self.substitute_texp(output.texp);
                        // unbound.extend(unbound_body);

                        // create typedtoplevel::output
                        typed_toplevels.push(TypedToplevel::Output(name, typed_expr.b()));
                    } else {
                        panic!("Error: Unsolved constraints: {:#?}", output.constraints);
                    }
                }
            }
        }

        (typed_toplevels, context.channels)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    TypeNotEqual(Type, Type),
    InfiniteType(TypeVar, Type),
    NotUnderTick,
}

fn get_with_custom_message(opt: Option<(Type, TypedExpr)>, message: String) -> (Type, TypedExpr) {
    match opt {
        Some(value) => value,
        None => panic!("{}", message),
    }
}

pub fn infer_all(prog: Prog) -> TypedProg {
    let toplevels = prog.0;

    let mut inference = Inference {
        unification_table: InPlaceUnificationTable::default(),
    };

    let (typed_toplevels, sorted_channels) =
        inference.infer_all_toplevels(toplevels, Default::default());

    TypedProg {
        defs: typed_toplevels,
        sorted_inputs: sorted_channels,
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn infer_lambda() {
        let lambda = Expr::Lam(
            vec!["x".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );

        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };

        let (ty, output) = inference.infer(Default::default(), lambda);
    }

    #[test]
    fn infer_returned_lambda() {
        let fun_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
        );
        let fun_args = vec!["x".to_owned()];
        let fun_body = Expr::Lam(
            vec!["y".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Var("y".to_owned()).b(),
            )
            .b(),
        );
        let fun = Toplevel::FunDef(
            "curried_add".to_owned(),
            fun_type.clone(),
            fun_args,
            fun_body,
        );

        let expected_fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());

        let inferred = infer_all(vec![fun].into());
        assert_eq!(inferred.len(), 1);
        assert_matches!(inferred[0].clone(),
            TypedToplevel::TFunDef(name, args, box body, ty)
            if name == "curried_add" &&
               args == [("x".to_owned(), Type::TInt)] &&
               ty == expected_fun_type
        );
    }

    #[test]
    fn infer_toplevel_with_applied_lambda_in_body() {
        let fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let lambda = Expr::Lam(
            vec!["y".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Var("y".to_owned()).b(),
            )
            .b(),
        );
        let fun_body = Expr::App(lambda.b(), Expr::Var("x".to_owned()).v());
        let fundef = Toplevel::FunDef("test".to_owned(), fun_type, vec!["x".to_owned()], fun_body);

        let prog = Prog(vec![fundef]);

        let inferred = infer_all(prog);
        assert_eq!(inferred.len(), 1);
        let typed_toplevel = inferred[0].clone();
        assert_matches!(inferred[0].clone(),
            TypedToplevel::TFunDef(name, args, box body, ty)
            if name == "test" &&
               args == [("x".to_owned(), Type::TInt)] &&
               ty == Type::TInt
        );
    }

    #[test]
    fn build_function_type_returns_correct_type() {
        let expected_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TBool.b(), Type::TBool.b()).b(),
        );
        let arg_types = vec![Type::TInt, Type::TBool];
        let ret_ty = Type::TBool;
        let actual_type = build_function_type(arg_types.as_slice(), ret_ty);
        assert_eq!(actual_type, expected_type);
    }

    #[test]
    fn infer_all_function_with_shadowing() {
        let fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let fun_args = vec!["x".to_owned()];
        let fun_body = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(40)).b(),
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );
        let fun = Toplevel::FunDef(
            "test".to_owned(),
            fun_type.clone(),
            fun_args,
            fun_body.clone(),
        );
        let expected_body = TypedExpr::TLet(
            "x".to_owned(),
            Type::TInt,
            TypedExpr::TConst(Const::CInt(40), Type::TInt).b(),
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
        );
        let inferred = infer_all(Prog(vec![fun]));
        assert_eq!(inferred.defs.len(), 1);

        assert_matches!(inferred.defs[0].clone(),
            TypedToplevel::TFunDef(name, args, box body, ty)
            if name == "test" &&
               args == [("x".to_owned(), Type::TInt)] &&
               ty == Type::TInt &&
               body == expected_body
        );
    }

    #[test]
    fn infer_all_function_with_shadowing_where_types_change() {
        let fun_type = Type::TFun(Type::TInt.b(), Type::TBool.b());
        let fun_args = vec!["x".to_owned()];
        let fun_body = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CBool(true)).b(),
            Expr::Var("x".to_owned()).b(),
        );
        let fun = Toplevel::FunDef(
            "test".to_owned(),
            fun_type.clone(),
            fun_args,
            fun_body.clone(),
        );
        let expected_body = TypedExpr::TLet(
            "x".to_owned(),
            Type::TBool,
            TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
            TypedExpr::TName("x".to_owned(), Type::TBool).b(),
        );
        let inferred = infer_all(Prog(vec![fun]));
        assert_eq!(inferred.defs.len(), 1);
        assert_matches!(inferred.defs[0].clone(),
            TypedToplevel::TFunDef(name, args, box body, ty)
            if name == "test" &&
               args == [("x".to_owned(), Type::TInt)] &&
               ty == Type::TBool &&
               body == expected_body
        );
    }

    #[test]
    fn infer_all_constant_function() {
        let fn_type = Type::TInt;
        let fun_body = Expr::Const(Const::CInt(2));
        let fun = Toplevel::FunDef("test".to_owned(), fn_type, vec![], fun_body);

        let inferred = infer_all(Prog(vec![fun]));
    }

    #[test]
    #[should_panic]
    fn infer_all_function_with_wrong_signature_should_panic() {
        let fn_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let fun_body = Expr::Const(Const::CBool(true));
        let fun = Toplevel::FunDef(
            "test".to_owned(),
            fn_type,
            vec![String::from("x")],
            fun_body,
        );

        let inferred = infer_all(Prog(vec![fun]));
    }

    #[test]
    fn infer_int_const() {
        let expr = Expr::Const(Const::CInt(42));
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(output.texp, TypedExpr::TConst(Const::CInt(42), Type::TInt));
    }

    #[test]
    fn infer_tuple_nested_tuple_access() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Tuple(vec![
                Expr::Const(Const::CBool(true)),
                Expr::Const(Const::CBool(false)),
            ]),
        ]);
        let outer_access = Expr::Access(tuple.b(), 1);
        let inner_access = Expr::Access(outer_access.b(), 0);
        let inner_tuple_type = Type::TProduct(vec![Type::TBool, Type::TBool]);
        let outer_tuple_type = Type::TProduct(vec![Type::TInt, inner_tuple_type.clone()]);
        let expected_texp = TypedExpr::TAccess(
            TypedExpr::TAccess(
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TTuple(
                            vec![
                                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                                TypedExpr::TConst(Const::CBool(false), Type::TBool),
                            ],
                            inner_tuple_type.clone(),
                        ),
                    ],
                    outer_tuple_type,
                )
                .b(),
                1,
                inner_tuple_type,
            )
            .b(),
            0,
            Type::TBool,
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), inner_access);
        assert_eq!(ty, Type::TBool);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_tuple_access() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), 0);
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(
            output.texp,
            TypedExpr::TAccess(
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool)
                    ],
                    Type::TProduct(vec![Type::TInt, Type::TBool])
                )
                .b(),
                0,
                Type::TInt
            )
        );
    }

    #[test]
    #[should_panic]
    fn infer_sub_zero_tuple_access_should_panic() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), -1);
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    fn infer_advance_name_bound_and_tick_in_context() {
        let clock_expr = ClockExpr::Cl("keyboard".to_owned());
        let expr = Expr::Advance("x".to_owned());
        let fun_type = Type::TLater(Type::TInt.b(), clock_expr.clone().into());
        let binding = (fun_type, Some(clock_expr.clone().into()));
        let mut context = BindingContext::Tick(
            HashMap::from([("x".to_owned(), binding)]),
            clock_expr.clone().into(),
            HashMap::new(),
        );

        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(context.into(), expr);
    }

    #[test]
    #[should_panic]
    fn infer_advance_name_bound_to_thunk_in_context_but_no_tick() {
        let expr = Expr::Advance("x".to_owned());
        let mut context = BindingContext::Bindings(HashMap::new());
        let fun_type = Type::TFun(Type::TUnit.b(), Type::TInt.b());
        context.insert("x".to_owned(), (fun_type.clone()));
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let _ = inference.infer(context.into(), expr);
    }

    #[test]
    #[should_panic]
    fn infer_advance_name_not_bound_in_context_should_panic() {
        let expr = Expr::Advance("x".to_owned());
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    #[should_panic]
    fn infer_advance_name_not_bound_to_thunk_in_context_should_panic() {
        let expr = Expr::Advance("x".to_owned());
        let mut context = HashMap::new();
        context.insert("x".to_owned(), (Type::TInt, None));
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(BindingContext::Bindings(context).into(), expr);
    }

    #[test]
    fn infer_delay_produces_later_thunk() {
        let clock_expr = ClockExpr::Cl("hey".to_owned());
        let expr = Expr::Delay(Expr::Const(Const::CInt(42)).b(), clock_expr.clone().into());
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        let expected_texp = TypedExpr::TLam(
            Vec::new(),
            TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
            Type::TLater(Type::TInt.b(), clock_expr.clone().into()),
            Some(clock_expr.clone().into()),
        );
        assert_eq!(ty, Type::TLater(Type::TInt.b(), clock_expr.clone().into()),);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    #[should_panic]
    fn infer_out_of_bounds_tuple_access_should_panic() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), 2);
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    fn infer_all_tuple_access_used_in_primitive_op_in_function() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CInt(40)),
        ]);
        let fun_body = Expr::Prim(
            Binop::Add,
            Expr::Var("x".to_owned()).b(),
            Expr::Access(tuple.b(), 2).b(),
        );
        let fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let fun_args = vec!["x".to_owned()];
        let fun = Toplevel::FunDef("test".to_owned(), fun_type.clone(), fun_args, fun_body);
        let expected_body = TypedExpr::TPrim(
            Binop::Add,
            TypedExpr::TName("x".to_owned(), Type::TInt).b(),
            TypedExpr::TAccess(
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                        TypedExpr::TConst(Const::CInt(40), Type::TInt),
                    ],
                    Type::TProduct(vec![Type::TInt, Type::TBool, Type::TInt]),
                )
                .b(),
                2,
                Type::TInt,
            )
            .b(),
            Type::TInt,
        );
        let inferred = infer_all(Prog(vec![fun]));
        assert_eq!(inferred.defs.len(), 1);
        match inferred.defs[0].clone() {
            TypedToplevel::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test");
                assert_eq!(args.len(), 1);
                assert_eq!(body, expected_body);
                assert_eq!(ty, Type::TInt);
            }
            _ => panic!("Failed to infer of function that adds argument to tuple access"),
        }
    }

    #[test]
    fn infer_let_bound_tuple_accessed_in_body() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CBool(false)),
        ]);
        let expr = Expr::Let(
            "tuple_binding".to_owned(),
            tuple.b(),
            Expr::Access(Expr::Var("tuple_binding".to_owned()).b(), 0).b(),
        );
        let tuple_type = Type::TProduct(vec![Type::TInt, Type::TBool, Type::TBool]);
        let tuple_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            tuple_type.clone(),
        );
        let expected_texp = TypedExpr::TLet(
            "tuple_binding".to_owned(),
            Type::TInt,
            tuple_texp.b(),
            TypedExpr::TAccess(
                TypedExpr::TName("tuple_binding".to_owned(), tuple_type.clone()).b(),
                0,
                Type::TInt,
            )
            .b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_two_element_tuple_with_second_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CBool(false)),
            Expr::Tuple(vec![
                Expr::Const(Const::CInt(42)),
                Expr::Const(Const::CBool(true)),
            ]),
        ]);
        let expected_second_element_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_type = Type::TProduct(vec![Type::TBool, expected_second_element_type.clone()]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                    ],
                    expected_second_element_type,
                ),
            ],
            expected_type.clone(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, expected_type);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_two_element_tuple_with_first_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Tuple(vec![
                Expr::Const(Const::CInt(42)),
                Expr::Const(Const::CBool(true)),
            ]),
            Expr::Const(Const::CBool(false)),
        ]);
        let expected_first_element_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_type = Type::TProduct(vec![expected_first_element_type.clone(), Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                    ],
                    expected_first_element_type,
                ),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            expected_type.clone(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, expected_type);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_three_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CBool(false)),
        ]);
        let expected_type = Type::TProduct(vec![Type::TInt, Type::TBool, Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            expected_type.clone(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, expected_type);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_two_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expected_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
            ],
            expected_type.clone(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, expected_type);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    #[should_panic]
    fn infer_single_element_tuple_should_panic() {
        let expr = Expr::Tuple(vec![Expr::Const(Const::CInt(42))]);
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    #[should_panic]
    fn infer_zero_element_tuple_should_panic() {
        let expr = Expr::Tuple(vec![]);
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    #[should_panic]
    fn infer_conditional_different_branch_types_should_panic() {
        let expr = Expr::IfThenElse(
            Expr::Const(Const::CBool(true)).b(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Const(Const::CBool(false)).b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (expr, output) = inference.infer(Default::default(), expr);
        inference
            .unification(&output.constraints)
            .expect("failure since branches have different types");
    }

    #[test]
    fn infer_conditional() {
        let expr = Expr::IfThenElse(
            Expr::Const(Const::CBool(true)).b(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Const(Const::CInt(0)).b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(
            output.texp,
            TypedExpr::TIfThenElse(
                TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
                TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
                TypedExpr::TConst(Const::CInt(0), Type::TInt).b(),
                Type::TInt
            )
        );
    }

    #[test]
    fn infer_var_in_context() {
        let expr = Expr::Var("x".to_owned());
        let mut context = HashMap::new();
        context.insert("x".to_owned(), (Type::TInt, None));
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(BindingContext::Bindings(context).into(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(output.texp, TypedExpr::TName("x".to_owned(), Type::TInt));
    }

    #[test]
    #[should_panic]
    fn infer_var_not_in_context_should_panic() {
        let expr = Expr::Var("x".to_owned());
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        inference.infer(Default::default(), expr);
    }

    #[test]
    fn infer_let_binding_with_prim_body() {
        let expr = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(2)).b(),
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Var("x".to_owned()).b(),
            )
            .b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(
            output.texp,
            TypedExpr::TLet(
                "x".to_owned(),
                Type::TInt,
                TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                TypedExpr::TPrim(
                    Binop::Add,
                    TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                    TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                    Type::TInt
                )
                .b()
            )
        );
    }

    #[test]
    fn infer_let_binding_with_rhs_as_body() {
        let expr = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Var("x".to_owned()).b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(
            output.texp,
            TypedExpr::TLet(
                "x".to_owned(),
                Type::TInt,
                TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
                TypedExpr::TName("x".to_owned(), Type::TInt).b()
            )
        );
    }

    #[test]
    fn infer_prim_add() {
        let expr = Expr::Prim(
            Binop::Add,
            Expr::Const(Const::CInt(40)).b(),
            Expr::Const(Const::CInt(2)).b(),
        );
        let expected_texp = TypedExpr::TPrim(
            Binop::Add,
            TypedExpr::TConst(Const::CInt(40), Type::TInt).b(),
            TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
            Type::TInt,
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(Default::default(), expr);
        assert_eq!(ty, Type::TInt);
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn check_valid_lambda() {
        let expr = Expr::Lam(
            vec!["x".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );
        let expected_texp = TypedExpr::TLam(
            vec![("x".to_owned(), Type::TInt)],
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
            Type::TInt,
            None,
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let output = inference.check(
            Default::default(),
            expr.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );
        assert_eq!(output.texp, expected_texp);
    }

    #[test]
    fn infer_valid_multiple_application() {
        let expr = Expr::App(
            Expr::App(
                Expr::Var("f".to_owned()).b(),
                Expr::Const(Const::CInt(2)).v(),
            )
            .b(),
            Expr::Const(Const::CInt(2)).v(),
        );
        let inner_fun_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
        );
        let mut context = HashMap::from([("f".to_owned(), (inner_fun_type.clone(), None))]);
        let expected_inner_fun = TypedExpr::TName("f".to_owned(), inner_fun_type.clone());
        let expected_inner_app = TypedExpr::TApp(
            expected_inner_fun.clone().b(),
            vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );

        let expected_outer_app = TypedExpr::TApp(
            expected_inner_app.clone().b(),
            vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
            Type::TInt,
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(BindingContext::Bindings(context).into(), expr);
        inference.unification(&output.constraints).unwrap();
        let (_, ty) = inference.substitute(ty);
        let (_, texp) = inference.substitute_texp(output.texp);
        assert_eq!(ty, Type::TInt);
        assert_eq!(texp, expected_outer_app);
    }

    #[test]
    #[should_panic]
    fn infer_invalid_application_should_panic() {
        let expr = Expr::App(
            Expr::Var("f".to_owned()).b(),
            Expr::Const(Const::CInt(42)).v(),
        );
        let mut context = HashMap::new();
        context.insert(
            "f".to_owned(),
            (Type::TFun(Type::TBool.b(), Type::TInt.b()), None),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };

        let (ty, output) = inference.infer(BindingContext::Bindings(context).into(), expr);

        inference.unification(&output.constraints).unwrap();
    }

    #[test]
    fn infer_valid_application() {
        let expr = Expr::App(
            Expr::Var("f".to_owned()).b(),
            Expr::Const(Const::CInt(42)).v(),
        );
        let mut context = BindingContext::Bindings(HashMap::new());
        context.insert("f".to_owned(), (Type::TFun(Type::TInt.b(), Type::TInt.b())));
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let (ty, output) = inference.infer(context.into(), expr);
        inference.unification(&output.constraints).unwrap();
        let (_, ty) = inference.substitute(ty);
        assert_eq!(ty, Type::TInt);
        let (_, texp) = inference.substitute_texp(output.texp);
        assert_eq!(
            texp,
            TypedExpr::TApp(
                TypedExpr::TName("f".to_owned(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                vec![TypedExpr::TConst(Const::CInt(42), Type::TInt)],
                Type::TInt
            )
        );
    }

    #[test]
    #[should_panic]
    fn tfun_len_n_given_non_tfun_panics() {
        tfun_len_n(Type::TBool, 1);
    }

    #[test]
    #[should_panic]
    fn tfun_len_n_given_tfun_and_out_of_bounds_n_panics() {
        tfun_len_n(Type::TFun(Type::TInt.b(), Type::TInt.b()), 2);
    }

    #[test]
    fn tfun_len_n_given_tfun_and_zero_returns_type_unmodified() {
        let tfun = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 0);
        assert_eq!(ret_ty, tfun);
        assert_eq!(types, vec![]);
    }

    #[test]
    fn tfun_len_n_given_singlearg_tfun_and_n_equals_length_of_args() {
        let tfun = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 1);
        assert_eq!(ret_ty, Type::TInt);
        assert_eq!(types, vec![Type::TInt]);
    }

    #[test]
    fn tfun_len_n_given_multiarg_tfun_and_n_equals_length_of_args() {
        let tfun = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TBool.b()).b(),
        );
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 2);
        assert_eq!(ret_ty, Type::TBool);
        assert_eq!(types, vec![Type::TInt, Type::TInt]);
    }

    #[test]
    fn infer_lambda_with_multiple_args_int_bool() {
        let expr = Expr::Lam(
            vec!["x".to_owned(), "y".to_owned()],
            Expr::Prim(
                Binop::Eq,
                Expr::Prim(
                    Binop::Eq,
                    Expr::Var("x".to_owned()).b(),
                    Expr::Const(Const::CInt(42)).b(),
                )
                .b(),
                Expr::Var("y".to_owned()).b(),
            )
            .b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let fun_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TBool.b(), Type::TBool.b()).b(),
        );
        let (ty, output) = inference.infer(Default::default(), expr);
        inference.unification(&output.constraints).unwrap();
        let (_, ty) = inference.substitute(ty);
        let (_, texp) = inference.substitute_texp(output.texp);
        assert_eq!(ty, fun_type);
    }

    #[test]
    fn infer_primitive_equality_nested() {
        let expr = Expr::Prim(
            Binop::Eq,
            Expr::Prim(
                Binop::Eq,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(42)).b(),
            )
            .b(),
            Expr::Var("y".to_owned()).b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };

        let mut context: HashMap<Sym, Binding> = [
            ("x".to_owned(), (Type::TInt, None)),
            ("y".to_owned(), (Type::TBool, None)),
        ]
        .into_iter()
        .collect();

        let (ty, output) = inference.infer(BindingContext::Bindings(context).into(), expr);

        inference.unification(&output.constraints).unwrap();
        let (_, ty) = inference.substitute(ty);
        let (_, texp) = inference.substitute_texp(output.texp);
        assert_eq!(ty, Type::TBool);
    }

    #[test]
    fn infer_lambda_with_multiple_args_all_ints() {
        let expr = Expr::Lam(
            vec!["x".to_owned(), "y".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Prim(
                    Binop::Add,
                    Expr::Var("x".to_owned()).b(),
                    Expr::Const(Const::CInt(42)).b(),
                )
                .b(),
                Expr::Var("y".to_owned()).b(),
            )
            .b(),
        );
        let mut inference = Inference {
            unification_table: InPlaceUnificationTable::default(),
        };
        let fun_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
        );
        let (ty, output) = inference.infer(Default::default(), expr);
        inference.unification(&output.constraints).unwrap();
        let (_, ty) = inference.substitute(ty);
        let (_, texp) = inference.substitute_texp(output.texp);
        assert_eq!(ty, fun_type);
    }
}
