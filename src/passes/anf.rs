use crate::anf::{AExpr, AnfExpr, AnfProg, AnfToplevel, CExpr};
use crate::passes::Pass;
use crate::types::{TypedExpr, TypedProg, TypedToplevel};

pub struct ANFConversion {
    counter: usize,
}

impl Default for ANFConversion {
    fn default() -> Self {
        Self::new()
    }
}

impl ANFConversion {
    pub fn new() -> Self {
        ANFConversion { counter: 0 }
    }

    fn fresh_name(&mut self) -> String {
        let name = format!("tmp_{}", self.counter);
        self.counter += 1;
        name
    }

    fn normalize(&mut self, expr: TypedExpr) -> AnfExpr {
        match expr {
            TypedExpr::TConst(c, ty) => AnfExpr::AExpr(AExpr::Const(c, ty)),
            TypedExpr::TName(x, ty) => AnfExpr::AExpr(AExpr::Var(x, ty)),
            TypedExpr::TPrim(op, e1, e2, ty) => {
                let (e1_anf, bindings1) = self.normalize_atom(*e1);
                let (e2_anf, bindings2) = self.normalize_atom(*e2);
                let comp = CExpr::Prim(op, e1_anf, e2_anf, ty);
                self.wrap_bindings(bindings1, bindings2, AnfExpr::CExp(comp))
            }
            TypedExpr::TApp(f, args, ty) => {
                let (f_anf, bindings_f) = self.normalize_atom(*f);
                let mut all_bindings = bindings_f;
                let mut anf_args = Vec::new();

                for arg in args {
                    let (arg_anf, bindings) = self.normalize_atom(arg);
                    all_bindings.extend(bindings);
                    anf_args.push(arg_anf);
                }

                let comp = CExpr::App(f_anf, anf_args, ty);
                self.wrap_bindings(all_bindings, vec![], AnfExpr::CExp(comp))
            }
            TypedExpr::TLet(x, ty, e1, e2) => {
                let e1_anf = self.normalize(*e1);
                let e2_anf = self.normalize(*e2);
                AnfExpr::Let(x, ty, Box::new(e1_anf), Box::new(e2_anf))
            }
            TypedExpr::TIfThenElse(cond, then_br, else_br, ty) => {
                let (cond_anf, bindings) = self.normalize_atom(*cond);
                let then_anf = self.normalize(*then_br);
                let else_anf = self.normalize(*else_br);
                let comp = CExpr::IfThenElse(cond_anf, Box::new(then_anf), Box::new(else_anf), ty);
                self.wrap_bindings(bindings, vec![], AnfExpr::CExp(comp))
            }
            TypedExpr::TTuple(es, ty) => {
                let mut all_bindings = Vec::new();
                let mut anf_es = Vec::new();

                for e in es {
                    let (e_anf, bindings) = self.normalize_atom(e);
                    all_bindings.extend(bindings);
                    anf_es.push(e_anf);
                }

                let comp = CExpr::Tuple(anf_es, ty);
                self.wrap_bindings(all_bindings, vec![], AnfExpr::CExp(comp))
            }
            TypedExpr::TAccess(e, idx, ty) => {
                let (e_anf, bindings) = self.normalize_atom(*e);
                let comp = CExpr::Access(e_anf, idx, ty);
                self.wrap_bindings(bindings, vec![], AnfExpr::CExp(comp))
            }
            TypedExpr::TLam(params, body, ty) => {
                let body_anf = self.normalize(*body);
                AnfExpr::AExpr(AExpr::Lam(params, Box::new(body_anf), ty))
            }
        }
    }

    fn normalize_atom(&mut self, expr: TypedExpr) -> (AExpr, Vec<(String, AnfExpr)>) {
        match expr {
            TypedExpr::TConst(c, ty) => (AExpr::Const(c, ty), vec![]),
            TypedExpr::TName(x, ty) => (AExpr::Var(x, ty), vec![]),
            expr => {
                let name = self.fresh_name();
                let ty = expr.ty();
                let normalized = self.normalize(expr);
                (AExpr::Var(name.clone(), ty), vec![(name, normalized)])
            }
        }
    }

    fn wrap_bindings(
        &self,
        bindings1: Vec<(String, AnfExpr)>,
        bindings2: Vec<(String, AnfExpr)>,
        expr: AnfExpr,
    ) -> AnfExpr {
        let mut result = expr;
        for (name, bound_expr) in bindings1.into_iter().chain(bindings2) {
            let ty = match &bound_expr {
                AnfExpr::AExpr(ae) => ae.ty(),
                AnfExpr::CExp(ce) => ce.ty(),
                AnfExpr::Let(_, _, _, aexpr) => aexpr.ty(),
            };

            result = AnfExpr::Let(name, ty, Box::new(bound_expr), Box::new(result));
        }
        result
    }
}

impl Pass<TypedProg, AnfProg> for ANFConversion {
    fn run(&mut self, input: TypedProg) -> AnfProg {
        let toplevel = input
            .0
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, args, body, ty) => {
                    let anf_body = self.normalize(*body);
                    AnfToplevel::FunDef(name, args, anf_body, ty)
                }
                TypedToplevel::Channel(chan) => AnfToplevel::Channel(chan),
                TypedToplevel::Output(name, body) => {
                    let (aexpr, _) = self.normalize_atom(*body);
                    AnfToplevel::Output(name, aexpr)
                }
            })
            .collect();
        AnfProg(toplevel)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Binop, Const, Type};
    use crate::types::{TypedExpr, TypedToplevel};

    fn run_anf(expr: TypedExpr) -> AnfExpr {
        let mut converter = ANFConversion::new();
        converter.normalize(expr)
    }

    #[test]
    fn test_atomic_expressions() {
        // Actual:   42
        // Expected: 42
        let const_expr = TypedExpr::TConst(Const::CInt(42), Type::TInt);
        assert_eq!(
            run_anf(const_expr),
            AnfExpr::AExpr(AExpr::Const(Const::CInt(42), Type::TInt))
        );

        // Actual:   x
        // Expected: x
        let var_expr = TypedExpr::TName("x".to_string(), Type::TInt);
        assert_eq!(
            run_anf(var_expr),
            AnfExpr::AExpr(AExpr::Var("x".to_string(), Type::TInt))
        );
    }

    #[test]
    fn test_simple_arithmetic() {
        // Actual:   1 + 2
        // Expected: 1 + 2
        let expr = TypedExpr::TPrim(
            Binop::Add,
            Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
            Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
            Type::TInt,
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::CExp(CExpr::Prim(..))));
    }

    #[test]
    fn test_nested_arithmetic() {
        // Actual:   (1 + 2) + (3 + 4)
        // Expected: let tmp_0 = 1 + 2 in
        //           let tmp_1 = 3 + 4 in
        //           tmp_0 + tmp_1
        let expr = TypedExpr::TPrim(
            Binop::Add,
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                Type::TInt,
            )),
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(4), Type::TInt)),
                Type::TInt,
            )),
            Type::TInt,
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::Let(..)));
    }

    #[test]
    fn test_function_application() {
        // Actual:   f 1 (2 + 3)
        // Expected: let tmp_0 = 2 + 3 in
        //           f 1 tmp_0
        let expr = TypedExpr::TApp(
            Box::new(TypedExpr::TName(
                "f".to_string(),
                Type::TFun(
                    Box::new(Type::TInt),
                    Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                ),
            )),
            vec![
                TypedExpr::TConst(Const::CInt(1), Type::TInt),
                TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                    Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
                    Type::TInt,
                ),
            ],
            Type::TInt,
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::Let(..)));
    }

    #[test]
    fn test_conditional() {
        // Actual:   if 1 + 2 then 3 else 4
        // Expected: let tmp_0 = 1 + 2 in
        //           if tmp_0 then 3 else 4
        let expr = TypedExpr::TIfThenElse(
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                Type::TInt,
            )),
            Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
            Box::new(TypedExpr::TConst(Const::CInt(4), Type::TInt)),
            Type::TInt,
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::Let(..)));
    }

    #[test]
    fn test_tuple() {
        // Actual:   (1, 2 + 3, 4)
        // Expected: let tmp_0 = 2 + 3 in
        //           (1, tmp_0, 4)
        let expr = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(1), Type::TInt),
                TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                    Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
                    Type::TInt,
                ),
                TypedExpr::TConst(Const::CInt(4), Type::TInt),
            ],
            Type::TProduct(vec![Type::TInt, Type::TInt, Type::TInt]),
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::Let(..)));
    }

    #[test]
    fn test_let_binding() {
        // Actual:   let x = 1 + 2 in x + 3
        // Expected: let x = 1 + 2 in
        //           x + 3
        let expr = TypedExpr::TLet(
            "x".to_string(),
            Type::TInt,
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                Type::TInt,
            )),
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
                Type::TInt,
            )),
        );

        let result = run_anf(expr);
        assert!(matches!(result, AnfExpr::Let(..)));
    }

    #[test]
    fn test_full_program() {
        // Actual:   fun f(x: int) = (x + 1) + (2 + 3)
        // Expected: fun f(x: int) =
        //           let tmp_0 = x + 1 in
        //           let tmp_1 = 2 + 3 in
        //           tmp_0 + tmp_1
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "f".to_string(),
            vec![("x".to_string(), Type::TInt)],
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                    Box::new(TypedExpr::TConst(Const::CInt(1), Type::TInt)),
                    Type::TInt,
                )),
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TConst(Const::CInt(2), Type::TInt)),
                    Box::new(TypedExpr::TConst(Const::CInt(3), Type::TInt)),
                    Type::TInt,
                )),
                Type::TInt,
            )),
            Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
        )]);

        let mut converter = ANFConversion::new();
        let result = converter.run(prog);

        assert_eq!(result.0.len(), 1);
    }
}
