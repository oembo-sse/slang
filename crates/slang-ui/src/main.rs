use slang::ast::{self, Expr, ExprKind, Stmt, StmtKind};
use slang_ui::{bail, Result};
use smtlib::prelude::*;

struct App;

impl slang_ui::Hook for App {
    fn analyze(&self, cx: &mut slang_ui::Context, file: &slang::SourceFile) -> Result<()> {
        // if file.contains_error() {
        //     bail!("wont continue while we have errors");
        // }

        let mut solver = cx.solver()?;

        for d in file.domains() {
            let span = tracing::info_span!("domain", name=%d.name);
            let _enter = span.enter();

            for f in d.functions() {
                let smt_fun = {
                    let args: Result<Vec<_>> =
                        f.args.iter().map(|arg| smt_sort(&arg.ty.1)).collect();
                    let args = args?;
                    let return_ty = smt_sort(&f.return_ty.1)?;
                    smtlib::funs::Fun::new(f.name.to_string(), args, return_ty)
                };
                solver.declare_fun(&smt_fun)?;

                let args: Result<Vec<_>> = f
                    .args
                    .iter()
                    .map(|arg| {
                        let sort = smt_sort(&arg.ty.1)?;
                        Ok(sort.new_const(arg.name.to_string()))
                    })
                    .collect();
                let args = args?;
                for ensures in f.ensures() {
                    let assertion =
                        smtlib::terms::forall(args.clone(), smt_expr(ensures)?.as_bool()?);
                    solver.assert(assertion)?;
                }
            }
            for ax in d.axioms() {
                solver.assert(smt_expr(&ax.expr)?.as_bool()?)?;
            }
        }
        for f in file.functions() {
            let args: Result<Vec<_>> = f.args.iter().map(|arg| smt_sort(&arg.ty.1)).collect();
            let args = args?;
            let return_ty = smt_sort(&f.return_ty.1)?;
            let smt_fun = smtlib::funs::Fun::new(f.name.to_string(), args, return_ty);
            solver.declare_fun(&smt_fun)?;
        }

        for m in file.methods() {
            let span = tracing::info_span!("method", name=%m.name);
            let _enter = span.enter();

            assert_true_lint(cx, &m.body.stmt);
        }

        for m in file.methods() {
            let span = tracing::info_span!("method", name=%m.name);
            let _enter = span.enter();

            let pre = m
                .requires()
                .cloned()
                .reduce(|a, b| a & b)
                .unwrap_or(Expr::bool(true));
            for post in m.ensures() {
                let p = wp(cx, &m.body.stmt, post.clone());
                let pre_imp_p = pre.imp(&p);
                cx.info(m.name.span, format!("{}", pre_imp_p));
                cx.info(m.name.span, format!("{}", smt_expr(&pre_imp_p)?));
                let pre_imp_p = smt_expr(&pre_imp_p)?;
                solver.scope(|solver| {
                    solver.assert(!pre_imp_p.as_bool()?)?;
                    match solver.check_sat_with_model()? {
                        smtlib::SatResultWithModel::Unsat => {
                            cx.info(post.span, "holds");
                        }
                        smtlib::SatResultWithModel::Sat(model) => {
                            cx.error(post.span, format!("might not hold: {model}"));
                        }
                        smtlib::SatResultWithModel::Unknown => {
                            cx.warning(post.span, "unknown sat result");
                        }
                    }
                    Ok(())
                })?;
            }
        }

        Ok(())
    }
}

fn smt_sort(ty: &ast::Type) -> Result<smtlib::sorts::Sort> {
    match ty {
        ast::Type::Unresolved => bail!("unknown type"),
        ast::Type::Int => Ok(smtlib::Int::sort()),
        ast::Type::Bool => Ok(smtlib::Bool::sort()),
        ast::Type::Domain { name, .. } => Ok(smtlib::sorts::Sort::new(name.to_string())),
        ast::Type::Unknown { name } => bail!("unknown type: {name}"),
        ast::Type::Error => bail!("type error"),
        _ => bail!("..."),
    }
}

fn smt_expr(expr: &Expr) -> Result<smtlib::terms::Dynamic> {
    match &expr.kind {
        ExprKind::Bool(t) => Ok(smtlib::Bool::from(*t).into_dynamic()),
        ExprKind::Num(n) => Ok(smtlib::Int::from(*n).into_dynamic()),
        ExprKind::Ident(i) => Ok(smt_sort(&expr.ty)?.new_const(i.to_string()).into_dynamic()),
        ExprKind::Old(_) => todo!(),
        ExprKind::Infix(l, op, r) => {
            let l = smt_expr(l)?;
            let r = smt_expr(r)?;
            Ok(match op {
                ast::Op::Mul => (l.as_int()? * r.as_int()?).into_dynamic(),
                ast::Op::Div => (l.as_int()? / r.as_int()?).into_dynamic(),
                ast::Op::Mod => (l.as_int()? % r.as_int()?).into_dynamic(),
                ast::Op::Add => (l.as_int()? + r.as_int()?).into_dynamic(),
                ast::Op::Sub => (l.as_int()? - r.as_int()?).into_dynamic(),
                ast::Op::Lsh => (l.as_int()? << r.as_int()?).into_dynamic(),
                ast::Op::Rsh => (l.as_int()? >> r.as_int()?).into_dynamic(),
                ast::Op::Lt => l.as_int()?.lt(r.as_int()?).into_dynamic(),
                ast::Op::Le => l.as_int()?.le(r.as_int()?).into_dynamic(),
                ast::Op::Gt => l.as_int()?.gt(r.as_int()?).into_dynamic(),
                ast::Op::Ge => l.as_int()?.ge(r.as_int()?).into_dynamic(),
                ast::Op::Eq => l._eq(r).into_dynamic(),
                ast::Op::Ne => l._neq(r).into_dynamic(),
                ast::Op::And => (l.as_bool()? & r.as_bool()?).into_dynamic(),
                ast::Op::Or => (l.as_bool()? | r.as_bool()?).into_dynamic(),
                ast::Op::Imp => l.as_bool()?.implies(r.as_bool()?).into_dynamic(),
            })
        }
        ExprKind::Ite(c, t, e) => Ok(smt_expr(c)?.as_bool()?.ite(smt_expr(t)?, smt_expr(e)?)),
        ExprKind::Quantifier(q, vars, x) => {
            let vars: Result<Vec<_>> = vars
                .iter()
                .map(|var| {
                    let sort = smt_sort(&var.ty.1)?;
                    Ok(sort.new_const(var.name.to_string()))
                })
                .collect();
            match q {
                ast::Quantifier::Forall => {
                    Ok(smtlib::terms::forall(vars?, smt_expr(x)?.as_bool()?).into_dynamic())
                }
                ast::Quantifier::Exists => {
                    Ok(smtlib::terms::exists(vars?, smt_expr(x)?.as_bool()?).into_dynamic())
                }
                _ => todo!(),
            }
        }
        ExprKind::FunctionCall {
            fun_name,
            args,
            function,
        } => {
            let f = function.get().unwrap();

            let params: Result<Vec<_>> = f.args.iter().map(|arg| smt_sort(&arg.ty.1)).collect();
            let params = params?;
            let return_ty = smt_sort(&f.return_ty.1)?;
            let f = smtlib::funs::Fun::new(fun_name.to_string(), params, return_ty);

            let args: Result<Vec<_>> = args.iter().map(smt_expr).collect();
            let args = args?;
            Ok(f.call(&args)?)
        }
        ExprKind::Error => bail!("smt_expr of error expression"),
        _ => todo!(),
    }
}

fn assert_true_lint(cx: &mut slang_ui::Context, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Seq(c1, c2) => {
            assert_true_lint(cx, c1);
            assert_true_lint(cx, c2);
        }
        StmtKind::Assert(x) => {
            if let ExprKind::Bool(true) = &x.kind {
                cx.info(x.span, "asserting true is a bit silly, no?".to_string());
            }
        }
        StmtKind::Match { body } | StmtKind::Loop { body, .. } => {
            for case in &body.cases {
                assert_true_lint(cx, &case.stmt);
            }
        }
        StmtKind::For { body, .. } => {
            assert_true_lint(cx, &body.stmt);
        }

        _ => {}
    }
}

fn wp(cx: &slang_ui::Context, stmt: &Stmt, q: Expr) -> Expr {
    match &stmt.kind {
        StmtKind::Seq(c1, c2) => wp(cx, c1, wp(cx, c2, q)),
        StmtKind::Assert(x) => x & q,
        StmtKind::Assume(x) => x.imp(&q),
        StmtKind::Assignment { name, expr } => q.subst(|x| x.as_ident() == Some(&name.ident), expr),
        StmtKind::VarDefinition { name, ty: _, expr } => {
            if let Some(expr) = expr {
                q.subst(|x| x.as_ident() == Some(&name.ident), expr)
            } else {
                q
            }
        }
        StmtKind::Return { expr: Some(expr) } => q.subst(|x| x.is_result(), expr),
        StmtKind::Return { expr: None } => {
            cx.todo(stmt.span);
            Expr::error()
        }
        StmtKind::Match { body } => body
            .cases
            .iter()
            .map(|case| case.condition.imp(&wp(cx, &case.stmt, q.clone())))
            .reduce(|a, b| a & b)
            .unwrap_or(Expr::bool(true)),
        // TODO: this is not entirely correct
        StmtKind::Loop { invariants, body } => {
            let inv_conj = invariants
                .iter()
                .cloned()
                .reduce(|a, b| a * b)
                .unwrap_or(Expr::bool(true));

            let cases = body.cases.iter().map(|case| {
                (&case.condition & &inv_conj).imp(&wp(cx, &case.stmt, inv_conj.clone()))
            });

            let stopped = !body
                .cases
                .iter()
                .map(|c| c.condition.clone())
                .reduce(|a, b| a & b)
                .unwrap_or(Expr::bool(true));
            let after = invariants.iter().fold(stopped, |acc, x| acc & x);

            cases.rfold(after.imp(&q), |a, b| a & b)
        }
        StmtKind::For { name, .. } => {
            cx.warning(name.span, "WP of for-loops not yet implemented");
            q.clone()
        }
        _ => {
            cx.todo(stmt.span);
            Expr::error()
        }
    }
}

// fn subst(expr: &Expr, f: impl Fn(&Expr) -> bool + Copy, to: &Expr) -> Expr {
//     return expr.subst(f, to);

//     if f(expr) {
//         to.clone()
//     } else {
//         match &expr.kind {
//             ExprKind::Bool(_) => expr.clone(),
//             ExprKind::Num(_) => expr.clone(),
//             ExprKind::Ident(_) => expr.clone(),
//             ExprKind::Old(_) => Expr::error(),
//             ExprKind::Result => expr.clone(),
//             ExprKind::Infix(l, op, r) => subst(l, f, to).op(*op, &subst(r, f, to)),
//             ExprKind::Ite(c, a, b) => subst(c, f, to).ite(&subst(a, f, to), &subst(b, f, to)),
//             ExprKind::Quantifier(_, _, _) => todo!(),
//             ExprKind::FunctionCall {
//                 fun_name,
//                 args,
//                 function,
//             } => {
//                 // if let Some(fun) = function.get() {
//                 //     fun.
//                 // }
//                 Expr::error()
//             }
//             ExprKind::Error => Expr::error(),
//             _ => todo!(),
//         }
//     }
// }

#[tokio::main]
async fn main() {
    slang_ui::run(App).await;
}
