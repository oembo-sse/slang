use crate::ast::{self, Expr, ExprKind, Function, Type};
use smtlib::Sorted;

use color_eyre::eyre::bail;
type Result<T, E = color_eyre::eyre::Error> = std::result::Result<T, E>;

pub fn smt_fun(f: &Function) -> Result<smtlib::funs::Fun> {
    let args = f
        .args
        .iter()
        .map(|arg| smt_sort(&arg.ty.1))
        .collect::<Result<Vec<_>>>()?;
    let return_ty = smt_sort(&f.return_ty.1)?;
    Ok(smtlib::funs::Fun::new(f.name.to_string(), args, return_ty))
}

pub fn smt_sort(ty: &Type) -> Result<smtlib::sorts::Sort> {
    match ty {
        Type::Unresolved => bail!("unknown type"),
        Type::Int => Ok(smtlib::Int::sort()),
        Type::Bool => Ok(smtlib::Bool::sort()),
        Type::Domain { name, .. } => Ok(smtlib::sorts::Sort::new(name.to_string())),
        Type::Unknown { name } => bail!("unknown type: {name}"),
        Type::Error => bail!("type error"),
    }
}

pub fn smt_expr(expr: &Expr) -> Result<smtlib::terms::Dynamic> {
    match &expr.kind {
        ExprKind::Bool(t) => Ok(smtlib::Bool::from(*t).into_dynamic()),
        ExprKind::Num(n) => Ok(smtlib::Int::from(*n).into_dynamic()),
        ExprKind::Ident(i) => Ok(smt_sort(&expr.ty)?.new_const(i.to_string()).into_dynamic()),
        ExprKind::Prefix(op, e) => {
            let e = smt_expr(e)?;
            Ok(match op {
                ast::PrefixOp::Neg => (-e.as_int()?).into_dynamic(),
                ast::PrefixOp::Not => (!e.as_bool()?).into_dynamic(),
            })
        }
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

            let args: Result<Vec<_>> = args.iter().map(|e| smt_expr(e)).collect();
            let args = args?;
            Ok(f.call(&args)?)
        }

        ExprKind::Error => bail!("smt_expr of error expression"),
        ExprKind::Result => bail!("smt_expr of result expression"),
        ExprKind::Old(_) => bail!("smt_expr of old expression"),
    }
}
