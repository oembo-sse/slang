use crate::ast::{self, Expr, ExprKind, Function, Type};
use smtlib::Sorted;

#[non_exhaustive]
#[derive(Debug, thiserror::Error)]
pub enum SmtError {
    #[error("type error")]
    TypeError,
    #[error("unresolved type")]
    Unresolved,
    #[error("unknown type: {name}")]
    UnknownType { name: String },
    #[error("error expression cannot be turned into smt")]
    ExprError,
    #[error("result expression cannot be turned into smt")]
    ResultToSmt,
    #[error("broke expression cannot be turned into smt")]
    BrokeToSmt,
    #[error("old expression cannot be turned into smt")]
    OldToSmt,
    #[error("smt error")]
    Smt(#[from] smtlib::Error),
}

impl Function {
    pub fn smt(&self) -> Result<smtlib::funs::Fun, SmtError> {
        let args = self
            .args
            .iter()
            .map(|arg| arg.ty.1.smt())
            .collect::<Result<Vec<_>, SmtError>>()?;
        let return_ty = self.return_ty.1.smt()?;
        Ok(smtlib::funs::Fun::new(
            self.name.to_string(),
            args,
            return_ty,
        ))
    }
}

impl Type {
    pub fn smt(&self) -> Result<smtlib::sorts::Sort, SmtError> {
        match self {
            Type::Unresolved => Err(SmtError::Unresolved),
            Type::Int => Ok(smtlib::Int::sort()),
            Type::Bool => Ok(smtlib::Bool::sort()),
            Type::Domain { name, .. } => Ok(smtlib::sorts::Sort::new(name.to_string())),
            Type::Unknown { name } => Err(SmtError::UnknownType {
                name: name.to_string(),
            }),
            Type::Error => Err(SmtError::TypeError),
        }
    }
}

impl Expr {
    pub fn smt(&self) -> Result<smtlib::terms::Dynamic, SmtError> {
        match &self.kind {
            ExprKind::Bool(t) => Ok(smtlib::Bool::from(*t).into_dynamic()),
            ExprKind::Num(n) => Ok(smtlib::Int::from(*n).into_dynamic()),
            ExprKind::Ident(i) => Ok(self.ty.smt()?.new_const(i.to_string()).into_dynamic()),
            ExprKind::Prefix(op, e) => {
                let e = e.smt()?;
                Ok(match op {
                    ast::PrefixOp::Neg => (-e.as_int()?).into_dynamic(),
                    ast::PrefixOp::Not => (!e.as_bool()?).into_dynamic(),
                })
            }
            ExprKind::Infix(l, op, r) => {
                let l = l.smt()?;
                let r = r.smt()?;
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
            ExprKind::Ite(c, t, e) => Ok(c.smt()?.as_bool()?.ite(t.smt()?, e.smt()?)),
            ExprKind::Quantifier(q, vars, x) => {
                let vars: Result<Vec<_>, SmtError> = vars
                    .iter()
                    .map(|var| {
                        let sort = var.ty.1.smt()?;
                        Ok(sort.new_const(var.name.to_string()))
                    })
                    .collect();
                match q {
                    ast::Quantifier::Forall => {
                        Ok(smtlib::terms::forall(vars?, x.smt()?.as_bool()?).into_dynamic())
                    }
                    ast::Quantifier::Exists => {
                        Ok(smtlib::terms::exists(vars?, x.smt()?.as_bool()?).into_dynamic())
                    }
                }
            }
            ExprKind::FunctionCall {
                fun_name,
                args,
                function,
            } => {
                let f = function.get().unwrap();

                let params: Result<Vec<_>, SmtError> =
                    f.args.iter().map(|arg| arg.ty.1.smt()).collect();
                let params = params?;
                let return_ty = f.return_ty.1.smt()?;
                let f = smtlib::funs::Fun::new(fun_name.to_string(), params, return_ty);

                let args: Result<Vec<_>, SmtError> = args.iter().map(|e| e.smt()).collect();
                let args = args?;
                Ok(f.call(&args)?)
            }

            ExprKind::Error => Err(SmtError::ExprError),
            ExprKind::Result => Err(SmtError::ResultToSmt),
            ExprKind::Broke => Err(SmtError::BrokeToSmt),
            ExprKind::Old(_) => Err(SmtError::OldToSmt),
        }
    }
}
