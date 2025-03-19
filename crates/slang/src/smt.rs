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
    pub fn smt<'st>(&self, st: &'st smtlib::Storage) -> Result<smtlib::funs::Fun<'st>, SmtError> {
        let args = self
            .args
            .iter()
            .map(|arg| arg.ty.1.smt(st))
            .collect::<Result<Vec<_>, SmtError>>()?;
        let return_ty = self.return_ty.1.smt(st)?;
        Ok(smtlib::funs::Fun::new(
            st,
            self.name.as_str(),
            args,
            return_ty,
        ))
    }
}

impl Type {
    pub fn smt<'st>(&self, st: &'st smtlib::Storage) -> Result<smtlib::sorts::Sort<'st>, SmtError> {
        match self {
            Type::Unresolved => Err(SmtError::Unresolved),
            Type::Int => Ok(smtlib::Int::sort()),
            Type::Bool => Ok(smtlib::Bool::sort()),
            Type::Domain { name, .. } => Ok(smtlib::sorts::Sort::new(st, name.as_str())),
            Type::Unknown { name } => Err(SmtError::UnknownType {
                name: name.to_string(),
            }),
            Type::Error => Err(SmtError::TypeError),
        }
    }
}

impl Expr {
    pub fn smt<'st>(
        &self,
        st: &'st smtlib::Storage,
    ) -> Result<smtlib::terms::Dynamic<'st>, SmtError> {
        match &self.kind {
            ExprKind::Bool(t) => Ok(smtlib::Bool::new(st, *t).into_dynamic()),
            ExprKind::Num(n) => Ok(smtlib::Int::new(st, *n).into_dynamic()),
            ExprKind::Ident(i) => Ok(self.ty.smt(st)?.new_const(st, i.as_str()).into_dynamic()),
            ExprKind::Prefix(op, e) => {
                let e = e.smt(st)?;
                Ok(match op {
                    ast::PrefixOp::Neg => (-e.as_int()?).into_dynamic(),
                    ast::PrefixOp::Not => (!e.as_bool()?).into_dynamic(),
                })
            }
            ExprKind::Infix(l, op, r) => {
                let l = l.smt(st)?;
                let r = r.smt(st)?;
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
            ExprKind::Ite(c, t, e) => Ok(c.smt(st)?.as_bool()?.ite(t.smt(st)?, e.smt(st)?)),
            ExprKind::Quantifier(q, vars, x) => {
                let vars: Result<Vec<_>, SmtError> = vars
                    .iter()
                    .map(|var| {
                        let sort = var.ty.1.smt(st)?;
                        Ok(sort.new_const(st, var.name.as_str()))
                    })
                    .collect();
                match q {
                    ast::Quantifier::Forall => {
                        Ok(smtlib::terms::forall(st, vars?, x.smt(st)?.as_bool()?).into_dynamic())
                    }
                    ast::Quantifier::Exists => {
                        Ok(smtlib::terms::exists(st, vars?, x.smt(st)?.as_bool()?).into_dynamic())
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
                    f.args.iter().map(|arg| arg.ty.1.smt(st)).collect();
                let params = params?;
                let return_ty = f.return_ty.1.smt(st)?;
                let f = smtlib::funs::Fun::new(st, fun_name.as_str(), params, return_ty);

                let args: Result<Vec<_>, SmtError> = args.iter().map(|e| e.smt(st)).collect();
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
