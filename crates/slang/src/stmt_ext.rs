use crate::{
    ast::{Block, Case, Cases, Expr, MethodRef, Name, Range, Stmt, StmtKind, Type},
    Span,
};

impl Stmt {
    pub fn new(kind: StmtKind) -> Stmt {
        Stmt {
            span: kind.infer_span().unwrap_or_default(),
            kind,
        }
    }
    pub fn vardef(name: &Name, ty: &Type, expr: &Option<Expr>) -> Stmt {
        Stmt::new(StmtKind::VarDefinition {
            name: name.clone(),
            ty: (Span::default(), ty.clone()),
            expr: expr.clone(),
        })
    }
    pub fn assign(name: &Name, expr: &Expr) -> Stmt {
        Stmt::new(StmtKind::Assignment {
            name: name.clone(),
            expr: expr.clone(),
        })
    }
    pub fn _match(cases: &Vec<Case>) -> Stmt {
        Stmt::new(StmtKind::Match {
            body: Cases {
                span: Span::default(),
                cases: cases.clone(),
            },
        })
    }
    pub fn _loop(invariants: &Vec<Expr>, variant: Option<Expr>, cases: &Vec<Case>) -> Stmt {
        Stmt::new(StmtKind::Loop {
            invariants: invariants.clone(),
            variant: variant.clone(),
            body: Cases {
                span: Span::default(),
                cases: cases.clone(),
            },
        })
    }
    pub fn _for(
        name: &Name,
        range: &Range,
        invariants: &Vec<Expr>,
        variant: Option<Expr>,
        body: &Stmt,
    ) -> Stmt {
        Stmt::new(StmtKind::For {
            name: name.clone(),
            range: range.clone(),
            invariants: invariants.clone(),
            variant: variant.clone(),
            body: Block {
                span: Span::default(),
                stmt: Box::new(body.clone()),
            },
        })
    }
    pub fn _break() -> Stmt {
        Stmt::new(StmtKind::Break)
    }
    pub fn _continue() -> Stmt {
        Stmt::new(StmtKind::Continue)
    }
    pub fn _return(expr: &Option<Expr>) -> Stmt {
        Stmt::new(StmtKind::Return { expr: expr.clone() })
    }
    pub fn assume(condition: &Expr) -> Stmt {
        Stmt::new(StmtKind::Assume {
            condition: condition.clone(),
        })
    }
    pub fn assert(condition: &Expr, message: &str) -> Stmt {
        Stmt::new(StmtKind::Assert {
            condition: condition.clone(),
            message: message.into(),
        })
    }
    pub fn seq(&self, other: &Stmt) -> Stmt {
        Stmt::new(StmtKind::Seq(
            Box::new(self.clone()),
            Box::new(other.clone()),
        ))
    }
    pub fn seqs(stmts: &Vec<Stmt>) -> Stmt {
        stmts
            .iter()
            .cloned()
            .reduce(|a, b| Stmt::seq(&a, &b))
            .unwrap_or(Stmt::nop())
    }
    pub fn methodcall(
        name: &Option<Name>,
        fun_name: &Name,
        args: &Vec<Expr>,
        method: &MethodRef,
    ) -> Stmt {
        Stmt::new(StmtKind::MethodCall {
            name: name.clone(),
            fun_name: fun_name.clone(),
            args: args.clone(),
            method: method.clone(),
        })
    }
    pub fn nop() -> Stmt {
        Stmt::assume(&Expr::bool(true))
    }

    pub fn assigned_vars(self) -> Vec<(Name, Type)> {
        match &self.kind {
            StmtKind::VarDefinition { name, ty, .. } => vec![(name.clone(), ty.1.clone())],
            StmtKind::MethodCall {
                name: Some(name),
                method,
                ..
            } => vec![(
                name.clone(),
                method.get().unwrap().return_ty.clone().unwrap().1,
            )],
            StmtKind::Assignment { name, expr } => vec![(name.clone(), expr.ty.clone())],
            StmtKind::Match { body } | StmtKind::Loop { body, .. } => body
                .cases
                .iter()
                .map(|case| case.clone().stmt.assigned_vars())
                .flatten()
                .collect(),
            StmtKind::For { name, body, .. } => {
                let mut vars = body.clone().stmt.assigned_vars();
                vars.push((name.clone(), Type::Int));
                vars
            }
            StmtKind::Break
            | StmtKind::Continue
            | StmtKind::Assume { .. }
            | StmtKind::Assert { .. }
            | StmtKind::MethodCall { name: None, .. }
            | StmtKind::Return { .. } => vec![],
            StmtKind::Seq(s1, s2) => {
                let mut vars1 = s1.clone().assigned_vars();
                let vars2 = s2.clone().assigned_vars();
                vars1.extend(vars2);
                vars1
            }
        }
    }
}

impl StmtKind {
    fn infer_span(&self) -> Option<Span> {
        Some(match self {
            StmtKind::VarDefinition { name, ty, expr } => {
                if let Some(expr) = expr {
                    name.span.union(ty.0).union(expr.span)
                } else {
                    name.span.union(ty.0)
                }
            }
            StmtKind::Assignment { name, expr } => name.span.union(expr.span),
            StmtKind::Match { body } => body.span,
            StmtKind::Loop {
                invariants,
                variant,
                body,
            } => invariants
                .iter()
                .chain(variant.iter())
                .map(|spec| spec.span)
                .fold(body.span, Span::union),
            StmtKind::For {
                name,
                range: _,
                invariants,
                variant,
                body,
            } => invariants
                .iter()
                .chain(variant.iter())
                .map(|spec| spec.span)
                .fold(name.span.union(body.span), Span::union),
            StmtKind::Break => return None,
            StmtKind::Continue => return None,
            StmtKind::Return { expr } => return expr.as_ref().map(|e| e.span),
            StmtKind::Assume { condition } | StmtKind::Assert { condition, .. } => condition.span,
            StmtKind::Seq(c1, c2) => c1.span.union(c2.span),
            StmtKind::MethodCall {
                name,
                fun_name,
                args,
                method: _,
            } => args.iter().map(|arg| arg.span).fold(
                if let Some(name) = name {
                    name.span.union(fun_name.span)
                } else {
                    fun_name.span
                },
                Span::union,
            ),
        })
    }
}
