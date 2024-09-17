use crate::{
    ast::{Block, Case, Cases, Cmd, CmdKind, Expr, MethodRef, Name, Range, Type},
    Span,
};

impl Cmd {
    pub fn new(kind: CmdKind) -> Cmd {
        Cmd {
            span: kind.infer_span().unwrap_or_default(),
            kind,
        }
    }
    pub fn vardef(name: &Name, ty: &Type, expr: &Option<Expr>) -> Cmd {
        Cmd::new(CmdKind::VarDefinition {
            name: name.clone(),
            ty: (Span::default(), ty.clone()),
            expr: expr.clone(),
        })
    }
    pub fn assign(name: &Name, expr: &Expr) -> Cmd {
        Cmd::new(CmdKind::Assignment {
            name: name.clone(),
            expr: expr.clone(),
        })
    }
    pub fn _match(cases: &[Case]) -> Cmd {
        Cmd::new(CmdKind::Match {
            body: Cases {
                span: Span::default(),
                cases: cases.to_vec(),
            },
        })
    }
    pub fn _loop(invariants: &[Expr], variant: Option<Expr>, cases: &[Case]) -> Cmd {
        Cmd::new(CmdKind::Loop {
            invariants: invariants.to_vec(),
            variant: variant.clone(),
            body: Cases {
                span: Span::default(),
                cases: cases.to_vec(),
            },
        })
    }
    pub fn _for(
        name: &Name,
        range: &Range,
        invariants: &[Expr],
        variant: Option<Expr>,
        body: &Cmd,
    ) -> Cmd {
        Cmd::new(CmdKind::For {
            name: name.clone(),
            range: range.clone(),
            invariants: invariants.to_vec(),
            variant: variant.clone(),
            body: Block {
                span: Span::default(),
                cmd: Box::new(body.clone()),
            },
        })
    }
    pub fn _break() -> Cmd {
        Cmd::new(CmdKind::Break)
    }
    pub fn _continue() -> Cmd {
        Cmd::new(CmdKind::Continue)
    }
    pub fn _return(expr: &Option<Expr>) -> Cmd {
        Cmd::new(CmdKind::Return { expr: expr.clone() })
    }
    pub fn assume(condition: &Expr) -> Cmd {
        Cmd::new(CmdKind::Assume {
            condition: condition.clone(),
        })
    }
    pub fn assert(condition: &Expr, message: &str) -> Cmd {
        Cmd::new(CmdKind::Assert {
            condition: condition.clone(),
            message: message.into(),
        })
    }
    pub fn seq(&self, other: &Cmd) -> Cmd {
        Cmd::new(CmdKind::Seq(
            Box::new(self.clone()),
            Box::new(other.clone()),
        ))
    }
    pub fn seqs(cmds: &[Cmd]) -> Cmd {
        cmds.iter()
            .cloned()
            .reduce(|a, b| Cmd::seq(&a, &b))
            .unwrap_or(Cmd::nop())
    }
    pub fn methodcall(
        name: &Option<Name>,
        fun_name: &Name,
        args: &[Expr],
        method: &MethodRef,
    ) -> Cmd {
        Cmd::new(CmdKind::MethodCall {
            name: name.clone(),
            fun_name: fun_name.clone(),
            args: args.to_vec(),
            method: method.clone(),
        })
    }
    pub fn nop() -> Cmd {
        Cmd::assume(&Expr::bool(true))
    }

    pub fn assigned_vars(self) -> Vec<(Name, Type)> {
        match &self.kind {
            CmdKind::VarDefinition { name, ty, .. } => vec![(name.clone(), ty.1.clone())],
            CmdKind::MethodCall {
                name: Some(name),
                method,
                ..
            } => vec![(
                name.clone(),
                method.get().unwrap().return_ty.clone().unwrap().1,
            )],
            CmdKind::Assignment { name, expr } => vec![(name.clone(), expr.ty.clone())],
            CmdKind::Match { body } | CmdKind::Loop { body, .. } => body
                .cases
                .iter()
                .flat_map(|case| case.clone().cmd.assigned_vars())
                .collect(),
            CmdKind::For { name, body, .. } => {
                let mut vars = body.clone().cmd.assigned_vars();
                vars.push((name.clone(), Type::Int));
                vars
            }
            CmdKind::Break
            | CmdKind::Continue
            | CmdKind::Assume { .. }
            | CmdKind::Assert { .. }
            | CmdKind::MethodCall { name: None, .. }
            | CmdKind::Return { .. } => vec![],
            CmdKind::Seq(s1, s2) => {
                let mut vars1 = s1.clone().assigned_vars();
                let vars2 = s2.clone().assigned_vars();
                vars1.extend(vars2);
                vars1
            }
        }
    }
}

impl CmdKind {
    fn infer_span(&self) -> Option<Span> {
        Some(match self {
            CmdKind::VarDefinition { name, ty, expr } => {
                if let Some(expr) = expr {
                    name.span.union(ty.0).union(expr.span)
                } else {
                    name.span.union(ty.0)
                }
            }
            CmdKind::Assignment { name, expr } => name.span.union(expr.span),
            CmdKind::Match { body } => body.span,
            CmdKind::Loop {
                invariants,
                variant,
                body,
            } => invariants
                .iter()
                .chain(variant.iter())
                .map(|spec| spec.span)
                .fold(body.span, Span::union),
            CmdKind::For {
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
            CmdKind::Break => return None,
            CmdKind::Continue => return None,
            CmdKind::Return { expr } => return expr.as_ref().map(|e| e.span),
            CmdKind::Assume { condition } | CmdKind::Assert { condition, .. } => condition.span,
            CmdKind::Seq(c1, c2) => c1.span.union(c2.span),
            CmdKind::MethodCall {
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
