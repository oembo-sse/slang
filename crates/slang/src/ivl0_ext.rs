use crate::{
    ast::{Expr, Ident, Name, Type},
    ivl0::{IVL0Stmt, IVL0StmtKind},
    Span,
};

impl IVL0Stmt {
    pub fn assign(name: &Name, expr: &Expr) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Assignment {
                name: name.clone(),
                expr: expr.clone(),
            },
        }
    }
    pub fn seq(&self, other: &IVL0Stmt) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Seq(Box::new(self.clone()), Box::new(other.clone())),
        }
    }
    pub fn seqs(stmts: &Vec<IVL0Stmt>) -> IVL0Stmt {
        stmts
            .iter()
            .cloned()
            .reduce(|a, b| IVL0Stmt::seq(&a, &b))
            .unwrap_or(IVL0Stmt::nop())
    }
    pub fn nondet(&self, other: &IVL0Stmt) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::NonDet(Box::new(self.clone()), Box::new(other.clone())),
        }
    }
    pub fn nondets(stmts: &Vec<IVL0Stmt>) -> IVL0Stmt {
        stmts
            .iter()
            .cloned()
            .reduce(|a, b| IVL0Stmt::nondet(&a, &b))
            .unwrap_or(IVL0Stmt::unreachable())
    }
    pub fn assume(expr: &Expr) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Assume(expr.clone()),
        }
    }
    pub fn assert(expr: &Expr, span: &Span, message: &str) -> IVL0Stmt {
        IVL0Stmt {
            kind: IVL0StmtKind::Assert(expr.clone(), message.to_owned()),
            span: span.clone(),
        }
    }
    pub fn havoc(name: &Name, ty: &Type) -> IVL0Stmt {
        IVL0Stmt {
            kind: IVL0StmtKind::Havoc {
                name: name.clone(),
                ty: ty.clone(),
            },
            span: Span::default(),
        }
    }
    pub fn nop() -> IVL0Stmt {
        IVL0Stmt::assume(&Expr::bool(true))
    }
    pub fn unreachable() -> IVL0Stmt {
        IVL0Stmt::assume(&Expr::bool(false))
    }
}

impl std::fmt::Display for IVL0Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            IVL0StmtKind::Assignment { name, expr } => write!(f, "{name} := {expr}"),
            IVL0StmtKind::Havoc { name, .. } => write!(f, "havoc {name}"),
            IVL0StmtKind::Assume(e) => write!(f, "assume {e}"),
            IVL0StmtKind::Assert(e, _) => write!(f, "assert {e}"),
            IVL0StmtKind::Seq(c1, c2) => write!(f, "{c1} ; {c2}"),
            IVL0StmtKind::NonDet(c1, c2) => write!(f, "{{ {c1} }} [] {{ {c2} }}"),
        }
    }
}

impl Name {
    pub fn ide(ident: Ident) -> Name {
        Name {
            ident,
            span: Span::default(),
        }
    }
}
