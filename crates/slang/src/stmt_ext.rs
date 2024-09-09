use crate::{
    ast::{Stmt, StmtKind},
    Span,
};

impl Stmt {
    pub fn new(kind: StmtKind) -> Stmt {
        Stmt {
            span: kind.infer_span().unwrap_or_default(),
            kind,
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
                specifications,
                body,
            } => specifications
                .iter()
                .map(|spec| spec.span())
                .fold(body.span, Span::union),
            StmtKind::For {
                name,
                range: _,
                specifications,
                body,
            } => specifications
                .iter()
                .map(|spec| spec.span())
                .fold(name.span.union(body.span), Span::union),
            StmtKind::Break => return None,
            StmtKind::Continue => return None,
            StmtKind::Return { expr } => return expr.as_ref().map(|e| e.span),
            StmtKind::Assume(e) => e.span,
            StmtKind::Assert(e, _) => e.span,
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
