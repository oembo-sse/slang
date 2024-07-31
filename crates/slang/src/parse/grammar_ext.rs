use crate::{
    ast::{Expr, ExprKind, FunctionRef, Name, Op, PrefixOp, Quantifier, Stmt, StmtKind, Var},
    Span,
};

impl Expr {
    pub(crate) fn parse_function_call((span, (fun_name, args)): (Span, (Name, Vec<Expr>))) -> Expr {
        Expr::new(
            span,
            ExprKind::FunctionCall {
                fun_name,
                args,
                function: FunctionRef::default(),
            },
        )
    }
    pub(crate) fn parse_op(op: Op, (span, (lhs, rhs)): (Span, (Expr, Expr))) -> Expr {
        Expr::new(span, ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)))
    }
    pub(crate) fn parse_prefix(op: PrefixOp, (span, expr): (Span, Expr)) -> Expr {
        Expr::new(span, ExprKind::Prefix(op, Box::new(expr)))
    }
    pub(crate) fn parse_ite((span, (cond, l, r)): (Span, (Expr, Expr, Expr))) -> Expr {
        Expr::new(
            span,
            ExprKind::Ite(Box::new(cond), Box::new(l), Box::new(r)),
        )
    }
    pub(crate) fn parse_quantifier(
        q: Quantifier,
        (span, (vars, expr)): (Span, (Vec<Var>, Expr)),
    ) -> Expr {
        Expr::new(span, ExprKind::Quantifier(q, vars, Box::new(expr)))
    }
}

impl StmtKind {
    pub(crate) fn parsed(self, start: usize, end: usize) -> Stmt {
        Stmt {
            span: Span::from_start_end(start, end),
            kind: self,
        }
    }
}
