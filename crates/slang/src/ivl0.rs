use crate::ast::{Expr, Name, Type};
use crate::Span;

#[derive(Debug, Clone)]
pub struct IVL0Stmt {
    pub span: Span,
    pub kind: IVL0StmtKind,
}

#[derive(Debug, Clone)]
pub enum IVL0StmtKind {
    Assignment { name: Name, expr: Expr },
    Havoc { name: Name, ty: Type },

    Assume(Expr),
    Assert(Expr, String),

    Seq(Box<IVL0Stmt>, Box<IVL0Stmt>),
    NonDet(Box<IVL0Stmt>, Box<IVL0Stmt>),
}
