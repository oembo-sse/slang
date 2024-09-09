use std::sync::{Arc, Weak};

use crate::{Items, Span};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn prefix(&self, pre: String) -> Ident {
        Ident(pre + "_" + self.as_str())
    }

    pub fn postfix(&self, post: String) -> Ident {
        Ident(self.to_string() + "_" + &post)
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name {
    pub span: Span,
    pub ident: Ident,
}

impl Name {
    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }

    pub fn prefix(&self, pre: String) -> Name {
        Name {
            ident: self.ident.prefix(pre),
            ..self.clone()
        }
    }

    pub fn postfix(&self, post: String) -> Name {
        Name {
            ident: self.ident.postfix(post),
            ..self.clone()
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub span: Span,
    pub name: Name,
    pub ty: (Span, Type),
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub ty: Type,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum ExprKind {
    Bool(bool),
    Num(i64),
    Ident(Ident),
    Old(Name),
    Prefix(PrefixOp, Box<Expr>),
    Result,
    Infix(Box<Expr>, Op, Box<Expr>),
    Ite(Box<Expr>, Box<Expr>, Box<Expr>),
    Quantifier(Quantifier, Vec<Var>, Box<Expr>),
    FunctionCall {
        fun_name: Name,
        args: Vec<Expr>,
        function: FunctionRef,
    },
    Error,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Quantifier {
    Forall,
    Exists,
}

macro_rules! def_op {
    ($([$s:literal, $name:ident, $prec:literal],)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Op {
            $($name,)*
        }
        impl Op {
            pub fn prec(self) -> usize {
                match self {
                    $(Op::$name => $prec,)*
                }
            }
        }
        impl std::fmt::Display for Op {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let s = match self {
                    $(Op::$name => $s,)*
                };
                s.fmt(f)
            }
        }
    };
}

def_op!(
    ["*", Mul, 3],
    ["/", Div, 3],
    ["%", Mod, 3],
    ["+", Add, 4],
    ["-", Sub, 4],
    ["<<", Lsh, 5],
    [">>", Rsh, 5],
    ["<", Lt, 6],
    ["<=", Le, 6],
    [">", Gt, 6],
    [">=", Ge, 6],
    ["==", Eq, 7],
    ["!=", Ne, 7],
    ["&&", And, 11],
    ["||", Or, 11],
    ["==>", Imp, 13],
);

macro_rules! def_prefix_op {
    ($([$s:literal, $name:ident],)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum PrefixOp {
            $($name,)*
        }
        impl PrefixOp {
            pub const fn lit(self) -> &'static str {
                match self {
                    $(PrefixOp::$name => $s,)*
                }
            }
        }
        impl std::fmt::Display for PrefixOp {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let s = match self {
                    $(PrefixOp::$name => $s,)*
                };
                s.fmt(f)
            }
        }
    };
}

def_prefix_op!(["-", Neg], ["!", Not],);

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unresolved,
    Int,
    Bool,
    Domain { name: Name, domain: DomainRef },
    Unknown { name: Name },
    Error,
}

impl Type {
    pub fn is_error_or_unknown(&self) -> bool {
        matches!(self, Type::Unresolved | Type::Error)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Unresolved => "unresolved",
            Type::Int => "Int",
            Type::Bool => "Bool",
            Type::Domain { name, domain } => return domain.fmt(f),
            Type::Unknown { name } => &name.ident.0,
            Type::Error => "error",
        };
        s.fmt(f)
    }
}

impl Expr {
    pub(crate) fn new(span: Span, kind: ExprKind) -> Expr {
        Expr {
            span,
            kind,
            ty: Type::Unresolved,
        }
    }
    pub fn error() -> Expr {
        Expr {
            span: Span::default(),
            kind: ExprKind::Error,
            ty: Type::Error,
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum StmtKind {
    VarDefinition {
        name: Name,
        ty: (Span, Type),
        expr: Option<Expr>,
    },
    Assignment {
        name: Name,
        expr: Expr,
    },
    Match {
        body: Cases,
    },
    Loop {
        invariants: Vec<Expr>,
        body: Cases,
    },
    For {
        name: Name,
        range: Range,
        invariants: Vec<Expr>,
        body: Block,
    },

    Break,
    Continue,
    Return {
        expr: Option<Expr>,
    },

    Assume(Expr),
    Assert(Expr),

    Seq(Box<Stmt>, Box<Stmt>),

    MethodCall {
        name: Option<Name>,
        fun_name: Name,
        args: Vec<Expr>,
        method: MethodRef,
    },
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Cases {
    pub span: Span,
    pub cases: Vec<Case>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Case {
    pub condition: Expr,
    pub stmt: Stmt,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Range {
    FromTo(Expr, Expr),
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub stmt: Box<Stmt>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Specification {
    Requires { span: Span, expr: Expr },
    Ensures { span: Span, expr: Expr },
    Modifies { span: Span, name: Name },
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Method {
    pub span: Span,
    pub name: Name,
    pub args: Vec<Var>,
    pub return_ty: Option<(Span, Type)>,
    pub specifications: Vec<Specification>,
    pub body: Block,
}

impl Method {
    pub fn requires(&self) -> impl Iterator<Item = &Expr> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Requires { expr, .. } => Some(expr),
            _ => None,
        })
    }
    pub fn ensures(&self) -> impl Iterator<Item = &Expr> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Ensures { expr, .. } => Some(expr),
            _ => None,
        })
    }
    pub fn modifies(&self) -> impl Iterator<Item = &Name> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Modifies { name, .. } => Some(name),
            _ => None,
        })
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct MethodRef(pub(crate) Ref);

#[derive(Debug, Default, Clone)]
pub(crate) enum Ref {
    #[default]
    Unresolved,
    Resolved(Ident, Weak<Items>),
}

impl std::hash::Hash for Ref {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Ref::Unresolved => {}
            Ref::Resolved(id, ptr) => {
                id.hash(state);
                (ptr.as_ptr() as usize).hash(state);
            }
        }
    }
}

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Resolved(l_id, l_items), Self::Resolved(r_id, r_items)) => {
                l_id == r_id && Weak::ptr_eq(l_items, r_items)
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for Ref {}

impl std::fmt::Debug for MethodRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get() {
            Some(method) => f
                .debug_tuple("MethodRef")
                .field(&method.name)
                .field(&method.args)
                .finish(),
            None => f.debug_tuple("MethodRef").field(&"??").finish(),
        }
    }
}

impl Ref {
    fn get(&self) -> Option<(&Ident, Arc<Items>)> {
        match self {
            Ref::Unresolved => None,
            Ref::Resolved(id, items) => Some((id, items.upgrade()?)),
        }
    }

    fn ident(&self) -> Option<&Ident> {
        match self {
            Ref::Unresolved => None,
            Ref::Resolved(ident, _) => Some(ident),
        }
    }
}

impl MethodRef {
    pub fn get(&self) -> Option<Arc<Method>> {
        let (id, items) = self.0.get()?;
        items.method(id)
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub name: Name,
    pub args: Vec<Var>,
    pub return_ty: (Span, Type),
    pub body: Option<Expr>,
    pub specifications: Vec<Specification>,
}

impl Function {
    pub fn requires(&self) -> impl Iterator<Item = &Expr> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Requires { expr, .. } => Some(expr),
            _ => None,
        })
    }
    pub fn ensures(&self) -> impl Iterator<Item = &Expr> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Ensures { expr, .. } => Some(expr),
            _ => None,
        })
    }
    pub fn modifies(&self) -> impl Iterator<Item = &Name> {
        self.specifications.iter().filter_map(|s| match s {
            Specification::Modifies { name, .. } => Some(name),
            _ => None,
        })
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct FunctionRef(pub(crate) Ref);

impl FunctionRef {
    pub fn get(&self) -> Option<Arc<Function>> {
        let (id, items) = self.0.get()?;
        items.function(id)
    }
}

impl std::fmt::Debug for FunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get() {
            Some(function) => f
                .debug_tuple("FunctionRef")
                .field(&function.name)
                .field(&function.args)
                .finish(),
            None => f.debug_tuple("FunctionRef").field(&"??").finish(),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Global {
    pub span: Span,
    pub var: Var,
    pub init: Option<Expr>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Domain {
    pub span: Span,
    pub name: Name,
    pub items: Vec<DomainItem>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum DomainItem {
    Function(Function),
    Axiom(DomainAxiom),
}

impl Domain {
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            DomainItem::Function(fun) => Some(fun),
            _ => None,
        })
    }
    pub fn axioms(&self) -> impl Iterator<Item = &DomainAxiom> {
        self.items.iter().filter_map(|item| match item {
            DomainItem::Axiom(ax) => Some(ax),
            _ => None,
        })
    }
}

// #[non_exhaustive]
// #[derive(Debug, Clone)]
// pub struct DomainFunction {
//     pub span: Span,
//     pub name: Name,
//     pub args: Vec<Var>,
//     pub return_ty: (Span, Type),
//     pub specifications: Vec<Specification>,
// }

// impl DomainFunction {
//     pub fn requires(&self) -> impl Iterator<Item = &Expr> {
//         self.specifications.iter().filter_map(|s| match s {
//             Specification::Requires { expr, .. } => Some(expr),
//             _ => None,
//         })
//     }
//     pub fn ensures(&self) -> impl Iterator<Item = &Expr> {
//         self.specifications.iter().filter_map(|s| match s {
//             Specification::Ensures { expr, .. } => Some(expr),
//             _ => None,
//         })
//     }
//     pub fn modifies(&self) -> impl Iterator<Item = &Name> {
//         self.specifications.iter().filter_map(|s| match s {
//             Specification::Modifies { name, .. } => Some(name),
//             _ => None,
//         })
//     }
// }

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct DomainAxiom {
    pub span: Span,
    pub expr: Expr,
}

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct DomainRef(pub(crate) Ref);

impl DomainRef {
    pub fn ident(&self) -> Option<&Ident> {
        self.0.ident()
    }
    pub fn get(&self) -> Option<Arc<Domain>> {
        let (id, items) = self.0.get()?;
        items.domain(id)
    }
}

impl std::fmt::Debug for DomainRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ident() {
            Some(ident) => f.debug_tuple("DomainRef").field(ident).finish(),
            None => f.debug_tuple("DomainRef").field(&"??").finish(),
        }
    }
}
impl std::fmt::Display for DomainRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ident() {
            Some(ident) => write!(f, "domain {ident}"),
            None => write!(f, "domain ???"),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Item {
    Method(Method),
    Function(Function),
    Global(Global),
    Domain(Domain),
}

#[non_exhaustive]
pub struct File {
    pub items: Vec<Item>,
}
