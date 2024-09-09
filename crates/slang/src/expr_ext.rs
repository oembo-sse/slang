use std::borrow::Cow;

use itertools::Itertools;

use crate::{
    ast::{Expr, ExprKind, FunctionRef, Ident, Name, Op, PrefixOp, Quantifier, Type, Var},
    Span,
};

macro_rules! impl_binop {
    ($name:ident, $Name:ident, $Trait:ident, $trait_fn:ident) => {
        impl Expr {
            pub fn $name(&self, other: &Expr) -> Expr {
                Expr::op(self, Op::$Name, other)
            }
        }
        impl std::ops::$Trait<Expr> for Expr {
            type Output = Expr;

            fn $trait_fn(self, rhs: Expr) -> Self::Output {
                Expr::$name(&self, &rhs)
            }
        }
        impl std::ops::$Trait<&Expr> for Expr {
            type Output = Expr;

            fn $trait_fn(self, rhs: &Expr) -> Self::Output {
                Expr::$name(&self, rhs)
            }
        }
        impl std::ops::$Trait<&Expr> for &Expr {
            type Output = Expr;

            fn $trait_fn(self, rhs: &Expr) -> Self::Output {
                Expr::$name(self, rhs)
            }
        }
        impl std::ops::$Trait<Expr> for &Expr {
            type Output = Expr;

            fn $trait_fn(self, rhs: Expr) -> Self::Output {
                Expr::$name(self, &rhs)
            }
        }
    };
}

impl_binop!(add, Add, Add, add);
impl_binop!(sub, Sub, Sub, sub);
impl_binop!(mul, Mul, Mul, mul);
impl_binop!(div, Div, Div, div);
impl_binop!(and, And, BitAnd, bitand);
impl_binop!(or, Or, BitOr, bitor);

impl std::ops::Not for Expr {
    type Output = Expr;

    fn not(self) -> Self::Output {
        self.prefix(PrefixOp::Not)
    }
}
impl std::ops::Neg for Expr {
    type Output = Expr;

    fn neg(self) -> Self::Output {
        self.prefix(PrefixOp::Neg)
    }
}

impl Expr {
    pub fn op(&self, op: Op, rhs: &Expr) -> Expr {
        let lhs = self;
        let ty = match op {
            // TODO: this should be updated if we want to support multiple integer types
            Op::Mul | Op::Div | Op::Mod | Op::Add | Op::Sub | Op::Lsh | Op::Rsh => Type::Int,
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Ne | Op::And | Op::Or | Op::Imp => {
                Type::Bool
            }
        };
        Expr {
            span: lhs.span.union(rhs.span),
            ty,
            kind: ExprKind::Infix(Box::new(lhs.clone()), op, Box::new(rhs.clone())),
        }
    }
    pub fn prefix(&self, op: PrefixOp) -> Expr {
        let ty = match op {
            PrefixOp::Neg => Type::Int,
            PrefixOp::Not => Type::Bool,
        };
        Expr {
            span: self.span,
            ty,
            kind: ExprKind::Prefix(op, Box::new(self.clone())),
        }
    }
    pub fn num(value: i64) -> Expr {
        Expr {
            span: Span::default(),
            kind: ExprKind::Num(value),
            ty: Type::Int,
        }
    }
    pub fn bool(value: bool) -> Expr {
        Expr {
            span: Span::default(),
            kind: ExprKind::Bool(value),
            ty: Type::Bool,
        }
    }
    pub fn ide(ide: Ident, ty: &Type) -> Expr {
        Expr {
            span: Span::default(),
            ty: ty.clone(),
            kind: ExprKind::Ident(ide),
        }
    }
    pub fn imp(&self, other: &Expr) -> Expr {
        Expr::op(self, Op::Imp, other)
    }
    pub fn ite(&self, then: &Expr, otherwise: &Expr) -> Expr {
        Expr {
            span: self.span.union(then.span).union(otherwise.span),
            kind: ExprKind::Ite(
                Box::new(self.clone()),
                Box::new(then.clone()),
                Box::new(otherwise.clone()),
            ),
            ty: then.ty.clone(),
        }
    }
    pub fn quantifier(q: Quantifier, vars: &[Var], body: &Expr) -> Expr {
        Expr {
            span: vars
                .iter()
                .map(|v| v.span)
                .chain(std::iter::once(body.span))
                .reduce(|a, b| a.union(b))
                .unwrap_or_default(),
            kind: ExprKind::Quantifier(q, vars.to_vec(), Box::new(body.clone())),
            ty: Type::Bool,
        }
    }
    pub fn call(name: Name, args: Vec<Expr>, fun_ref: FunctionRef) -> Expr {
        let ty = if let Some(fun) = fun_ref.get() {
            fun.return_ty.1.clone()
        } else {
            Type::Error
        };
        Expr {
            span: args
                .iter()
                .map(|a| a.span)
                .chain(std::iter::once(name.span))
                .reduce(|a, b| a.union(b))
                .unwrap_or_default(),
            kind: ExprKind::FunctionCall {
                fun_name: name,
                args,
                function: fun_ref.clone(),
            },
            ty,
        }
    }
    pub fn subst(&self, mut f: impl FnMut(&Expr) -> bool, to: &Expr) -> Expr {
        self.pre_order_map(|x| f(x).then(|| to.clone()))
    }
    pub fn pre_order_map(&self, mut f: impl FnMut(&Expr) -> Option<Expr>) -> Expr {
        self.pre_order_map_impl(&mut f)
            .unwrap_or_else(|| self.clone())
    }
    fn pre_order_map_impl(&self, f: &mut dyn FnMut(&Expr) -> Option<Expr>) -> Option<Expr> {
        if let Some(x) = f(self) {
            return Some(x);
        }

        fn g<'a>(f: &mut dyn FnMut(&Expr) -> Option<Expr>, x: &'a Expr) -> Cow<'a, Expr> {
            match x.pre_order_map_impl(f) {
                Some(y) => Cow::Owned(y),
                None => Cow::Borrowed(x),
            }
        }

        match &self.kind {
            ExprKind::Bool(_) => None,
            ExprKind::Num(_) => None,
            ExprKind::Ident(_) => None,
            ExprKind::Old(_) => None,
            ExprKind::Result => None,
            ExprKind::Prefix(op, e) => match g(f, e) {
                Cow::Borrowed(_) => None,
                Cow::Owned(e) => Some(e.prefix(*op)),
            },
            ExprKind::Infix(l, op, r) => (g(f, l), g(f, r)).cow_up().map(|(l, r)| l.op(*op, &r)),
            ExprKind::Ite(c, t, o) => (g(f, c), g(f, t), g(f, o))
                .cow_up()
                .map(|(c, t, o)| c.ite(&t, &o)),
            ExprKind::Quantifier(q, vars, x) => match g(f, x) {
                Cow::Borrowed(_) => None,
                Cow::Owned(x) => Some(Expr::quantifier(*q, vars, &x)),
            },
            ExprKind::FunctionCall {
                fun_name,
                args,
                function,
            } => args
                .iter()
                .map(|a| g(f, a))
                .collect_vec()
                .cow_up()
                .map(|args| Expr::call(fun_name.clone(), args, function.clone())),
            ExprKind::Error => None,
        }
    }
    pub fn post_order_map(&self, mut f: impl FnMut(&Expr) -> Option<Expr>) -> Expr {
        self.post_order_map_impl(&mut f)
            .unwrap_or_else(|| self.clone())
    }
    fn post_order_map_impl(&self, f: &mut dyn FnMut(&Expr) -> Option<Expr>) -> Option<Expr> {
        fn g<'a>(f: &mut dyn FnMut(&Expr) -> Option<Expr>, x: &'a Expr) -> Cow<'a, Expr> {
            match x.post_order_map_impl(f) {
                Some(y) => Cow::Owned(y),
                None => Cow::Borrowed(x),
            }
        }

        let changed_inner = match &self.kind {
            ExprKind::Bool(_) => None,
            ExprKind::Num(_) => None,
            ExprKind::Ident(_) => None,
            ExprKind::Old(_) => None,
            ExprKind::Result => None,
            ExprKind::Infix(l, op, r) => (g(f, l), g(f, r)).cow_up().map(|(l, r)| l.op(*op, &r)),
            ExprKind::Prefix(op, e) => match g(f, e) {
                Cow::Borrowed(_) => None,
                Cow::Owned(e) => Some(e.prefix(*op)),
            },
            ExprKind::Ite(c, t, o) => (g(f, c), g(f, t), g(f, o))
                .cow_up()
                .map(|(c, t, o)| c.ite(&t, &o)),
            ExprKind::Quantifier(q, vars, x) => match g(f, x) {
                Cow::Borrowed(_) => None,
                Cow::Owned(x) => Some(Expr::quantifier(*q, vars, &x)),
            },
            ExprKind::FunctionCall {
                fun_name,
                args,
                function,
            } => args
                .iter()
                .map(|a| g(f, a))
                .collect_vec()
                .cow_up()
                .map(|args| Expr::call(fun_name.clone(), args, function.clone())),
            ExprKind::Error => None,
        };

        match changed_inner {
            Some(new) => f(&new),
            None => f(self),
        }
    }
}

trait CowUp {
    type Target;
    fn cow_up(self) -> Option<Self::Target>;
}

impl<'a, A: Clone, B: Clone> CowUp for (Cow<'a, A>, Cow<'a, B>) {
    type Target = (A, B);

    fn cow_up(self) -> Option<Self::Target> {
        matches!(self, (Cow::Owned(_), _) | (_, Cow::Owned(_)))
            .then(|| (self.0.into_owned(), self.1.into_owned()))
    }
}
impl<'a, A: Clone, B: Clone, C: Clone> CowUp for (Cow<'a, A>, Cow<'a, B>, Cow<'a, C>) {
    type Target = (A, B, C);

    fn cow_up(self) -> Option<Self::Target> {
        matches!(
            self,
            (Cow::Owned(_), _, _) | (_, Cow::Owned(_), _) | (_, _, Cow::Owned(_))
        )
        .then(|| {
            (
                self.0.into_owned(),
                self.1.into_owned(),
                self.2.into_owned(),
            )
        })
    }
}
impl<'a, T: Clone> CowUp for Vec<Cow<'a, T>> {
    type Target = Vec<T>;

    fn cow_up(self) -> Option<Self::Target> {
        if self.iter().any(|t| matches!(t, Cow::Owned(_))) {
            Some(self.into_iter().map(|t| t.into_owned()).collect())
        } else {
            None
        }
    }
}

impl Expr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match &self.kind {
            ExprKind::Ident(i) => Some(i),
            _ => None,
        }
    }
    pub fn is_result(&self) -> bool {
        matches!(&self.kind, ExprKind::Result)
    }
    pub fn is_ident(&self, i: &Ident) -> bool {
        matches!(&self.kind, ExprKind::Ident(x) if x == i)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Bool(b) => write!(f, "{b}"),
            ExprKind::Num(n) => write!(f, "{n}"),
            ExprKind::Ident(i) => write!(f, "{i}"),
            ExprKind::Old(o) => write!(f, "old({o})"),
            ExprKind::Result => write!(f, "result"),
            ExprKind::Prefix(op, e) => write!(f, "{op}{e}"),
            ExprKind::Infix(l, op, r) => write!(f, "({l} {op} {r})"),
            ExprKind::Ite(c, l, r) => write!(f, "({c} ? {l} : {r})"),
            ExprKind::Quantifier(q, args, body) => {
                write!(f, "{q} {} :: {body}", args.iter().format(", "))
            }
            ExprKind::FunctionCall { fun_name, args, .. } => {
                write!(f, "{fun_name}({})", args.iter().format(", "))
            }
            ExprKind::Error => write!(f, "error"),
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.1)
    }
}

impl std::fmt::Display for Quantifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantifier::Forall => write!(f, "forall"),
            Quantifier::Exists => write!(f, "exists"),
        }
    }
}
