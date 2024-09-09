use std::sync::Arc;

use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    ast::{
        Block, Case, Cases, Domain, DomainAxiom, DomainItem, DomainRef, Expr, ExprKind, File, Function, FunctionRef, Global, Ident, Item, LoopSpecification, Method, MethodRef, Name, Op, PrefixOp, Range, Specification, Stmt, StmtKind, Type, Var
    },
    Items, Span,
};

#[derive(Debug)]
pub struct Error {
    pub(crate) span: Span,
    pub(crate) msg: String,
}

impl Error {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn msg(&self) -> String {
        self.msg.to_string()
    }
}

#[derive(Clone)]
struct MethodSignature {
    name: Name,
    args: Vec<Var>,
    return_ty: Option<Type>,
}

#[derive(Clone)]
struct FunctionSignature {
    name: Name,
    args: Vec<Var>,
    return_ty: Type,
}

#[derive(Default)]
struct FileContext {
    errors: Vec<Error>,
    methods: IndexMap<Ident, (MethodSignature, MethodRef)>,
    functions: IndexMap<Ident, (FunctionSignature, FunctionRef)>,
    globals: IndexMap<Name, (Span, Type)>,
    domains: IndexMap<Name, DomainRef>,
}
struct BlockContext<'a> {
    file: &'a mut FileContext,
    scope: Vec<(Name, Type)>,
    def_end: usize,
    expected_return_ty: Option<(Span, Type)>,
    is_loop: bool,
}
impl FileContext {
    fn error(&mut self, span: Span, msg: String) {
        self.errors.push(Error { span, msg });
    }
    fn block_cx(&mut self) -> BlockContext {
        let scope = self
            .globals
            .iter()
            .map(|(name, (_, ty))| (name.clone(), ty.clone()))
            .collect_vec();
        BlockContext {
            file: self,
            def_end: scope.len(),
            scope,
            expected_return_ty: None,
            is_loop: false,
        }
    }

    fn resolve_ty(&mut self, span: Span, ty: &Type) -> Type {
        match ty {
            Type::Unknown { name } => {
                if let Some((_, domain)) = self.domains.iter().find(|(d, _)| d.ident == name.ident)
                {
                    Type::Domain {
                        name: name.clone(),
                        domain: domain.clone(),
                    }
                } else {
                    self.error(span, format!("unknown type `{name}`"));
                    Type::Unknown { name: name.clone() }
                }
            }
            Type::Unresolved => Type::Unresolved,
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Domain { name, domain } => Type::Domain {
                name: name.clone(),
                domain: domain.clone(),
            },
            Type::Error => Type::Error,
        }
    }

    fn resolve_vars(&mut self, vars: &[Var]) -> Vec<Var> {
        vars.iter().map(|var| self.resolve_var(var)).collect()
    }

    fn resolve_var(&mut self, var: &Var) -> Var {
        Var {
            span: var.span,
            name: var.name.clone(),
            ty: (var.ty.0, self.resolve_ty(var.ty.0, &var.ty.1)),
        }
    }
}
impl<'a> BlockContext<'a> {
    fn nest<T>(&mut self, f: impl FnOnce(&mut BlockContext<'a>) -> T) -> T {
        let len = self.scope.len();
        let res = f(self);
        self.scope.truncate(len);
        res
    }
    fn in_loop<T>(&mut self, f: impl FnOnce(&mut BlockContext<'a>) -> T) -> T {
        let len = self.scope.len();
        let was_loop = self.is_loop;
        self.is_loop = true;
        let res = f(self);
        self.is_loop = was_loop;
        self.scope.truncate(len);
        res
    }
    fn with_expected_return_ty(self, expected_return_ty: Option<(Span, Type)>) -> Self {
        Self {
            expected_return_ty,
            ..self
        }
    }

    fn lookup_method(&mut self, name: &Ident) -> Option<&(MethodSignature, MethodRef)> {
        self.file.methods.get(name)
    }
    fn lookup_function(&mut self, name: &Ident) -> Option<&(FunctionSignature, FunctionRef)> {
        self.file.functions.get(name)
    }
    #[must_use]
    fn register_vars(&mut self, vars: Vec<Var>) -> Vec<Var> {
        vars.into_iter().map(|var| self.register_var(var)).collect()
    }
    #[must_use]
    fn register_var(&mut self, var: Var) -> Var {
        let ty = self.resolve_ty(var.ty.0, &var.ty.1);
        self.scope.push((var.name.clone(), ty.clone()));
        Var {
            span: var.span,
            name: var.name,
            ty: (var.ty.0, ty),
        }
    }
    fn register(&mut self, name: Name, ty: Type) {
        self.scope.push((name, ty));
    }
    fn names_in_scope(&self) -> impl Iterator<Item = &Name> {
        self.file
            .globals
            .iter()
            .map(|(name, _)| name)
            .chain(self.scope.iter().map(|(name, _)| name))
    }
    fn lookup_global(&self, ident: &Ident) -> Option<(&Name, &Type)> {
        self.file
            .globals
            .iter()
            .find(|(name, _)| &name.ident == ident)
            .map(|(name, (_, ty))| (name, ty))
    }
    fn lookup_var(&self, ident: &Ident) -> Option<(&Name, &Type)> {
        self.scope
            .iter()
            .rev()
            .find(|(name, _)| &name.ident == ident)
            .map(|(name, ty)| (name, ty))
            .or_else(|| self.lookup_global(ident))
    }
    fn lookup_old_var(&self, ident: &Ident) -> Option<(&Name, &Type)> {
        self.scope[0..self.def_end]
            .iter()
            .rev()
            .find(|(name, _)| &name.ident == ident)
            .map(|(name, ty)| (name, ty))
            .or_else(|| self.lookup_global(ident))
    }
    fn var_ty(&mut self, span: Span, name: &Ident) -> Type {
        match self.lookup_var(name) {
            Some((_, ty)) => ty.clone(),
            None => {
                if let Some(suggestion) =
                    did_you_mean(name.as_str(), self.names_in_scope().map(|n| n.as_str())).first()
                {
                    self.error(
                        span,
                        format!(
                            "variable `{name}` not found in this scope. \
                             did you mean `{suggestion}`?"
                        ),
                    );
                } else {
                    self.error(span, format!("variable `{name}` not found in this scope"));
                }

                Type::Error
            }
        }
    }
    fn old_var_ty(&mut self, span: Span, name: &Ident) -> Type {
        match self.lookup_old_var(name) {
            Some((_, ty)) => ty.clone(),
            None => {
                self.error(
                    span,
                    format!("variable `{name}` not found in arguments or as a global"),
                );
                Type::Error
            }
        }
    }

    fn resolve_ty(&mut self, span: Span, ty: &Type) -> Type {
        self.file.resolve_ty(span, ty)
    }

    fn error(&mut self, span: Span, msg: String) {
        self.file.errors.push(Error { span, msg });
    }

    fn are_equal(&self, ty1: &Type, ty2: &Type) -> bool {
        if ty1.is_error_or_unknown() || ty2.is_error_or_unknown() {
            return true;
        }
        match (ty1, ty2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => true,
            (
                Type::Domain {
                    name: _,
                    domain: d1,
                },
                Type::Domain {
                    name: _,
                    domain: d2,
                },
            ) if d1 == d2 => true,
            _ => false,
        }
    }

    fn expect_cmp_equal(&mut self, span: Span, ty1: &Type, ty2: &Type) -> bool {
        if !self.are_equal(ty1, ty2) {
            self.error(span, format!("tried to compare `{ty1}` with `{ty2}`"));
            false
        } else {
            true
        }
    }

    fn expect_numeric(&mut self, span: Span, ty: &Type) -> bool {
        if ty.is_error_or_unknown() {
            return true;
        }
        if ty != &Type::Int {
            self.error(span, format!("expected `{}`, found `{ty}`", Type::Int));
            false
        } else {
            true
        }
    }
    fn expect_int(&mut self, span: Span, ty: &Type) -> bool {
        if ty.is_error_or_unknown() {
            return true;
        }
        if ty != &Type::Int {
            self.error(span, format!("expected `{}`, found `{ty}`", Type::Int));
            false
        } else {
            true
        }
    }

    fn expect_bool(&mut self, span: Span, ty: &Type) -> bool {
        if ty.is_error_or_unknown() {
            return true;
        }
        if ty != &Type::Bool {
            self.error(span, format!("expected `{}`, found `{ty}`", Type::Bool));
            false
        } else {
            true
        }
    }
    fn expect_single(&mut self, spans: Vec<Span>) -> bool {
        if spans.len() > 1 {
            let mut spans = spans;
            spans.reverse();
            self.error(spans[0], format!("expected a single instance, found multiple"));
            false
        } else {
            true
        }
    }

    fn expect_to_be(&mut self, span: Span, expected: &Type, actual: &Type) -> bool {
        if !self.are_equal(expected, actual) {
            self.error(span, format!("expected `{expected}` but found `{actual}`"));
            false
        } else {
            true
        }
    }

    fn check_args(&mut self, fun_name: &Name, def_args: &[Var], args: &[Expr]) {
        if def_args.len() != args.len() {
            self.error(
                fun_name.span,
                format!(
                    "function `{}` expected {} argument(s), but {} was given",
                    fun_name.ident,
                    def_args.len(),
                    args.len(),
                ),
            );
        }
        for (def, given) in def_args.iter().zip(args) {
            self.expect_to_be(given.span, &given.ty, &def.ty.1);
        }
    }

    fn mark_def_end(&mut self) {
        self.def_end = self.scope.len();
    }
}

impl Expr {
    fn tc(self, cx: &mut BlockContext) -> Expr {
        match self.kind {
            ExprKind::Error => Expr {
                ty: Type::Error,
                ..self
            },
            ExprKind::Num(_) => Expr {
                ty: Type::Int,
                ..self
            },
            ExprKind::Bool(_) => Expr {
                ty: Type::Bool,
                ..self
            },
            ExprKind::Ident(ident) => Expr {
                ty: cx.var_ty(self.span, &ident),
                kind: ExprKind::Ident(ident),
                ..self
            },
            ExprKind::Old(old_name) => Expr {
                ty: cx.old_var_ty(old_name.span, &old_name.ident),
                kind: ExprKind::Old(old_name),
                ..self
            },
            ExprKind::Result => Expr {
                ty: if let Some((_, return_ty)) = &cx.expected_return_ty {
                    return_ty.clone()
                } else {
                    // TODO: should we report an error here?
                    Type::Error
                },
                kind: ExprKind::Result,
                ..self
            },
            ExprKind::FunctionCall {
                fun_name,
                args,
                function: _,
            } => {
                let args = args.into_iter().map(|arg| arg.tc(cx)).collect_vec();
                if let Some((sig, fun)) = cx.lookup_function(&fun_name.ident).cloned() {
                    cx.check_args(&fun_name, &sig.args, &args);
                    return Expr {
                        ty: sig.return_ty,
                        kind: ExprKind::FunctionCall {
                            fun_name,
                            args,
                            function: fun,
                        },
                        ..self
                    };
                }

                if cx.lookup_method(&fun_name.ident).is_some() {
                    cx.error(
                        fun_name.span,
                        format!("no function named `{fun_name}` exists, but a method exists with that name")
                    );
                } else if let Some(suggestion) = did_you_mean(
                    &fun_name.ident.0,
                    cx.file.functions.keys().map(|ident| ident.as_str()),
                )
                .first()
                {
                    cx.error(
                        fun_name.span,
                        format!(
                            "no function named `{fun_name}` exists. did you mean `{suggestion}`?"
                        ),
                    );
                } else {
                    cx.error(
                        fun_name.span,
                        format!("no function named `{fun_name}` exists"),
                    );
                }
                Expr {
                    ty: Type::Unresolved,
                    kind: ExprKind::FunctionCall {
                        fun_name,
                        args,
                        function: FunctionRef::default(),
                    },
                    ..self
                }
            }
            ExprKind::Prefix(op, expr) => {
                let expr = expr.tc(cx);

                let ty = match op {
                    PrefixOp::Neg => {
                        cx.expect_numeric(expr.span, &expr.ty);
                        expr.ty.clone()
                    }
                    PrefixOp::Not => {
                        cx.expect_bool(expr.span, &expr.ty);
                        expr.ty.clone()
                    }
                };

                Expr {
                    kind: ExprKind::Prefix(op, Box::new(expr)),
                    ty,
                    ..self
                }
            }
            ExprKind::Infix(lhs, op, rhs) => {
                let lhs = lhs.tc(cx);
                let rhs = rhs.tc(cx);

                let ty = if op.is_equality() {
                    cx.expect_cmp_equal(rhs.span, &lhs.ty, &rhs.ty);
                    Type::Bool
                } else if op.is_numeric() {
                    if cx.expect_numeric(lhs.span, &lhs.ty) && cx.expect_numeric(rhs.span, &rhs.ty)
                    {
                        cx.expect_cmp_equal(rhs.span, &lhs.ty, &rhs.ty);
                    }
                    Type::Int
                } else if op.is_relational() {
                    cx.expect_int(lhs.span, &lhs.ty);
                    cx.expect_int(rhs.span, &rhs.ty);
                    Type::Bool
                } else if op.is_logical() {
                    cx.expect_bool(lhs.span, &lhs.ty);
                    cx.expect_bool(rhs.span, &rhs.ty);
                    Type::Bool
                } else {
                    unreachable!()
                };

                Expr {
                    kind: ExprKind::Infix(Box::new(lhs), op, Box::new(rhs)),
                    ty,
                    ..self
                }
            }
            ExprKind::Ite(cond, l, r) => {
                let cond = cond.tc(cx);

                cx.expect_bool(cond.span, &cond.ty);

                let l = l.tc(cx);
                let r = r.tc(cx);

                cx.expect_to_be(r.span, &l.ty, &r.ty);

                let ty = l.ty.clone();

                Expr {
                    kind: ExprKind::Ite(Box::new(cond), Box::new(l), Box::new(r)),
                    ty,
                    ..self
                }
            }
            ExprKind::Quantifier(q, vars, expr) => cx.nest(|cx| {
                let vars = cx.register_vars(vars);
                let expr = expr.tc(cx);
                cx.expect_bool(expr.span, &expr.ty);
                Expr {
                    ty: Type::Bool,
                    kind: ExprKind::Quantifier(q, vars, Box::new(expr)),
                    ..self
                }
            }),
            ExprKind::Broke => Expr {
                ty: Type::Bool,
                kind: ExprKind::Broke,
                ..self
            },
        }
    }
}

impl Op {
    fn is_numeric(self) -> bool {
        matches!(
            self,
            Op::Mul | Op::Div | Op::Mod | Op::Add | Op::Sub | Op::Lsh | Op::Rsh
        )
    }
    fn is_relational(self) -> bool {
        matches!(self, Op::Lt | Op::Le | Op::Gt | Op::Ge)
    }
    fn is_equality(self) -> bool {
        matches!(self, Op::Eq | Op::Ne)
    }
    fn is_logical(self) -> bool {
        matches!(self, Op::And | Op::Or | Op::Imp)
    }
}

impl Stmt {
    fn tc(self, cx: &mut BlockContext) -> Stmt {
        match self.kind {
            StmtKind::VarDefinition { name, ty, expr } => {
                let expr = expr.map(|expr| expr.tc(cx));
                let Var {
                    name,
                    ty: (ty_span, ty),
                    ..
                } = cx.register_var(Var {
                    span: name.span,
                    name: name.clone(),
                    ty,
                });
                let var_ty = match (&ty, &expr) {
                    (Type::Unresolved, None) => {
                        cx.error(
                            name.span,
                            "var definitions require at least a type or an initializer".to_string(),
                        );
                        &Type::Error
                    }
                    (Type::Unresolved, Some(expr)) => &expr.ty,
                    (ty, None) => ty,
                    (ty, Some(expr)) => {
                        cx.expect_to_be(expr.span, ty, &expr.ty);
                        ty
                    }
                };
                Stmt {
                    kind: StmtKind::VarDefinition {
                        name,
                        ty: (ty_span, var_ty.clone()),
                        expr,
                    },
                    ..self
                }
            }
            StmtKind::Assignment { name, expr } => {
                let ident_ty = cx.var_ty(name.span, &name.ident);

                match expr.kind.clone() {
                    ExprKind::FunctionCall {
                        fun_name,
                        args,
                        function: _,
                    } if cx.lookup_method(&fun_name.ident).is_some() => Stmt {
                        span: self.span,
                        kind: StmtKind::MethodCall {
                            name: Some(name),
                            fun_name,
                            args,
                            method: MethodRef::default(),
                        },
                    }
                    .tc(cx),
                    _ => {
                        let expr = expr.tc(cx);
                        cx.expect_to_be(expr.span, &expr.ty, &ident_ty);
                        Stmt {
                            kind: StmtKind::Assignment { name, expr },
                            ..self
                        }
                    }
                }
            }
            StmtKind::Seq(c1, c2) => {
                let c1 = c1.tc(cx);
                let c2 = c2.tc(cx);
                Stmt {
                    kind: StmtKind::Seq(Box::new(c1), Box::new(c2)),
                    ..self
                }
            }
            StmtKind::Match { body } => {
                let body = body.tc(cx);
                Stmt {
                    kind: StmtKind::Match { body },
                    ..self
                }
            }
            StmtKind::Loop { specifications, body } => {
                let specifications = tc_loopspecifications(cx, specifications);
                let body = cx.in_loop(|cx| body.tc(cx));
                Stmt {
                    kind: StmtKind::Loop { specifications, body },
                    ..self
                }
            }
            StmtKind::For {
                name,
                range,
                specifications,
                body,
            } => cx.nest(|cx| {
                let range = range.tc(cx);

                cx.register(name.clone(), range.elem_ty());

                let specifications = tc_loopspecifications(cx, specifications);
                let body = cx.in_loop(|cx| body.tc(cx));
                Stmt {
                    kind: StmtKind::For {
                        name,
                        range,
                        specifications,
                        body,
                    },
                    ..self
                }
            }),
            StmtKind::Break => {
                if !cx.is_loop {
                    cx.error(self.span, "`break` is only allowed in loops".to_string());
                }
                self
            }
            StmtKind::Continue => {
                if !cx.is_loop {
                    cx.error(self.span, "`continue` is only allowed in loops".to_string());
                }
                self
            }
            StmtKind::Return { expr } => {
                let expr = if let Some(expr) = expr {
                    let expr = expr.tc(cx);
                    match cx.expected_return_ty.clone() {
                        Some((_, ty)) => {
                            // TODO: Update error message to say something about return type
                            cx.expect_to_be(expr.span, &ty, &expr.ty);
                        }
                        None => {
                            let msg = format!(
                                "method has no return type, buyt a value of type `{}` was returned",
                                expr.ty
                            );
                            cx.error(expr.span, msg);
                        }
                    }
                    Some(expr)
                } else {
                    if let Some((_, ty)) = &cx.expected_return_ty {
                        cx.error(
                            self.span,
                            format!("missing return expression of type `{ty}`"),
                        )
                    }
                    None
                };
                Stmt {
                    kind: StmtKind::Return { expr },
                    ..self
                }
            }

            StmtKind::Assume(x) => {
                let x = x.tc(cx);
                cx.expect_bool(x.span, &x.ty);
                Stmt {
                    kind: StmtKind::Assume(x),
                    ..self
                }
            }
            StmtKind::Assert(x, msg) => {
                let x = x.tc(cx);
                cx.expect_bool(x.span, &x.ty);
                Stmt {
                    kind: StmtKind::Assert(x, msg),
                    ..self
                }
            }
            StmtKind::MethodCall {
                name,
                fun_name,
                args,
                method: _,
            } => {
                let name_ty = name
                    .as_ref()
                    .map(|name| (name, cx.var_ty(name.span, &name.ident)));
                let args = args.into_iter().map(|arg| arg.tc(cx)).collect_vec();
                if let Some((sig, method)) = cx.lookup_method(&fun_name.ident).cloned() {
                    let def_args = &sig.args;
                    cx.check_args(&fun_name, def_args, &args);

                    match (name_ty, sig.return_ty) {
                        (None, None) => {}
                        (None, Some(_)) => {
                            // NOTE: method returns something, but it's not assigned
                        }
                        (Some((name, _)), None) => cx.error(
                            name.span,
                            format!(
                                "method `{}` was a assigned to a variable, but has no return type",
                                sig.name
                            ),
                        ),
                        (Some((_, name_ty)), Some(return_ty)) => {
                            cx.expect_to_be(fun_name.span, &return_ty, &name_ty);
                        }
                    }

                    Stmt {
                        kind: StmtKind::MethodCall {
                            name,
                            fun_name,
                            args,
                            method,
                        },
                        ..self
                    }
                } else {
                    cx.error(
                        fun_name.span,
                        format!("no method named `{fun_name}` exists"),
                    );
                    Stmt {
                        kind: StmtKind::MethodCall {
                            name,
                            fun_name,
                            args,
                            method: MethodRef::default(),
                        },
                        ..self
                    }
                }
            }
        }
    }
}

impl Range {
    fn tc(self, cx: &mut BlockContext) -> Range {
        match self {
            Range::FromTo(from, to) => {
                let from = from.tc(cx);
                let to = to.tc(cx);
                cx.expect_numeric(from.span, &from.ty);
                cx.expect_cmp_equal(to.span, &from.ty, &to.ty);
                Range::FromTo(from, to)
            }
        }
    }
    pub fn elem_ty(&self) -> Type {
        match self {
            Range::FromTo(from, _) => from.ty.clone(),
        }
    }
}

impl Block {
    fn tc(self, cx: &mut BlockContext) -> Block {
        Block {
            stmt: Box::new(self.stmt.tc(cx)),
            ..self
        }
    }
}

impl Cases {
    fn tc(self, cx: &mut BlockContext) -> Cases {
        Cases {
            cases: self.cases.into_iter().map(|case| case.tc(cx)).collect(),
            ..self
        }
    }
}

impl Case {
    fn tc(self, cx: &mut BlockContext) -> Case {
        let condition = self.condition.tc(cx);
        cx.expect_bool(condition.span, &condition.ty);
        let stmt = self.stmt.tc(cx);
        Case { condition, stmt }
    }
}

fn tc_specifications(
    cx: &mut BlockContext,
    specifications: Vec<Specification>,
) -> Vec<Specification> {
    cx.expect_single(specifications.clone()
        .into_iter()
        .filter_map(|spec| match spec {
            Specification::Decreases { span, .. } => Some(span),
            _ => None})
        .collect());
    specifications
        .into_iter()
        .map(|spec| match spec {
            Specification::Requires { span, expr } => {
                let expr = expr.tc(cx);
                cx.expect_bool(expr.span, &expr.ty);
                Specification::Requires { span, expr }
            }
            Specification::Ensures { span, expr } => {
                let expr = expr.tc(cx);
                cx.expect_bool(expr.span, &expr.ty);
                Specification::Ensures { span, expr }
            }
            Specification::Modifies { span, name, ty } => {
                match cx.file.globals.iter().find(|(g, _)| g.ident == name.ident) {
                    Some((_, t)) => Specification::Modifies { span, name, ty: t.1.clone() },
                    None => {
                        cx.error(name.span, format!("no global named `{name}` exists"));
                        Specification::Modifies { span, name, ty }
                    }
                }
            }
            Specification::Decreases { span, expr } => {
                let expr = expr.tc(cx);
                cx.expect_int(expr.span, &expr.ty);
                Specification::Decreases { span, expr }
            }
        })
        .collect()
}

fn tc_loopspecifications(
    cx: &mut BlockContext,
    specifications: Vec<LoopSpecification>,
) -> Vec<LoopSpecification> {
    cx.expect_single(specifications.clone()
        .into_iter()
        .filter_map(|spec| match spec {
            LoopSpecification::Decreases { span, .. } => Some(span),
            _ => None})
        .collect());
    specifications
        .into_iter()
        .map(|spec| match spec {
            LoopSpecification::Invariant { span, expr } => {
                let expr = expr.tc(cx);
                cx.expect_bool(expr.span, &expr.ty);
                LoopSpecification::Invariant { span, expr }
            }
            LoopSpecification::Decreases { span, expr } => {
                let expr = expr.tc(cx);
                cx.expect_int(expr.span, &expr.ty);
                LoopSpecification::Decreases { span, expr }
            }
        })
        .collect()
}

impl Method {
    fn tc(self, cx: &mut FileContext) -> Method {
        let mut block_cx = cx
            .block_cx()
            .with_expected_return_ty(self.return_ty.clone());
        for var in &self.args {
            let _ = block_cx.register_var(var.clone());
        }
        block_cx.mark_def_end();
        let specifications = tc_specifications(&mut block_cx, self.specifications);
        let body = self.body.tc(&mut block_cx);
        Method {
            body,
            specifications,
            ..self
        }
    }
}

impl Function {
    fn tc(self, cx: &mut FileContext) -> Function {
        let mut block_cx = cx
            .block_cx()
            .with_expected_return_ty(Some(self.return_ty.clone()));
        for var in &self.args {
            let _ = block_cx.register_var(var.clone());
        }
        block_cx.mark_def_end();
        let body = self.body.map(|body| body.tc(&mut block_cx));
        let specifications = tc_specifications(&mut block_cx, self.specifications);
        Function { body, specifications, ..self }
    }
}

impl Domain {
    fn tc(self, cx: &mut FileContext, items: &Arc<Items>) -> Domain {
        let items = self
            .items
            .into_iter()
            .map(|item| match item {
                DomainItem::Function(function) => {
                    let mut cx = cx.block_cx();
                    for arg in &function.args {
                        let _ = cx.register_var(arg.clone());
                    }
                    cx.mark_def_end();
                    let specifications = tc_specifications(&mut cx, function.specifications);
                    let function = Function {
                        specifications,
                        ..function
                    };
                    items.insert_function(
                        function.name.ident.clone(),
                        true,
                        Arc::new(function.clone()),
                    );
                    DomainItem::Function(function)
                }
                DomainItem::Axiom(axiom) => {
                    let mut cx = cx.block_cx();
                    let expr = axiom.expr.tc(&mut cx);
                    cx.expect_bool(expr.span, &expr.ty);
                    DomainItem::Axiom(DomainAxiom { expr, ..axiom })
                }
            })
            .collect();
        Domain { items, ..self }
    }
}

impl Global {
    fn tc(self, cx: &mut FileContext) -> Global {
        let mut cx = cx.block_cx();
        let init = self.init.map(|init| {
            let init = init.tc(&mut cx);
            cx.expect_to_be(init.span, &self.var.ty.1, &init.ty);
            init
        });
        Global { init, ..self }
    }
}

impl Item {
    pub fn name(&self) -> Name {
        match self {
            Item::Method(method) => method.name.clone(),
            Item::Function(function) => function.name.clone(),
            Item::Global(global) => global.var.name.clone(),
            Item::Domain(domain) => domain.name.clone(),
        }
    }
}

impl File {
    pub(crate) fn tc(mut self) -> (Arc<Items>, Vec<Error>) {
        let mut file_cx = FileContext::default();
        let cx = &mut file_cx;

        let items = Arc::new(Items::default());

        for item in &self.items {
            if let Item::Domain(domain) = item {
                cx.domains.insert(
                    domain.name.clone(),
                    DomainRef(items.new_ref(domain.name.ident.clone())),
                );
            }
        }

        for item in &mut self.items {
            match item {
                Item::Method(method) => {
                    method.args = cx.resolve_vars(&method.args);
                    method.return_ty = method
                        .return_ty
                        .as_ref()
                        .map(|(span, ty)| (*span, cx.resolve_ty(*span, ty)));
                    cx.methods.insert(
                        method.name.ident.clone(),
                        (
                            MethodSignature {
                                name: method.name.clone(),
                                args: method.args.clone(),
                                return_ty: method.return_ty.as_ref().map(|(_, ty)| ty.clone()),
                            },
                            MethodRef(items.new_ref(method.name.ident.clone())),
                        ),
                    );
                }
                Item::Function(f) => {
                    f.args = cx.resolve_vars(&f.args);
                    f.return_ty = (f.return_ty.0, cx.resolve_ty(f.return_ty.0, &f.return_ty.1));
                    cx.functions.insert(
                        f.name.ident.clone(),
                        (
                            FunctionSignature {
                                name: f.name.clone(),
                                args: f.args.clone(),
                                return_ty: f.return_ty.1.clone(),
                            },
                            FunctionRef(items.new_ref(f.name.ident.clone())),
                        ),
                    );
                }
                Item::Global(global) => {
                    global.var = cx.resolve_var(&global.var);
                    cx.globals.insert(
                        global.var.name.clone(),
                        (global.var.span, global.var.ty.1.clone()),
                    );
                }
                Item::Domain(domain) => {
                    for item in &mut domain.items {
                        match item {
                            DomainItem::Function(f) => {
                                f.args = cx.resolve_vars(&f.args);
                                f.return_ty =
                                    (f.return_ty.0, cx.resolve_ty(f.return_ty.0, &f.return_ty.1));
                                cx.functions.insert(
                                    f.name.ident.clone(),
                                    (
                                        FunctionSignature {
                                            name: f.name.clone(),
                                            args: f.args.clone(),
                                            return_ty: f.return_ty.1.clone(),
                                        },
                                        FunctionRef(items.new_ref(f.name.ident.clone())),
                                    ),
                                );
                            }
                            DomainItem::Axiom(_) => {}
                        }
                    }
                }
            }
        }

        for item in &self.items {
            match item {
                Item::Method(x) => {
                    items.insert_method(item.name().ident, Arc::new(x.clone().tc(cx)));
                }
                Item::Function(x) => {
                    items.insert_function(item.name().ident, false, Arc::new(x.clone().tc(cx)));
                }
                Item::Global(x) => {
                    items.insert_global(item.name().ident, Arc::new(x.clone().tc(cx)));
                }
                Item::Domain(x) => {
                    items.insert_domain(item.name().ident, Arc::new(x.clone().tc(cx, &items)));
                }
            }
        }

        (items, file_cx.errors)
    }
}

pub(crate) fn did_you_mean<T, I>(v: &str, possible_values: I) -> Vec<String>
where
    T: AsRef<str>,
    I: IntoIterator<Item = T>,
{
    let mut candidates: Vec<(f64, String)> = possible_values
        .into_iter()
        // GH clap-rs/clap/#4660: using `jaro` because `jaro_winkler` implementation in `strsim-rs` is wrong
        // causing strings with common prefix >=10 to be considered perfectly similar
        .map(|pv| (strsim::jaro(v, pv.as_ref()), pv.as_ref().to_owned()))
        // Confidence of 0.7 so that bar -> baz is suggested
        .filter(|(confidence, _)| *confidence > 0.7)
        .collect();
    candidates.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
    candidates.into_iter().map(|(_, pv)| pv).collect()
}
