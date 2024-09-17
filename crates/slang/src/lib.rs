#![warn(clippy::todo)]

pub mod ast;
mod expr_ext;
mod parse;
mod span;
mod stmt_ext;
pub mod tc;

use std::sync::{Arc, RwLock};

use ast::{Ident, Ref};
use indexmap::IndexMap;
pub use parse::ParseResult;
pub use span::{Position, Span};

#[derive(Debug)]
pub struct SourceFile {
    items: Arc<Items>,
    pub parse_errors: Vec<parse::ParseError>,
    pub tc_errors: Vec<tc::Error>,
}
impl SourceFile {
    pub fn methods(&self) -> Vec<Arc<ast::Method>> {
        self.items.methods()
    }
    pub fn functions(&self) -> Vec<Arc<ast::Function>> {
        self.items.functions()
    }
    pub fn domains(&self) -> Vec<Arc<ast::Domain>> {
        self.items.domains()
    }
    pub fn globals(&self) -> Vec<Arc<ast::Global>> {
        self.items.globals()
    }

    pub fn contains_error(&self) -> bool {
        !self.parse_errors.is_empty() || !self.tc_errors.is_empty()
    }
}

#[non_exhaustive]
#[derive(Debug, Default)]
pub struct Items {
    methods: RwLock<IndexMap<ast::Ident, Arc<ast::Method>>>,
    functions: RwLock<IndexMap<ast::Ident, (bool, Arc<ast::Function>)>>,
    domains: RwLock<IndexMap<ast::Ident, Arc<ast::Domain>>>,
    globals: RwLock<IndexMap<ast::Ident, Arc<ast::Global>>>,
}

impl Items {
    fn new_ref(self: &Arc<Self>, ident: Ident) -> Ref {
        Ref::Resolved(ident, Arc::downgrade(self))
    }
    pub fn methods(&self) -> Vec<Arc<ast::Method>> {
        self.methods.read().unwrap().values().cloned().collect()
    }
    pub fn method(&self, id: &Ident) -> Option<Arc<ast::Method>> {
        self.methods.read().ok()?.get(id).cloned()
    }
    fn insert_method(&self, id: Ident, ast: Arc<ast::Method>) {
        self.methods.write().unwrap().insert(id, ast);
    }
    pub fn functions(&self) -> Vec<Arc<ast::Function>> {
        self.functions
            .read()
            .unwrap()
            .values()
            .filter_map(|(is_domain, f)| if *is_domain { None } else { Some(f.clone()) })
            .collect()
    }
    pub fn function(&self, id: &Ident) -> Option<Arc<ast::Function>> {
        Some(self.functions.read().ok()?.get(id).cloned()?.1)
    }
    fn insert_function(&self, id: Ident, is_domain: bool, ast: Arc<ast::Function>) {
        self.functions.write().unwrap().insert(id, (is_domain, ast));
    }
    pub fn domains(&self) -> Vec<Arc<ast::Domain>> {
        self.domains.read().unwrap().values().cloned().collect()
    }
    pub fn domain(&self, id: &Ident) -> Option<Arc<ast::Domain>> {
        self.domains.read().ok()?.get(id).cloned()
    }
    fn insert_domain(&self, id: Ident, ast: Arc<ast::Domain>) {
        self.domains.write().unwrap().insert(id, ast);
    }
    pub fn globals(&self) -> Vec<Arc<ast::Global>> {
        self.globals.read().unwrap().values().cloned().collect()
    }
    pub fn global(&self, id: &Ident) -> Option<Arc<ast::Global>> {
        self.globals.read().ok()?.get(id).cloned()
    }
    fn insert_global(&self, id: Ident, ast: Arc<ast::Global>) {
        self.globals.write().unwrap().insert(id, ast);
    }
}

pub fn parse_file(src: &str) -> SourceFile {
    let ParseResult {
        ast,
        errors: parse_errors,
    } = parse::file(src);

    if let Some((items, tc_errors)) = ast.map(|file| file.tc()) {
        SourceFile {
            items,
            parse_errors,
            tc_errors,
        }
    } else {
        SourceFile {
            items: Default::default(),
            parse_errors,
            tc_errors: Vec::new(),
        }
    }
}
