#![warn(clippy::todo)]

//! # slang

pub mod ast;
mod cmd_ext;
mod expr_ext;
mod parse;
pub mod smt;
mod span;
pub mod tc;

use std::sync::{Arc, RwLock};

use ast::{DomainRef, FunctionRef, Ident, MethodRef, Ref};
use indexmap::IndexMap;
pub use parse::ParseResult;
pub use span::{Position, Span};

/// A parsed and type checked source file.
#[derive(Debug)]
pub struct SourceFile {
    items: Arc<Items>,
    /// Errors that occurred during parsing.
    pub parse_errors: Vec<parse::ParseError>,
    /// Errors that occurred during type checking.
    pub tc_errors: Vec<tc::Error>,
}
impl SourceFile {
    /// Returns the methods defined in the source file.
    pub fn methods(&self) -> Vec<Arc<ast::Method>> {
        self.items.methods()
    }
    /// Returns the functions defined in the source file.
    pub fn functions(&self) -> Vec<Arc<ast::Function>> {
        self.items.functions()
    }
    /// Returns the domains defined in the source file.
    pub fn domains(&self) -> Vec<Arc<ast::Domain>> {
        self.items.domains()
    }
    /// Returns the globals defined in the source file.
    pub fn globals(&self) -> Vec<Arc<ast::Global>> {
        self.items.globals()
    }

    /// Returns the method with the given identifier.
    ///
    /// The returned ref will be unresolved if no method with that name exists.
    pub fn get_method_ref(&self, ident: Ident) -> MethodRef {
        MethodRef(self.items.new_ref(ident))
    }
    /// Returns the function with the given identifier.
    ///
    /// The returned ref will be unresolved if no function with that name
    /// exists.
    pub fn get_function_ref(&self, ident: Ident) -> FunctionRef {
        FunctionRef(self.items.new_ref(ident))
    }
    /// Returns the domain with the given identifier.
    ///
    /// The returned ref will be unresolved if no domain with that name exists.
    pub fn get_domain_ref(&self, ident: Ident) -> DomainRef {
        DomainRef(self.items.new_ref(ident))
    }

    /// Returns true if there are any parse or type checking errors.
    pub fn contains_error(&self) -> bool {
        !self.parse_errors.is_empty() || !self.tc_errors.is_empty()
    }
}

/// A collection of items defined in a source file.
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
    /// Get the list of methods defined in the source file.
    pub fn methods(&self) -> Vec<Arc<ast::Method>> {
        self.methods.read().unwrap().values().cloned().collect()
    }
    /// Get the method with the given identifier, if any exists.
    pub fn method(&self, id: &Ident) -> Option<Arc<ast::Method>> {
        self.methods.read().ok()?.get(id).cloned()
    }
    fn insert_method(&self, id: Ident, ast: Arc<ast::Method>) {
        self.methods.write().unwrap().insert(id, ast);
    }
    /// Get the list of functions defined in the source file.
    pub fn functions(&self) -> Vec<Arc<ast::Function>> {
        self.functions
            .read()
            .unwrap()
            .values()
            .filter_map(|(is_domain, f)| if *is_domain { None } else { Some(f.clone()) })
            .collect()
    }
    /// Get the function with the given identifier, if any exists.
    pub fn function(&self, id: &Ident) -> Option<Arc<ast::Function>> {
        Some(self.functions.read().ok()?.get(id).cloned()?.1)
    }
    fn insert_function(&self, id: Ident, is_domain: bool, ast: Arc<ast::Function>) {
        self.functions.write().unwrap().insert(id, (is_domain, ast));
    }
    /// Get the list of domains defined in the source file.
    pub fn domains(&self) -> Vec<Arc<ast::Domain>> {
        self.domains.read().unwrap().values().cloned().collect()
    }
    /// Get the domain with the given identifier, if any exists.
    pub fn domain(&self, id: &Ident) -> Option<Arc<ast::Domain>> {
        self.domains.read().ok()?.get(id).cloned()
    }
    fn insert_domain(&self, id: Ident, ast: Arc<ast::Domain>) {
        self.domains.write().unwrap().insert(id, ast);
    }
    /// Get the list of globals defined in the source file.
    pub fn globals(&self) -> Vec<Arc<ast::Global>> {
        self.globals.read().unwrap().values().cloned().collect()
    }
    /// Get the global with the given identifier, if any exists.
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
