mod grammar_ext;

use lalrpop_util::lalrpop_mod;
use miette::Diagnostic;
use once_cell::sync::Lazy;
use thiserror::Error;

use crate::{ast::File, Span};

lalrpop_mod!(grammar, "/parse/grammar.rs");

#[derive(Debug)]
pub struct ParseResult<T> {
    pub ast: Option<T>,
    pub errors: Vec<ParseError>,
}

fn parse<'a, T>(
    src: &'a str,
    result: Result<T, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, CustomError>>,
) -> ParseResult<T> {
    match result {
        Ok(ast) => ParseResult {
            ast: Some(ast),
            errors: Vec::new(),
        },
        Err(err) => ParseResult {
            ast: None,
            errors: [ParseError::new(src, err)].to_vec(),
        },
    }
}

pub fn file(src: &str) -> ParseResult<File> {
    static PARSER: Lazy<grammar::FileParser> = Lazy::new(grammar::FileParser::new);
    parse(src, PARSER.parse(src))
}

#[derive(Debug, Error, Diagnostic, Clone)]
pub enum ParseError {
    #[error("Invalid Token")]
    #[diagnostic()]
    InvalidToken {
        #[source_code]
        src: String,
        #[label("This token is not valid in this context")]
        err_span: Span,
    },
    #[error("Unrecognized Token")]
    #[diagnostic(help("Expected tokens here are: {expected}{}", if let Some(hint) = hint { format!("\n{hint}") } else { "".to_string() }))]
    UnrecognizedToken {
        #[source_code]
        src: String,
        #[label = "The token `{token}` is unrecognized in this context."]
        err_span: Span,
        token: String,
        expected: String,
        hint: Option<String>,
    },
    #[error("Unrecognized EOF")]
    #[diagnostic(help("Expected tokens in this context are:\n{expected}"))]
    UnrecognizedEof {
        #[source_code]
        src: String,
        #[label = "The document ends too early. Are you missing a token?"]
        err_span: Span,
        expected: String,
    },
    #[error("Integer is too large")]
    #[diagnostic(help("The integer is too large to be represented"))]
    IntegerTooLarge {
        #[source_code]
        src: String,
        #[label = "The integer is too large to be represented"]
        err_span: Span,
    },
}

pub(crate) enum CustomError {
    IntegerTooLarge { span: Span },
}

impl ParseError {
    pub(crate) fn new(
        src: &str,
        e: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, CustomError>,
    ) -> Self {
        let prep_src = || format!("{src}\n");

        match e {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::InvalidToken {
                src: prep_src(),
                err_span: Span::from_start_len(location, 0),
            },
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                ParseError::UnrecognizedEof {
                    src: prep_src(),
                    err_span: Span::from_start_len(location, 0),
                    expected: expected.join(", "),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseError::UnrecognizedToken {
                    src: prep_src(),
                    err_span: Span::from_start_end(token.0, token.2),
                    token: token.1.to_string(),
                    expected: expected.join(", "),
                    hint: None,
                }
            }
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { error } => match error {
                CustomError::IntegerTooLarge { span } => ParseError::IntegerTooLarge {
                    src: prep_src(),
                    err_span: span,
                },
            },
        }
    }
    pub fn span(&self) -> Span {
        *match self {
            ParseError::InvalidToken { err_span, .. } => err_span,
            ParseError::UnrecognizedToken { err_span, .. } => err_span,
            ParseError::UnrecognizedEof { err_span, .. } => err_span,
            ParseError::IntegerTooLarge { err_span, .. } => err_span,
        }
    }
    pub fn msg(&self) -> String {
        if let Some(help) = self.help() {
            format!("{self}\n{help}")
        } else {
            self.to_string()
        }
    }
}
