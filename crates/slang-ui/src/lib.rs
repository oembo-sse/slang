mod monaco;

use std::{
    collections::BTreeMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use axum::{
    extract::State,
    http::{header, StatusCode, Uri},
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use clap::Parser;
use color_eyre::eyre::Context as _;
use itertools::Itertools;
use rust_embed::Embed;
use serde::{Deserialize, Serialize};
use slang::{Position, SourceFile, Span};
use tapi::endpoints::RouterExt;
use tracing_subscriber::prelude::*;

pub type Result<T, E = color_eyre::eyre::Error> = std::result::Result<T, E>;

pub use color_eyre::eyre::{bail, eyre};

pub mod prelude {
    pub use super::Result;
    pub use color_eyre::eyre::{bail, eyre};
    pub use miette;
    pub use slang;
    pub use smtlib::{self, prelude::*};
    pub use tracing;
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Info,
    Warning,
    Error,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub struct Report {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
}

#[non_exhaustive]
#[derive(Debug, Clone, Serialize, tapi::Tapi)]
pub enum Color {
    Red,
    Green,
    Blue,
}

pub struct Context {
    storage: smtlib::Storage,
    reports: RwLock<Vec<Report>>,
    message: RwLock<Option<(String, Color)>>,
}

struct Logger {}

impl smtlib::Logger for Logger {
    fn exec(&self, cmd: smtlib::lowlevel::ast::Command) {
        let span = tracing::span!(tracing::Level::INFO, "smt");
        let _enter = span.enter();
        tracing::info!("> {cmd}")
    }

    fn response(&self, _cmd: smtlib::lowlevel::ast::Command, res: &str) {
        let span = tracing::span!(tracing::Level::INFO, "smt");
        let _enter = span.enter();
        tracing::info!("< {}", res.trim())
    }
}

impl Context {
    fn new(_file: &SourceFile) -> Context {
        Context {
            storage: smtlib::Storage::new(),
            reports: Default::default(),
            message: Default::default(),
        }
    }

    pub fn smt_st(&self) -> &smtlib::Storage {
        &self.storage
    }

    pub fn solver(&self) -> Result<smtlib::Solver<smtlib::backend::z3_binary::Z3Binary>> {
        let mut solver = smtlib::Solver::new(
            self.smt_st(),
            smtlib::backend::z3_binary::Z3Binary::new("z3")
                .context("failed to find `z3`. is it installed and in your path?")?,
        )?;
        solver.set_logger(Logger {});
        solver.set_timeout(2_000)?;
        Ok(solver)
    }

    pub fn report(&self, severity: Severity, span: Span, message: impl std::fmt::Display) {
        let message = message.to_string();
        self.reports.write().unwrap().push(Report {
            severity,
            span,
            message,
        })
    }
    pub fn info(&self, span: Span, message: impl std::fmt::Display) {
        self.report(Severity::Info, span, message)
    }
    pub fn warning(&self, span: Span, message: impl std::fmt::Display) {
        self.report(Severity::Warning, span, message)
    }
    pub fn error(&self, span: Span, message: impl std::fmt::Display) {
        self.report(Severity::Error, span, message)
    }

    pub fn set_message(&self, message: impl std::fmt::Display, color: Color) {
        *self.message.write().unwrap() = Some((message.to_string(), color))
    }

    pub fn reports(&self) -> Vec<Report> {
        self.reports.read().unwrap().clone()
    }

    #[track_caller]
    pub fn todo(&self, span: Span) {
        let msg = format!("not yet implemented: {}", std::panic::Location::caller());
        tracing::error!("{msg}");
        self.warning(span, msg);
    }
}

pub struct HoverResponse {
    pub span: Span,
    pub contents: Vec<String>,
}

pub trait Hook {
    fn analyze(&self, cx: &Context, file: &SourceFile) -> Result<()>;
    #[allow(unused_variables)]
    fn hover(&self, cx: &Context, file: &SourceFile, pos: Position) -> Option<HoverResponse> {
        None
    }
}

#[derive(Embed)]
#[folder = "static/"]
struct Asset;

pub async fn run(hook: impl Hook + Send + Sync + 'static) {
    let _ = color_eyre::install();

    tracing_subscriber::Registry::default()
        .with(tracing_error::ErrorLayer::default())
        .with(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing_subscriber::filter::LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(false)
                .without_time(),
        )
        .with(tracing_subscriber::filter::FilterFn::new(|m| {
            !m.target().contains("hyper")
        }))
        .init();

    match run_impl(Arc::new(hook)).await {
        Ok(()) => {}
        Err(err) => println!("{err:?}"),
    }
}

pub struct TestResult {
    reports: Vec<Report>,
    error: Option<color_eyre::eyre::Error>,
}

pub fn test(hook: impl Hook + 'static, src: &str) -> TestResult {
    match run_hook(&hook, src) {
        Ok(reports) => TestResult {
            reports,
            error: None,
        },
        Err((reports, error)) => TestResult {
            reports,
            error: Some(error),
        },
    }
}

impl TestResult {
    pub fn has_message(&self, message: &str) -> bool {
        self.reports.iter().any(|r| r.message == message)
    }
    pub fn has_errors(&self) -> bool {
        self.error.is_some() || self.reports.iter().any(|r| r.severity == Severity::Error)
    }
    pub fn error(&self) -> Option<&color_eyre::eyre::Error> {
        self.error.as_ref()
    }
    pub fn reports(&self) -> Vec<Report> {
        self.reports.clone()
    }
}

#[derive(clap::Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(clap::Subcommand, Default, Clone)]
enum Command {
    #[default]
    Ui,
    Check {
        #[clap(long, short, default_value = "human")]
        format: OutputFormat,
        path: PathBuf,
    },
}

#[derive(Debug, Default, Clone, Copy, clap::ValueEnum)]
enum OutputFormat {
    #[default]
    Human,
    Json,
}

async fn run_impl(hook: Arc<dyn Hook + Send + Sync + 'static>) -> Result<()> {
    let cli = Cli::parse();

    match cli.command.clone().unwrap_or_default() {
        Command::Ui => {
            let endpoints = endpoints();

            populate_js_client(&endpoints);

            let app = Router::new()
                .nest("/api", Router::new().tapis(endpoints.into_iter()))
                .route("/", get(index_handler))
                .route("/index.html", get(index_handler))
                .route("/{*file}", get(static_handler))
                .with_state(AppState { hook });

            let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
            axum::serve(listener, app).await.unwrap();

            Ok(())
        }
        Command::Check { path, format } => {
            let src = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read '{}'", path.display()))?;

            let reports = match run_hook(&*hook, &src) {
                Ok(reports) => reports,
                Err((_, error)) => return Err(error),
            };

            let diagnostics = reports.into_iter().map(|report| {
                miette::diagnostic!(
                    labels = vec![miette::LabeledSpan::at(
                        (report.span.start(), report.span.len()),
                        &report.message
                    ),],
                    severity = match report.severity {
                        Severity::Info => miette::Severity::Advice,
                        Severity::Warning => miette::Severity::Warning,
                        Severity::Error => miette::Severity::Error,
                    },
                    "{}",
                    report.message
                )
            });
            match format {
                OutputFormat::Human => {
                    for diag in diagnostics {
                        let report = miette::Report::new(diag).with_source_code(
                            miette::NamedSource::new(path.display().to_string(), src.to_string()),
                        );
                        println!("{report:?}");
                    }
                }
                OutputFormat::Json => {
                    let output = serde_json::to_string(&diagnostics.collect_vec())?;
                    println!("{}", output);
                }
            }

            Ok(())
        }
    }
}

/// Write the JavaScript client file if it exists.
///
/// Returns `true` if the file exists, `false` otherwise.
fn populate_js_client(endpoints: &tapi::endpoints::Endpoints<AppState>) -> bool {
    let js_client_path = std::path::PathBuf::from("./crates/slang-ui/static/tapi.js");
    // write JavaScript client if and only if the path already exists
    if js_client_path.exists() {
        // only write if the contents are different
        let contents = endpoints.js_client();
        let prev = std::fs::read_to_string(&js_client_path).unwrap_or_default();
        if prev != contents {
            let _ = std::fs::write(&js_client_path, contents);
        }
        true
    } else {
        false
    }
}

async fn index_handler() -> impl IntoResponse {
    static_handler("/index.html".parse::<Uri>().unwrap()).await
}

async fn static_handler(uri: Uri) -> impl IntoResponse {
    StaticFile(uri.path().trim_start_matches('/').to_string())
}

pub struct StaticFile<T>(pub T);

impl<T> IntoResponse for StaticFile<T>
where
    T: Into<String>,
{
    fn into_response(self) -> Response {
        let path = self.0.into();

        match Asset::get(path.as_str()) {
            Some(content) => {
                let mime = mime_guess::from_path(path).first_or_octet_stream();
                ([(header::CONTENT_TYPE, mime.as_ref())], content.data).into_response()
            }
            None => (StatusCode::NOT_FOUND, "404 Not Found").into_response(),
        }
    }
}

#[derive(Clone)]
struct AppState {
    hook: Arc<dyn Hook + Send + Sync>,
}

impl std::fmt::Debug for AppState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AppState").field("hook", &"...").finish()
    }
}

fn endpoints() -> tapi::endpoints::Endpoints<'static, AppState> {
    tapi::endpoints::Endpoints::new([
        &sample_files::endpoint as &dyn tapi::endpoints::Endpoint<AppState>,
        &heartbeat::endpoint as _,
        &analyze::endpoint as _,
        &hover::endpoint as _,
    ])
}

fn run_hook(
    hook: &dyn Hook,
    src: &str,
) -> Result<Vec<Report>, (Vec<Report>, color_eyre::eyre::Error)> {
    let span = tracing::span!(parent: tracing::Span::none(), tracing::Level::INFO, "analyze");
    let _enter = span.enter();

    let file = slang::parse_file(src);
    let cx = Context::new(&file);
    for err in &file.parse_errors {
        cx.error(err.span(), format!("parse error: {}", err.msg()));
    }
    for err in &file.tc_errors {
        cx.error(err.span(), err.msg());
    }
    match hook.analyze(&cx, &file) {
        Ok(()) => Ok(cx.reports()),
        Err(err) => Err((cx.reports(), err)),
    }
}

#[derive(Debug, Serialize, Deserialize, tapi::Tapi)]
enum Heartbeat {
    Alive,
}

#[tapi::tapi(path = "/heartbeat", method = Get)]
async fn heartbeat() -> tapi::endpoints::Sse<Heartbeat> {
    let (tx, rx) = tokio::sync::mpsc::channel(1);

    tokio::spawn(async move {
        loop {
            if tx.send(Ok(Heartbeat::Alive)).await.is_err() {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_secs(10)).await;
        }
    });

    tapi::endpoints::Sse::new(tokio_stream::wrappers::ReceiverStream::new(rx))
}

#[derive(Debug, Default, Serialize, tapi::Tapi)]
struct SampleFiles {
    files: BTreeMap<String, String>,
}

#[tapi::tapi(path = "/sample-files", method = Get)]
async fn sample_files() -> Json<SampleFiles> {
    let Ok(files) = glob::glob("**/*.slang") else {
        return Json(SampleFiles::default());
    };
    Json(SampleFiles {
        files: files
            .filter_map(|f| {
                let f = f.ok()?;
                let src = std::fs::read_to_string(&f).ok()?;
                Some((f.display().to_string(), src))
            })
            .collect(),
    })
}

#[derive(Debug, Serialize, Deserialize, tapi::Tapi)]
pub struct AnalyzeParams {
    file: String,
}

#[derive(Debug, Serialize, tapi::Tapi)]
pub struct AnalyzeResult {
    markers: Vec<monaco::MarkerData>,
    analysis_errored: bool,
    message: Option<(Option<String>, Color)>,
}

#[tapi::tapi(path = "/analyze", method = Post)]
async fn analyze(state: State<AppState>, params: Json<AnalyzeParams>) -> Json<AnalyzeResult> {
    let (reports, analysis_errored) = match run_hook(state.hook.as_ref(), &params.file) {
        Ok(reports) => (reports, false),
        Err((reports, err)) => {
            eprintln!("{err:?}");
            (reports, true)
        }
    };
    Json(AnalyzeResult {
        markers: reports
            .iter()
            .map(|r| monaco::MarkerData {
                related_information: None,
                tags: None,
                severity: match r.severity {
                    Severity::Info => monaco::MarkerSeverity::Info,
                    Severity::Warning => monaco::MarkerSeverity::Warning,
                    Severity::Error => monaco::MarkerSeverity::Error,
                },
                message: r.message.clone(),
                span: monaco::MonacoSpan::from_source_span(&params.file, r.span),
            })
            .collect(),
        analysis_errored,
        message: None,
    })
}

#[derive(Debug, Deserialize, tapi::Tapi)]
pub struct HoverParams {
    pub file: String,
    pub pos: monaco::MonacoPosition,
}
#[derive(Debug, Serialize, tapi::Tapi)]
pub struct HoverResult {
    pub span: monaco::MonacoSpan,
    pub contents: Vec<String>,
}

#[tapi::tapi(path = "/hover", method = Post)]
async fn hover(state: State<AppState>, params: Json<HoverParams>) -> Json<Option<HoverResult>> {
    let file = slang::parse_file(&params.file);
    let cx = Context::new(&file);
    let result = match state.hook.hover(
        &cx,
        &file,
        slang::Position {
            column: params.pos.column,
            line: params.pos.line_number,
        },
    ) {
        Some(res) => Some(HoverResult {
            span: monaco::MonacoSpan::from_source_span(&params.file, res.span),
            contents: res.contents,
        }),
        None => None,
    };

    Json(result)
}
