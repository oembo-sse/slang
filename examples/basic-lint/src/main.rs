use slang::{
    ast::{ExprKind, Stmt, StmtKind},
    SourceFile,
};
use slang_ui::prelude::*;

struct App;

impl slang_ui::Hook for App {
    fn analyze(&self, cx: &mut slang_ui::Context, file: &SourceFile) -> slang_ui::Result<()> {
        for m in file.methods() {
            let span = tracing::info_span!("method", name=%m.name);
            let _enter = span.enter();

            if let Some(body) = m.body.clone() {
                assert_true_lint(cx, &body.stmt)
            }
        }

        Ok(())
    }
}
fn assert_true_lint(cx: &mut slang_ui::Context, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Seq(c1, c2) => {
            assert_true_lint(cx, c1);
            assert_true_lint(cx, c2);
        }
        StmtKind::Assert { condition, .. } => {
            if let ExprKind::Bool(true) = &condition.kind {
                cx.info(
                    condition.span,
                    "asserting true is a bit silly, no?".to_string(),
                );
            }
        }
        StmtKind::Match { body } | StmtKind::Loop { body, .. } => {
            for case in &body.cases {
                assert_true_lint(cx, &case.stmt);
            }
        }
        StmtKind::For { body, .. } => {
            assert_true_lint(cx, &body.stmt);
        }

        _ => {}
    }
}

#[tokio::main]
async fn main() {
    slang_ui::run(App).await;
}
