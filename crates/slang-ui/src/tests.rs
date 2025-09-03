use color_eyre::eyre::Context;
use smtlib::Bool;
use tracing_test::traced_test;

struct App;

impl crate::Hook for App {
    fn analyze(&self, cx: &crate::Context, _file: &slang::SourceFile) -> crate::Result<()> {
        let mut solver = cx.solver()?;
        solver.assert(Bool::new(cx.smt_st(), true))?;
        let model = solver.check_sat_with_model().context("getting model")?;
        println!("{:?}", model);
        Ok(())
    }
}

#[test]
#[traced_test]
fn asdf() -> crate::Result<()> {
    Err(crate::test(App, "").error.unwrap())
}
