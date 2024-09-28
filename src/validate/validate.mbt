
pub type! ValidationError {
  NameResolution(NameResolutionError)
  TypeChecker(TypeCheckerError)
} derive(Show)

pub fn validated(prog: @parse.Program) -> @parse.Program!ValidationError {
  let pass = NameResolutionPass::new()

  match pass.resolved?(prog) {
    Ok(resolved) => {
      let checker = TypeChecker::new()
      match checker.check_program?(resolved) {
        Ok(_) => resolved
        Err(error) => raise TypeChecker(error)
      }
    }
    Err(error) => raise NameResolution(error)
  }
}
