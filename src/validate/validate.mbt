
pub type! ValidationError {
  NameResolution(NameResolutionError)
  TypeChecker(TypeCheckerError)
} derive(Show)

pub fn validated(prog: @parse.Program) -> (@parse.Program, Map[String, SymbolTableEntry])!ValidationError {
  let pass = NameResolutionPass::new()

  match pass.resolved?(prog) {
    Ok(resolved) => {
      let checker = TypeChecker::new()
      match checker.typed_program?(resolved) {
        Ok(typed) => (typed, checker.symbols)
        Err(error) => raise TypeChecker(error)
      }
    }
    Err(error) => raise NameResolution(error)
  }
}
