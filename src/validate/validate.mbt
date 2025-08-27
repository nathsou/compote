
pub type! ValidationError {
  NameResolution(NameResolutionError)
  TypeChecker(TypeCheckerError)
  SwitchAnalysis(SwitchResolutionError)
} derive(Show)

pub fn validated(prog: @parse.Program) -> (@parse.Program, Map[String, SymbolTableEntry]) raise ValidationError {
  let resolver = NameResolutionPass::new()

  match resolver.resolved?(prog) {
    Ok(resolved) => {
      let checker = TypeChecker::new()
      match checker.typed_program?(resolved) {
        Ok(typed) => {
          match analyze_switch_stmts?(typed) {
            Ok(switch_analyzed) => (switch_analyzed, checker.symbols)
            Err(error) => raise SwitchAnalysis(error)
          }
        }
        Err(error) => raise TypeChecker(error)
      }
    }
    Err(error) => raise NameResolution(error)
  }
}
