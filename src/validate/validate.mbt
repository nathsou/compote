pub suberror ValidationError {
  NameResolution(NameResolutionError)
  TypeChecker(TypeCheckerError)
  SwitchAnalysis(SwitchResolutionError)
} derive(Show)

pub fn validated(prog: Program) -> (Program, Map[String, SymbolTableEntry]) raise ValidationError {
  let resolver = NameResolutionPass::new()

  let resolved = try resolver.resolved(prog) catch {
    error => raise NameResolution(error)
  }
  
  let checker = TypeChecker::new()
  let typed = try checker.typed_program(resolved) catch {
    error => raise TypeChecker(error)
  }
  
  let switch_analyzed = try analyze_switch_stmts(typed) catch {
    error => raise SwitchAnalysis(error)
  }
  
  (switch_analyzed, checker.symbols)
}
