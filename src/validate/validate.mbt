
pub type! ValidationError {
  VariableResolution(VariableResolutionError)
} derive(Show)

pub fn validated(prog: @parse.Program) -> @parse.Program!ValidationError {
  let pass = VariableResolutionPass::new()

  match pass.resolved?(prog) {
    Ok(resolved) => resolved
    Err(error) => raise VariableResolution(error)
  }
}
